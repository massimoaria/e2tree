#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <algorithm>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// ============================================================================
// Fast impurity computation for all candidate splits
// y: dissimilarity sub-matrix (n_sub x n_sub) for observations in the node
// S: split matrix (n_sub x p) — binary, 1=left, 0=right
// Returns: numeric vector of length p with impurity for each split
// ============================================================================
// [[Rcpp::export]]
NumericVector compute_impurity_cpp(NumericMatrix y, IntegerMatrix S) {
  int n = y.nrow();
  int p = S.ncol();
  NumericVector imp(p);

  for (int j = 0; j < p; j++) {
    // Count nL and nR
    int nL = 0, nR = 0;
    double sumL = 0.0, sumR = 0.0;

    for (int i = 0; i < n; i++) {
      if (S(i, j) == 1) nL++; else nR++;
    }

    // Both groups must have >= 2 observations for valid impurity
    if (nL < 2 || nR < 2) {
      imp[j] = R_PosInf;
      continue;
    }

    // Compute within-group sums directly (upper triangle only, then double)
    for (int i = 0; i < n - 1; i++) {
      int si = S(i, j);
      for (int k = i + 1; k < n; k++) {
        if (si == S(k, j)) {
          if (si == 1) {
            sumL += y(i, k);
          } else {
            sumR += y(i, k);
          }
        }
      }
    }
    // y is symmetric: full sub-matrix sum = 2 * upper triangle sum
    sumL *= 2.0;
    sumR *= 2.0;

    double sL = sumL / ((double)n * (nL - 1));
    double sR = sumR / ((double)n * (nR - 1));
    imp[j] = sL + sR;
  }

  return imp;
}

// ============================================================================
// Co-occurrence matrix computation (sparse triplet output)
// Returns a list with vectors i, j, x for sparse matrix construction
// ============================================================================
// [[Rcpp::export]]
List compute_cooccurrences_sparse_cpp(
    std::string type,
    DataFrame obs,
    int tree_index,
    int n_total,
    double maxvar = NA_REAL
) {
  int n = obs.nrows();

  IntegerVector node_column = obs[tree_index + 1];
  NumericVector resp = obs["resp"];

  // Group observations by node
  std::unordered_map<int, std::vector<int>> node_groups;
  for (int i = 0; i < n; ++i) {
    node_groups[node_column[i]].push_back(i);
  }

  // Pre-estimate number of triplets for reservation
  size_t est_triplets = 0;
  for (auto& group : node_groups) {
    size_t gs = group.second.size();
    est_triplets += gs * (gs + 1) / 2;
  }

  std::vector<int> ti, tj;
  std::vector<double> tx;
  ti.reserve(est_triplets);
  tj.reserve(est_triplets);
  tx.reserve(est_triplets);

  if (type == "classification") {
    for (auto& group : node_groups) {
      const std::vector<int>& indices = group.second;

      std::unordered_map<double, int> freq_map;
      for (int idx : indices) {
        freq_map[resp[idx]]++;
      }
      int max_freq = 0;
      for (auto& pair : freq_map) {
        if (pair.second > max_freq) max_freq = pair.second;
      }

      double weight = static_cast<double>(max_freq) / indices.size();
      for (size_t j = 0; j < indices.size(); ++j) {
        for (size_t k = j; k < indices.size(); ++k) {
          ti.push_back(indices[j] + 1);  // 1-indexed for R
          tj.push_back(indices[k] + 1);
          tx.push_back(weight);
          if (j != k) {
            ti.push_back(indices[k] + 1);
            tj.push_back(indices[j] + 1);
            tx.push_back(weight);
          }
        }
      }
    }
  } else if (type == "regression") {
    for (auto& group : node_groups) {
      const std::vector<int>& indices = group.second;

      double mean_resp = 0.0;
      for (int idx : indices) mean_resp += resp[idx];
      mean_resp /= indices.size();

      double var_resp = 0.0;
      for (int idx : indices) {
        var_resp += (resp[idx] - mean_resp) * (resp[idx] - mean_resp);
      }
      var_resp /= indices.size();

      double weight = 1.0 - var_resp / (maxvar * indices.size() / n);
      weight = std::max(weight, 0.0);

      for (size_t j = 0; j < indices.size(); ++j) {
        for (size_t k = j; k < indices.size(); ++k) {
          ti.push_back(indices[j] + 1);
          tj.push_back(indices[k] + 1);
          tx.push_back(weight);
          if (j != k) {
            ti.push_back(indices[k] + 1);
            tj.push_back(indices[j] + 1);
            tx.push_back(weight);
          }
        }
      }
    }
  }

  return List::create(
    Named("i") = wrap(ti),
    Named("j") = wrap(tj),
    Named("x") = wrap(tx),
    Named("n") = n
  );
}

// [[Rcpp::export]]
NumericMatrix compute_cooccurrences_cpp(
    std::string type,
    DataFrame obs,
    NumericMatrix w,
    int tree_index,
    double maxvar = NA_REAL
) {
  int n = obs.nrows();
  NumericMatrix co_occurrences(n, n);
  
  IntegerVector node_column = obs[tree_index + 1]; // Colonna dei nodi
  NumericVector resp = obs["resp"];               // Colonna delle risposte
  
  // Raggruppa osservazioni per nodo
  std::unordered_map<int, std::vector<int>> node_groups;
  for (int i = 0; i < n; ++i) {
    node_groups[node_column[i]].push_back(i);
  }
  
  if (type == "classification") {
    // Calcolo per classificazione
    for (auto& group : node_groups) {
      const std::vector<int>& indices = group.second;
      
      // Calcola la frequenza della moda per il gruppo
      std::unordered_map<double, int> freq_map;
      for (int idx : indices) {
        freq_map[resp[idx]]++;
      }
      int max_freq = 0;
      for (auto& pair : freq_map) {
        if (pair.second > max_freq) {
          max_freq = pair.second;
        }
      }
      
      // Assegna i pesi di co-occorrenza
      double weight = static_cast<double>(max_freq) / indices.size();
      for (size_t j = 0; j < indices.size(); ++j) {
        for (size_t k = j; k < indices.size(); ++k) {
          co_occurrences(indices[j], indices[k]) += weight;
          if (j != k) co_occurrences(indices[k], indices[j]) += weight; // Aggiornamento simmetrico
        }
      }
    }
  } else if (type == "regression") {
    // Calcolo per regressione
    for (auto& group : node_groups) {
      const std::vector<int>& indices = group.second;
      
      // Calcola la varianza del gruppo
      double mean_resp = 0.0;
      for (int idx : indices) {
        mean_resp += resp[idx];
      }
      mean_resp /= indices.size();
      
      double var_resp = 0.0;
      for (int idx : indices) {
        var_resp += (resp[idx] - mean_resp) * (resp[idx] - mean_resp);
      }
      var_resp /= indices.size();
      
      // Calcola il peso e assegna co-occorrenze
      double weight = 1.0 - var_resp / (maxvar * indices.size() / n);
      weight = std::max(weight, 0.0);
      
      for (size_t j = 0; j < indices.size(); ++j) {
        for (size_t k = j; k < indices.size(); ++k) {
          co_occurrences(indices[j], indices[k]) += weight;
          if (j != k) co_occurrences(indices[k], indices[j]) += weight; // Aggiornamento simmetrico
        }
      }
    }
  }
  
  return co_occurrences;
}


// ============================================================================
// Process ALL trees in a single C++ call, accumulating into one matrix.
// Avoids R-level foreach loop and per-tree matrix allocation overhead.
// Optionally uses OpenMP for thread-level parallelism.
// ============================================================================
// [[Rcpp::export]]
NumericMatrix compute_all_cooccurrences_cpp(
    std::string type,
    DataFrame obs,
    int ntree,
    int n_cores,
    double maxvar = NA_REAL
) {
  int n = obs.nrows();
  NumericVector resp = obs["resp"];

  // Pre-extract all tree columns as integer vectors
  // obs layout: [OBS(0), Tree1(1), Tree2(2), ..., TreeN(ntree), resp(ntree+1)]
  // Tree columns may be numeric (ranger) or integer (randomForest),
  // so we read as NumericVector and convert to int on the fly.
  std::vector<std::vector<int>> tree_cols(ntree);
  for (int t = 0; t < ntree; t++) {
    NumericVector col = as<NumericVector>(obs[t + 1]);
    tree_cols[t].resize(n);
    for (int i = 0; i < n; i++) {
      tree_cols[t][i] = static_cast<int>(col[i]);
    }
  }

#ifdef _OPENMP
  if (n_cores > 1) {
    // --- Parallel path: each thread accumulates into its own matrix ---
    omp_set_num_threads(n_cores);
    int actual_threads = n_cores;

    // Allocate per-thread accumulators
    std::vector<std::vector<double>> thread_acc(actual_threads,
                                                 std::vector<double>(n * n, 0.0));

    #pragma omp parallel
    {
      int tid = omp_get_thread_num();
      std::vector<double>& local = thread_acc[tid];

      #pragma omp for schedule(dynamic, 4)
      for (int t = 0; t < ntree; t++) {
        const std::vector<int>& node_col = tree_cols[t];

        // Group observations by terminal node
        std::unordered_map<int, std::vector<int>> node_groups;
        for (int i = 0; i < n; ++i) {
          node_groups[node_col[i]].push_back(i);
        }

        if (type == "classification") {
          for (auto& group : node_groups) {
            const std::vector<int>& idx = group.second;
            // Compute modal frequency
            std::unordered_map<double, int> freq_map;
            for (int ii : idx) freq_map[resp[ii]]++;
            int max_freq = 0;
            for (auto& p : freq_map) {
              if (p.second > max_freq) max_freq = p.second;
            }
            double weight = static_cast<double>(max_freq) / idx.size();
            for (size_t j = 0; j < idx.size(); ++j) {
              int rj = idx[j];
              for (size_t k = j; k < idx.size(); ++k) {
                int rk = idx[k];
                local[rj * n + rk] += weight;
                if (j != k) local[rk * n + rj] += weight;
              }
            }
          }
        } else {
          for (auto& group : node_groups) {
            const std::vector<int>& idx = group.second;
            double mean_r = 0.0;
            for (int ii : idx) mean_r += resp[ii];
            mean_r /= idx.size();
            double var_r = 0.0;
            for (int ii : idx) {
              var_r += (resp[ii] - mean_r) * (resp[ii] - mean_r);
            }
            var_r /= idx.size();
            double weight = 1.0 - var_r / (maxvar * idx.size() / n);
            weight = std::max(weight, 0.0);
            for (size_t j = 0; j < idx.size(); ++j) {
              int rj = idx[j];
              for (size_t k = j; k < idx.size(); ++k) {
                int rk = idx[k];
                local[rj * n + rk] += weight;
                if (j != k) local[rk * n + rj] += weight;
              }
            }
          }
        }
      }
    }

    // Merge thread-local accumulators into result
    NumericMatrix result(n, n);
    for (int tid = 0; tid < actual_threads; tid++) {
      const std::vector<double>& local = thread_acc[tid];
      for (int i = 0; i < n * n; i++) {
        result[i] += local[i];
      }
    }
    return result;

  } else
#endif
  {
    // --- Sequential path ---
    NumericMatrix result(n, n);

    for (int t = 0; t < ntree; t++) {
      const std::vector<int>& node_col = tree_cols[t];

      std::unordered_map<int, std::vector<int>> node_groups;
      for (int i = 0; i < n; ++i) {
        node_groups[node_col[i]].push_back(i);
      }

      if (type == "classification") {
        for (auto& group : node_groups) {
          const std::vector<int>& idx = group.second;
          std::unordered_map<double, int> freq_map;
          for (int ii : idx) freq_map[resp[ii]]++;
          int max_freq = 0;
          for (auto& p : freq_map) {
            if (p.second > max_freq) max_freq = p.second;
          }
          double weight = static_cast<double>(max_freq) / idx.size();
          for (size_t j = 0; j < idx.size(); ++j) {
            int rj = idx[j];
            for (size_t k = j; k < idx.size(); ++k) {
              int rk = idx[k];
              result(rj, rk) += weight;
              if (j != k) result(rk, rj) += weight;
            }
          }
        }
      } else {
        for (auto& group : node_groups) {
          const std::vector<int>& idx = group.second;
          double mean_r = 0.0;
          for (int ii : idx) mean_r += resp[ii];
          mean_r /= idx.size();
          double var_r = 0.0;
          for (int ii : idx) {
            var_r += (resp[ii] - mean_r) * (resp[ii] - mean_r);
          }
          var_r /= idx.size();
          double weight = 1.0 - var_r / (maxvar * idx.size() / n);
          weight = std::max(weight, 0.0);
          for (size_t j = 0; j < idx.size(); ++j) {
            int rj = idx[j];
            for (size_t k = j; k < idx.size(); ++k) {
              int rk = idx[k];
              result(rj, rk) += weight;
              if (j != k) result(rk, rj) += weight;
            }
          }
        }
      }
    }
    return result;
  }
}


// ============================================================================
// Compute dissimilarity matrix directly from co-occurrence matrix in C++
// Replaces the R-level outer() + division + subtraction
// dis[i,j] = 1 - a[i,j] / max(a[i,i], a[j,j])
// ============================================================================
// [[Rcpp::export]]
NumericMatrix compute_dissimilarity_from_cooc_cpp(NumericMatrix a) {
  int n = a.nrow();
  NumericMatrix dis(n, n);

  // Extract diagonal
  std::vector<double> diag(n);
  for (int i = 0; i < n; i++) {
    diag[i] = a(i, i);
  }

#ifdef _OPENMP
  #pragma omp parallel for schedule(static)
#endif
  for (int i = 0; i < n; i++) {
    dis(i, i) = 0.0;  // self-dissimilarity = 0
    for (int j = i + 1; j < n; j++) {
      double denom = std::max(diag[i], diag[j]);
      double val = (denom > 0.0) ? (1.0 - a(i, j) / denom) : 1.0;
      dis(i, j) = val;
      dis(j, i) = val;
    }
  }

  return dis;
}