#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;

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