# e2tree 1.0.0

## New functions

- `goi()`: Goodness of Interpretability (GoI) index measuring how well the E2Tree-estimated proximity matrix reconstructs the original ensemble proximity matrix.
- `goi_perm()`: Permutation test for the GoI index to assess statistical significance.
- `goi_analysis()`: Combined GoI analysis returning both the observed statistic and permutation results.
- `plot.goi_perm()`: Plot method for `goi_perm` objects displaying the permutation distribution.
- `plot_e2tree_vis()`: Interactive E2Tree visualization using `visNetwork` with draggable nodes, zoom/pan, and multiple layout options.
- `plot_e2tree_click()`: Interactive E2Tree plot in the R graphics device with click-to-inspect node details.
- `save_e2tree_html()`: Save an interactive `visNetwork` tree plot as a standalone HTML file.
- `print_e2tree_summary()`: Print a formatted summary of an e2tree object.

## Performance improvements

- `createDisMatrix`: C++ backend (`CoOccurrences.cpp`) with OpenMP thread-level parallelism replaces the R-level `foreach`/`doParallel` loop; co-occurrence normalization also moved to C++.
- `e2tree`: vectorized `Wt` computation using `vapply`; simplified internal `get_classes` helper.
- `ePredTree`: split rules are now pre-parsed once (`parse_all_splits` + `apply_split_rule`), eliminating repeated `regex`/`eval(parse())` calls during prediction.
- `vimp`: three `group_by` operations consolidated into one; `eval`/`parse` removed.
- `split`: `ordSplit` and `catSplit` vectorized with `outer()`.
- `eImpurity`: single-step integer matrix conversion.

## Bug fixes

- Fixed Rcpp type conversion (`NumericVector` + `static_cast<int>`) for ranger compatibility (ranger returns double columns for tree node assignments).
- Fixed `ePredTree` returning character instead of double for regression trees.
- Fixed bare `filter()` call in `proximity_longer.R` causing namespace conflict with `dplyr`.
- Added missing `NAMESPACE` imports for `graphics`, `grDevices`, and `utils`.

## Internal changes

- New `aaa_utils.R` with shared internal helpers: `e2_variance()`, `get_ensemble_type()`, `check_package()`.
- Refactored `eComparison`, `eImpurity`, and `eStoppingRules` for consistency and performance.

# e2tree 0.2.0

- Added support for 'ranger' models
- Several improvements in e2tree plots

# e2tree 0.1.2

# e2tree 0.1.1

# e2tree 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
