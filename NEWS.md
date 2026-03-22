# e2tree (development version)

## S3 class system overhaul

- `e2tree` class now properly listed as first in the class vector (`c("e2tree", "list")`).
- New S3 methods for `e2tree`: `predict()`, `fitted()`, `residuals()`.
- `predict.e2tree()` replaces `ePredTree()` as the standard prediction interface.
  For regression, returns a data frame with `fit` and `sd` (node-level standard deviation).
  For classification, returns a data frame with `fit`, `accuracy`, and `score`.
- `fitted.e2tree()` returns fitted values for training data.
- `residuals.e2tree()` returns residuals for regression E2Trees.
- `methods(class = "e2tree")` now shows: `as.rpart`, `e2splits`, `fitted`, `nodes`, `plot`, `predict`, `print`, `residuals`, `summary`.
- `methods(class = "eValidation")` now shows: `measures`, `plot`, `print`, `proximity`, `summary`.

## Accessor functions (new)

- `nodes()`: Extract tree node data frame from an `e2tree` object, with optional `terminal` filter.
- `e2splits()`: Extract split and categorical split information.
- `measures()`: Extract validation measures from an `eValidation` object.
- `proximity()`: Extract proximity matrices (ensemble, e2tree, or both) from an `eValidation` object.

## Coercion methods (new)

- `as.rpart()`: Generic and method for converting `e2tree` to `rpart` format.
- `as.party()`: Method for converting `e2tree` to `partykit`'s `constparty` format (registered conditionally when partykit is installed). Produces proper bar plots in terminal nodes for classification trees.
- `rpart2Tree()` retained for backward compatibility with a deprecation note.

## Validation framework improvements

- `eValidation()` gains a `test` argument: `"mantel"` (Mantel test only), `"measures"` (divergence/similarity measures only), or `"both"` (default). This allows choosing between association testing and agreement testing.
- `print.eValidation()`, `summary.eValidation()`, and `plot.eValidation()` updated to handle all three test modes gracefully.

## Variable importance improvements

- `vimp()`: Auto-detects classification/regression from the `e2tree` object; `type` argument now optional.
- Fixed incorrect y-axis label ("Variance" instead of "Variable") in variable importance plots.
- Regression variable importance bars now sorted by importance (previously unsorted).
- Consistent column naming (`Variable`, `MeanImpurityDecrease`) across classification and regression.
- Internal logic refactored into `.vimp_classification()` and `.vimp_regression()`.

## Documentation

- All man page titles standardized to Title Case.
- `\dontrun{}` replaced with `\donttest{}` in all examples; interactive-only examples wrapped in `if (interactive())`.
- Examples updated to use accessor functions and `predict()` instead of direct `$` access.
- New vignette `e2tree-introduction` covering classification, regression, validation (Mantel test, divergence measures, LoI decomposition), and comparison with partykit/stablelearner.
- `ePredTree()` documentation updated with deprecation note pointing to `predict.e2tree()`.

## Package infrastructure

- Added `partykit`, `knitr`, `rmarkdown` to Suggests.
- Added `VignetteBuilder: knitr` to DESCRIPTION.
- `e2tree` object now stores `fitted.values`, `y`, and `data` for S3 method support.
- Conditional `.onLoad` hook for registering `as.party.e2tree` when partykit is loaded.

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
- Refactored `eValidation`, `eImpurity`, and `eStoppingRules` for consistency and performance.

# e2tree 0.2.0

- Added support for 'ranger' models
- Several improvements in e2tree plots

# e2tree 0.1.2

# e2tree 0.1.1

# e2tree 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
