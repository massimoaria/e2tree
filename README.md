
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/massimoaria/e2tree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/massimoaria/e2tree/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Setup

You can install the developer version of e2tree from
[GitHub](https://github.com) with:

``` r
install.packages("remotes")
remotes::install_github("massimoaria/e2tree")
```

``` r
require(e2tree)
require(tidyverse)
options(dplyr.summarise.inform = FALSE)
require(randomForest)
require(Matrix)
require(future.apply)
```

## Warnings

The package is still under development and therefore, for the time
being, there are the following limitations:

- Only ensembles trained with the randomforest package are supported.
  Additional packages and approaches will be supported in the future;

- Currently e2tree works only in the case ofu classification problems.
  It will gradually be extended to other problems related to the nature
  of the response variable: regression, counting, multivariate response,
  etc.

## Example 1: IRIS dataset

In this example, we want to show the main functions of the e2tree
package.

Starting from the IRIS dataset, we will train an ensemble tree using the
randomforest package and then subsequently use e2tree to obtain an
explainable tree synthesis of the ensemble classifier.

``` r
# Set random seed to make results reproducible:
set.seed(0)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)
# Assign the data to the correct sets
training <- iris[indexes,]
validation <- iris[-indexes,]
response_training <- training[,5]
response_validation <- validation[,5]
```

Train an Random Forest model with 1000 weak learners

``` r
# Perform training:
rf = randomForest(Species ~ ., data=training, ntree=1000, mtry=2, importance=TRUE, keep.inbag = TRUE, proximity=T)
```

Here, we create the dissimilarity matrix between observations through
the createDisMatrix function

``` r
D <- createDisMatrix(rf, data=training)
#> 
#> Analized  100  trees
#> Analized  200  trees
#> Analized  300  trees
#> Analized  400  trees
#> Analized  500  trees
#> Analized  600  trees
#> Analized  700  trees
#> Analized  800  trees
#> Analized  900  trees
#> Analized  1000  trees
#dis <- 1-rf$proximity
```

setting e2tree parameters

``` r
setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)
```

Costruisco l’albero sintesi del RF

``` r
tree <- e2tree(D, training[,-5], response_training, setting)
#> [1] 1
#> [1] 2
#> [1] 3
#> [1] 6
#> [1] 13
#> [1] 12
#> [1] 7
```

Un’occhiata all’oggetto info1

``` r
tree %>% knitr::kable()
```

| node |   n | pred       | prob              |  impTotal | impChildren |    decImp | decImpSur | variable     | split | splitLabel          | variableSur  | splitLabelSur       | parent | children | terminal | obs                                                                                                                                                                                                                                                                                               | path                                                             | pred_val |
|-----:|----:|:-----------|:------------------|----------:|------------:|----------:|----------:|:-------------|------:|:--------------------|:-------------|:--------------------|-------:|:---------|:---------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------|---------:|
|    1 |  75 | setosa     | 0.386666666666667 | 0.6989521 |   0.3628642 | 0.3360879 | 0.2496373 | Petal.Length |    53 | Petal.Length \<=1.9 | Petal.Width  | Petal.Width \<=0.6  |      0 | 2, 3     | FALSE    | 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75 |                                                                  |        1 |
|    2 |  29 | setosa     | 1                 | 0.0162866 |          NA |        NA |        NA | NA           |    NA | NA                  | NA           | NA                  |      1 | NA       | TRUE     | 4, 5, 8, 11, 14, 17, 21, 23, 26, 27, 29, 30, 32, 35, 37, 39, 42, 43, 44, 46, 47, 48, 49, 57, 60, 62, 64, 72, 73                                                                                                                                                                                   | Petal.Length \<=1.9                                              |        1 |
|    3 |  46 | virginica  | 0.58695652173913  | 0.5813588 |   0.2091960 | 0.3721627 | 0.3301853 | Petal.Width  |    92 | Petal.Width \<=1.7  | Petal.Length | Petal.Length \<=4.7 |      1 | 6, 7     | FALSE    | 1, 2, 3, 6, 7, 9, 10, 12, 13, 15, 16, 18, 19, 20, 22, 24, 25, 28, 31, 33, 34, 36, 38, 40, 41, 45, 50, 51, 52, 53, 54, 55, 56, 58, 59, 61, 63, 65, 66, 67, 68, 69, 70, 71, 74, 75                                                                                                                  | !Petal.Length \<=1.9                                             |        3 |
|    6 |  20 | versicolor | 0.95              | 0.3126219 |   0.2283824 | 0.0842395 | 0.0226385 | Petal.Length |    64 | Petal.Length \<=4.7 | Petal.Width  | Petal.Width \<=1.5  |      3 | 12, 13   | FALSE    | 2, 6, 7, 10, 12, 13, 20, 22, 24, 33, 50, 51, 54, 55, 56, 61, 65, 68, 69, 75                                                                                                                                                                                                                       | !Petal.Length \<=1.9 & Petal.Width \<=1.7                        |        2 |
|    7 |  26 | virginica  | 1                 | 0.1296377 |          NA |        NA |        NA | NA           |    NA | NA                  | NA           | NA                  |      3 | NA       | TRUE     | 1, 3, 9, 15, 16, 18, 19, 25, 28, 31, 34, 36, 38, 40, 41, 45, 52, 53, 58, 59, 63, 66, 67, 70, 71, 74                                                                                                                                                                                               | !Petal.Length \<=1.9 & !Petal.Width \<=1.7                       |        3 |
|   12 |  16 | versicolor | 1                 | 0.1525959 |          NA |        NA |        NA | NA           |    NA | NA                  | NA           | NA                  |      6 | NA       | TRUE     | 2, 6, 7, 10, 13, 20, 24, 33, 51, 54, 55, 56, 61, 65, 68, 75                                                                                                                                                                                                                                       | !Petal.Length \<=1.9 & Petal.Width \<=1.7 & Petal.Length \<=4.7  |        2 |
|   13 |   4 | versicolor | 0.75              | 0.5315285 |          NA |        NA |        NA | NA           |    NA | NA                  | NA           | NA                  |      6 | NA       | TRUE     | 12, 22, 50, 69                                                                                                                                                                                                                                                                                    | !Petal.Length \<=1.9 & Petal.Width \<=1.7 & !Petal.Length \<=4.7 |        2 |

Predizione con il nuovo albero (esempio sul training)

``` r
pred <- ePredTree(tree, training[,-5], target="virginica")
#> [1] 1
#> [1] 2
#> [1] 3
#> [1] 4
```

Comparazione tra le predizioni (campione training) di RF e eTree

``` r
table(pred$fit,rf$predicted)
#>             
#>              setosa versicolor virginica
#>   setosa         29          0         0
#>   versicolor      0         18         2
#>   virginica       0          0        26
```

Comparazione tra le predizioni (campione training) di RF e risposta
corretta

``` r
table(rf$predicted, response_training)
#>             response_training
#>              setosa versicolor virginica
#>   setosa         29          0         0
#>   versicolor      0         17         1
#>   virginica       0          2        26
```

Comparazione tra le predizioni (campione training) di eTree e risposta
corretta

``` r
table(pred$fit,response_training)
#>             response_training
#>              setosa versicolor virginica
#>   setosa         29          0         0
#>   versicolor      0         19         1
#>   virginica       0          0        26
```

Variable Importance

``` r
rfimp <- rf$importance %>% as.data.frame %>% 
  mutate(Variable = rownames(rf$importance),
         RF_Var_Imp = round(MeanDecreaseAccuracy,2)) %>% 
  select(Variable, RF_Var_Imp)

V <- vimp(tree, response_training, training[,-5])
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-14-2.png" width="100%" />

``` r
V <- V %>% select(.data$Variable, .data$MeanImpurityDecrease, .data$`ImpDec_ setosa`, .data$`ImpDec_ versicolor`,.data$`ImpDec_ virginica`) %>% 
  mutate_at(c("MeanImpurityDecrease","ImpDec_ setosa", "ImpDec_ versicolor","ImpDec_ virginica"), round,2) %>% 
  left_join(rfimp, by = "Variable") %>% 
  select(Variable, RF_Var_Imp, MeanImpurityDecrease, starts_with("ImpDec")) %>% 
  rename(ETree_Var_Imp = MeanImpurityDecrease)

V
#> # A tibble: 2 × 6
#>   Variable     RF_Var_Imp ETree_Var_Imp `ImpDec_ setosa` `ImpDec_ versicolor`
#>   <chr>             <dbl>         <dbl>            <dbl>                <dbl>
#> 1 Petal.Length       0.27          0.36             0.34                 0.02
#> 2 Petal.Width        0.34          0.23            NA                   NA   
#> # … with 1 more variable: `ImpDec_ virginica` <dbl>
```

Comparazione con il campione di validazione

``` r
rf.pred <- predict(rf, validation[,-5], proximity = TRUE)

pred_val<- ePredTree(tree, validation[,-5], target="virginica")
#> [1] 1
#> [1] 2
#> [1] 3
#> [1] 4
```

Comparazione tra le predizioni (campione validation) di RF e eTree

``` r
table(pred_val$fit,rf.pred$predicted)
#>             
#>              setosa versicolor virginica
#>   setosa         21          0         0
#>   versicolor      0         34         0
#>   virginica       0          0        20
```

Comparazione tra le predizioni (campione validation) di RF e risposta
corretta

``` r
table(rf.pred$predicted, response_validation)
#>             response_validation
#>              setosa versicolor virginica
#>   setosa         21          0         0
#>   versicolor      0         30         4
#>   virginica       0          1        19
rf.prob <- predict(rf, validation[,-5], proximity = TRUE, type="prob")
roc_rf <- roc(response_validation,rf.prob$predicted[,"virginica"],target="virginica")
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

``` r
roc_rf$auc
#> [1] 0.9873725
```

Comparazione tra le predizioni (campione validation) di eTree e risposta
corretta

``` r
table(pred_val$fit,response_validation)
#>             response_validation
#>              setosa versicolor virginica
#>   setosa         21          0         0
#>   versicolor      0         30         4
#>   virginica       0          1        19
roc_res <- roc(response_validation,pred_val$score,target="virginica")
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="100%" />

``` r
roc_res$auc
#> [1] 0.96395
```
