utils::globalVariables(".")
#' @import stats
#' @import doParallel
#' @import parallel
#' @import ggplot2
#' @import future.apply
#' @import purrr
#' @import partitions
#' @importFrom Rcpp sourceCpp evalCpp
#' @import RSpectra
#' @importFrom randomForest randomForest 
#' @importFrom ranger ranger 
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom dplyr %>%
#' @importFrom dplyr if_else
#' @importFrom dplyr distinct
#' @importFrom dplyr pick
#' @importFrom dplyr row_number
#' @importFrom dplyr across
#' @importFrom dplyr tibble
#' @importFrom dplyr as_tibble
#' @importFrom dplyr first
#' @importFrom dplyr arrange
#' @importFrom dplyr do
#' @importFrom dplyr n
#' @importFrom dplyr count
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr top_n
#' @importFrom dplyr relocate
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_tail
#' @importFrom dplyr starts_with
#' @importFrom dplyr group_by_at
#' @importFrom tidyr complete
#' @importFrom tidyr replace_na
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom Matrix %&%
#' @importFrom Matrix all.equal
#' @importFrom Matrix as.array
#' @importFrom Matrix as.matrix
#' @importFrom Matrix Matrix
#' @importFrom Matrix rowSums
#' @importFrom Matrix sparseMatrix
#' @importFrom rpart.plot rpart.plot
#' @importFrom grDevices colorRampPalette
#' @importFrom utils globalVariables setTxtProgressBar tail txtProgressBar
#' @importFrom ape mantel.test
#' @useDynLib e2tree
#'
.onAttach<-function(...){
packageStartupMessage("Explainable Ensemble Trees (E2Tree) \n\n",
  "If you use e2tree in research, please cite: \n",
                      "Aria, M., Gnasso, A., Iorio, C., & Pandolfo, G. (2024). Explainable ensemble trees.\nComputational Statistics, 39(1), 3-19. DOI: 10.1007/s00180-022-01312-6")
}
