#' mcpredomics: Interpretable Multiclass Classification of Metagenomic Data
#'
#' @description
#' Tools to train, evaluate, and interpret multiclass models for metagenomic data,
#' extending the 'predomics' framework (OvA/OvO strategies, aggregations, plots, and
#' utilities). This file also declares namespace imports used across the package.
#'
#' @docType package
#' @name mcpredomics
#'
#' @import predomics
#' @importFrom gridExtra grid.arrange
#' @importFrom RColorBrewer brewer.pal
#' @importFrom pROC plot.roc ci.auc lines.roc
#' @importFrom stats chisq.test cor median p.adjust predict qt sd
#' @importFrom graphics legend
#' @importFrom grDevices pdf dev.off
#' @importFrom utils combn head str
#' @importFrom dplyr %>% across all_of if_else
NULL
