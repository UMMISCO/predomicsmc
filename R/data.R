#-----------------------------------------------------
# DATASETS FOR MULTICLASS PREDOMICS ANALYSIS
#-----------------------------------------------------

#' mc.input
#'
#' Formatted input data for multiclass Predomics analysis, based on bacterial species frequencies.
#'
#' This dataset includes a sample of 894 individuals classified into four enterotypes:
#' \itemize{
#'   \item \code{Rum}: 183 individuals
#'   \item \code{Bact1}: 299 individuals
#'   \item \code{Bact2}: 168 individuals
#'   \item \code{Prev}: 244 individuals
#' }
#'
#' @docType data
#' @name mc.input
#' @usage data(mc.input)
#' @format A list with:
#' \describe{
#'   \item{\code{X}}{A matrix of species abundances (features × samples)}
#'   \item{\code{y.df}}{A data.frame with the corresponding enterotype labels}
#' }
#' @keywords microbiome, enterotypes, species, METACARDIS
#' @author Edi Prifti, Eugeni Belda, Fabien Kambu
NULL

#' Analysis_Dataset_Complet_Sota_Predo
#'
#' Comprehensive dataset containing the results of all experiments conducted using Predomics and state-of-the-art (SOTA) methods.
#'
#' This \code{data.frame} provides a structured summary of model performances across different configurations (OvA, OvO, constraints), datasets, and evaluation metrics. It is used for visualization (e.g., bar plots, forest plots) and statistical comparison (e.g., linear mixed models).
#'
#' @docType data
#' @name Analysis_Dataset_Complet_Sota_Predo
#' @usage data(Analysis_Dataset_Complet_Sota_Predo)
#' @format A \code{data.frame} with multiple rows and columns, including:
#' \describe{
#'   \item{\code{dataset}}{Name of the evaluated dataset}
#'   \item{\code{method}}{Method used (Predomics or SOTA)}
#'   \item{\code{binarisation}}{Binarization strategy (e.g., "Terbeam")}
#'   \item{\code{aggregation}}{Aggregation method (e.g., Tie, majority, max)}
#'   \item{\code{constraint}}{Model constraint type (e.g., semi-constrained, etc)}
#'   \item{\code{accuracy, recall, etc.}}{Performance metrics train/test}
#' }
#' @keywords predomics, sota, benchmark, model, performance
#' @author Edi Prifti, Eugeni Belda, Fabien Kambu
NULL

#' predomics.inputs.ExperimentHubMulticlass
#'
#' Collection of real multiclass microbiome datasets from ExperimentHub, pre-processed for multiclass Predomics.
#'
#' This object is a named list of 16 real-world datasets. Each sublist includes:
#' \describe{
#'   \item{\code{X}}{A binary matrix (features × samples), indicating presence/absence}
#'   \item{\code{y.df}}{A data.frame with the multiclass target variable \code{study_condition}}
#' }
#'
#' Examples:
#' \itemize{
#'   \item \strong{FengQ_2015} — 153 samples: 47 adenoma, 60 control, 46 CRC
#'   \item \strong{KarlssonFH_2013} — 142 samples: 42 control, 48 IGT, 52 T2D
#'   \item Other datasets: GhensiP_2019, HMP_2019_t2d, HanniganGD_2017, IaniroG_2022, LiJ_2014, LiJ_2017,
#'   LiSS_2016, LoombaR_2017, MetaCardis_2020_a, ThomasAM_2018a, VatanenT_2016,
#'   XieH_2016, YachidaS_2019, ZellerG_2014
#' }
#'
#' All datasets are formatted for use in interpretable multiclass classification with Predomics.
#'
#' @docType data
#' @name predomics.inputs.ExperimentHubMulticlass
#' @usage data(predomics.inputs.ExperimentHubMulticlass)
#' @format A named list of 16 datasets. Each item is a list with:
#' \describe{
#'   \item{\code{X}}{Binary matrix of microbiome profiles}
#'   \item{\code{y.df}}{Data frame with target labels (\code{study_condition})}
#' }
#' @keywords microbiome, multiclass, classification, ExperimentHub
#' @author Edi Prifti, Eugeni Belda, Fabien Kambu
NULL


