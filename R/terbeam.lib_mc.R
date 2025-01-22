################################################################
#  _____       _                   ____            _           #
# |_   _|     | |                 / __ \          (_)          #
#   | |  _ __ | |_ ___  __ _ _ __| |  | |_ __ ___  _  ___ ___  #
#   | | | '_ \| __/ _ \/ _` | '__| |  | | '_ ` _ \| |/ __/ __| #
#   | |_| | | | ||  __| (_| | |  | |__| | | | | | | | (__\__ \ #
# |_____|_| |_|\__\___|\__, |_|   \____/|_| |_| |_|_|\___|___/ #
#                       __/ |                                  #
#                      |___/                                   #
################################################################

################################################################
# @script: terbeam.lib_mc.R
# @author: Fabien Kambu Mbuangi
# @author: Edi Prifti
# @date: November 2023
################################################################

#' @title generateAllSingleFeatureModel_mc
#' @description This function generate All Single Feature Models for Multiclass Classificatio.
#' @param X A matrix of features, where columns correspond to samples.
#' @param y A vector of labels for the samples.
#' @param clf A classifier object with its parameters and coefficients.
#' @param ind.sub A list of subsets of indices for samples (default: NULL, which uses all samples).
#' @param eval.all A logical flag indicating whether to evaluate all models (default: FALSE).
#' @param approch A string indicating the approach for decomposition: "ovo" (default) or "ova".
#' @return A list of models generated based on the specified approach.
#'
generateAllSingleFeatureModel_mc <- function(X, y, clf, ind.sub = NULL, eval.all = FALSE, approch = "ovo") {


  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X

  # Validate or initialize sample subsets
  if (!is.null(ind.sub[[1]])) {
    for (j in 1:length(list_X)) {
      # Ensure the indices match the dataset
      if (any(is.na(match(ind.sub[[j]], seq_len(nrow(list_X[[j]])))))) {
        stop("generateAllSingleFeatureModel_mc: index does not match with X")
      }
    }
  } else {
    ind.sub <- seq_len(nrow(X))  # Default to all samples
  }

  # Generate models based on the sign of coefficients
  if (clf$params$testAllSigns) {
    listOfVecPos <- as.list(ind.sub)

    # Generate models for positive coefficients
    clfPos <- clf
    clfPos$coeffs <- abs(clfPos$coeffs)
    popPos <- listOfSparseVecToListOfModels_mc(X, y, clfPos, v = listOfVecPos, approch = approch)

    # Generate models for negative coefficients
    listOfVecNeg <- as.list(ind.sub)
    clfNeg <- clf
    clfNeg$coeffs <- -abs(clfNeg$coeffs)
    popNeg <- listOfSparseVecToListOfModels_mc(X, y, clfNeg, v = listOfVecNeg, approch = approch)

    # Combine models
    pop <- c(popPos, popNeg)
  } else {
    # Generate models without considering sign of coefficients
    listOfVec <- as.list(ind.sub)
    pop <- listOfSparseVecToListOfModels_mc(X, y, clf, v = listOfVec, approch = approch)
  }

  # Return the generated models
  return(pop)
}


#' Generate All Combinations of Features for Multiclass Classification
#'
#' @title generateAllCombinations_mc
#' @description This function generates all possible combinations of features based on a specified sparsity level
#' @param X A matrix of features, where columns represent samples.
#' @param y A vector of labels for the samples.
#' @param clf A classifier object containing its parameters and coefficients.
#' @param ind.features.to.keep A list of feature indices to be considered for combinations.
#' @param sparsity The number of features to include in each combination.
#' @param allFeatures A logical flag indicating whether all features should be included.
#' @param approch The approach to use for multiclass classification (default: "ovo").
#' @importFrom gtools combinations
#' @return A list of models generated from all possible combinations of features.
#'
generateAllCombinations_mc <- function(X, y, clf, ind.features.to.keep, sparsity, allFeatures, approch = "ovo") {

  # Initialize a list to store feature combinations
  pop_vec <- list()

  # Generate all combinations of features for each feature group
  for (j in 1:(length(ind.features.to.keep))) {
    pop_vec[[j]] <- combinations(
      n = length(ind.features.to.keep[[j]]),
      r = sparsity,
      v = ind.features.to.keep[[j]]
    )
  }

  # Filter combinations if the classifier is using "ratio" language and "auc" objective
  if (clf$params$language == "ratio" & clf$params$objective == "auc") {
    for (j in 1:length(ind.features.to.keep)) {
      popvec <- pop_vec[[j]]
      ind <- (apply(popvec, 1, min) < length(ind.features.to.keep[[j]]) / 2) &
        (apply(popvec, 1, max) > length(ind.features.to.keep[[j]]) / 2)
      pop_vec[[j]] <- popvec[ind, ]
    }
  }

  # Return NULL if no valid combinations are found
  if (all(class(pop_vec[[1]]) != "matrix")) {
    return(NULL)
  }

  # Transform combinations matrix into a list of sparse vector objects
  for (j in 1:length(pop_vec)) {
    popvec <- pop_vec[[j]]
    pop_vec[[j]] <- lapply(seq_len(nrow(popvec)), function(i, popvec) popvec[i, ], popvec)
  }

  # Create a list of indices for each combination
  list_indices <- list()
  for (i in 1:length(pop_vec[[1]])) {
    list_indices[[i]] <- list()
    for (j in 1:length(pop_vec)) {
      if (i <= length(pop_vec[[j]])) {
        list_indices[[i]][[j]] <- pop_vec[[j]][[i]]
      } else {
        list_indices[[i]][[j]] <- list_indices[[i - 1]][[j]]
      }
    }
  }

  # Update the population vector with the generated indices
  pop_vec <- list_indices

  # Transform the sparse vector objects into predictive model objects
  pop <- listOfSparseVecToListOfModels_mc(X, y, clf, pop_vec, approch = approch)

  # Return the generated models
  return(pop)
}


#' Count Each Feature's Appearance in a Multiclass Population
#' @title countEachFeatureApperance_mc
#' @description This function counts the frequency of feature appearances in a population,including their occurrences in the "best" and "very best" populations. It also evaluates the associated coefficients for each feature.
#' @param clf A classifier object, containing model coefficients.
#' @param allFeatures A logical flag indicating whether all features are considered.
#' @param pop The entire population of models or features.
#' @param best The subset of the population representing the "best" models or features.
#' @param veryBest The subset of the population representing the "very best" models or features.
#' @param approch The approach for multiclass classification (default: "ovo").
#' @return A list of data frames, where each data frame contains the number of appearances of
#' each feature in the "best" and "very best" populations, their normalized frequencies,
#' and their associated coefficients.
#'
countEachFeatureApperance_mc <- function(clf, allFeatures, pop, best, veryBest, approch = "ovo") {

  # If the population is invalid, return NULL
  if (!isPopulation(pop)) {
    return(NULL)
  }

  # Initialize lists for storing results
  feat.coeff <- list()
  list_res <- list()
  list_coeff <- clf$coeffs_

  # Get unique features in different populations
  featuresInPop <- unique(
    populationGet_X(element2get = "names_", toVec = TRUE, na.rm = TRUE)(pop = pop)
  )
  featuresInBest <- unique(
    populationGet_X(element2get = "names_", toVec = TRUE, na.rm = TRUE)(pop = best)
  )
  featuresInVeryBest <- unique(
    populationGet_X(element2get = "names_", toVec = TRUE, na.rm = TRUE)(pop = veryBest)
  )

  # Count occurrences of features in "best" and "very best" populations
  nbInBest <- sapply(
    featuresInPop,
    function(x, featuresInBest) sum(x == featuresInBest),
    featuresInBest
  )
  nbInVeryBest <- sapply(
    featuresInPop,
    function(x, featuresInVeryBest) sum(x == featuresInVeryBest),
    featuresInVeryBest
  )

  # Normalize the frequency of features in the "best" population
  freqInBest <- nbInBest / max(nbInBest, na.rm = TRUE)

  # Iterate over coefficients and compute metrics for each feature
  for (i in 1:length(list_coeff)) {
    clf$coeffs_ <- list_coeff[[i]]
    feat.coeff <- clf$coeffs_[featuresInPop]

    # Create a data frame for the results
    res <- data.frame(
      nbInBest = nbInBest,
      nbInVeryBest = nbInVeryBest,
      freqInBest = freqInBest,
      feat.coeff = feat.coeff
    )

    # Sort the results by occurrences in "very best" and "best" populations
    res <- res[order(res[, "nbInVeryBest"], res[, "nbInBest"], decreasing = TRUE), ]
    list_res[[i]] <- res
  }

  # Return the list of results
  res <- list_res
  return(res)
}


#' Get Features to Keep in Multiclass Classification
#' @title getFeatures2Keep_mc
#' @description This function determines which features should be retained based on their frequency of appearance in the "best" models and their associated coefficients. It applies a threshold to filter out less relevant features
#' @param clf A classifier object containing model parameters.
#' @param featuresApperance A data frame containing information about feature appearances,
#' including their frequency and coefficients.
#' @param threshold A numeric value specifying the minimum frequency required to retain a feature
#' (default: 0.01).
#' @param approch The approach for multiclass classification (default: "ovo").
#' @return A list of features to keep for further processing.

getFeatures2Keep_mc <- function(clf, featuresApperance, threshold = 0.01, approch = "ovo") {

  # If the features appearance data is NULL, return NULL
  if (is.null(featuresApperance)) {
    return(NULL)
  }

  # Initialize lists to store results
  list_tokeep <- list()
  list_featuresApperance <- list()
  passerel <- list()
  passerel <- featuresApperance

  # Iterate over each entry in the features appearance data
  for (i in 1:length(passerel)) {
    featuresApperance <- passerel[[i]]

    # Handle the specific case for ratio language and AUC objective
    if (clf$params$language == "ratio" & clf$params$objective == "auc") {

      # Separate features with positive and negative coefficients
      fa.pos <- featuresApperance[featuresApperance$feat.coeff == "1", ]
      fa.neg <- featuresApperance[featuresApperance$feat.coeff == "-1", ]

      # Determine features to keep for positive coefficients
      tookeep.pos <- fa.pos$nbInVeryBest != 0 | fa.pos$freqInBest > threshold
      mask <- rep(FALSE, nrow(fa.pos))
      mask[1:(clf$params$nbBest / 2)] <- TRUE
      tookeep.pos <- tookeep.pos | mask

      # Determine features to keep for negative coefficients
      tookeep.neg <- fa.neg$nbInVeryBest != 0 | fa.neg$freqInBest > threshold
      mask <- rep(FALSE, nrow(fa.neg))
      mask[1:(clf$params$nbBest / 2)] <- TRUE
      tookeep.neg <- tookeep.neg | mask

      # Combine positive and negative features
      tokeep <- c(rownames(fa.pos)[tookeep.pos], rownames(fa.neg)[tookeep.neg])

    } else {
      # General case: keep features based on frequency and best appearances
      tokeep <- (featuresApperance[, "nbInVeryBest"] != 0 | featuresApperance[, "freqInBest"] > threshold)
      tokeep <- rownames(featuresApperance[tokeep, ])
    }

    # Store the features to keep in the result list
    list_tokeep[[i]] <- tokeep
  }

  # Return the final list of features to keep
  tokeep <- list_tokeep
  return(tokeep)
}



