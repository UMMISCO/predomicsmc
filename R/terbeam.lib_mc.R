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
generateAllSingleFeatureModel_mc <- function(X, y, clf, ind.sub = NULL, eval.all = FALSE, approch ="ovo")
{
  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  # Dataset decomposition phase using the one-versus-one and one-versus-all approaches
  if (approch == "ovo") {
    k <- 1
    for (i in 1:(length(nClasse)-1)) {
      for (j in (i+1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)
        y_pair <- y[indices]
        X_pair <- X[, indices]
        list_y[[k]] <- as.vector(y_pair)
        list_X[[k]] <- X_pair
        k <- k + 1
      }
    }
  } else {
    for (i in 1:length(nClasse)) {
      class_i <- nClasse[i]
      y_temp <- ifelse(y == class_i, as.character(class_i), "All")
      list_y[[i]] <- as.vector(y_temp)
      list_X[[i]] <- X
    }
  }
  # if we wish to focus on a subset
  if(!is.null(ind.sub[[1]]))
  {
    for(j in 1:(length(list_X))){
    # sanity check
    if(any(is.na(match(ind.sub[[j]], seq_len(nrow(list_X[[j]]))))))
    {
      stop("generateAllSingleFeatureModel_ovo: index does not match with X")
    }
    }

  }else
  {
    # otherwise take all
    ind.sub <- seq_len(nrow(X))
  }

  # if we want to generate all the models
  if(clf$params$testAllSigns)
  {
    listOfVecPos <- as.list(ind.sub)
    clfPos <- clf
    clf$coeffs <- abs(clf$coeffs)
    popPos <- listOfSparseVecToListOfModels_mc(X, y, clfPos, v = listOfVecPos,approch = approch)

    listOfVecNeg <- as.list(ind.sub)
    clfNeg <- clf
    clf$coeffs <- -abs(clf$coeffs)
    popNeg <- listOfSparseVecToListOfModels_mc(X, y, clgNeg, v = listOfVecNeg, approch = approch)

    pop <- c(popPos, popNeg)

  } else
  {
    listOfVec <- as.list(ind.sub)
    pop <- listOfSparseVecToListOfModels_mc(X, y, clf, v = listOfVec, approch = approch)
  }

  # return the population
  return(pop)
}



#' Generate every possible combination of a list of features and evaluate them
#'
#' @title generateAllCombinations
#' @importFrom  gtools combinations
generateAllCombinations_mc <- function(X, y, clf, ind.features.to.keep, sparsity, allFeatures,approch=approch)
{
  # Generate a matrix containing every combination of size sparsity of the
  # features contained in ind.features.to.keep
  pop_vec <- list()
  for(j in 1:(length(ind.features.to.keep))){
  pop_vec[[j]] <- combinations(n = length(ind.features.to.keep[[j]]), r = sparsity, v = ind.features.to.keep[[j]])
  }
  if(clf$params$language == "ratio" & clf$params$objective == "auc")
  {
    for(j in 1:(ind.features.to.keep)){
    popvec = pop_vec[[j]]
    ind <- (apply(pop_vec[[j]],1,min) < length(ind.features.to.keep[[j]])/2) & (apply(pop_vec[[j]], 1, max) > length(ind.features.to.keep[[j]])/2)
    pop_vec[[j]] <- popvec[ind,]

    }
  }

  # if no more models are found
  if(all(class(pop_vec[[1]]) != "matrix"))
  {
    return(NULL)
  }

  # transform pop_vec from matrix to list of sparseVec objects
  for(j in 1:length(pop_vec)){
  popvec = pop_vec[[j]]
  pop_vec[[j]] <- lapply(seq_len(nrow(popvec)), function(i, popvec) {popvec[i,]}, popvec)
  }

  list_indices <- list()

  # Parcourez les indices
  for (i in 1:length(pop_vec[[1]])) {
    # Initialiser la liste des indices pour chaque itération
    list_indices[[i]] <- list()

    # Parcourez les différentes listes dans list_evolved_pop
    for (j in 1:length(pop_vec)) {
      # Vérifiez si l'itération actuelle dépasse la longueur de la sous-liste
      if (i <= length(pop_vec[[j]])) {
        # Ajoutez l'élément correspondant à l'itération actuelle
        list_indices[[i]][[j]] <- pop_vec[[j]][[i]]
      } else {
        # Si l'itération actuelle dépasse la longueur de la sous-liste, remplacez par l'indice précédent
        list_indices[[i]][[j]] <- list_indices[[i - 1]][[j]]
      }
    }
  }
  pop_vec = list_indices

  # transform the sparseVec objects onto predomics objects
  pop <- listOfSparseVecToListOfModels_mc(X, y, clf, pop_vec,approch = approch)
  return(pop)
}


countEachFeatureApperance_mc <- function(clf, allFeatures, pop, best, veryBest,approch = approch)
{
  if(!isPopulation(pop))
  {
    return(NULL)
  }

  feat.coeff <- list()
  list_res <- list()
  list_coeff <- list()
  list_coeff <- clf$coeffs_
  featuresInPop      <- unique(populationGet_X(element2get = "names_", toVec = TRUE, na.rm = TRUE)(pop = pop))
  #featuresInPop      <- featuresInPop[order(featuresInPop)]
  featuresInBest     <- unique(populationGet_X(element2get = "names_", toVec = TRUE, na.rm = TRUE)(pop = best))
  featuresInVeryBest <- unique(populationGet_X(element2get = "names_", toVec = TRUE, na.rm = TRUE)(pop = veryBest))
  nbInBest           <- sapply(featuresInPop,
                               function(x, featuresInBest)
                               {sum(x == featuresInBest)},
                               featuresInBest)
  nbInVeryBest       <- sapply(featuresInPop,
                               function(x, featuresInBest)
                               {sum(x == featuresInVeryBest)}, featuresInBest)
  # normalize to 1
  freqInBest <- nbInBest / max(nbInBest, na.rm = TRUE)

  for(i in 1:(length(list_coeff))){

  clf$coeffs_ <-  list_coeff[[i]]
  feat.coeff <- clf$coeffs_[featuresInPop]

  res <- data.frame(nbInBest, nbInVeryBest, freqInBest, feat.coeff)
  res <- res[order(res[, "nbInVeryBest"], res[, "nbInBest"], decreasing = TRUE),]
  list_res[[i]] <- res
  }
  res <- list()
  res <- list_res
  return(res)
}


getFeatures2Keep_mc <- function(clf, featuresApperance, threshold = 0.01,approch=approch)
{
  # added threshold as the minimum percentage of models where a feature apprers in the best models
  if(is.null(featuresApperance))
  {
    return(NULL)
  }
  list_tokeep <- list()
  list_featuresApperance <- list()
  passerel <- list()
  passerel <- featuresApperance
  for(i in 1:(length(passerel))){
    featuresApperance = passerel[[i]]
  if(clf$params$language == "ratio" & clf$params$objective == "auc")
  {
    fa.pos <- featuresApperance[featuresApperance$feat.coeff == "1",]
    fa.neg <- featuresApperance[featuresApperance$feat.coeff == "-1",]

    tookeep.pos <- fa.pos$nbInVeryBest !=0 | fa.pos$freqInBest > threshold #| 1:clf$params$nbBest/2
    mask <- rep(FALSE,nrow(fa.pos)); mask[1:clf$params$nbBest/2] <- TRUE
    tookeep.pos <- tookeep.pos | mask

    tookeep.neg <- fa.neg$nbInVeryBest !=0 | fa.neg$freqInBest > threshold
    mask <- rep(FALSE,nrow(fa.neg)); mask[1:clf$params$nbBest/2] <- TRUE
    tookeep.neg <- tookeep.neg | mask

    tokeep <- c(rownames(fa.pos)[tookeep.pos], rownames(fa.neg)[tookeep.neg])
  }else
  {
    tokeep <- (featuresApperance[,"nbInVeryBest"] !=0  | featuresApperance[,"freqInBest"] > threshold)
    tokeep <- rownames(featuresApperance[tokeep,])
  }
    list_tokeep[[i]] <- tokeep
  }
  tokeep <- list()
  tokeep <- list_tokeep

  return(tokeep)
}

