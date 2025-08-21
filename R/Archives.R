
#' This function evaluates the models tests.
#' @title evaluatesBestsModelsTest
#' @param experiences experiences.
#' @param y y
#' @param X X
#' @param force.re.evaluation Boolean to force re-evaluation of the model even if it is already evaluated.
#' @param clf Object clf.
#' @param approach Type of approach to be used ("ovo" or "ova").
#' @param aggregation Type of aggregation method to be used ("votingAggregation", "weightedAggregation", "Predomics_aggregation_ova", "maximizationAggregation", "rankingAggregation").
#' @return The function returns a list best modesls test evaluated
#' @export
evaluatesBestsModelsTest <- function(experiences, approch = "ovo", aggregation_ = "votingAggregation", clf = clf, y = y, X = X) {

  bests_models_test <- list()
  clf <- regenerate_clf(clf, X, y, approch = approch)
  for (j in 1:length(experiences$classifier$models)) {

    lst_mod <- experiences$classifier$models[[j]]
    list_ <- list()

    for (i in 1:length(lst_mod)) {

      mod_test <- evaluateModel_mc(
        mod = lst_mod[[i]],
        X = X.test,
        y = y.test,
        clf = clf,
        eval.all = TRUE,
        force.re.evaluation = TRUE,
        approch = approch,
        aggregation_ = aggregation_,
        mode = "test"
      )

      list_[[i]] <- mod_test
    }

    accuracies <- sapply(list_, function(x) x$accuracy_)
    sorted_indices <- order(accuracies, decreasing = TRUE)
    sorted_models <- list_[sorted_indices]
    bests_models_test[[j]] <- sorted_models[[1]]
  }

  return(bests_models_test)
}

summarySE <- function(data, measurevar, groupvars, na.rm = FALSE, conf.interval = .95) {
  # Ensure that measurevar and groupvars are non-null and valid
  if (is.null(data) || is.null(measurevar) || is.null(groupvars)) {
    stop("Data, measurevar, and groupvars must be provided.")
  }

  data %>%
    dplyr::group_by(across(all_of(groupvars))) %>%
    dplyr::summarise(
      N = sum(!is.na(.data[[measurevar]]), na.rm = na.rm),

      sd = sd(.data[[measurevar]], na.rm = na.rm),
      value = mean(.data[[measurevar]], na.rm = na.rm),
      .groups = 'drop' # This will drop the grouping after summarise
    ) %>%
    dplyr::mutate(
      se = sd / sqrt(N),
      ci = if_else(N > 1,
                   se * qt(conf.interval/2 + 0.5, N - 1),
                   NA_real_  # Or your chosen default value for CI when N <= 1
      )
    )
}


#' listOfModels2ModelCollection
#'
#' @description Structures a list of predomics objects into a structured collection by k_sparsity.
#' @param pop: is population (a list) of predomics objects
#' @param nBest: number of elements to return for each sparsity (default:NA)
#' @return an model collection object
#' @export
listOfModels2ModelCollection <- function(pop, nBest = NA)
{
  # this is the old select_nBest_BySparsity while the first listOfModels2ModelCollection is deactivated
  spar <- populationGet_X(element2get = "eval.sparsity", toVec = TRUE, na.rm = TRUE)(pop)

  # get for every sparsity, the indices of the individuals that have this sparsity
  real.sparsity <- as.numeric(names(table(spar)))
  real.sparsity <- real.sparsity[real.sparsity != 0] # delete sparsity 0 if any

  # get the index of samples with that sparsity
  indivBySparsity <- lapply(real.sparsity,
                            function(x, spar)
                            {
                              which(spar == x)
                            }, spar)

  res <- lapply(seq_along(indivBySparsity),
                function(x, indivBySparsity)
                {
                  pop[indivBySparsity[[x]]]
                },
                indivBySparsity)
  names(res) <- lapply(real.sparsity, function(x) paste("k", x, sep = "_"))


  # selection
  if(!is.na(nBest))
  {
    res <- lapply(res, function(x) # for each k_sparsity
    {
      x <- sortPopulation(pop = x, evalToOrder = "fit_", decreasing = TRUE)

      # filter by number of elements we would like to keep
      if(length(x) > nBest)
        x <- x[1:nBest]
      return(x)
    })
  }

  return(res)
}


#' sparseVecToModel
#'
#' @description Builds a model object based on model that is in the sparse (short) format.
#' @param X: dataset
#' @param y: labels
#' @param v: A vector of indexes (example v=c(1,11))
#' @param clf: classifier information
#' @param eval.all: Should the model be evaluated (default:FALSE)
#' @param obj: an object model to add to the model (default:NULL)
#' @return an model object
sparseVecToModel <- function(X, y, v, clf, eval.all=FALSE, obj = NULL)
{
  if(is.null(v))
  {
    return(NULL)
  }
  res <- individual(X, y, clf, ind = v, eval.all = eval.all, obj = obj)
  return(res)
}

#' Computes the predected classification using a given model
#'
#' @description This function evaluates the predicted classification either using (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param score: the score passed directly
#' @param intercept: the intercept passed directly
#' @param sign: the sign passed directly
#' @return a vector with the predicted classification of the samples
evaluateYhat <- function(mod = NULL, X, y, clf, score=NULL, intercept=NULL, sign=NULL)
{
  if(!isModel(mod))
  {
    stop("evaluateYhat: please provide a valid model object BTR or SOTA.")
  }

  if(isModelSotaRF(mod))
  {
    yhat <- predict(object = mod$obj, t(X[mod$indices_,]))
  }

  if(isModelSotaSVM(mod))
  {
    yhat <- predict(object = mod$obj, t(X[mod$indices_,]))
  }

  if(isModelSotaGLMNET(mod))
  {
    yhat <- predict(object = mod$obj, s = mod$lambda, newx = t(X), type="class")[,1]
  }

  if(!isModelSota(mod)) # if BTR
  {
    # score_
    if(!myAssertNotNullNorNa(mod$score_))
    {
      scorelist <- getModelScore(mod = mod, X = X, clf = clf, force.re.evaluation = FALSE) # compute the score of the model
      score <- scorelist$score_

      if(is.null(score))
      {
        return(NULL)
      }

    } else
    {
      score       <- mod$score_
    }

    if(!myAssertNotNullNorNa(mod$intercept_) | !myAssertNotNullNorNa(mod$sign_))
    {
      mod <- evaluateIntercept(mod = mod, X = X, y = y, clf = clf)
      intercept <- mod$intercept_
      sign <- mod$sign_
    }else{
      intercept <- mod$intercept_
      sign <- mod$sign_
    }

    if(!myAssertNotNullNorNa(score, "missing score from evaluateYhat", stop = FALSE)) return(NULL)
    if(!myAssertNotNullNorNa(intercept, "missing intercept from evaluateYhat", stop = FALSE)) return(NULL)
    if(!myAssertNotNullNorNa(sign, "missing sign from evaluateYhat", stop = FALSE)) return(NULL)

    lev   <- levels(as.factor(clf$data$y))

    # NOTE: in the score we may have infite values that come for instance from the ratio language
    # This means that whatever the intercept these examples are positive ones. As such we can omit
    # them when computing the intercept.
    ind.infinite  <- is.infinite(mod$score_)
    ind.nan       <- is.nan(mod$score_)

    # For the ratio language
    # CASE 1
    # a/b > teta
    # a > teta * b
    # if b is infinite than whatever the teta the inequation is true
    # CASE 2
    # a/b < teta
    # a < teta * b
    # if b is infinite than whatever the teta the inequation is false

    if(mod$sign==">")
    {
      yhat.bool <- (score - intercept > 0)
      yhat.bool[ind.infinite] <- TRUE
      yhat.bool[ind.nan] <- FALSE # since 0 is not > than 0
      # compute the class
      yhat  <- factor(lev[as.factor(yhat.bool)], levels=lev)

    }else
    {
      yhat.bool <- (score - intercept < 0)
      yhat.bool[ind.infinite] <- FALSE
      yhat.bool[ind.nan] <- FALSE # since 0 is not < than 0
      # compute the class
      yhat  <- factor(lev[as.factor(yhat.bool)], levels=lev)
    }
  }

  return(yhat)
}

#' Compute other prediction scores such as precision, recall and f-score
#'
#' @description This function computes prediction scores based on the confusion matrix such as accuracy, precision, recall and f-score
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param mode: training or testing mode
#' @return a model whose evaluation parameters are updated
evaluateAdditionnalMetrics <- function(mod, X, y, clf, mode = "train")
{

  # if the following attributes are selected, then we need to fix it since they are derivates of a score
  if(clf$params$objective == "auc" & (clf$params$evalToFit != "fit_" | clf$params$evalToFit != "unpenalized_fit_"))
  {
    clf$params$evalToFit <- "accuracy_"
  }

  if(clf$params$evalToFit == "accuracy_") # additional metrics is auc_
  {
    # compute the auc
    aucg                 <- evaluateAUC(score = mod$score, y = y, sign = ">")
    aucl                 <- evaluateAUC(score = mod$score, y = y, sign = "<")
    mod$auc_             <- max(aucg, aucl)
  }

  if(clf$params$evalToFit == "auc_") # additional metrics is auc_
  {
    # evalute the accuracy that is not measured
    mod <- evaluateAccuracy(mod = mod, X = X, y = y, clf = clf, force.re.evaluation = TRUE, mode = mode)
  }

  if(!myAssertNotNullNorNa(mod$confusionMatrix_))
  {
    # visit this website for more information on the measures https://en.wikipedia.org/wiki/Precision_and_recall
    mod$confusionMatrix_ <- computeConfusionMatrix(mod, X, y, clf)

    if(is.null(mod$confusionMatrix_))
    {
      return(NULL)
    }
  }

  # precision = tp/(tp+fp)
  # cm = confusion matrix (2,2 is the positive class)
  if(all(dim(mod$confusionMatrix) == 2)) # if confusion matrix is OK (2x2)
  {
    mod$precision_ <- mod$confusionMatrix[2, 2] / (mod$confusionMatrix[2, 2] + mod$confusionMatrix[2, 1])
    #mod$precision_ <- mod$confusionMatrix[1, 1] / (mod$confusionMatrix[1, 1] + mod$confusionMatrix[2, 1])
    # recall = tp/(tp+fn), aka sensitivity
    #mod$recall_    <- mod$confusionMatrix[1, 1] / (mod$confusionMatrix[1, 1] + mod$confusionMatrix[1, 2])
    mod$recall_    <- mod$confusionMatrix[2, 2] / (mod$confusionMatrix[2, 2] + mod$confusionMatrix[1, 2])
    mod$f1_       <- 2 * (mod$precision_ * mod$recall_) / (mod$precision_ + mod$recall_)

  }else # otherwise we don't compute it
  {
    mod$precision_  <- NA
    mod$recall_     <- NA
    mod$f1_         <- NA
  }

  return(mod)
}

#' Computes the AUC of a model
#'
#' @description Computes the AUC of a model
#' @param score: the ^y score of the model
#' @param y: the response vector
#' @param sign: in which direction to make the comparison? "auto" (default): automatically define in which group
#' the median is higher and take the direction accordingly. ">": if the predictor values for the control group
#' are higher than the values of the case group (controls > t >= cases). "<": if the predictor values for the
#' control group are lower or equal than the values of the case group (controls < t <= cases).
#' @return an auc value
#' @importFrom pROC roc
evaluateAUC <- function(score, y, sign = '>')
{
  # NOTE: in the score we may have infite values that come for instance from the ratio language
  # This means that whatever the intercept these examples are positive ones. As such we can omit
  # them when computing the intercept.
  ind.infinite  <- is.infinite(score)
  ind.nan       <- is.nan(score)
  ind.filter    <- ind.infinite | ind.nan

  # if all the observations are not numbers return NA
  if(sum(ind.filter) == length(y))
  {
    return(NA)
  }

  score <- score[!ind.filter]
  y     <- y[!ind.filter]

  # if y does not contain exactly 2 levels, and if these don't have at least
  # 1 count then we don't compute the AUC
  if(length(table(y)) != 2 | any(table(y)==0))
  {
    auc <- NA

  }else # otherwise we compute it
  {
    rocobj <- suppressMessages(roc(response = y, predictor = score, direction = sign))
    auc <- as.numeric(rocobj$auc)
  }

  if(sign == "auto")
  {
    resg <- evaluateAUC(score = score, y = y, sign = ">")
    resl <- evaluateAUC(score = score, y = y, sign = "<")
    auc <- max(resg, resl)
  }

  return(auc)
}

#' Evaluates the accuracy of a model
#'
#' @description This function evaluates the accuracy of either (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param force.re.evaluation: evaluate again all the elements needed for accuracy (default:FALSE)
#' @param mode: training or test mode. If training, the funciton maximizes accuracy.
#' @return either (1) a model whose evaluation parameters are updated or (2) the accuracy
#' @export
evaluateAccuracy <- function(mod = NULL, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{

  #If mod is not a valid model
  if(!isModel(obj = mod))
  {
    stop("evaluateAccuracy: please provide a valid model object BTR or SOTA")
  }else
  {
    # test if the confusion matrix exists
    if(!myAssertNotNullNorNa(mod$confusionMatrix_) | force.re.evaluation)
    {
      # NOTE: we consider that evaluateFit is the main function where we would have computed the score and intercept if force.re.evaluation.
      # here we need to update the confusionMatrix_

      # compute the score if it does not exist
      if(!myAssertNotNullNorNa(mod$score_))
      {
        scorelist       <- getModelScore(mod = mod, X = X, clf, force.re.evaluation = force.re.evaluation) # compute the score of the model
        mod$score_      <- scorelist$score_
        mod$pos_score_  <- scorelist$pos_score_
        mod$neg_score_  <- scorelist$neg_score_

        if(is.null(mod$score_))
        {
          return(NULL)
        }
      }

      # compute the intercept and/or the sign if they do not exist
      if((!myAssertNotNullNorNa(mod$intercept_) | !myAssertNotNullNorNa(mod$sign_)) & !isModelSota(mod))
      {
        mod <- evaluateIntercept(mod = mod, X = X, y = y, clf = clf)
      }

      if(!isModelSota(mod))
      {
        if(!myAssertNotNullNorNa(mod$intercept_)) return(NULL)
        if(!myAssertNotNullNorNa(mod$sign_)) return(NULL)
      }

      # compute the confusion matrix
      mod$confusionMatrix_ <- computeConfusionMatrix(mod, X, y, clf)

      if(is.null(mod$confusionMatrix_))
      {
        return(NULL)
      }
    } # end re-evaluation or missing confusion matrix

    # EXPERIMENTAL TODO: add other accuraciy (weighted)
    # compute a confusionMatrix that is weighted for a better AUC
    #props <- prop.table(table(y))
    #confusionMatrix_.prop <- apply(mod$confusionMatrix_, 1, fun <- function(x){x*rev(props)})
    # accuracy = (tp+tn)/(tp+fp+tn+fn)
    #a1 <- sum(diag(confusionMatrix_.prop)) / sum(confusionMatrix_.prop)
    # EXPERIMENTAL

    if(mode == "train")
    {
      a1 <- sum(diag(mod$confusionMatrix_)) / sum(mod$confusionMatrix_)
      a2 <- sum(diag(apply(mod$confusionMatrix_, 2, rev))) / sum(mod$confusionMatrix_)

      if(a1 < a2  & !isModelSota(mod))
      {
        # inverse the sign
        if(mod$sign_ == ">")
        {
          mod$sign_ <- "<"
        }else
        {
          mod$sign_ <- ">"
        }

        # and recompute everything. This works with a recursive calling but due to R limits recursive calling in some deep cases
        # throws errors. This is why we are recoding another solution
        # mod <- evaluateAccuracy(mod = mod, X = X, y = y, force.re.evaluation = force.re.evaluation, mode = mode)

        # recompute the confusion matrix
        mod$confusionMatrix_ <- computeConfusionMatrix(mod, X, y, clf)
        if(is.null(mod$confusionMatrix_))
        {
          return(NULL)
        }

        # and accuracy
        mod$accuracy_ <- sum(diag(mod$confusionMatrix_)) / sum(mod$confusionMatrix_)
      }else
      {
        mod$accuracy_ <- a1
      }
    }else
    {
      mod$accuracy_ <- sum(diag(mod$confusionMatrix_)) / sum(mod$confusionMatrix_)
    }
  } # end train/test

  return(mod)
}





#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @return a model object with the fitting score
evaluateIntercept <- function(mod, X, y, clf)
{
  if(!isModel(mod))
  { # if model is not an object
    if(is.character(mod))
    { # if model is of the form of variable names
      mod <- index2names(X, mod)
    }
    mod <- individual(X, y, clf = clf, ind = mod)
  }

  if(isModelSota(mod))
  {
    if(clf$params$warnings) warning("evaluateIntercept: no intercept for sota models. Returning unchanged.")
    return(mod)
  }

  if((clf$params$intercept=="NULL"))
  {

    # compute the fitting scores depending on the method
    if(!myAssertNotNullNorNa(mod$score_))
    {
      scorelist <- getModelScore(mod = mod, X = X, clf = clf)
      mod$score_ <- scorelist$score_
      mod$pos_score_ <- scorelist$pos_score_
      mod$neg_score_ <- scorelist$neg_score_

      if(is.null(mod$score_))
      {
        return(NULL)
      }
    }

    # NOTE: in the score we may have infite values that come for instance from the ratio language
    # This means that whatever the intercept these examples are positive ones. As such we can omit
    # them when computing the intercept.
    ind.infinite  <- is.infinite(mod$score_)
    ind.nan       <- is.nan(mod$score_)

    score         <- mod$score_
    # the ind.infinite scores should be replaced by a big value let say max(X)+1
    try(score[ind.infinite] <- clf$data$X.max + 1, silent = TRUE)
    # the ind.nan should be replaced by a small value for instance min(X)-1
    try(score[ind.nan] <- clf$data$X.min - 1, silent = TRUE)


    switch(clf$params$objective,
           auc={ # THE AUC objective
             interc             <- computeIntercept(score = score, y = y, clf)
             mod$intercept_     <- interc$intercept
             mod$sign_          <- interc$sign
           },
           cor={
             mod$intercept_     <- "NULL"
             mod$sign_          <- "NULL"
           },
           {
             if(clf$params$warnings) warning("evaluateIntercept: This objective does not exist !")
           }
    )
  } else
  {
    mod$intercept_ <- clf$params$intercept
    mod$sign_ <- ">"
  }

  return(mod)
}


#' Evaluates the confusion Matrix of the predicted class and the class to predict
#'
#' @description This function evaluates the accuracy of a model
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @return a confusion matrix
computeConfusionMatrix <- function(mod, X, y, clf)
{
  yhat <- evaluateYhat(mod = mod, X = X, y = y, clf = clf)
  if(is.null(yhat))
  {
    return(NULL)
  }
  # yhat <- factor(yhat, levels = names(table(clf$data$y)))

  if(length(yhat) != length(y))
  {
    return(NULL)
  }
  cm <- table(y, yhat, dnn = c("y", "yhat"))

  return(cm)
}

#' sortPopulation
#'
#' @description Sort a population according to a given attribute (evalToOrder)
#' @param pop: a population (list) of evaluated predomics objects
#' @param evalToOrder: the attribute to be used in the sorting (default:fit_)
#' @param decreasing: whether the sorting should be be decreasing or not (default:decreasing)
#' @return a sorted population of predomics objects
#' @export
sortPopulation <- function(pop, evalToOrder = "fit_", decreasing = TRUE)
{

  if(!isPopulation(pop))
  {
    warning("sortPopulation: the population object is not a a valid one, returning NULL")
    return(NULL)
  }

  # # OVERRIDE
  # # for correlation use the standard error of the mean and reverse
  # if(pop[[1]]$objective == "cor")
  # {
  #   #
  #   evalToOrder   <- "ser_"
  #   decreasing    <- TRUE
  # }

  evaluation <- populationGet_X(element2get = evalToOrder, toVec = TRUE, na.rm = TRUE)(pop)

  if(length(evaluation) > 0)
  {
    ord <- order(evaluation, decreasing = decreasing)
  }else
  {
    return(NULL)
  }

  return(pop[ord])
}




#' Group model population by combination index
#'
#' Structures a list of predomics models into a grouped collection based on their combination index,
#' regardless of their sparsity level.
#'
#' @param pop A list of model populations indexed by sparsity level. Each element contains multiple models,
#'            and each model contains several pairwise combinations (e.g., A vs B, A vs C).
#'
#' @return A named list where each entry (e.g., "comb_1", "comb_2", ...) contains all submodels
#'         corresponding to the same combination across all sparsity levels.
group_population_by_combination <- function(pop) {
  regroupement <- list()

  for (s_index in seq_along(pop)) {
    cat("Processing sparsity index:", s_index, "\n")  # debug
    sparsity_models <- pop[[s_index]]  # list of models for this sparsity level

    for (m_index in seq_along(sparsity_models)) {
      model <- sparsity_models[[m_index]]
      nb_combinaisons <- length(model)

      for (i in seq_len(nb_combinaisons)) {
        comb_name <- paste0("comb_", i)

        if (!comb_name %in% names(regroupement)) {
          regroupement[[comb_name]] <- list()
        }

        regroupement[[comb_name]] <- append(
          regroupement[[comb_name]],
          list(model[[i]])
        )
      }
    }
  }

  return(regroupement)
}


#' Sort and filter combinations by performance
#'
#' For each combination group, this function sorts the models in descending order of their accuracy
#' and retains only the top N most performant ones.
#'
#' @param regroupement A named list of model combinations (e.g., result from group_population_by_combination),
#'                     where each element contains multiple models with an accuracy_ field.
#' @param top_n Integer. Number of top models to retain per combination. Default is 500.
#'
#' @return A list containing the top N models for each combination, sorted by descending accuracy.
sort_and_filter_by_performance <- function(regroupement, top_n = 100) {
  resultat <- list()

  for (comb_name in names(regroupement)) {
    liste_models <- regroupement[[comb_name]]

    # Check if models contain the 'accuracy_' field
    if (is.null(liste_models[[1]]$accuracy_)) {
      stop(paste("The field 'accuracy_' is missing in", comb_name))
    }

    # Sort models in descending order by accuracy_
    sorted <- liste_models[order(sapply(liste_models, function(x) x$accuracy_), decreasing = TRUE)]

    # Keep only the top_n best models
    resultat[[comb_name]] <- head(sorted, top_n)
  }

  return(resultat)
}



#' Create list of best models by index across combinations
#'
#' Constructs a list where each element contains the i-th best model from all combinations.
#' Useful for evaluating top-k aggregated models across different combination groups.
#'
#' @param top_models A named list of sorted top models (e.g., from sort_and_filter_by_performance),
#'                   where each element (e.g., "comb_1", "comb_2", ...) contains a list of models sorted by accuracy.
#' @param k Integer. Number of top entries to extract by index. Default is 500.
#'
#' @return A list of length k, where each element is a list of models (one per combination) at the same rank.
create_top_models_by_index <- function(top_models, k = 100) {
  # Get the combination names (e.g., "comb_1", "comb_2", "comb_3", ...)
  comb_names <- names(top_models)

  # Find the smallest number of models available across all combinations
  max_len <- min(sapply(top_models, length))  # safety check

  if (k > max_len) {
    warning(paste("k reduced to", max_len, "because some combinations have fewer models."))
    k <- max_len
  }

  # Final list of top-k model sets
  liste_par_index <- vector("list", k)

  for (i in 1:k) {
    current_set <- list()
    for (comb_name in comb_names) {
      current_set[[comb_name]] <- top_models[[comb_name]][[i]]
    }
    liste_par_index[[i]] <- current_set
  }

  return(liste_par_index)
}

digestModelCollection <- function(obj, X = NULL, clf, k.penalty = 0, mmprev = FALSE)
{
  if(!isModelCollection(obj))
  {
    # STOP for debug
    stop("digestModelCollection: The object is not a modelCollection")
  }

  res <- list()
  res$learner <- clf$learner

  # extract the best models
  # TODO include getTheBestModel & getBestIndividual in getBestModels
  if(mmprev & !is.null(X))
  {
    res$best_models           <- getNBestModels(obj = obj,
                                                significance = TRUE,
                                                by.k.sparsity = TRUE,
                                                k.penalty = k.penalty,
                                                n.best = 1,
                                                single.best = FALSE,
                                                single.best.cv = FALSE,
                                                single.best.k = NULL,
                                                max.min.prevalence = TRUE, # max min prevalence
                                                X = NULL,
                                                verbose = FALSE,
                                                evalToOrder = "fit_",
                                                return.population = TRUE, # population
                                                unique.control = TRUE
    )
    res$best_model            <- getNBestModels(obj = obj,
                                                significance = TRUE,
                                                by.k.sparsity = TRUE,
                                                k.penalty = k.penalty,
                                                n.best = 1,
                                                single.best = TRUE, # !!! give best
                                                single.best.cv = FALSE, # based on CV
                                                single.best.k = NULL,
                                                max.min.prevalence = TRUE, # max min prevalence
                                                X = NULL,
                                                verbose = FALSE,
                                                evalToOrder = "fit_",
                                                return.population = FALSE # this will be a model
    )
  }else
  {
    res$best_models           <- getNBestModels(obj = obj,
                                                significance = TRUE,
                                                by.k.sparsity = TRUE,
                                                k.penalty = k.penalty,
                                                n.best = 1,
                                                single.best = FALSE,
                                                single.best.cv = FALSE,
                                                single.best.k = NULL,
                                                max.min.prevalence = FALSE,
                                                X = NULL,
                                                verbose = FALSE,
                                                evalToOrder = "fit_",
                                                return.population = TRUE, # population
                                                unique.control = TRUE
    )
    res$best_model            <- getNBestModels(obj = obj,
                                                significance = TRUE,
                                                by.k.sparsity = TRUE,
                                                k.penalty = k.penalty,
                                                n.best = 1,
                                                single.best = TRUE, # !!! give best
                                                single.best.cv = FALSE, # based on CV
                                                single.best.k = NULL,
                                                max.min.prevalence = FALSE,
                                                X = NULL,
                                                verbose = FALSE,
                                                evalToOrder = "fit_",
                                                return.population = FALSE # this will be a model
    )
  }
  res$best_models.fit       <- populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = FALSE)(pop = res$best_models)
  res$best_models.accuracy  <- populationGet_X(element2get = "accuracy_", toVec = TRUE, na.rm = FALSE)(pop = res$best_models)
  res$best_models.precision <- populationGet_X(element2get = "precision_", toVec = TRUE, na.rm = FALSE)(pop = res$best_models)
  res$best_models.recall    <- populationGet_X(element2get = "recall_", toVec = TRUE, na.rm = FALSE)(pop = res$best_models)
  res$best_models.f1_       <- populationGet_X(element2get = "f1_", toVec = TRUE, na.rm = FALSE)(pop = res$best_models)
  res$best_models.cor_      <- populationGet_X(element2get = "cor_", toVec = TRUE, na.rm = FALSE)(pop = res$best_models)

  return(res)
}

