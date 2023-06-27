

#' Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function
#'
#' @description Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function, one versus one.
#' @import foreach
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param parallel.local: weather or not to run in //
#' @return a list of +1 & -1 for each one-vs-all classification of our dataset
#' @export
getSign_ovo <- function(X, y, clf = NULL, parallel.local = FALSE)
{
  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  k <- 1
  list_Sign <- list()

  for (i in 1:(length(nClasse)-1)) {
    for (j in (i+1):length(nClasse)) {
      class_i <- nClasse[i]
      class_j <- nClasse[j]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]
      X_pair <- X[,indices]
      list_y[[k]] <- y_pair
      list_X[[k]] <- X_pair
      k <- k + 1
    }
  }

  for(i in 1:length(list_y)){
    Sign <- getSign(X=list_X[[i]], y=list_y[[i]], clf = NULL, parallel.local = FALSE)
    list_Sign[[i]] = Sign
  }
  res <- list_Sign

  return(res)
}




#' Creates an object individual ovo
#'
#' @description Creates an object individual ovo
#' @param X: the list data matrix with variables in the rows and observations in the columns
#' @param y: the list vector
#' @param clf: the object containing the classifier information
#' @param ind: the indexes of the variables forming the individual could be null if we give the function a dense vector (via the coeff parameter) or if we also want to generate the individual
#' @param coeffs: the coefficients of the model, it could be a dense vector (in this case, ind need to be null), or it could be only the non zero values, or if it's null a new individual will be genrated
#' @param obj: an object to be incorporated in the model (default:NULL). We use this usually for SOTA.
#' @param res_clf: if provided information on mda etc can be found and transmitted to the model object
#' @return an list individual (model) object
#' @export
individual_ovo <- function(X, y, clf, coeffs = NULL, ind = NULL, eval.all= FALSE, signs = NULL, obj = NULL, res_clf = NULL)
{
  listcoeffs <- list()
  listcoeffs <- clf$coeffs_
  nClasse <- unique(y)
  list_res <- list()
  list_y <- list()
  list_X <- list()
  list_individual <- list()
  k <- 1
  for (i in 1:(length(nClasse)-1)) {
    for (j in (i+1):length(nClasse)) {
      class_i <- nClasse[i]
      class_j <- nClasse[j]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]
      X_pair <- X[,indices]
      list_y[[k]] <- y_pair
      list_X[[k]] <- X_pair
      k <- k + 1
    }
  }
  for(i in 1:length(list_y)){
    clf$coeffs_     <- listcoeffs[[i]]
    individual <- individual(X= list_X[[i]], y= list_y[[i]], clf,  ind = ind)
    list_individual[[i]] = individual
  }
  individual = list_individual
  return(individual)

}

#' Computes the best intercept for the model while minimizing error
#'
#' @description Computes the best intercept for the model
#' @param score: the ^y score of the model
#' @param y: the response vector
#' @param verbose: print running information when set to TRUE
#' @param sign: weather the score should be greater or smaller than the intercept (default:"auto")
#' @param return.all: if TRUE, the function will return the intercept as well as the table used to compute it.
#' @param plot: if TRUE, the score will be visialized (default:FALSE)
#' @return the intercept, the sign and the accuracy
#' @export
computeIntercept_ovo <- function(score, y, verbose=FALSE, sign="auto", plot = FALSE) {

  nClasse <- unique(y)
  list_compute <- list()
  list_y <- list()
  k <- 1
  for (i in 1:(length(nClasse)-1)) {
    for (j in (i+1):length(nClasse)) {
      class_i <- nClasse[i]
      class_j <- nClasse[j]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]
      list_y[[k]] <- y_pair
      k <- k + 1
    }
  }
  listcoeffs <- list()
  listcoeffs     <- clf$coeffs_
  for(i in 1:length(list_y)){
    clf$coeffs_     <- listcoeffs[[i]]
    interc <-  computeIntercept(score = score, y = list_y[[i]], clf)
    list_compute[[i]] = interc
  }
  res <- list_compute
  return(res)
}




#' Computes the ^y score of the model
#'
#' @description Returns the ^y score of the model
#' @importFrom kernlab predict
#' @param mod: a model object where the score will be computed
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param force.re.evaluation: we recompute the score (default:TRUE)
#' @return a list containing the predicted ^y score for each observation
#' @export

getModelScore_ovo<- function(mod, X, clf, force.re.evaluation = TRUE)
{

  list_X <- list()
  list_X <- clf$data$X
  list_mod <- list()
  list_mod <- mod
  list_res <- list()
  listcoeffs <- list()
  listcoeffs     <- clf$coeffs_
  for (i in 1:length(listcoeffs)){
   # clf$coeffs_     <- listcoeffs[[i]]
    res <- getModelScore(mod = mod[[i]], X = list_X[[i]], clf = clf)
    list_res[[i]] <- res
  }

  res <- list_res

  return(res)
}



#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @return a model object with the fitting score
evaluateIntercept_ovo <- function(mod, X, y, clf)
{
  nClasse <- unique(y)
  list_mod <- list()
  list_y <- list()
  list_X <- list()
  listcoeffs <- list()
  k <- 1
  for (i in 1:(length(nClasse)-1)) {
    for (j in (i+1):length(nClasse)) {
      class_i <- nClasse[i]
      class_j <- nClasse[j]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]
      X_pair <- X[,indices]
      list_y[[k]] <- y_pair
      list_X[[k]] <- X_pair
      k <- k + 1
    }
  }


  listcoeffs     <- clf$coeffs_
  #list_mod <- mod


  if(!isModel(mod[[1]]))
  { # if model is not an object
    if(is.character(mod[[1]]))
    { # if model is of the form of variable names
      mod <- index2names(X=list_X[[1]], mod[[1]])
    }
    mod <- individual_ovo(X, y, clf = clf, ind = mod)
  }

  if(isModelSota(mod[[1]]))
  {
    if(clf$params$warnings) warning("evaluateIntercept: no intercept for sota models. Returning unchanged.")
    return(mod)
  }

  if((clf$params$intercept=="NULL"))
  {

    # compute the fitting scores depending on the method
    if(!myAssertNotNullNorNa(mod$score_))
    {
      scorelist <- getModelScore_ovo(mod = mod, X = X, clf = clf)
      for(i in 1:length(scorelist)){

        mod[[i]]$score_ <- scorelist[[i]]$score_
        mod[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
        mod[[i]]$neg_score_ <- scorelist[[i]]$neg_score_

      }


      if(is.null(mod[[1]]$score_))
      {
        return(NULL)
      }
    }

    # NOTE: in the score we may have infite values that come for instance from the ratio language
    # This means that whatever the intercept these examples are positive ones. As such we can omit
    # them when computing the intercept.
    score <- list()

    for(i in 1:length(mod)){
      ind.infinite  <- is.infinite(mod[[i]]$score_)
      ind.nan       <- is.nan(mod[[i]]$score_)

      score__         <- mod[[i]]$score_
      # the ind.infinite scores should be replaced by a big value let say max(X)+1
      try(score__[ind.infinite] <- clf$data$X.max[[i]] + 1, silent = TRUE)
      # the ind.nan should be replaced by a small value for instance min(X)-1
      try(score__[ind.nan] <- clf$data$X.min[[i]] - 1, silent = TRUE)
      score[[i]] <- score__
    }

    switch(clf$params$objective,
           auc={ # THE AUC objective
             interc             <- computeIntercept_ovo(score = score, y = y, clf)
             for(i in 1:length(interc)){
               mod[[i]]$intercept_     <- interc[[i]]$intercept
               mod[[i]]$sign_          <- interc[[i]]$sign
             }

           },
           cor={
             for(i in 1:length(mod)){
               mod[[i]]$intercept_     <- "NULL"
               mod[[i]]$sign_          <- "NULL"
             }

           },
           {
             if(clf$params$warnings) warning("evaluateIntercept: This objective does not exist !")
           }
    )
  } else
  {
    for(i in 1:length(mod)){
      mod[[i]]$intercept_ <- clf$params$intercept
      mod[[i]]$sign_ <- ">"
    }

  }

  return(mod)
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
#'
evaluateYhat_ovo <- function(mod = NULL, X, y, clf, score=NULL, intercept=NULL, sign=NULL)
  {
  list_yhat <- list()
  score <- list()
  intercept <- list()
  sign <- list()
  if(!isModel(mod[[1]]))
  {
    stop("evaluateYhat: please provide a valid model object BTR or SOTA.")
  }

  if(isModelSotaRF(mod[[1]]))
  {
    for(i in 1:length(mod)){
      X <- clf$data$X[[i]]
      yhat <- predict(object = mod[[i]]$obj, t(X[mod[[i]]$indices_,]))
      list_yhat[[i]] <- yhat
    }

  }

  if(isModelSotaSVM(mod[[1]]))
  {
    for(i in 1:length(mod)){
      X <- clf$data$X[[i]]
      yhat <- predict(object = mod[[i]]$obj, t(X[mod[[i]]$indices_,]))
      list_yhat[[i]] <- yhat
    }
  }

  if(isModelSotaGLMNET(mod[[1]]))
  {
    for(i in 1:length(mod)){
      X <- clf$data$X[[i]]
      yhat <- predict(object = mod[[i]]$obj, s = mod[[i]]$lambda, newx = t(X), type="class")[,1]
      list_yhat[[i]] <- yhat
    }
  }

  if(!isModelSota(mod[[1]])) # if BTR
  {
    # score_
    if(!myAssertNotNullNorNa(mod[[1]]$score_))
    {
      scorelist <- getModelScore_ovo(mod = mod, X = X, clf = clf, force.re.evaluation = FALSE) # compute the score of the model

      for(i in 1:length(scorelist)){

        score[[i]] <- scorelist[[i]]$score_
      }

      if(is.null(score[[1]]))
      {
        return(NULL)
      }

    } else
    {
      for(i in 1:length(mod)){
        score[[i]]       <- mod[[i]]$score_
      }

    }

    if(!myAssertNotNullNorNa(mod[[1]]$intercept_) | !myAssertNotNullNorNa(mod[[1]]$sign_))
    {
      mod <- evaluateIntercept_ovo(mod = mod, X = X, y = y, clf = clf)

      for(i in 1:length(mod)){
        intercept[[i]] <- mod[[i]]$intercept_
        sign[[i]] <- mod[[i]]$sign_
      }

    }else{
      for(i in 1:length(mod)){
        intercept[[i]] <- mod[[i]]$intercept_
        sign[[i]] <- mod[[i]]$sign_
      }
    }
    for(i in 1:length(score)){
      if(!myAssertNotNullNorNa(score[[i]], "missing score from evaluateYhat", stop = FALSE)) return(NULL)
      if(!myAssertNotNullNorNa(intercept[[i]], "missing intercept from evaluateYhat", stop = FALSE)) return(NULL)
      if(!myAssertNotNullNorNa(sign[[i]], "missing sign from evaluateYhat", stop = FALSE)) return(NULL)
    }


    nClasse <- unique(y)
    list_y <- list()
    lev <- list()
    k <- 1
    for (i in 1:(length(nClasse)-1)) {
      for (j in (i+1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)
        y_pair <- y[indices]
        lev[[k]]   <- levels(as.factor(y_pair))
        k <- k + 1
      }
    }

    # NOTE: in the score we may have infite values that come for instance from the ratio language
    # This means that whatever the intercept these examples are positive ones. As such we can omit
    # them when computing the intercept.
    ind.infinite <- list()
    ind.nan <- list()
    for(i in 1:length(mod)){
      ind.infinite[[i]]  <- is.infinite(mod[[i]]$score_)
      ind.nan[[i]]       <- is.nan(mod[[i]]$score_)
    }


    for(i in 1:length(mod)){
      if(mod[[i]]$sign==">")
      {
        yhat.bool <- (score[[i]] - intercept[[i]] > 0)
        yhat.bool[ind.infinite[[i]]] <- TRUE
        yhat.bool[ind.nan[[i]]] <- FALSE # since 0 is not > than 0
        # compute the class
        yhat  <- factor(lev[[i]][as.factor(yhat.bool)], levels=lev[[i]])
        list_yhat[[i]] <- yhat
      }
      #}
      else
      {
        #for(i in 1:length(mod)){
        yhat.bool <- (score[[i]] - intercept[[i]] < 0)
        yhat.bool[ind.infinite[[i]]] <- FALSE
        yhat.bool[ind.nan[[i]]] <- FALSE # since 0 is not < than 0
        # compute the class
        yhat  <- factor(lev[[i]][as.factor(yhat.bool)], levels=lev[[i]])
        list_yhat[[i]] <- yhat
      }
      }
    }
    yhat <- list_yhat
    return(yhat)
  }


################################################################
# COMPUTING MODEL OBJECTS, SCORES, ERRORS...
################################################################


#' Evaluates the confusion Matrix of the predicted class and the class to predict
#'
#' @description This function evaluates the accuracy of a model
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @return a list confusion matrix
computeConfusionMatrix_ovo <- function(mod, X, y, clf)
{
  yhat <- list()
  cm <- list()
  yhat <- evaluateYhat_ovo(mod = mod, X = X, y = y, clf = clf)
  if(is.null(yhat[[1]]))
  {
    return(NULL)
  }

  nClasse <- unique(y)
  list_y <- list()
  k <- 1
  for (i in 1:(length(nClasse)-1)) {
    for (j in (i+1):length(nClasse)) {
      class_i <- nClasse[i]
      class_j <- nClasse[j]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]
      list_y[[k]] <- y_pair
      k <- k + 1
    }
  }
  y <- list_y
  for(i in 1:length(yhat)){
    yhat[[i]] <- factor(yhat[[i]], levels = names(table(y[[i]])))
  }

  if(length(yhat[[1]]) != length(y[[1]]))
  {
    return(NULL)
  }
  for(i in 1:length(yhat)){
    cm[[i]] <- table(y[[i]], yhat[[i]], dnn = c("y", "yhat"))
  }
  return(cm)
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
evaluateAccuracy_ovo <- function(mod = NULL, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{

  #If mod is not a valid model
  if(!isModel(obj = mod[[1]]))
  {
    stop("evaluateAccuracy: please provide a valid model object BTR or SOTA")
  }else
  {
    for(i in 1:length(mod)){
      # test if the confusion matrix exists
      if(!myAssertNotNullNorNa(mod[[i]]$confusionMatrix_) | force.re.evaluation)
      {
        # NOTE: we consider that evaluateFit is the main function where we would have computed the score and intercept if force.re.evaluation.
        # here we need to update the confusionMatrix_

        # compute the score if it does not exist
        if(!myAssertNotNullNorNa(mod[[i]]$score_))
        {
          scorelist       <- getModelScore_ovo(mod = mod, X = X, clf, force.re.evaluation = force.re.evaluation) # compute the score of the model
          mod[[i]]$score_      <- scorelist[[i]]$score_
          mod[[i]]$pos_score_  <- scorelist[[i]]$pos_score_
          mod[[i]]$neg_score_  <- scorelist[[i]]$neg_score_

          if(is.null(mod[[i]]$score_))
          {
            return(NULL)
          }
        }

        # compute the intercept and/or the sign if they do not exist
        if((!myAssertNotNullNorNa(mod[[i]]$intercept_) | !myAssertNotNullNorNa(mod[[i]]$sign_)) & !isModelSota(mod[[i]]))
        {
          mod <- evaluateIntercept_ovo(mod = mod, X = X, y = y, clf = clf)
        }

        if(!isModelSota(mod[[i]]))
        {
          if(!myAssertNotNullNorNa(mod[[i]]$intercept_)) return(NULL)
          if(!myAssertNotNullNorNa(mod[[i]]$sign_)) return(NULL)
        }

        # compute the confusion matrix
        confusioMatrix_ <- computeConfusionMatrix_ovo(mod, X, y, clf)
        mod[[i]]$confusionMatrix_ <- confusioMatrix_[[i]]

        if(is.null(mod[[i]]$confusionMatrix_))
        {
          return(NULL)
        }
      } # end re-evaluation or missing confusion matrix


      if(mode == "train")
      {
        a1 <- sum(diag(mod[[i]]$confusionMatrix_)) / sum(mod[[i]]$confusionMatrix_)
        a2 <- sum(diag(apply(mod[[i]]$confusionMatrix_, 2, rev))) / sum(mod[[i]]$confusionMatrix_)

        if(a1 < a2  & !isModelSota(mod[[i]]))
        {
          # inverse the sign
          if(mod[[i]]$sign_ == ">")
          {
            mod[[i]]$sign_ <- "<"
          }else
          {
            mod[[i]]$sign_ <- ">"
          }

          # and recompute everything. This works with a recursive calling but due to R limits recursive calling in some deep cases
          # throws errors. This is why we are recoding another solution
          # mod <- evaluateAccuracy(mod = mod, X = X, y = y, force.re.evaluation = force.re.evaluation, mode = mode)

          # recompute the confusion matrix
          confusioMatrix_ <- computeConfusionMatrix_ovo(mod, X, y, clf)
          mod[[i]]$confusionMatrix_ <- confusioMatrix_[[i]]
          if(is.null(mod[[i]]$confusionMatrix_))
          {
            return(NULL)
          }

          # and accuracy
          mod[[i]]$accuracy_ <- sum(diag(mod[[i]]$confusionMatrix_)) / sum(mod[[i]]$confusionMatrix_)
        }else
        {
          mod[[i]]$accuracy_ <- a1
        }
      }else
      {
        mod[[i]]$accuracy_ <- sum(diag(mod[[i]]$confusionMatrix_)) / sum(mod[[i]]$confusionMatrix_)
      }
    }
  } # end train/test

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
evaluateAUC_ovo <- function(score, y, sign = '>')
{
  nClasse <- unique(y)
  yhat_list <- list()
  list_y <- list()
  list_X <- list()
  sign = sign
  k <- 1
  list_auc <- list()

  for (i in 1:(length(nClasse)-1)) {
    for (j in (i+1):length(nClasse)) {
      class_i <- nClasse[i]
      class_j <- nClasse[j]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]
      list_y[[k]] <- y_pair
      k <- k + 1
    }
  }

  # list values auc_ one versus one
  for(i in 1:length(list_y)){
    au <- evaluateAUC(score, y = list_y[[i]], sign)
    list_auc[[i]] = au
  }
  auc <- list_auc
  return(auc)
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
evaluateAdditionnalMetrics_ovo <- function(mod, X, y, clf, mode = "train")
{

  # if the following attributes are selected, then we need to fix it since they are derivates of a score
  if(clf$params$objective == "auc" & (clf$params$evalToFit != "fit_" | clf$params$evalToFit != "unpenalized_fit_"))
  {
    clf$params$evalToFit <- "accuracy_"
  }

  if(clf$params$evalToFit == "accuracy_") # additional metrics is auc_
  {
    # compute the auc
    aucg                 <- evaluateAUC_ovo(score = mod$score, y = y, sign = ">")
    aucl                 <- evaluateAUC_ovo(score = mod$score, y = y, sign = "<")
    for(i in 1:length(aucg)){
      mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
    }
  }

  if(clf$params$evalToFit == "auc_") # additional metrics is auc_
  {
    # evalute the accuracy that is not measured
    mod <- evaluateAccuracy_ovo(mod = mod, X = X, y = y, clf = clf, force.re.evaluation = TRUE, mode = mode)
  }

  if(!myAssertNotNullNorNa(mod[[1]]$confusionMatrix_))
  {
    for(i in 1:length(mod)){
      # visit this website for more information on the measures https://en.wikipedia.org/wiki/Precision_and_recall
      confusioMatrix_ <- computeConfusionMatrix_ovo(mod, X, y, clf)
      mod[[i]]$confusionMatrix_ <- confusioMatrix_[[i]]
    }
    if(is.null(mod[[1]]$confusionMatrix_))
    {
      return(NULL)
    }
  }

  for (i in 1:length(mod)){
    # precision = tp/(tp+fp)
    # cm = confusion matrix (2,2 is the positive class)
    mod[[i]]$precision_ <- mod[[i]]$confusionMatrix[2, 2] / (mod[[i]]$confusionMatrix[2, 2] + mod[[i]]$confusionMatrix[2, 1])
    #mod$precision_ <- mod$confusionMatrix[1, 1] / (mod$confusionMatrix[1, 1] + mod$confusionMatrix[2, 1])

    # recall = tp/(tp+fn), aka sensitivity
    #mod$recall_    <- mod$confusionMatrix[1, 1] / (mod$confusionMatrix[1, 1] + mod$confusionMatrix[1, 2])
    mod[[i]]$recall_    <- mod[[i]]$confusionMatrix[2, 2] / (mod[[i]]$confusionMatrix[2, 2] + mod[[i]]$confusionMatrix[1, 2])

    mod[[i]]$f1_       <- 2 * (mod[[i]]$precision_ * mod[[i]]$recall_) / (mod[[i]]$precision_ + mod[[i]]$recall_)
  }

  return(mod)
}

#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @param mode: A choice from c("train", "test") indicates wether we wish to learn the threthold
#' of the model (default:"train") or not "test" for the c("terinter","bininter","ratio") languages
#' @return a model object with the fitting score
#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @param mode: A choice from c("train", "test") indicates wether we wish to learn the threthold
#' of the model (default:"train") or not "test" for the c("terinter","bininter","ratio") languages
#' @return a model object with the fitting score
evaluateFit_ovo <- function(mod, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{
  # if the models is not a valid object
  if(!isModel(mod[[1]]))
  {
    if(!is.character(mod[[1]]) | !is.numeric(mod[[1]]))
    {
      stop("evaluateFit: please provide a valid model object or a feature index vector.")
    }

    if(is.character(mod[[1]]))
    {
      # if model is of the form of variable names

      for(i in 1:length(mod)) {
        mod[[i]] <- names2index(X = clf$data$X[[i]], var.names = mod[[i]])
      }
    }
    mod <- individual_ovo(X, y, clf = clf, ind = mod)
  }

  # compute the score in the case it is asked to recompute
  if(force.re.evaluation)
  {
    scorelist <- getModelScore_ovo(mod = mod, X = X, clf = clf, force.re.evaluation = force.re.evaluation)
    for(i in 1:length(scorelist)) {
      mod[[i]]$score_ <- scorelist[[i]]$score_
      mod[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
      mod[[i]]$neg_score_ <- scorelist[[i]]$neg_score_
    }
  }else
  {
    # compute the score if it does not exist
    if(!myAssertNotNullNorNa(mod[[1]]$score_))
    {
      scorelist <- getModelScore_ovo(mod = mod, X = X, clf = clf, force.re.evaluation = force.re.evaluation)
      for(i in 1:length(scorelist)){
        if(!any(is.na(scorelist[[i]])) | isModelSota(mod[[i]]))
        {
          mod[[i]]$score_ <- scorelist[[i]]$score_
          mod[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
          mod[[i]]$neg_score_ <- scorelist[[i]]$neg_score_
        }
      }
    }else
    {
      list_x <- list()
      list_x <- clf$data$X
      for(i in 1:length(mod)){
        # in the case the score has been computed before but for an other X, we recompute
        X <- list_x[[i]]
        if(length(mod[[i]]$score_) != ncol(X))
        {
          scorelist <- getModelScore_ovo(mod = mod, X = X, clf = clf, force.re.evaluation = force.re.evaluation)
          # if(!any(is.na(scorelist)))

          # }
          if(!any(is.na(scorelist[[i]])) | isModelSota(mod[[i]]))
          {
            mod[[i]]$score_ <- scorelist[[i]]$score_
            mod[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
            mod[[i]]$neg_score_ <- scorelist[[i]]$neg_score_
          }
        }
      }
    }
  }

  # if after all the above steps we still don't have a score than we kill the model.
  if(!myAssertNotNullNorNa(mod[[1]]$score_))
  {
    return(NULL)
  }
  for(i in 1:length(mod)){
    if(is.null(mod[[i]]$eval.sparsity)) # if sparsity is not set
    {
      mod[[i]]$eval.sparsity <- length(mod[[i]]$indices_)
    }
  }



  switch(clf$params$objective,
         # THE AUC objective
         auc = {
           # compute the intercept and sign
           if(mode == 'train')
           {
             if((mod[[1]]$language != "ter" & mod[[1]]$language != "bin") & !isModelSota(mod[[1]]))
             {
               mod                <- evaluateIntercept_ovo(X = X, y = y, clf = clf, mod = mod)
             }else
             {
               for(i in 1:length(mod)){
                 # force intercept to NA for sota
                 if(isModelSota(mod[[i]]))
                 {
                   mod[[i]]$intercept_   <- NA
                   mod[[i]]$sign_        <- NA
                 }else
                 {
                   mod[[i]]$intercept_   <- 0
                 }
               }
             }

             # sanity check
             if(!clf$params$evalToFit %in% names(mod[[1]]))
             {
               stop("evaluateFit: the evalToFit parameter seems not to be a valid one. Please make sure it is among the available ones")
             }

             # if the following attributes are selected, then we need to fix it since they are derivates of a score
             if(clf$params$evalToFit != "fit_" | clf$params$evalToFit != "unpenalized_fit_")
             {
               clf$params$evalToFit <- "accuracy_"
             }

             # in case it is auc
             if(clf$params$evalToFit == "auc_") # in this case the auc will be computed in evaluate other metrics
             {
               # compute the auc
               score <- list()

               for(i in 1:length(mod)) {
                 score[[i]] <- mod[[i]]$score
               }

               aucg                 <- evaluateAUC_ovo(score = score, y = y, sign = ">")
               aucl                 <- evaluateAUC_ovo(score = score, y = y, sign = "<")
               for(i in 1:length(aucg)){
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$auc_
               }
             }

             # in case it is accuracy
             if(clf$params$evalToFit == "accuracy_") # in this case the auc will be computed in evaluate other metrics
             {
               mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
               for(i in 1:length(mod)){
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$accuracy_
               }
             }

             # otherwise compute the rest
             if(clf$params$evalToFit != "auc_" & clf$params$evalToFit != "accuracy_")
             {
               mod <- evaluateAdditionnalMetrics_ovo(mod = mod, X = X, y = y, clf = clf, mode = mode)
               for(i in 1:length(mod)){
                 mod[[i]]$unpenalized_fit_ <- mod[[i]][[clf$params$evalToFit]]
               }
               # compte accuracy also
               mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
               # and auc, since these are helpful information
               score <- list()
               for(i in 1:length(mod)){
                 score[[i]] <- mod[[i]]$score
               }
               aucg                 <- evaluateAUC_ovo(score = score, y = y, sign = ">")
               aucl                 <- evaluateAUC_ovo(score = mod$score, y = y, sign = "<")
               for(i in 1:length(aucg)){
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
               }
             }
           } # if test mode, we don't recompute the intercept
           else
           {
             # sanity check
             if(!clf$params$evalToFit %in% names(mod[[1]]))
             {
               stop("evaluateFit: the evalToFit parameter seems not to be a valid one. Please make sure it is among the available ones")
             }

             # if the following attributes are selected, then we need to fix it since they are derivates of a score
             if(clf$params$evalToFit != "fit_" | clf$params$evalToFit != "unpenalized_fit_")
             {
               clf$params$evalToFit <- "accuracy_"
             }

             # in case it is auc
             if(clf$params$evalToFit == "auc_") # in this case the auc will be computed in evaluate other metrics
             {
               # compute the auc
               score <- list()
               for(i in 1:length(mod)) {
                 score[[i]] <- mod[[i]]$score
              }
               aucg                 <- evaluateAUC_ovo(score = score, y = y, sign = ">")
               aucl                 <- evaluateAUC_ovo(score = score, y = y, sign = "<")
               for(i in 1:length(mod)){
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$auc_
               }
             }

             # in case it is accuracy
             if(clf$params$evalToFit == "accuracy_") # in this case the auc will be computed in evaluate other metrics
             {
               mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
               for(i in 1:length(mod)){
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$accuracy_
               }
             }

             # otherwise compute the rest
             if(clf$params$evalToFit != "auc_" & clf$params$evalToFit != "accuracy_")
             {
               mod <- evaluateAdditionnalMetrics_ovo(mod = mod, X = X, y = y, clf = clf, mode = mode)
               for(i in 1:length(mod)){
                 mod[[i]]$unpenalized_fit_ <- mod[[i]][[clf$params$evalToFit]]
               }
               # compte accuracy also
               mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
               # and auc, since these are helpful information
               score <- list()
               for(i in 1:length(mod)){
                 score[[i]] <- mod[[i]]$score
               }
               aucg                 <- evaluateAUC_ovo(score = score, y = y, sign = ">")
               aucl                 <- evaluateAUC_ovo(score = score, y = y, sign = "<")
               for(i in 1:length(aucg)){
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
               }
             }
           } # end mode = test

         },
         # THE REGRESSION correlation method
         cor = {
           #added may 8th 2016 fix the mix of population negative & positive

           tryCatch({
             # as of 2018/07/09 the regression process changes, We will search
             # for a preason correlation and will maximise the r2. We also
             # implemented standard error of the mean (which needs to be minimized)


             nClasse <- unique(y)
             list_y <- list()
             k <- 1
             for (i in 1:(length(nClasse)-1)) {
               for (j in (i+1):length(nClasse)) {
                 class_i <- nClasse[i]
                 class_j <- nClasse[j]
                 indices <- which(y == class_i | y == class_j)
                 y_pair <- y[indices]
                 list_y[[k]] <- y_pair
                 k <- k + 1
               }
             }

             for(i in 1:length(mod)){
               ina      <- is.na(mod[[i]]$score_) | is.na(y=list_y[[i]]) | is.infinite(mod[[i]]$score) | is.infinite(y=list_y[[i]])
               y <- list_y[[i]]
               mod[[i]]$cor_ <- abs(cor(mod[[i]]$score[!ina], y[!ina], method = "pearson"))

               # use the r2 instead
               score.scaled <- as.vector(scale(mod[[i]]$score_[!ina], center = TRUE, scale = TRUE))
               y.scaled <- as.vector(scale(y[!ina], center = TRUE, scale = TRUE))
               mod[[i]]$rsq_ <- abs(cor(mod[[i]]$score[!ina], y[!ina], method = "pearson"))^2
               mod[[i]]$ser_ <- sqrt(sum((score.scaled - y.scaled)^2, na.rm = TRUE)/(length(score.scaled) - 2))
             }
           })


           if(is.null(mod[[1]])) return(mod)
           if(myAssertNotNullNorNa(mod[[1]]$cor_))
           {
             for(i in 1:length(mod)){
               mod[[i]]$cor_           <- as.numeric(mod[[i]]$cor_)
             }
           }
           if(myAssertNotNullNorNa(mod[[1]]$rsq_))
           {
             for(i in 1:length(mod)){
               mod[[i]]$rsq_           <- as.numeric(mod[[i]]$rsq_)
             }
           }
           if(myAssertNotNullNorNa(mod[[1]]$ser_))
           {
             for(i in 1:length(mod)){
               mod[[i]]$ser_           <- as.numeric(mod[[i]]$ser_)
             }
           }
           list_y <- list()
           list_x <- list()
           list_y <- clf$data$y
           list_x <- clf$data$X


           # get the value to maximize in the general optimization variable
           #mod$unpenalized_fit_ <- mod$rsq_
           for(i in 1:length(mod)){
             mod[[i]]$unpenalized_fit_ <- mod[[i]]$rsq_
           }
         },



         aic={ # THE AIC objective
           # TODO test it out
           for(i in 1:length(mod)){
             mod[[i]]$aic_             <- estimateCoefficientsIndividual(X=list_x[[i]], y=list_y[[i]], ind = mod[[i]]$indices_)$aic
             mod[[i]]$unpenalized_fit_ <- mod[[i]]$aic_
           }
         },
         { # else
           if(clf$params$warnings) warning('This objective method does not exist !')
         }
  )

  # apply the penalty based on model size
  for(i in 1:length(mod)){
    mod[[i]]$fit_ <- max(mod[[i]]$unpenalized_fit_ - clf$params$k_penalty * mod[[i]]$eval.sparsity, 0)
  }
  #mod$fit_ <- mod$fit_ - clf$params$k_penalty * sqrt(mod$eval.sparsity) # Square root when to make it softer

  return(mod)
}

#' Evaluates the fitting coefficents of a model object
#'
#' @description Evaluates the fitting coefficients of a model object.
#' @param mod: a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param eval.all: should the function evaluate all the scores (default:FALSE)
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @return a model object with the fitting scores evaluated
#' @export
evaluateModelRegression_ovo <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE)
{
  if(!isModel(mod[[1]]))
  {
    stop("evaluateModelRegression: the model to be evaluated does not exist.")
  }

  if(isModelSota(mod[[1]]))
  {
    if(clf$params$warnings) warning("evaluateModelRegression: no intercept for sota models. Returning unchanged.")
    return(mod)
  }
  mod.res <- list()
  for(i in 1:length(mod)){

    mod.res[[i]] <- mod[[i]]
  }

  for(i in 1:length(mod.res)){
    # If sparsity is not the same
    if(mod.res[[i]]$eval.sparsity > length(unique(mod.res[[i]]$indices_)))
    {
      if(clf$params$warnings) warning("evaluateModelRegression: An individual has at least one indice replicated")
      values2keep             <- which(mod.res[[i]]$indices_ == unique(mod.res[[i]]$indices_))
      mod.res[[i]]$indices_        <- mod.res[[i]]$indices_[values2keep]
      mod.res[[i]]$names_          <- mod.res[[i]]$names_ [values2keep]
      mod.res[[i]]$coeffs_         <- mod.res[[i]]$coeffs[values2keep]
      mod.res[[i]]$eval.sparsity   <- length(unique(mod.res[[i]]$indices_))
    }
  }

  # Compute score
  for(i in 1:length(mod.res)){
    if(is.null(mod.res[[i]]$score_) | force.re.evaluation)
    {
      scorelist <- getModelScore_ovo(mod = mod.res, X = X, clf = clf)
      mod.res[[i]]$score_ <- scorelist[[i]]$score_
      mod.res[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
      mod.res[[i]]$neg_score_ <- scorelist[[i]]$neg_score_

      if(is.null(mod.res[[i]]$score_))
      {
        return(NULL)
      }
    }
  }
  mod.res                   <- evaluateFit_ovo(mod = mod.res, X=X, y=y, clf=clf, force.re.evaluation = force.re.evaluation)
  return(mod.res)
}


#' Estimates the importance of each feature in the model object
#'
#' @description Estimates the importance of each feature in the model object
#' @param mod: a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param attribute: which attribute should be used to compute the importance (default:unpenalized_fit_)
#' @param plot.importance: should the function plot the improtance of the features (default:FALSE)
#' @return a model object with the importance of each feature computed. Negative importance of a feature means that the feature is not beneficial.
#' @export
estimateFeatureImportance_ovo <- function(mod, X, y, clf, attribute = "unpenalized_fit_", plot.importance = FALSE)
{
  if(!isModel(obj = mod[[1]]))
  {
    stop("estimateFeatureImportance: please provide a valid predomics model")
  }

  if(isModelSota(mod[[1]]))
  {
    if(clf$params$warnings) warning("estimateFeatureImportance: estimating feature importance is active only for BTR models")
    return(mod)
  }

  # recompute the model if the attribute is NA
  if(is.na(mod[[1]][[attribute]]))
  {
    mod   <- evaluateModel_ovo(mod = mod.tmp, X = X, y = y, clf = clf,
                                                   eval.all = TRUE, force.re.evaluation = TRUE, mode = "train")

  }
  for(i in 1:length(mod)){

    importance <- rep(NA, length(mod[[i]]$indices_))
    names(importance) <- mod[[i]]$names_

    if(length(mod[[i]]$indices_) == 1)
    {
      importance[1]             <- mod[[i]][[attribute]]
    }else
    {
      for(i in 1:length(mod[[i]]$indices_))
      {
        mod.tmp <- mod

        # omit one feature
        mod.tmp[[i]]$indices_        <- mod.tmp[[i]]$indices_[-i]
        mod.tmp[[i]]$names_          <- mod.tmp[[i]]$names_[-i]
        mod.tmp[[i]]$coeffs_         <- mod.tmp[[i]]$coeffs_[-i]
        mod.tmp[[i]]$eval.sparsity   <- mod.tmp[[i]]$eval.sparsity -1

        # recompute the model with one feature less
        mod.tmp                 <- evaluateModel_ovo(mod = mod.tmp, X = X, y = y, clf = clf, eval.all = TRUE, force.re.evaluation = TRUE, mode = "train")

        # compute the importance
        available.attributes <- c("fit_", "unpenalized_fit_", "auc_", "accuracy_", "cor_", "aic_",
                                  "intercept_", "eval.sparsity", "precision_", "recall_", "f1_")
        if(!attribute %in% available.attributes)
        {
          stop("estimateFeatureImportance: attribute does not exist.")
        }

        if(!attribute %in% names(mod[[i]]) | !attribute %in% names(mod.tmp[[i]]))
        {
          stop("estimateFeatureImportance: attribute does not exist in the models.")
        }

        importance[i]           <- mod[[i]][[attribute]] - mod.tmp[[i]][[attribute]]
      }
    }


    if(plot.importance)
    {
      barplot(sort(importance), col="darkgreen", horiz=TRUE, las=2)
    }
    mod[[i]]$importance_           <- importance

  }
  return(mod)
}


#' Evaluates the fitting score of a model object ovo
#'
#' @description Evaluates the fitting score of a model object ovo.
#' @param mod: a model object ovo
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param eval.all: should the function evaluate all the scores (default:FALSE)
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @param estim.feat.importance: evaluate the importance in the model object (default:FALSE)
#' @param mode: A choice from c("train", "test") indicates wether we wish to learn the threthold
#' of the model (default:"train") or not "test" for the c("terinter","bininter","ratio") languages
#' @return a model object with the fitting scores evaluated
#' @export

evaluateModel_ovo <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE, estim.feat.importance = FALSE, mode = 'train')
{

  if(mode != "train" & mode != "test")
  {
    stop("evaluateModel: mode should be one of c('train','test')")
  }


  if(!isModel(mod[[1]]))
  {
    # if not a model object but a valid index, create a model
    if(!is.list(mod))
    {
      mod <- individual(X = X, y = y, clf = clf, ind = mod[[1]]) # transform into a model
    }else
    {
      if(clf$params$warnings) warning("evaluateModel: the model to be evaluated does not exist, returning NULL.")
      return(NULL)
    }
  }






  # DON'T EVALUATE RATIO, TER and TERINTER MODELS WITHOUT NEGATIVE AND POSITIVE TERMS
  if(!isModelSota(mod[[1]]))
  {
    # at this stage the model should be a valid one. If not return NULL
    if(!isModel(mod[[1]]))
    {
      if(clf$params$warnings) warning("evaluateModel: the model to be evaluated does not exist and at this stage it should be one, returning NULL.")
      return(NULL)
    }

   # if(mod[[1]]$language == "ratio" | mod[[1]]$language == "ter" | mod[[1]]$language == "terinter")
    #{

     # if(length(table(sign(mod[[1]]$coeffs_))) != 2)
     # {
        #return(NULL)
      #}

    #}





  }

  # make a copy of the model object
  mod.res <- mod

  if(mode == "train")
  {
    for(i in 1:length(mod.res)){
    # reset the attributes that need to be recomputed
    # general attributes
    mod.res[[i]]$fit_                <- NA
    mod.res[[i]]$unpenalized_fit_    <- NA
    # classification
    mod.res[[i]]$auc_                <- NA
    mod.res[[i]]$accuracy_           <- NA
    mod.res[[i]]$precision_          <- NA
    mod.res[[i]]$recall_             <- NA
    mod.res[[i]]$f1_                 <- NA
    mod.res[[i]]$intercept_          <- NA # the intercept
    mod.res[[i]]$sign_               <- NA # the sign of the model
    # regression
    mod.res[[i]]$cor_                <- NA
    mod.res[[i]]$rsq_                <- NA # r2
    mod.res[[i]]$ser_                <- NA # standard error of the mean
    mod.res[[i]]$aic_                <- NA
    mod.res[[i]]$score_              <- NA # model score
    mod.res[[i]]$pos_score_          <- NA # the positive model score
    mod.res[[i]]$neg_score_          <- NA # the negative model score
  }
  }

  # If this is a regression problem no need to find intercepts etc...
  if(clf$params$objective == "cor")
  {
    if(!isModelSota(mod.res[[1]]))
    {
      mod.res <- evaluateModelRegression_ovo(mod = mod.res, X = X, y = y, clf = clf, eval.all = eval.all, force.re.evaluation = force.re.evaluation)
      return(mod.res)
    }else
    {
      if(clf$params$warnings) warning("evaluateModel: evaluating a sota model in correlation objective")
    }
  }







  if(isModelSota(mod.res[[1]]))
  {
    # feature importance estimation will be switched off for the sotas, since the internal model structure is very different
    if(estim.feat.importance)
    {
      estim.feat.importance = FALSE
    }
  }

for(i in 1:length(mod.res)){
  # If sparsity is not the same
  if(mod.res[[i]]$eval.sparsity != length(unique(mod.res[[i]]$indices_)))
  {
    if(clf$params$warnings) warning("evaluateModel: An individual has at least one indice replicated")
    values2keep           <- which(mod.res[[i]]$indices_ == unique(mod.res[[i]]$indices_))
    mod.res[[i]]$indices_      <- mod.res[[i]]$indices_[values2keep]
    mod.res[[i]]$names_        <- mod.res[[i]]$names_[values2keep]
    mod.res[[i]]$coeffs_       <- mod.res[[i]]$coeffs[values2keep]
    mod.res[[i]]$eval.sparsity <- length(unique(mod.res[[i]]$indices_))
  }
}


  # first evaluate the fit
  mod.res <- evaluateFit_ovo(mod = mod.res, X=X, y=y, clf=clf, force.re.evaluation = force.re.evaluation, mode = mode)

  # compute all other evaluation metrics
  if(eval.all)
  {
    # At this stage this should not happen but for debug stop it
    if((!myAssertNotNullNorNa(mod.res[[1]]$intercept_) | !myAssertNotNullNorNa(mod.res[[1]]$sign_)) & !isModelSota(mod.res[[1]]))
    {
      if(clf$params$warnings) warning("evaluateModel: model without intercept at this stage is not normal.")
      return(NULL)
    }

    mod.res         <- evaluateAdditionnalMetrics_ovo(mod = mod.res, X = X, y = y, clf = clf, mode = mode)
    if(!isModel(mod.res[[1]]))
    {
      # if(clf$params$warnings) warning("evaluateModel: returning an empty model.")
      return(NULL)
    }
  }

  # if BTR
  if(!isModelSota(mod.res[[1]]))
  {
    if(estim.feat.importance)
    {
      mod.res  <- estimateFeatureImportance_ovo(mod = mod.res, X = X, y = y, clf = clf, plot.importance = FALSE)
    }
  }

  # At this stage this should not happen but for debug stop it
  if(!myAssertNotNullNorNa(mod.res[[1]]$unpenalized_fit_))
  {
    if(clf$params$warnings) warning("evaluateModel: model does not have a valid unpenalized_fit_ attribute.")
    return(NULL)
  }

  return(mod.res)
}


#' @description Evaluates an entire population of models, that be predomics objects or individuals
#'
#' @import foreach
#' @title evaluatePopulation
#' @name evaluatePopulation
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the class vector
#' @param clf: the object containing the classifier information
#' @param pop: the population of models to be evaluated
#' @param eval.all: should the function evaluate all the scores for each of the models (default:FALSE)
#' @param force.re.evaluation: re-evaluate all the scores even if they exist for each of the models (default:FALSE)
#' @param estim.feat.importance: evaluate the importance in the model object for each of the models (default:FALSE)
#' @param mode: A choice from c("train", "test") indicates wether we wish to learn the threthold
#' of each of the models (default:"train") or not "test" for the c("terinter","bininter","ratio") languages
#' @param delete.null.models: should null indivuals be deleted (default:TRUE)
#' @param lfolds: compute evaluation in crossval (default:NULL)
#' @return an individual object
#' @export
evaluatePopulation_ovo <- function(X, y, clf, pop, eval.all = FALSE,
                               force.re.evaluation = FALSE,
                               estim.feat.importance = FALSE,
                               mode = "train",
                               delete.null.models = TRUE,
                               lfolds = NULL)
{
  # test the classifier object
  if(!isClf(clf))
  {
    stop("fit: please provide a valid classifier object!")
  }

  # clean the null models
  if(delete.null.models)
  {
    pop <- pop[!sapply(pop, is.null)]
  }

  # if crossval call this in recursive
  if(!is.null(lfolds))
  {
    pop.lfolds <- list()
    for(f in 1:length(lfolds))
    {
      if(mode == "train")
      {
        pop.lfolds[[f]] <- evaluatePopulation_ovo(X[,-lfolds[[f]]],
                                              y[-lfolds[[f]]],
                                              clf,
                                              pop,
                                              eval.all = eval.all,
                                              force.re.evaluation = force.re.evaluation,
                                              estim.feat.importance = estim.feat.importance,
                                              mode = mode,
                                              delete.null.models = delete.null.models,
                                              lfolds = NULL)
      }else # test
      {
        pop.lfolds[[f]] <- evaluatePopulation_ovo(X[,lfolds[[f]]],
                                              y[lfolds[[f]]],
                                              clf,
                                              pop,
                                              eval.all = eval.all,
                                              force.re.evaluation = force.re.evaluation,
                                              estim.feat.importance = estim.feat.importance,
                                              mode = mode,
                                              delete.null.models = delete.null.models,
                                              lfolds = NULL)
      }
    }
    names(pop.lfolds) <- names(lfolds)
    return(pop.lfolds)
  }

  # otherwise we continue to evaluate normally the population

  res <- list()
  for (i in 1:length(pop)) # for all the individuals in the population
  {
    mod <- pop[[i]]
    if(!is.null(mod))
    {
      res[[i]] <- evaluateModel_ovo(mod = mod,
                                X = X,
                                y = y,
                                clf = clf,
                                eval.all = eval.all,
                                force.re.evaluation = force.re.evaluation,
                                estim.feat.importance = estim.feat.importance,
                                mode = mode)
      #print(i)
    } # end else existance pop
  } # end for loop

  # clean population after evaluation as well
  if(delete.null.models)
  {
    res <- cleanPopulation(pop = res, clf = clf)
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
sparseVecToModel_ovo <- function(X, y, v, clf, eval.all=FALSE, obj = NULL)
{
  if(is.null(v))
  {
    return(NULL)
  }

 res <- individual_ovo(X, y, clf, ind = v,  eval.all = eval.all, obj = obj)

  return(res)

}


#' listOfSparseVecToListOfModels
#'
#' @description Converts an list of "SparseVec" objects onto a list of predomics objects
#' @import snow
#' @param X: dataset
#' @param y: labels
#' @param clf: classifier
#' @param v: list of vectors of coeffs. For example, v=list( c(0.0,1.0,0.0,-1.0) , c(1.0,1.0,0.0,0.0) , c(0.0,1.0,1.0,-1.0) )
#' @param lobj: a list of objects to add as elements in the model objects if not null (default:NULL)
#' @param eval.all: evaluate population (default:FALSE)
#' @return an model object
#' @export
listOfSparseVecToListOfModels_ovo <- function(X, y, clf, v, lobj = NULL, eval.all = FALSE)
{

  if(!is.null(lobj))
  {
    if(!is.list(lobj))
    {
      stop("listOfDenseVecToListOfModels: lobj should be a list of objects.")
    }

    if(length(lobj) != length(v))
    {
      stop("listOfDenseVecToListOfModels: lobj should be a list the same length as v")
    }
  }

  if(length(v) == 0)
  {
    if(clf$params$warnings) warning("listOfSparseVecToListOfModels: empty list returning NULL")
    return(NULL)
  }

  pop <- list()

    for(i in 1:length(v))
    {
      model <- v[[i]]
      pop[[i]] <-  sparseVecToModel_ovo(X, y, model, clf, eval.all=TRUE, obj = lobj[[i]])
    }

  return(pop)
}





#########################################################################################
#########################################################################################

