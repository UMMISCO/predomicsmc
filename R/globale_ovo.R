################################################################
# @script:  globale_ovo.R
# @author:  Edi Prifti
# @author:  Fabien KAMBU MBUANGI
# @author:  Jean-Daniel Zucker
# @date:    Juin 2023
################################################################

#' Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function
#' @title getSign_mc
#' @description Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function, one versus one.
#' @import foreach
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param parallel.local: weather or not to run in //
#' @return a list of +1 & -1 for each one-vs-all classification of our dataset
#' @export
#'
getSign_mc <- function(X, y, clf = NULL, approch = "ovo", parallel.local = FALSE) {

  nClasse <- unique(y)
  list_Sign <- list() # List of different combinations of Sign
  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
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
  for (i in 1:(length(list_y))) {
    Sign <- getSign(X = list_X[[i]], y = list_y[[i]], clf = clf, parallel.local = FALSE)
    list_Sign[[i]] <- Sign
  }
  res <- list_Sign

  return(res)
}

#' Evaluates the accuracy of a model
#' @title evaluateAccuracy_mc
#' @description This function one versus one, evaluates the accuracy of either (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign.
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param force.re.evaluation: evaluate again all the elements needed for accuracy (default:FALSE)
#' @param mode: training or test mode. If training, the funciton maximizes accuracy.
#' @return either (1) list a model whose evaluation parameters are updated or (2) the accuracy
#' @export
evaluateAccuracy_mc <- function(mod = NULL, X, y, clf, force.re.evaluation = FALSE, approch = "ovo", mode = "train")
{
  nClasse <- unique(y)
  list_mod <- list() # list of models
  listmod <- list() # list of models
  list_y <- list() # list of y
  list_X <- list() # list of X
  listcoeffs <- list() # list of coeffs_
  listX <- list() # list of X
  listXmin <- list() # list of min X
  listXmax <- list() # list of max X
  listy <- list() # list of y
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y
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

  list_mod <- mod
  for(i in 1:(length(list_y))){
    clf$coeffs_ <- listcoeffs[[i]]
    clf$data$X <- listX[[i]]
    clf$data$X.min <- listXmin[[i]]
    clf$data$X.max <- listXmax[[i]]
    clf$data$y <- listy[[i]]
    modacc <- evaluateAccuracy(mod = list_mod[[i]], X= list_X[[i]], y= list_y[[i]], clf, force.re.evaluation = force.re.evaluation, mode = mode)
    listmod[[i]] <- modacc
  }
  mod <- listmod

  return(mod)
}

#' Computes the AUC of a model
#' @title evaluateAUC_mc
#' @description Computes the AUC of a model
#' @param score: the ^y score of the model
#' @param y: the response vector
#' @param sign: in which direction to make the comparison? "auto" (default): automatically define in which group
#' the median is higher and take the direction accordingly. ">": if the predictor values for the control group
#' are higher than the values of the case group (controls > t >= cases). "<": if the predictor values for the
#' control group are lower or equal than the values of the case group (controls < t <= cases).
#' @return  a list of auc values
#' @importFrom pROC roc
evaluateAUC_mc <- function(score, y,approch = "ovo", sign = '>')
{
  nClasse <- unique(y)
  list_y <- list() # list of y
  list_X <- list() # list of X
  sign = sign
  list_auc <- list()
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
  # list values auc_
  for(i in 1:length(list_y)){
    au <- evaluateAUC(score, y = list_y[[i]], sign = sign)
    list_auc[[i]] = au
  }
  auc <- list_auc
  return(auc)
}

#' Compute other prediction scores such as precision, recall and f-score
#' @title evaluateAdditionnalMetrics_mc
#' @description This function computes prediction scores based on the confusion matrix such as accuracy, precision, recall and f-score
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param mode: training or testing mode
#' @return a list of models whose evaluation parameters are updated
evaluateAdditionnalMetrics_mc <- function(mod, X, y, clf, approch = "ovo", mode = "train")
{
  nClasse <- unique(y)
  list_mod <- list() # list of mod
  listmod <- list()
  list_y <- list() # list of y
  list_X <- list() # list of X
  listcoeffs <- list() # list of coeffs
  listX <- list()
  listXmin <- list() # list of min X
  listXmax <- list() # list of max X
  listy <- list()
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y

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

  listcoeffs <- clf$coeffs_
  list_mod <- mod
  for(i in 1:(length(list_y))){
    clf$coeffs_ <- listcoeffs[[i]]
    clf$data$X <- listX[[i]]
    clf$data$X.min <- listXmin[[i]]
    clf$data$X.max <- listXmax[[i]]
    clf$data$y <- listy[[i]]
    modmetric <- evaluateAdditionnalMetrics(mod = list_mod[[i]], X= list_X[[i]], y= list_y[[i]], clf=clf, mode = mode)
    listmod[[i]] <- modmetric
  }
  mod <- listmod

  return(mod)
}



#' Evaluates the fitting coefficents of a model object
#' @title evaluateModelRegression_ovo
#' @description Evaluates the fitting coefficients of a model object.
#' @param mod: a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param eval.all: should the function evaluate all the scores (default:FALSE)
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @return a model object with the fitting scores evaluated
#' @export
evaluateModelRegression_mc <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE)
{
  nClasse <- unique(y)
  list_mod <- list()
  listmod <- list()
  list_y <- list()
  list_X <- list()
  listcoeffs <- list()

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
  listcoeffs <- clf$coeffs_
  list_mod <- mod
  for(i in 1:(length(list_mod))){
    clf$coeffs_ <- listcoeffs[[i]]
    mod.res <- evaluateModelRegression(mod = list_mod[[i]], X= list_X[[i]], y= list_y[[i]], clf=clf, eval.all = eval.all,force.re.evaluation = force.re.evaluation)
    listmod[[i]] <- mod.res
  }
  mod.res <- listmod
  return(mod.res)
}



#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object, multi class.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @param mode: A choice from c("train", "test") indicates wether we wish to learn the threthold
#' of the model (default:"train") or not "test" for the c("terinter","bininter","ratio") languages
#' @return a model object with the fitting score
evaluateFit_mc <- function(mod, X, y, clf, force.re.evaluation = FALSE,approch="ovo", mode = "train")
{
  nClasse <- unique(y)
  list_mod <- list() # list of mod
  listmod <- list()
  list_y <- list() # list of y
  list_X <- list() # list of X
  scorelist <- list() # list of score
  listcoeffs <- list() # list of coeffs
  listX <- list()
  listXmin <- list() # list min of X
  listXmax <- list() # list max of X
  listy <- list()
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y

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

  listcoeffs <- clf$coeffs_
  list_mod <- mod

  for(i in 1: length(list_y)) {

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
        mod[[1]] <- names2index(X = list_X[[1]], var.names = mod[[1]])
      }
      clf$coeffs_ <- listcoeffs[[1]]
      mod[[1]] <- individual(X= list_X[[1]], y= list_y[[1]], clf = clf, ind = mod[[1]])
    }
  }


  # compute the score in the case it is asked to recompute
  for(i in 1: length(list_y)) {
    if(force.re.evaluation)
    {
      clf$coeffs_ <- listcoeffs[[i]]
      clf$data$X <- listX[[i]]
      clf$data$X.min <- listXmin[[i]]
      clf$data$X.max <- listXmax[[i]]
      clf$data$y <- listy[[i]]
      scorelist[[i]] <- getModelScore(mod = list_mod[[i]], X = list_X[[i]], clf = clf, force.re.evaluation = force.re.evaluation)
      mod[[i]]$score_ <- scorelist[[i]]$score_
      mod[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
      mod[[i]]$neg_score_ <- scorelist[[i]]$neg_score_

    }
    else
    {

      # compute the score if it does not exist
      if(!myAssertNotNullNorNa(mod[[i]]$score_))
      {
        clf$coeffs_ <- listcoeffs[[i]]
        clf$data$X <- listX[[i]]
        clf$data$X.min <- listXmin[[i]]
        clf$data$X.max <- listXmax[[i]]
        clf$data$y <- listy[[i]]
        scorelist[[i]] <- getModelScore(mod = list_mod[[i]], X = list_X[[i]], clf = clf, force.re.evaluation = force.re.evaluation)

        if(!any(is.na(scorelist[[i]])) | isModelSota(mod[[i]]))
        {
          mod[[i]]$score_ <- scorelist[[i]]$score_
          mod[[i]]$pos_score_ <- scorelist[[i]]$pos_score_
          mod[[i]]$neg_score_ <- scorelist[[i]]$neg_score_
        }
      }else
      {
        # in the case the score has been computed before but for an other X, we recompute
        if(length(mod[[i]]$score_) != ncol(list_X[[i]]))
        {
          clf$coeffs_ <- listcoeffs[[i]]
          clf$data$X <- listX[[i]]
          clf$data$X.min <- listXmin[[i]]
          clf$data$X.max <- listXmax[[i]]
          clf$data$y <- listy[[i]]
          scorelist[[i]] <- getModelScore(mod = list_mod[[i]], X = list_X[[i]], clf = clf, force.re.evaluation = force.re.evaluation)

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


  for(i in 1:length(list_y)) {
    # if after all the above steps we still don't have a score than we kill the model.
    if(!myAssertNotNullNorNa(mod[[i]]$score_))
    {
      return(NULL)
    }
  }

  for(i in 1:length(list_y)) {
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
             for(i in 1: length(list_y)){
               clf$coeffs_ <- listcoeffs[[i]]
               clf$data$X <- listX[[i]]
               clf$data$X.min <- listXmin[[i]]
               clf$data$X.max <- listXmax[[i]]
               clf$data$y <- listy[[i]]
               if((mod[[i]]$language != "ter" & mod[[i]]$language != "bin") & !isModelSota(mod[[i]]))
               {

                 mod[[i]]                <- evaluateIntercept(X  = list_X[[i]], y  = list_y[[i]], clf = clf, mod = mod[[i]])
               }else
               {
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

               # sanity check
               if(!clf$params$evalToFit %in% names(mod[[i]]))
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

                 aucg                 <- evaluateAUC(score = mod[[i]]$score, y = list_y[[i]], sign = ">")
                 aucl                 <- evaluateAUC(score = mod[[i]]$score, y = list_y[[i]], sign = "<")
                 mod[[i]]$auc_             <- max(aucg, aucl)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$auc_


               }

               # in case it is accuracy
               if(clf$params$evalToFit == "accuracy_") # in this case the auc will be computed in evaluate other metrics
               {
                 clf$coeffs_ <- listcoeffs[[i]]
                 clf$data$X <- listX[[i]]
                 clf$data$X.min <- listXmin[[i]]
                 clf$data$X.max <- listXmax[[i]]
                 clf$data$y <- listy[[i]]

                 mod[[i]] <- evaluateAccuracy(mod=mod[[i]], X=list_X[[i]], y=list_y[[i]], clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$accuracy_
               }

               # otherwise compute the rest
               if(clf$params$evalToFit != "auc_" & clf$params$evalToFit != "accuracy_")
               {
                 clf$coeffs_ <- listcoeffs[[i]]
                 clf$data$X <- listX[[i]]
                 clf$data$X.min <- listXmin[[i]]
                 clf$data$X.max <- listXmax[[i]]
                 clf$data$y <- listy[[i]]
                 mod[[i]] <- evaluateAdditionnalMetrics(mod = mod[[i]], X = list_X[[i]], y = list_y[[i]], clf = clf, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]][[clf$params$evalToFit]]
                 # compte accuracy also
                 mod[[i]] <- evaluateAccuracy(mod = mod[[i]], X=list_X[[i]], y=list_y[[i]], clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 # and auc, since these are helpful information


                 aucg                 <- evaluateAUC(score =  mod[[i]]$score, y = list_y[[i]], sign = ">")
                 aucl                 <- evaluateAUC(score = mod[[i]]$score, y =list_y[[i]], sign = "<")
                 mod[[i]]$auc_             <- max(aucg, aucl)

               }
             }
           }

           # if test mode, we don't recompute the intercept
           else
           {
             for(i in 1:length(list_y)) {
               clf$coeffs_ <- listcoeffs[[i]]
               # sanity check
               if(!clf$params$evalToFit %in% names(mod[[i]]))
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

                 aucg                 <- evaluateAUC(score = mod[[i]]$score, y = list_y[[i]], sign = ">")
                 aucl                 <- evaluateAUC(score = mod[[i]]$score, y = list_y[[i]], sign = "<")
                 mod[[i]]$auc_             <- max(aucg, aucl)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$auc_

               }

               # in case it is accuracy
               if(clf$params$evalToFit == "accuracy_") # in this case the auc will be computed in evaluate other metrics
               {
                 clf$coeffs_ <- listcoeffs[[i]]
                 clf$data$X <- listX[[i]]
                 clf$data$X.min <- listXmin[[i]]
                 clf$data$X.max <- listXmax[[i]]
                 clf$data$y <- listy[[i]]
                 mod[[i]] <- evaluateAccuracy(mod = mod[[i]], X= list_X[[i]], y= list_y[[i]], clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$accuracy_
               }

               # otherwise compute the rest
               if(clf$params$evalToFit != "auc_" & clf$params$evalToFit != "accuracy_")
               {
                 clf$coeffs_ <- listcoeffs[[i]]
                 clf$data$X <- listX[[i]]
                 clf$data$X.min <- listXmin[[i]]
                 clf$data$X.max <- listXmax[[i]]
                 clf$data$y <- listy[[i]]
                 mod[[i]] <- evaluateAdditionnalMetrics(mod = mod[[i]], X  = list_X[[i]], y =  list_y[[i]], clf = clf, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]][[clf$params$evalToFit]]
                 # compte accuracy also
                 mod[[i]] <- evaluateAccuracy(mod=mod[[i]], X= list_X[[i]], y= list_y[[i]], clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 # and auc, since these are helpful information
                 for(j in 1:length(mod)){
                   scorelist[[j]] <- mod[[j]]$score
                 }

                 aucg                 <- evaluateAUC(score = mod[[i]]$score, y = list_y[[i]], sign = ">")
                 aucl                 <- evaluateAUC(score = mod[[i]]$score, y = list_y[[i]], sign = "<")
                 mod[[i]]$auc_             <- max(aucg, aucl)

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

             for(i in 1:length(list_y)){
               y <- list_y[[i]]
               ina      <- is.na(mod[[i]]$score_) | is.na(y) | is.infinite(mod[[i]]$score) | is.infinite(y)
               mod[[i]]$cor_ <- abs(cor(mod[[i]]$score[!ina], y[!ina], method = "pearson"))

               # # for optimization reasons the cor will be a pearson implementation,
               # y is already ranked for objective being "cor" performed in the fit()
               # # We just need to tank the score to obtain the same result as a spearman correlation.
               #abs(cor.test(mod$score, y, objective = "spearman")$estimate)

               # use the r2 instead
               score.scaled <- as.vector(scale(mod[[i]]$score_[!ina], center = TRUE, scale = TRUE))
               y.scaled <- as.vector(scale(y[!ina], center = TRUE, scale = TRUE))
               mod[[i]]$rsq_ <- abs(cor(mod[[i]]$score[!ina], y[!ina], method = "pearson"))^2
               mod[[i]]$ser_ <- sqrt(sum((score.scaled - y.scaled)^2, na.rm = TRUE)/(length(score.scaled) - 2))

             }})

           for(i in 1:length(list_y)){

             if(is.null(mod[[i]])) return(mod[[i]])
             if(myAssertNotNullNorNa(mod[[i]]$cor_))
             {
               mod[[i]]$cor_           <- as.numeric(mod[[i]]$cor_)
             }
             if(myAssertNotNullNorNa(mod[[i]]$rsq_))
             {
               mod[[i]]$rsq_           <- as.numeric(mod[[i]]$rsq_)
             }
             if(myAssertNotNullNorNa(mod[[i]]$ser_))
             {
               mod[[i]]$ser_           <- as.numeric(mod[[i]]$ser_)
             }

             # get the value to maximize in the general optimization variable
             #mod$unpenalized_fit_ <- mod$rsq_
             mod[[i]]$unpenalized_fit_ <- mod[[i]]$rsq_
           } },
         aic={ # THE AIC objective
           # TODO test it out
           for(i in 1:length(list_mod)){
             mod[[i]]$aic_             <- estimateCoefficientsIndividual(X=list_X[[i]], y=list_y[[i]], ind = mod[[i]]$indices_)$aic
             mod[[i]]$unpenalized_fit_ <- mod[[i]]$aic_
           }},
         { # else
           if(clf$params$warnings) warning('This objective method does not exist !')
         }
  )

  # apply the penalty based on model size
  for(i in 1:length(list_y)) {
    mod[[i]]$fit_ <- max(mod[[i]]$unpenalized_fit_ - clf$params$k_penalty * mod[[i]]$eval.sparsity, 0)
  }
  return(mod)
}

#' Evaluates the fitting score of a model object ovo
#' @title evaluateModel_mc
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
#' @return a model ovo object with the fitting scores evaluated
#' @export

evaluateModel_mc <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE, estim.feat.importance = FALSE, approch="ovo",mode = 'train')
{

  if(mode != "train" & mode != "test")
  {
    stop("evaluateModel: mode should be one of c('train','test')")
  }

  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list() # list of mod
  list_y <- list() # list of y
  list_X <- list() # list of X
  listcoeffs <- list()
  list_mod <- list()
  mod.res <- list()
  listX <- list()
  listXmin <- list() # list min of X
  listXmax <- list() # list max of X
  listy <- list()
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y
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

list_mod = mod

  if (mode == "test") {
    if (isModelSota(mod)) {
      list_mod <- list()

      for(ite in 1:(length(list_y))){
        list_mod[[ite]] <- mod
      }

      for (i in 1:length(list_y)) {
        # Create a vector with unique values from  list_y[[i]]
        valeurs_uniques <- unique(unlist(list_y[[i]]))

        #  Initialize the variables obj, names_, indices_, etc. to NULL.
        obj <- NULL
        names_ <- NULL
        indices_ <- NULL
        score_ = NULL
        pos_score_ = NULL
        neg_score_ = NULL
        confusionMatrix_ = NULL

        for (j in 1:length(list_y)) {
          # Create a vector with unique values from list_mod[[j]]$obj$y
          boo <- unique(as.vector(mod$obj[[j]]$y))

          # Check if the vectors contain the same elements.
          if (all(valeurs_uniques %in% boo) && all(boo %in% valeurs_uniques)) {
            obj <- mod$obj[[j]]
            names_ <- mod$names_[[j]]
            indices_ <- mod$indices_[[j]]
            score_ <- mod$score_[[j]]
            pos_score_ <- mod$pos_score_[[j]]
            neg_score_ <- mod$neg_score_[[j]]
            confusionMatrix_ <- mod$confusionMatrix_[[j]]
            break
          }
        }

        if (!is.null(obj)) {
          list_mod[[i]]$obj <- obj
          list_mod[[i]]$names_ <- names_
          list_mod[[i]]$indices_ <- indices_
          list_mod[[i]]$score_ <- score_
          list_mod[[i]]$pos_score_ <- pos_score_
          list_mod[[i]]$neg_score_ <- neg_score_
          list_mod[[i]]$confusionMatrix_ <- confusionMatrix_
        } else {
          print(paste("No match found for list_y[", i, "]."))
        }
      }
    } else{

    liste_indicess <- list()
    liste_namess <- list()
    liste_coeffss <- list()
    liste_signn <- list()
    liste_scoree <- list()
    liste_pos <- list()
    liste_neg <- list()
    liste_matrix <- list()
    list_mod <- list()
    liste_indicess = mod$indices_
    liste_namess = mod$names_
    liste_coeffss = mod$coeffs_
    liste_signn = mod$sign_
    liste_scoree = mod$score_
    liste_pos = mod$pos_score_
    liste_neg = mod$neg_score_
    liste_matrix = mod$confusionMatrix_

for (j in 1:(length(list_y))){
  temp_list <- list(
    learner = mod$learner,
    language = mod$language,
    objective = mod$objective,
    evalToFit = mod$evalToFit,
    indices_ = liste_indicess[[j]],
    names_ = liste_namess[[j]],
    coeffs_ = liste_coeffss[[j]],
    fit_ = mod$fit_,
    unpenalized_fit_ = mod$unpenalized_fit_,
    auc_ = mod$auc_,
    accuracy_ = mod$accuracy_,
    cor_ = mod$cor_,
    aic_ = mod$aic_,
    intercept_ = mod$intercept_,
    eval.sparsity = mod$eval.sparsity,
    precision_ = mod$precision_,
    recall_ = mod$recall_,
    f1_ = mod$f1_,
    sign_ = liste_signn[[j]],
    rsq_ = mod$rsq_,
    ser_ = mod$ser_,
    score_ = liste_scoree[[j]],
    pos_score_ = liste_pos[[j]],
    neg_score_ = liste_neg[[j]],
    confusionMatrix_ = liste_matrix[[j]]
  )
  list_mod[[j]] <- temp_list
}
}
  }
mod_res <- list()
mod_res <- list_mod[[1]]
mod.res = list_mod

  # DON'T EVALUATE RATIO, TER and TERINTER MODELS WITHOUT NEGATIVE AND POSITIVE TERMS
  for(i in 1:length(list_mod)){
    if(!isModelSota(list_mod[[i]]))
    {
      # at this stage the model should be a valid one. If not return NULL
      if(!isModel(list_mod[[i]]))
      {
        if(clf$params$warnings) warning("evaluateModel: the model to be evaluated does not exist and at this stage it should be one, returning NULL.")
        return(NULL)
      }
    }
  }


  # make a copy of the model object
  #mod.res <- list_mod

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
    for(i in 1:length(mod.res)) {
      if(!isModelSota(mod.res[[i]])){
        mod.res <- evaluateModelRegression_mc(mod = mod.res, X = X, y = y, clf = clf, eval.all = eval.all, force.re.evaluation = force.re.evaluation)
        return(mod.res)
      }
      else
      {
        if(clf$params$warnings) warning("evaluateModel: evaluating a sota model in correlation objective")
      }
    }
  }

  for(i in 1:length(mod.res)){
    if(isModelSota(mod.res[[i]]))
    {
      # feature importance estimation will be switched off for the sotas, since the internal model structure is very different
      if(estim.feat.importance)
      {
        estim.feat.importance = FALSE
      }
    }
  }




  for(i in 1:length(list_y)){
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
  mod.res <- evaluateFit_mc(mod = mod.res, X=X, y=y, clf=clf, force.re.evaluation = force.re.evaluation,approch = approch, mode = mode)

  # compute all other evaluation metrics
  if(eval.all)
  {
    for(i in 1:length(mod.res)){
      # At this stage this should not happen but for debug stop it
      if((!myAssertNotNullNorNa(mod.res[[i]]$intercept_) | !myAssertNotNullNorNa(mod.res[[i]]$sign_)) & !isModelSota(mod.res[[i]]))
      {
        if(clf$params$warnings) warning("evaluateModel: model without intercept at this stage is not normal.")
        return(NULL)
      }
    }

    mod.res  <- evaluateAdditionnalMetrics_mc(mod = mod.res, X = X, y = y, clf = clf, approch = approch, mode = mode)
    for(i in 1:length(mod.res)){
      if(!isModel(mod.res[[i]]))
      {
        # if(clf$params$warnings) warning("evaluateModel: returning an empty model.")
        return(NULL)
      }
    }
  }

  # if BTR
  for(i in 1:length(mod.res)) {
    if(!isModelSota(mod.res[[i]]))
    {
      if(estim.feat.importance)
      {

        clf$coeffs_ <- listcoeffs[[i]]
        clf$data$X <- listX[[i]]
        clf$data$X.min <- listXmin[[i]]
        clf$data$X.max <- listXmax[[i]]
        clf$data$y <- listy[[i]]
        mod.res[[i]]  <- estimateFeatureImportance(mod = mod.res[[i]], X = list_X[[i]], y = list_y[[i]], clf = clf, plot.importance = FALSE)
      }
    }
  }

  for(i in 1:length(mod.res)){
    # At this stage this should not happen but for debug stop it
    if(!myAssertNotNullNorNa(mod.res[[i]]$unpenalized_fit_))
    {
      if(clf$params$warnings) warning("evaluateModel: model does not have a valid unpenalized_fit_ attribute.")
      return(NULL)
    }
  }

  #Unification phase of the multiclass model in binary
  #mod.res <- mod.res
  if(mode == "train"){

  mod.res <- mod.res
  nClass = length(mod.res)

  learnerr <- mod.res[[1]]$learner
  languagee <- mod.res[[1]]$language
  objectivee <- mod.res[[1]]$objective
  evalToFitt <- mod.res[[1]]$evalToFit
  indicess <- list()
  fit_ovo = 0
  unpenalized_fit_ovo = 0
  auc_ovo =0
  accuracy_ovo = 0
  intercept_ovo <- 0
  list_intercept_ovo <- list()
  eval.sparsity_ovo <- 0
  precision_ovo = 0
  recall_ovo = 0
  f1_ovo = 0
  confusionMatrix_ovo <- list()
  namess <- list()
  coeffss <- list()
  corr <- NA
  aicc <- NA
  signn <- list()
  rsqq <- NA
  serr <- NA
  scoree <- list()
  pos_scoree <- list()
  neg_scoree <- list()

  for(i in 1:length(mod.res)){
    indicess[[i]] = mod.res[[i]]$indices_
    namess[[i]] = mod.res[[i]]$names_
    coeffss[[i]] = mod.res[[i]]$coeffs_
    fit_ovo = fit_ovo + mod.res[[i]]$fit_
    unpenalized_fit_ovo= unpenalized_fit_ovo + mod.res[[i]]$unpenalized_fit_
    auc_ovo = auc_ovo + mod.res[[i]]$auc_
    accuracy_ovo = accuracy_ovo + mod.res[[i]]$accuracy_
    intercept_ovo =  mod.res[[i]]$intercept_[[1]]
    list_intercept_ovo[[i]] =  mod.res[[i]]$intercept_
    eval.sparsity_ovo = eval.sparsity_ovo + mod.res[[i]]$eval.sparsity
    signn[[i]] =  mod.res[[i]]$sign_
    precision_ovo = precision_ovo + mod.res[[i]]$precision_
    recall_ovo = recall_ovo + mod.res[[i]]$recall_
    f1_ovo = f1_ovo + mod.res[[i]]$f1_
    scoree[[i]] = mod.res[[i]]$score_
    pos_scoree[[i]] = mod.res[[i]]$pos_score_
    neg_scoree[[i]] = mod.res[[i]]$neg_score_
    confusionMatrix_ovo[[i]] = mod.res[[i]]$confusionMatrix_
  }
  # retrieve average of evaluation metric
  fit_ovo = fit_ovo/nClass
  unpenalized_fit_ovo = unpenalized_fit_ovo/nClass
  auc_ovo = auc_ovo/nClass
  accuracy_ovo = accuracy_ovo/nClass
  intercept_ovo = intercept_ovo
  eval.sparsity_ovo =  eval.sparsity_ovo/nClass
  precision_ovo = precision_ovo/nClass
  recall_ovo = recall_ovo/nClass
  f1_ovo = f1_ovo/nClass
  confusionMatrix_ovo = confusionMatrix_ovo

  #generate a single model output
  mod_res$learner <- learnerr
  mod_res$language <- languagee
  mod_res$objective <- objectivee
  mod_res$evalToFit <- evalToFitt

  if(isModelSota(mod.res[[1]])){

    list_obj <- list()
    for(ko in 1:(length(mod.res))){
      list_obj[[ko]] <-  mod.res[[ko]]$obj
    }

  }

  mod_res$coeffs_ <- coeffss
  mod_res$indices_ <- indicess
  mod_res$names_ <- namess


  if(isModelSota(mod_res)){
    mod_res$obj <- list_obj

  }

  mod_res$fit_ <- fit_ovo
  mod_res$unpenalized_fit_ <- unpenalized_fit_ovo
  mod_res$auc_ <-auc_ovo
  mod_res$accuracy_ <- accuracy_ovo
  mod_res$intercept_ <-  intercept_ovo
  mod_res$list_intercept_ovo <- list_intercept_ovo
  mod_res$eval.sparsity <- eval.sparsity_ovo
  mod_res$precision_ <- precision_ovo
  mod_res$recall_ <- recall_ovo
  mod_res$f1_ <- f1_ovo
  mod_res$cor_ <- corr
  mod_res$aic_ <- aicc
  mod_res$sign_ <- signn
  mod_res$rsq_ <- rsqq
  mod_res$ser_ <- serr
  mod_res$score_ <- scoree
  mod_res$pos_score_ <- pos_scoree
  mod_res$neg_score_ <- neg_scoree
  mod_res$confusionMatrix_ <- confusionMatrix_ovo
  mod.res <- list()
  mod.res <- mod_res
  }

  else {

    mod_f1 <- numeric(length(mod.res))  # Initialize a vector to store the f1

  # Loop to retrieve the f1 for each model.
  for (i in seq_along(mod.res)) {
    mod_f1[i] <- mod.res[[i]]$f1_
  }

  # Find the index of the model with the highest f1
  index_best_model <- which.max(mod_f1)

  # Access the model with the highest f1
  best_model <- mod.res[[index_best_model]]
  mod.res <- list()
  mod.res = best_model
  }
  return(mod.res)
}


#' evaluate Model multi class
#' @title evaluateModel_mc
#' @description Evaluates an entire population of models, that be predomics objects or individuals
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
#' @return an individual ovo object
#' @export
evaluatePopulation_mc <- function(X, y, clf, pop, eval.all = FALSE,
                                   force.re.evaluation = FALSE,
                                   estim.feat.importance = FALSE,
                                   mode = "train",
                                   delete.null.models = TRUE,
                                   approch = "ovo",
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
        pop.lfolds[[f]] <- evaluatePopulation_mc(X[,-lfolds[[f]]],
                                                  y[-lfolds[[f]]],
                                                  clf,
                                                  pop,
                                                  eval.all = eval.all,
                                                  force.re.evaluation = force.re.evaluation,
                                                  estim.feat.importance = estim.feat.importance,
                                                  mode = mode,
                                                  delete.null.models = delete.null.models,
                                                  approch = approch,
                                                  lfolds = NULL)
      }else # test
      {
        pop.lfolds[[f]] <- evaluatePopulation_mc(X[,lfolds[[f]]],
                                                  y[lfolds[[f]]],
                                                  clf,
                                                  pop,
                                                  eval.all = eval.all,
                                                  force.re.evaluation = force.re.evaluation,
                                                  estim.feat.importance = estim.feat.importance,
                                                  mode = mode,
                                                  delete.null.models = delete.null.models,
                                                  approch = approch,
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
      res[[i]] <- evaluateModel_mc(mod = mod,
                                    X = X,
                                    y = y,
                                    clf = clf,
                                    eval.all = eval.all,
                                    force.re.evaluation = force.re.evaluation,
                                    estim.feat.importance = estim.feat.importance,
                                    approch = approch,
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




#' listOfSparseVecToListOfModels
#' @title listOfSparseVecToListOfModels_ovo
#' @description Converts an list of "SparseVec" objects onto a list of predomics objects
#' @import snow
#' @param X: dataset
#' @param y: labels
#' @param clf: classifier
#' @param v: list of vectors of coeffs. For example, v=list( c(0.0,1.0,0.0,-1.0) , c(1.0,1.0,0.0,0.0) , c(0.0,1.0,1.0,-1.0) )
#' @param lobj: a list of objects to add as elements in the model objects if not null (default:NULL)
#' @param eval.all: evaluate population (default:FALSE)
#' @return an list of model object one versus one
#' @export
listOfSparseVecToListOfModels_mc <- function(X, y, clf, v, lobj = NULL, eval.all = FALSE, approch ="ovo")
{
  nClasse <- unique(y)
  list_y <- list() # list y
  list_X <- list() # list X
  listcoeffs <- list()
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
  listcoeffs <- clf$coeffs_


  if (!is.null(lobj)) {
    if (!is.list(lobj)) {
      stop("listOfDenseVecToListOfModels: lobj should be a list of objects.")
    }

    if (length(lobj) != length(v)) {
      stop("listOfDenseVecToListOfModels: lobj should be a list the same length as v")
    }
  }

  if (length(v) == 0) {
    if (clf$params$warnings) warning("listOfSparseVecToListOfModels: empty list returning NULL")
    return(NULL)
  }

  pop <- list()
  list_pop <- list()
    for(i in 1:length(v))
    {
      for(j in 1:length(list_y)){

        clf$coeffs_ = listcoeffs[[j]]
        model <- v[[i]][[j]]
        pop[[j]] <- sparseVecToModel(X=list_X[[j]], y = list_y[[j]], model, clf, eval.all = eval.all, obj = lobj[[i]][[j]])
      }
      list_pop[[i]] = pop
    }
    pop <- list()
    pop = list_pop


  return(pop)
}




#' evaluates the feature importance in a population of models
#' @title evaluateFeatureImportanceInPopulation_ovo
#' @description This function perturbes the dataset by shuffling one at a time a subset of features that appear in a population of models
#' and recomputes the evaluation of those models. The mean deltas of the score to consider will give a measure of importance. Two methods
#' are implemented: the first (extensive), will shuffle feature by feature multiple times and will compute the evaluation for the whole
#' population of models, which can be very time consuming. The second (optimized) and the default approach consists on using a different
#' seed when shuffling a given feature and computing the population. In this setting it is not needed to run multiple seeds on the whole
#' dataset. This procedure is designed to be applied in cross validation.
#' @param pop: a population of models to be considered. This population will be filtered if filter.ci = TRUE (default) using the interval
#' confidence computed around the best model using a binomial distribution.
#' @param X: dataset used to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param score: the attribute of the model to be considered in the evaluation (default:fit_)
#' @param filter.ci: filter the population based on the best model confidence interval (default:TRUE)
#' @param method: Two methods are implemented: the first (extensive), will shuffle feature by feature multiple times and will compute the
#' evaluation for the whole population of models, which can be very time consuming. The second (optimized) and the default approach consists
#' on using a different seed when shuffling a given feature and computing the population.
#' @param seed: one or more seeds to be used in the extensive method shuffling (default:c(1:10). For the optimized method only the first seed will be used
#' and the rest of the seeds that are needed for each model will be incremented from there.
#' @param aggregation: the method to be used to aggregate the evaluation for a the whole population (default: mean), but can be either mean or median.
#' @param verbose: wether to print out information during the execution process.
#' @return a data.frame with features in rows and the population mean/median score for each model*seed of the population
#' @export

evaluateFeatureImportanceInPopulation_mc <- function(pop, X, y, clf, score = "fit_", filter.ci = TRUE, method = "optimized",
                                                      seed = c(1:10), aggregation = "mean", approch = "ovo", verbose = TRUE)
{

  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  listcoeffs <- list()
  list_res <- list()

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

  listcoeffs <- clf$coeffs_
  listX <- list()
  listXmin <- list()
  listXmax <- list()
  listy <- list()
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y

  if(!isPopulation(pop))
  {
    if(clf$params$warnings) warning("evaluateFeatureImportanceInPopulation: please masure to provide a valid population.")
    return(NULL)
  }


  if(!method %in% c("optimized","extensive"))
  {
    stop("evaluateFeatureImportanceInPopulation: please provide a valid method either optimized or extensive.")
  }else
  {
    if(method == "optimized")
    {
      optimized <- TRUE
    }else
    {
      optimized <- FALSE
    }
  }

  if(!aggregation %in% c("mean","median"))
  {
    stop("evaluateFeatureImportanceInPopulation: please provide a valid aggregation method either mean or median.")
  }else
  {
    if(aggregation == "mean")
    {
      aggr.mean <- TRUE
    }else
    {
      aggr.mean <- FALSE
    }
  }

  if(verbose) print(paste("There are",length(pop), "models in this population"))
  if(filter.ci)
  {
    # select the best models in the population after penalizing for sparsity
    pop         <- selectBestPopulation(pop, p = 0.05, k_penalty = 0.75/100)
    if(verbose) print(paste("There are",length(pop), "models in this population after filtering"))
  }

  if(!isPopulation(pop))
  {
    if(clf$params$warnings) warning("evaluateFeatureImportanceInPopulation: no models are left after best model selection.")
    return(NULL)
  }



  # Reevaluate the population in X (which is the x_test) in generalization
  pop <- evaluatePopulation_mc(X = X, y = y, clf = clf, pop = pop, eval.all = TRUE, force.re.evaluation = TRUE, approch = approch, mode = "test")
  #pop <- evaluatePopulation_ovo(X = X, y = y, clf = clf, pop = pop, eval.all = TRUE, force.re.evaluation = TRUE)
  # compute the presence of features in the models
  listfa  <- makeFeatureAnnot_mc(pop = pop, X = X, y = y, clf = clf, approch = approch)
  for(h in 1:length(list_y)) {
    X = list_X[[h]]
    y = list_y[[h]]
    clf$coeffs_ <- listcoeffs[[h]]
    clf$data$X <- listX[[h]]
    clf$data$X.min <- listXmin[[h]]
    clf$data$X.max <- listXmax[[h]]
    clf$data$y <- listy[[h]]

    fa  <- listfa[[h]]
    #pop  <- popul[[h]]
    feat.prev     <- rowSums(fa$pop.noz != 0, na.rm = TRUE)
    # order by prevalence in the population of models
    feat.prev     <- feat.prev[order(feat.prev, decreasing = TRUE)]

    # create a matrix presence mask
    feat.prez.mask     <- fa$pop.noz != 0 & !is.na(fa$pop.noz)

    # the pool of features
    feat.pool     <- names(feat.prev)
    if(verbose) print(paste("Feature prevalence is computed. There are ", length(feat.pool), "features to consider."))

    eval.orig     <- populationGet_X(element2get = score, toVec = TRUE, na.rm = FALSE)(pop)

    if(optimized)
    {
      # for each feature perturb the data to test for its importance
      res.all <- list()

      for(i in 1:length(feat.pool))
      {
        if(verbose) cat(paste(feat.pool[i], "\t"))

        # the eval shuffle mask
        eval.shuf.mask <- rep(NA, length(pop))
        names(eval.shuf.mask) <- names(pop)
        # the feature mask
        feat.prez.ind <- feat.prez.mask[feat.pool[i],]

        # put data in the new population
        pop.new <- list()

        # For each model in the population
        for(j in 1:length(pop[feat.prez.ind]))
        {
          # store a copy to change and reset
          X.shuf <- X
          if(seed[1] == 0)
          {
            # for testing purposes if seed is 0 than no shuffling occurs and the DA should be 0
            ind                   <- c(1:ncol(X))
          }else
          {
            # shuffle the feature j
            set.seed(seed[1] + j)
            ind <- sample(x = c(1:ncol(X)), size = ncol(X), replace = FALSE)
          }
          X.shuf[feat.pool[i],] <- X.shuf[feat.pool[i],ind]
          pop.new[[j]] <- evaluateModel(mod = pop[[i]], X = X.shuf, y = y, clf = clf, eval.all = TRUE, force.re.evaluation = TRUE, mode = "test")
          #pop.new[[j]] <- evaluateModel(mod = pop[[i]], X = X.shuf, y = y, clf = clf, eval.all = TRUE, force.re.evaluation = TRUE)
          if(verbose) cat("*")
        } # end loop models

        # get the evaluation after perturbation
        eval.shuf <- populationGet_X(element2get = score, toVec = TRUE, na.rm = FALSE)(pop.new)
        # compute the delta of the evaluation before and after perturbation and store it. We call this DA (decreased accuracy)
        eval.shuf.mask[feat.prez.ind] <- c(eval.orig[feat.prez.ind] - eval.shuf)

        res.all[[i]] <- eval.shuf.mask
        if(verbose) cat(paste("\n"))

      } # end loop each feature
      names(res.all) <- feat.pool

    }else # extensive
    {
      # for each feature perturb the data to test for its importance
      res.all <- list()

      for(i in 1:length(feat.pool))
      {
        if(verbose) cat(paste(feat.pool[i], "\t"))

        # the eval shuffle mask
        eval.shuf.mask <- rep(NA, length(pop))
        names(eval.shuf.mask) <- names(pop)
        # the feature mask
        feat.prez.ind <- feat.prez.mask[feat.pool[i],]

        # we can do this multiple times
        res.f <- c()
        for(j in 1:length(seed))
        {
          # store a copy to change and reset
          X.shuf                <- X
          if(seed[j] == 0)
          {
            # for testing purposes if seed is 0 than no shuffling occurs and the DA should be 0
            ind                   <- c(1:ncol(X))
          }else
          {
            # shuffle the feature j
            set.seed(seed[j])
            ind                   <- sample(x = c(1:ncol(X)), size = ncol(X), replace = FALSE)
          }

          X.shuf[feat.pool[i],] <- X.shuf[feat.pool[i],ind]
          pop.eval              <- evaluatePopulation(X = X.shuf, y = y, clf = clf, pop = pop[feat.prez.ind],
                                                      eval.all = TRUE, force.re.evaluation = TRUE)
          # get the evaluation after perturbation
          eval.shuf             <- populationGet_X(element2get = score, toVec = TRUE, na.rm = FALSE)(pop.eval)
          # compute the delta of the evaluation before and after perturbation and store it. We call this DA (decreased accuracy)
          eval.shuf.mask[feat.prez.ind] <- c(eval.orig[feat.prez.ind] - eval.shuf)
          # concatenate if multiple perturbations
          res.f                 <- c(res.f, eval.shuf.mask)
          if(verbose) cat("*")
        } # end loop seeds

        if(verbose) cat(paste("\n"))
        res.all[[i]] <- res.f
      } # end loop each feature
      names(res.all) <- feat.pool
    }

    # put all the DAs in a data frame
    res.all.df                <- t(data.frame(res.all))
    # standard deviation
    SDA                       <- apply(res.all.df, 1, sd, na.rm = TRUE)
    # Prevalence of DA
    PDA                       <- rowSums(!is.na(res.all.df))

    # transform the data
    if(aggr.mean)
    {
      # compute the MDA (mean decreased accuracy)
      eval.aggr                 <- apply(res.all.df, 1, mean, na.rm = TRUE)
    }else
    {
      # compute the MDA (median decreased accuracy)
      eval.aggr                 <- apply(res.all.df, 1, median, na.rm = TRUE)
    }

    names(eval.aggr)            <- feat.pool
    res                         <- list(feat.catalogue = feat.pool,
                                        feat.catalogue.annot =  fa$feature.df[feat.pool,],
                                        feat.pop.prev = feat.prev,
                                        feat.pop.da.list = res.all,
                                        feat.pop.da.df = res.all.df,
                                        mda = eval.aggr,
                                        sda = SDA,
                                        pda = PDA/ncol(res.all.df) # as a percentage
    )
    list_res[[h]] = res
  }
  res <- list()
  res = list_res
  return(res)

}




################################################################
# CONVERTORS
################################################################
# natts is the number of attributes,

#' Transform the model object onto dense format (long) one
#'
#' @description Builds a model object based on model that is in the dense (long) format.
#' @param natts: the number of attributes
#' @param mod: a predomics model object
#' @return a dense (long) format model
#' @export
modelToDenseVec_mc <- function(natts, mod)
{
  # test model validity
  if(!isModel(obj = mod))
  {
    stop("modelToDenseVec: the model object is not valid.")
  }

  # test the number of attributes
  if(class(natts)!="integer" | length(natts)!=1)
  {
    stop("modelToDenseVec: please make sure the natts attribute is an integer number of features.")
  }

  # initialize the v
  v <- rep(0, natts)
  mod$coeffs_ = mod$coeffs_[[1]]
  mod$indices_ = mod$indices_[[1]]

  if(isModelSota(mod))
  {
    mod$coeffs_ <- rep("*", length(mod$indices_))
    names(mod$coeffs_) <- names(mod$indices_)
  }

  for(i in 1:length(mod$indices_))
  {
    v[[ mod$indices_[[i]] ]] <- mod$coeffs_[[i]]
  }
  return(v)
}







#' Builds a list of dense vector coefficients from a list of models
#'
#' @param clf: classifier
#' @param X: dataset
#' @param y: labels
#' @param list.models: list of models
#' @return a list of dense vectors of coefficient
#' @export
listOfModelsToListOfDenseVec_mc <- function(clf, X, y, list.models)
{
  if(!isPopulation(obj = list.models))
  {
    stop("listOfModelsToListOfDenseVec_ovo: Please specify a population of model objects")
  }

  res <- list()
  for(i in 1:length(list.models))
  {
    res[[i]] <- modelToDenseVec_ovo(nrow(X), list.models[[i]])
  }
  return(res)
}

#' listOfModelsToDenseCoefMatrix
#'
#' @description For each model in the list of models it will convert to dense format and convert to a data.frame
#' @param clf: the classifier object
#' @param X: the dataset
#' @param y: the class vector
#' @param list.model: a list of model objects
#' @param rm.empty: remove null models in the list if any (default:TRUE)
#' @param order.row: order rows by occurence (default:TRUE)
#' @return an data frame with model coefficients in rows
#' @export
listOfModelsToDenseCoefMatrix_mc <- function(clf, X, y, list.models, rm.empty = TRUE, order.row = TRUE)
{
  if(!isPopulation(list.models))
  {
    stop("listOfModelsToDenseCoefMatrix: please provide a valid population of models")
  }


  # Transform the model objects into dense vectors
  pop.dense <- listOfModelsToListOfDenseVec_mc(clf = clf, X=X, y=y, list.models = list.models)

  # concatenate them into a matrix

  pop.dense <- as.matrix(do.call(cbind.data.frame, pop.dense))
  rownames(pop.dense) <- rownames(X); colnames(pop.dense) <- paste(clf$learner, c(1:ncol(pop.dense)), sep="_")
  mod.names <- colnames(pop.dense)

  if(any(is.na(rownames(pop.dense))))
  {
    print("listOfModelsToDenseCoefMatrix: some features are NA in pop.noz ... omitting them")
    pop.dense <- pop.dense[!is.na(rownames(pop.dense)),]
  }

  # if order the rows by participation occurance
  if(order.row)
  {
    pop.dense.noz <- pop.dense[order(rowSums(pop.dense !=0 , na.rm = TRUE), decreasing = TRUE),]
    if(any(class(pop.dense.noz) == "numeric")) # if a single model
    {
      pop.dense.noz <- as.matrix(as.data.frame(pop.dense.noz))
    }
  } else
  {
    pop.dense.noz <- pop.dense
  }

  if(any(is.na(rownames(pop.dense.noz))))
  {
    print("listOfModelsToDenseCoefMatrix: some features are NA in pop.noz ... omitting them")
    pop.dense.noz <- pop.dense.noz[!is.na(rownames(pop.dense.noz)),]
  }

  if(rm.empty)
  {
    # filter rows that are zero for all the models
    features.ord <- rownames(pop.dense.noz)
    ind.tokeep <- rowSums(pop.dense.noz != 0, na.rm = TRUE) != 0
    pop.dense.noz <- pop.dense.noz[ind.tokeep,]
    if(any(class(pop.dense.noz) == "numeric")) # if a single model
    {
      pop.dense.noz <- as.matrix(as.data.frame(pop.dense.noz))
      rownames(pop.dense.noz) <- features.ord[ind.tokeep]
      colnames(pop.dense.noz) <- mod.names
    }
  }
  return(pop.dense.noz)
}



#' Evaluates the prevalence of a list of features in the whole dataset and per each class
#'
#' @description Evaluate the prevalence of a given model
#' @param features: a list of features or features indexes for which we wish to compute prevalence
#' @param X: dataset where to compute the prevalence
#' @param y: if provided it will also compute hte prevalence per each class (default:NULL)
#' @param prop: weather to compute the prevalence in number or as a proportion (default:TRUE)
#' @param zero.value: the value that specifies what is zero. This can be a different than 0 in log transformed data for instance (default = 0)
#' @return A list containing the prevalence in the whole dataset as well as classes (if provided)
#' @export
getFeaturePrevalence_mc <- function(features, X, y = NULL, prop = TRUE, zero.value = 0)
{

  if(all(is.character(features)))
  {
    # check weather they exist in the dataset's rownames
    ind <- match(features,rownames(X))
    if(any(is.na(ind)))
    {
      stop(paste("getFeaturePrevalence: ",sum(is.na(ind)),"features are not found as rownames of X"))
    }
  }

  # if features are indices than check weather they are in the range
  if(all(is.numeric(features)))
  {
    if(max(features > nrow(X)))
    {
      stop("getFeaturePrevalence: feature indexes are out of range. Please check again.")
    }
    else
    {
      ind <- features
    }
  }

  # get the corresponding data
  if(ncol(X) == 1)
  {
    data <- as.matrix(X[ind,])
  }else{
    data <- X[ind,]
  }

  if(length(features) == 1)
  {
    # when only 1 feature
    data <- t(as.matrix(data))
    rownames(data) <- features
  }

  res.all <- list()
  # compute the prevalence for the whole vector
  res <- rowSums(data!= zero.value, na.rm = TRUE) # they can be negative if logged
  if(prop) res <- res / ncol(data)
  names(res) <- features
  res.all[[1]] <- res

  # also for each class
  if(!is.null(y))
  {
    lev <- names(table(y))
    for(i in 1:length(lev))
    {
      if(length(features) == 1)
      {
        res <- sum(data[,y==lev[i]] != zero.value, na.rm = TRUE) # they can be negative if logged
      }else
      {
        if(ncol(X) == 1)
        {
          if(table(y)[lev[i]] == 0)
          {
            res <- rep(0, nrow(data))
            names(res) <- features
          }else
          {
            res <- (data[,y==lev[i]] != zero.value)+0.0 # they can be negative if logged
          }
        }else
        {
          res <- rowSums(data[,y==lev[i]] != zero.value, na.rm = TRUE) # they can be negative if logged
          names(res) <- features
        }
      }
      if(table(y)[lev[i]] != 0)
      {
        if(prop)
        {
          res <- res / table(y)[lev[i]]
        }
      }
      res.all[[i+1]] <- res
    }
    names(res.all) <- c("all",lev)
  }else
  {
    names(res.all) <- c("all")
  }
  return(res.all)
}



# this function takes a list of vectors with proportions and transforms it in a melted version
# class -1 prevalence is multiplied by -1 for the plot
meltScoreList_mc <- function(v=v.prop, prepare.for.graph=TRUE, topdown=TRUE)
{

  # Vérifier si v est une liste de vecteurs de même taille
  if (!is.list(v) || any(sapply(v, function(x) !is.vector(x) || length(x) != length(v[[1]])))) {
    stop("v should be a list of vectors of the same size.")
  }

  # Trouver la longueur maximale parmi tous les vecteurs
  max_length <- max(sapply(v, length))

  # Initialiser le data frame avec le bon nombre de colonnes et lignes
  v.prop.melt <- data.frame(matrix(NA, nrow = max_length, ncol = 0))

  # Parcourir les éléments de la liste v
  for (i in 1:length(v)) {
    # Créer un data frame temporaire avec les informations actuelles
    temp_df <- data.frame(
      name = names(v[[i]]),
      value = c(v[[i]], rep(NA, max_length - length(v[[i]]))),  # Remplir avec NA pour les valeurs manquantes
      group = rep(names(v)[i], max_length)
    )

    # Ajouter les colonnes du data frame temporaire à v.prop.melt
    v.prop.melt <- cbind(v.prop.melt, temp_df)
  }

  # Afficher le résultat
  ##print(v.prop.melt)
  v.prop.melt <- data.frame(t(v.prop.melt));
  colnames(v.prop.melt) <- c("name","score","group")
  # transform score into a number
  v.prop.melt$score <- as.numeric(as.character(v.prop.melt$score))
  # fix factor level order
  if(topdown)
  {
    v.prop.melt$name <- factor(v.prop.melt$name, levels = rev(names(v$all)))
  }else
  {
    v.prop.melt$name <- factor(v.prop.melt$name, levels = names(v$all))
  }
  # put prevalence score 1 in negative for the plot
  if(prepare.for.graph) v.prop.melt[v.prop.melt$group=="-1",]$score <- v.prop.melt[v.prop.melt$group=="-1",]$score *-1

  return(v.prop.melt)
}





#' computeCardEnrichment
#'
#' @description Computes statistic for enrichment of the cardinality of a score for a two class vector
#' @param v.card.mat: a dataframe with the cardinality of each feature (columns) and each group in the y vector (rows)
#' @param y: the vector containing the class specification for each sample
#' @return a data.frame with the statistics computed
#' @export
computeCardEnrichment_mc <- function(v.card.mat, y)
{
  # build the res object
  res <- list()
  res$v.card.mat <- v.card.mat
  res$y <- y

  if(is.null(y))
  {
    return(res)
  }

  card.all <- table(y)
  ##if(length(card.all)!=2) stop("The number of classes should be 2!")
  dat <- v.card.mat[names(card.all),]
  if(ncol(v.card.mat) == 1)
  {
    dat <- as.matrix(dat)
    colnames(dat) <- colnames(v.card.mat)
  }
  # commpute the negative
  dat.negative <- dat
  for(i in 1:length(card.all))
  {
    dat.negative[i,] <- card.all[i] - dat.negative[i,]
  }

  # compute the chisq.test() p-values
  chisq.p <- c()
  chisq.mat.list <- list()
  for(i in 1:ncol(v.card.mat))
  {
    chisq.mat.list[[i]] <- chisq.mat <- rbind(dat[,i],dat.negative[,i]); rownames(chisq.mat) <- c("present","missing")
    chisq.p <- c(chisq.p, signif(chisq.test(chisq.mat)$p.value,3))
    #barplot(chisq.mat,col=c("orangered", "darkolivegreen2"))
  }
  names(chisq.mat.list) <- names(chisq.p) <- colnames(v.card.mat)
  chisq.p[is.nan(chisq.p)] <- 1 # put maximal p-val when warning
  chisq.q <- p.adjust(chisq.p, method = "bonferroni")
  #plot(chisq.q<0.05)

  # add statistics to object
  res$card.all <- card.all
  res$chisq.p <- chisq.p
  res$chisq.q <- chisq.q
  return(res)
}

#' Plots the prevalence of a list of features in the whole dataset and per each class
#'
#' @description Plots the prevalence of a given number of features
#' @param features: a list of features or features indexes for which we wish to compute prevalence
#' @param X: dataset where to compute the prevalence
#' @param y: if provided it will also compute hte prevalence per each class (default:NULL)
#' @param topdown: showing features from top-down or the other way around (default:TRUE)
#' @param main: main title (default:none)
#' @param plot: if TRUE this provides a plot, otherwise will return different metrics such as prevalence and enrichment statistics
#' @param col.pt: colors for the point border (-1:deepskyblue4, 1:firebrick4)
#' @param col.bg: colors for the point fill (-1:deepskyblue1, 1:firebrick1)
#' @param zero.value: the value that specifies what is zero. This can be a different than 0 in log transformed data for instance (default = 0)
#' @return a ggplot object
#' @export
plotPrevalence_mc <- function(features, X, y, topdown = TRUE, main = "", plot = TRUE,
                           col.pt = c("deepskyblue4", "firebrick4"),
                           col.bg = c("deepskyblue1", "firebrick1"),
                           zero.value = 0)
{
  # build object
  v.prop <- getFeaturePrevalence_ovo(features = features, X = X, y = y, prop = TRUE, zero.value = zero.value)
  v.card <- getFeaturePrevalence_ovo(features = features, X = X, y = y, prop = FALSE, zero.value = zero.value)
  v.prop.mat <- do.call(rbind, v.prop)
  v.card.mat <- do.call(rbind, v.card)
  # build melted version
  v.prop.melt <- meltScoreList_ovo(v = v.prop, prepare.for.graph = FALSE, topdown = topdown)
  colnames(v.prop.melt) <- c("feature","prevalence", "group")

  # convert to percentage
  v.prop.melt$prevalence <- v.prop.melt$prevalence *100

  # get enrichment information
  prev.enrichment <- computeCardEnrichment_ovo(v.card.mat = v.card.mat, y = y)
  if(!is.null(prev.enrichment$chisq.q))
  {
    qvals <- rep("",length(prev.enrichment$chisq.q))
    qvals[prev.enrichment$chisq.q<0.05] <- "*"

    # plot object
    p <- ggplot(v.prop.melt, aes(x=feature, y=prevalence, fill=group)) +
      geom_bar(data=subset(v.prop.melt, group %in% c("all")), stat="identity", position="identity") +
      coord_flip() +
      geom_point(data = subset(v.prop.melt, group %in% c("-1", "1")), aes(x=feature, y=prevalence, color=group, shape=group)) +
      scale_color_manual("Dataset", values = c("all"="gray90", "-1"=col.pt[1], "1"=col.pt[2])) +
      scale_fill_manual("Dataset", values = c("all"="gray90", "-1"=col.bg[1], "1"=col.bg[2])) +
      scale_shape_manual(values=c(25,24)) +
      theme_bw() +
      theme(legend.position="none", axis.text=element_text(size=9)) +
      ggtitle(main)

    if(topdown){
      p <- p +  annotate("text", y = rep(101,length(qvals)), x = seq(1,length(qvals),1)-0.3, label = rev(qvals), color="gray", size=7)
    }else{
      p <- p +  annotate("text", y = rep(101,length(qvals)), x = seq(1,length(qvals),1)-0.3, label = qvals, color="gray", size=7)
    }

  }else
  {
    # plot object
    p <- ggplot(v.prop.melt, aes(x=feature, y=prevalence, fill=group)) +
      geom_bar(data=subset(v.prop.melt, group %in% c("all")), stat="identity", position="identity") +
      coord_flip() +
      scale_color_manual("Dataset", values = c("all"="gray90")) +
      scale_fill_manual("Dataset", values = c("all"="gray90")) +
      theme_bw() +
      theme(legend.position="none", axis.text=element_text(size=9)) +
      ggtitle(main)
  }

  if(!plot)
  {
    return(prev.enrichment)
  }else
  {
    return(p)
  }
}


#' Plots the prevalence of a list of features in the whole dataset and per each class
#'
#' @description Plots the abundance of a given number of features for each class and tests significance
#' @import reshape2
#' @param features: a list of features or features indexes for which we wish to compute prevalence
#' @param X: dataset where to compute the prevalence
#' @param y: if provided it will also compute hte prevalence per each class (default:NULL)
#' @param topdown: showing features from top-down or the other way around (default:TRUE)
#' @param main: main title (default:none)
#' @param plot: if TRUE this provides a plot, otherwise will return different metrics such as prevalence and enrichment statistics
#' @param col.pt: colors for the point border (-1:deepskyblue4, 1:firebrick4)
#' @param col.bg: colors for the point fill (-1:deepskyblue1, 1:firebrick1)
#' @return a ggplot object
#' @export
plotAbundanceByClass_mc <- function(features, X, y, topdown = TRUE, main = "", plot = TRUE, approch = "ovo",col.pt = c("deepskyblue4", "firebrick4"), col.bg = c("deepskyblue1", "firebrick1"))
{

  if(any(is.na(match(features, rownames(X)))))
  {
    stop(paste("plotAbundanceByClass: These features are not found in the dataset",
               features[is.na(match(features, rownames(X)))]))
  }


  nClasse <- unique(y)
  feature.cor   <- list()
  list_y <- list()
  list_X <- list()
  list_dat.test <- list()
  list_p <- list()


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

  for (ik in 1:(length(list_y))) {

  X = list_X[[ik]]
  y = list_y[[ik]]
  if(!is.matrix(X))
  {
    X <- as.matrix(X)
  }
  mode <- "classification"
  if(class(y) == "numeric" & length(table(y)) > 2)
  {
    #cat("... plotAbundanceByClass will not work for a continous y - probably in regression mode. Adapting as a uniclass\n")
    mode <- "regression"
  }
  }
  if(mode == "classification")
  {
    for (ib in 1:(length(list_y))) {

      X = list_X[[ib]]
      y = list_y[[ib]]

    # get levels
    lev <- names(table(y))

    # compute p-value of the non parametric abundance test
    if(length(features) == 1)
    {
      dat <- t(as.matrix(X[features, ]))
      rownames(dat) <- features
      datl1 <- t(as.matrix(X[features, y == lev[1]]))
      rownames(datl1) <- features
      datl2 <- t(as.matrix(X[features, y == lev[2]]))
      rownames(datl2) <- features
    }else
    {
      dat <- X[features, ]
      datl1 <- X[features, y == lev[1]]
      datl2 <- X[features, y == lev[2]]
      if(ncol(X) == 1)
      {
        dat <- as.matrix(dat)
        datl1 <- as.matrix(datl1)
        datl2 <- as.matrix(datl2)
      }
    }

    dat.test <- filterfeaturesK(dat, y, k = nrow(dat), sort = FALSE)

    if(plot)
    {
      pvals <- dat.test$p
      qvals <- rep("",nrow(dat.test))
      qvals[dat.test$q<0.05] <- "*"

      datl1.reshape <- melt(datl1)
      colnames(datl1.reshape) <- c("feature","observation","abundance")
      datl1.reshape$class <- rep(lev[1], nrow(datl1.reshape))

      datl2.reshape <- melt(datl2)
      colnames(datl2.reshape) <- c("feature","observation","abundance")
      datl2.reshape$class <- rep(lev[2], nrow(datl2.reshape))

      dat.reshape <- as.data.frame(t(data.frame(t(datl1.reshape), t(datl2.reshape))))
      dat.reshape$abundance <- as.numeric(as.character(dat.reshape$abundance))

      # fix factor level order
      if(topdown)
      {
        # use the same factor levels as features
        dat.reshape$feature <- factor(dat.reshape$feature, levels=rev(features))
      }else
      {
        # use the same factor levels as features
        dat.reshape$feature <- factor(dat.reshape$feature, levels=features)
      }

      # plot object
      p <- ggplot(dat.reshape, aes(x=feature, y = abundance, fill=class, color=class)) +
        geom_boxplot() +
        #scale_x_continuous(limits = range(dat.reshape$abundance)) +
        coord_flip() +
        #facet_grid(. ~ class) +
        theme_bw() +
        scale_color_manual(values = col.pt) +
        scale_fill_manual(values = col.bg) +
        theme(legend.position="none") +
        ggtitle(main)

      pad <- max(dat.reshape$abundance) + max(dat.reshape$abundance)*0.1

      if(topdown){
        p <- p +  annotate("text", y = rep(pad,length(qvals)), x = seq(1,length(qvals),1) - 0.3, label = rev(qvals), color="gray", size=7)
      }else{
        p <- p +  annotate("text", y = rep(pad,length(qvals)), x = seq(1,length(qvals),1) - 0.3, label = qvals, color="gray", size=7)
      }
      list_p[[ib]] = p
      return(list_p)
    }else
    {
      list_dat.test[[ib]] = data.test
      return(list_dat.test)
    }

    }

    }else # mode regression
  {

    for (m in 1:(length(list_y))) {

      X = list_X[[m]]
      y = list_y[[m]]


    # get levels

    # compute p-value of the non parametric abundance test
    if(length(features) == 1)
    {
      dat <- t(as.matrix(X[features, ]))
      rownames(dat) <- features
    }else
    {
      dat <- X[features, ]
      if(ncol(X) == 1)
      {
        dat <- as.matrix(dat)
      }
    }

    # we can still correlate and compute p-values
    dat.test <- filterfeaturesK(dat, y, k = nrow(dat), sort = FALSE)

    if(plot)
    {
      pvals <- dat.test$p
      qvals <- rep("",nrow(dat.test))
      qvals[dat.test$q<0.05] <- "*"

      dat.reshape <- melt(dat)
      colnames(dat.reshape) <- c("feature","observation","abundance")
      dat.reshape$class <- rep("all", nrow(dat.reshape))

      # fix factor level order
      if(topdown)
      {
        # use the same factor levels as features
        dat.reshape$feature <- factor(dat.reshape$feature, levels=rev(features))
      }else
      {
        # use the same factor levels as features
        dat.reshape$feature <- factor(dat.reshape$feature, levels=features)
      }

      # plot object
      p <- ggplot(dat.reshape, aes(x=feature, y = abundance, fill=class, color=class)) +
        geom_boxplot() +
        #scale_x_continuous(limits = range(dat.reshape$abundance)) +
        coord_flip() +
        #facet_grid(. ~ class) +
        theme_bw() +
        scale_color_manual(values = "gray40") +
        scale_fill_manual(values = "gray80") +
        theme(legend.position="none") +
        ggtitle(main)

      pad <- max(dat.reshape$abundance) + max(dat.reshape$abundance)*0.1

      if(topdown){
        p <- p +  annotate("text", y = rep(pad,length(qvals)), x = seq(1,length(qvals),1) - 0.3, label = rev(qvals), color="gray", size=7)
      }else{
        p <- p +  annotate("text", y = rep(pad,length(qvals)), x = seq(1,length(qvals),1) - 0.3, label = qvals, color="gray", size=7)
      }

      list_p[[ik]] = p

      return(list_p)
    }else
    {
      list_dat.test[[m]] = data.test
      return(list_dat.test)
    }
    }
  }

}


#' Prints as text the detail on a given experiment along with summarized results (if computed)
#'
#' @description This function takes a population of models and creates a table with annotation on the features,
#' such as prevalence in the models and dataset as well as different statistics
#' @param pop: a population of models
#' @param X: the X dataset where to compute the abundance and prevalence
#' @param y: the target class
#' @param clf: an object containing the different parameters of the classifier
#' @return a list with two data.frames one containing the coefficients per each model and the other a data.frame on the features
#' @export
makeFeatureAnnot_ovot <- function(pop, X, y, clf, approch = "ovo")
{
  if(!isPopulation(pop))
  {
    warning("makeFeatureAnnot: unvalid population, returning NULL")
    return(NULL)
  }

  pop.noz <- listOfModelsToDenseCoefMatrix_ovo(clf = clf, X = X, y = y, list.models = pop)
  if(any(is.na(rownames(pop.noz))))
  {
    print("makeFeatureAnnot_ovo: some features are NA in pop.noz ... omitting them")
    pop.noz <- pop.noz[!is.na(rownames(pop.noz)),]
  }

  # order on the prevalence on best models
  if(clf$params$objective == "cor")
  {
    prev <- getFeaturePrevalence_ovo(features = rownames(pop.noz), X = X, y = NULL, prop = TRUE)
  }else
  {
    prev <- getFeaturePrevalence_ovo(features = rownames(pop.noz), X = X, y = y, prop = TRUE)
  }

  # a) prevalence in each class and in X
  feat.preval.X <- as.data.frame(prev); colnames(feat.preval.X) <- paste("prev.prop",names(prev))
  # b) enrichment analyses based on prevalence
  if(clf$params$objective == "cor")
  {
    prev.enrichment <- plotPrevalence_ovo(features = rownames(pop.noz), X, y = NULL, plot=FALSE)
  }else
  {
    prev.enrichment <- plotPrevalence_ovo(features = rownames(pop.noz), X, y, plot=FALSE)
  }

  feat.preval.chisq <- as.data.frame(prev.enrichment[c("chisq.p", "chisq.q")])
  # c) prevalence in models
  mod.prev <- rowSums(abs(pop.noz))
  # d) mean coefficient in real models
  pop.noz.na <- pop.noz # in order not to take into account the zeros we transform them to NA
  pop.noz.na[pop.noz == 0] <- NA
  coeff.mean <- rowMeans(pop.noz.na, na.rm = TRUE)
  # f) differential abundance analyses
  ##suppressWarnings(feat.abund.wilcox <- plotAbundanceByClass_ovo(features = rownames(pop.noz), X, y, plot=FALSE)[,c("p","q","status")])


 ## for (w in 1:length(feat.abund.wilcox)){
 ## colnames(feat.abund.wilcox[[w]]) <- c("wilcox.p", "wilcox.q", "wilcox.class")
 ## }

  ##if(sum(dim(feat.preval.chisq)) == 0)
 ## {
   ## feat.preval.chisq <- rep(NA, nrow(feat.abund.wilcox))
 ## }
  # put everything together
  feature.df <- data.frame(feat.preval.X,
                           feat.preval.chisq,
                           mod.prev,
                         ##  feat.abund.wilcox,
                           coeff.mean,
                           check.names = FALSE)
  return(list(pop.noz = pop.noz,
              feature.df = feature.df))
}




makeFeatureAnnot_mc <- function(pop, X, y, clf, approch = "ovo")
{

  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  listfa <- list()
  listcoeffs <- list()
  listX <- list()
  listXmin <- list()
  listXmax <- list()
  listy <- list()
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y

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

  for (i in 1:length(list_X)) {
    clf$coeffs_ <- listcoeffs[[i]]
    clf$data$X <- listX[[i]]
    clf$data$X.min <- listXmin[[i]]
    clf$data$X.max <- listXmax[[i]]
    clf$data$y <- listy[[i]]
    listfa[[i]] <- makeFeatureAnnot(pop = pop, X = list_X[[i]], y = list_y[[i]], clf = clf)
  }

  return(listfa)


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




#' updateObjectIndex
#'
#' @description Update the index of a model, population, or modelCollection.
#' @param obj: the object can be a model, population, or modelCollection
#' @param features: the list of features which overrides the clf$data$features if this exists.
#' @return the same object type as input, but updated
#' @export
updateObjectIndex_mc <- function(obj, features = NULL)
{
  if(!(isModelCollection(obj) | isModel(obj) | isPopulation(obj)))
  {
    warning("updateIndexes: please provide a model collection or a population or a single model object")
    return(NULL)
  }

  if(is.null(features))
  {
    stop("updateIndexes: the features object is missing, which is necessary for this function.")
  }

  # Model
  if(isModel(obj))
  {
    res <- updateModelIndex_mc(obj = obj, features = features)
  }

  # Population
  if(isPopulation(obj))
  {
    res <- list()
    for(i in 1:length(obj))
    {
      res[[i]] <- updateModelIndex_mc(obj = obj[[i]], features = features)
    }
  }

  # Model collection
  if(isModelCollection(obj))
  {
    pop <- modelCollectionToPopulation(obj)
    pop.new <- list()
    for(i in 1:length(pop))
    {
      pop.new[[i]] <- updateModelIndex_mc(obj = pop[[i]], features = features)
    }
    res <- listOfModels2ModelCollection(pop.new)
  }

  return(res)
}


#' updateModelIndex
#'
#' @description Update the index of a model objectn.
#' @param obj: the object is a model
#' @param features: the list of features which overrides the clf$data$features if this exists.
#' @return the same object type as input, but updated
updateModelIndex_mc <- function(obj, features = NULL)
{
  if(!(isModel(obj)))
  {
    stop("updateIndexes: please provide a valid model object")
  }

  if(is.null(features))
  {
    stop("updateIndexes: the features object is missing, which is necessary for this function.")
  }
  for(i in 1:length(obj$indices_)){
  obj$indices_[[i]]  <- match(obj$names_[[i]] , features)


  if(any(is.na(obj$indices_[[i]] )))
  {
    warning("Some indices are not found.")
  }
  }
  return(obj)
}





################################################################
# DIGESTING RESULTS
################################################################

#' Summarize the results from an experiment object
#'
#' @description Sumarizes the results of an experiment object of the type
#' `obj$classifier` and `obj$crossval`. This is different from the digestMC(),
#' which sumarizes a model collection obj$models
#' @import ggplot2
#' @param obj: The experiment object resulting from the learning process `fit()`
#' @param penalty: A coefficient between 0 and 1, which is applied to penalize
#' the performance of models as a consequence of model-size. We use this to select
#' the best model of the population of models (default:NULL)
#' @param best.cv: Should we chose the best model based on information learnerd
#' cross validation (default:TRUE). This will work if the crossvalidation data is
#' available. If not the best model will be selected with empirical results.
#' @param best.k: If we do not wish to let the algorithm select the model size,
#' we can fix this by setting the best.k with an integer indicating the number of
#' variables in the model (default:NULL).
#' @param plot: Should the digested results be plotted ? (default:FALSE)
#' @param omit.na: Omit data with empty results (default:TRUE)
#' @return an object with digested information such as the best models for each
#' model-size, their respective scores, the best model.
#' @export
digestmc <- function(obj,
                   penalty = NULL,
                   best.cv = TRUE,
                   best.k = NULL,
                   plot = FALSE,
                   omit.na = TRUE)
  {
  if(!isExperiment(obj))
  {
    stop("digest: The object did not pass the sanity check for an Experiment object!")
  }

  # so that it works with below
  if(is.null(penalty))
  {
    penalty <- 0
  }

  if(length(penalty) > 1)
  {
    stop("digest: Please provide a single value for penalty!")
  }

  res                       <- list()
  res$learner               <- obj$classifier$learner

  # Empirical
  res$best                  <- list()
  res$best$models           <- getNBestModels(obj = obj,
                                              significance = TRUE,
                                              by.k.sparsity = TRUE,
                                              k.penalty = penalty,
                                              n.best = 1,
                                              single.best = FALSE,
                                              single.best.cv = best.cv,
                                              single.best.k = best.k,
                                              max.min.prevalence = FALSE,
                                              X = NULL,
                                              verbose = FALSE,
                                              evalToOrder = "fit_",
                                              return.population = TRUE, # population
                                              unique.control = TRUE
  )

  # sanity check
  if(is.null(res$best$models))
  {
    warning("digest: no models are found. Returning empty handed.")
    return(NULL)
  }
  res$best$model            <- getNBestModels(obj = obj,
                                              significance = TRUE,
                                              by.k.sparsity = TRUE,
                                              k.penalty = penalty,
                                              n.best = 5,
                                              single.best = TRUE, # give best
                                              single.best.cv = best.cv, # based on CV
                                              single.best.k = best.k,
                                              max.min.prevalence = FALSE,
                                              X = NULL,
                                              verbose = FALSE,
                                              evalToOrder = "fit_",
                                              return.population = FALSE # population
  )

  res$best$scores           <- list()
  res$best$scores$fit_      <- populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$auc_      <- populationGet_X(element2get = "auc_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$accuracy_ <- populationGet_X(element2get = "accuracy_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$precision_<- populationGet_X(element2get = "precision_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$recall_   <- populationGet_X(element2get = "recall_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$f1_       <- populationGet_X(element2get = "f1_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$cor_      <- populationGet_X(element2get = "cor_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$ser_      <- populationGet_X(element2get = "ser_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)
  res$best$scores$rsq_      <- populationGet_X(element2get = "rsq_", toVec = TRUE, na.rm = FALSE)(pop = res$best$models)


  # Cross Validation (if activated)
  res$cv <- list()
  if(!is.null(obj$crossVal))
  {
    res$cv                  <-   obj$crossVal
    crossval                <- TRUE
  }else
  {
    warning(paste("crossval information unavailable for", obj$learner))
    crossval                <- FALSE
  }

  maj.class <- NA # default value for maj.class

  # Majoritary class for all folds
  if(!is.null(obj$classifier$lfolds))
  {
    if(obj$classifier$params$objective == "auc")
    {
      if(obj$classifier$params$evalToFit == "accuracy_")
      {
        maj.class           <- rep(NA, length(obj$classifier$lfolds))
        # if accuracy
        for(i in 1:length(maj.class))
        {
          maj.class[i]      <- max(table(obj$classifier$data$y[-obj$classifier$lfolds[[i]]])/length(obj$classifier$data$y[-obj$classifier$lfolds[[i]]]))
        }
      }else
      {
        # if AUC
        maj.class           <- 0.5
      }
    } # if correlation it is NA
  }

  ##########################################################
  # Plotting the results
  ##########################################################
  if(plot)
  {
    # set the limits
    ylim <- c(0,1)

    # make an empty plot in case it does not work
    g.empty <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(ylim) +
      theme_bw() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      ylab("") +
      xlab("Model parismony sparse") +
      ggtitle("") +
      geom_hline(aes(yintercept=1), lty=2, col="lightgray") +
      geom_hline(aes(yintercept=0.5), lty=2, col="lightgray")

    # if correlation
    if(obj$classifier$params$objective=="cor")
    {
      if(crossval)
      {
        #-----------------------------------------------------
        # CORRELATION
        #-----------------------------------------------------
        # overall training accuracy learner results
        dat <- res$cv$scores$empirical.cor
        if(omit.na)
        {
          dat <- dat[rowSums(!is.na(dat))!=0,]
        }
        df <- data.frame(parsimony = rownames(dat),dat)
        df.melt <- melt(df, id.vars = "parsimony")
        df.melt$parsimony <- as.factor(df.melt$parsimony)
        df.melt$parsimony <- factor(df.melt$parsimony,
                                    levels = levels(df.melt$parsimony)[order(as.numeric(gsub("k_","",levels(df.melt$parsimony))))]
        )
        g.cor.emp.cv <- ggplot(data = df.melt, aes(y = value, x = parsimony)) +
          geom_point(aes(color = variable), position=position_jitterdodge(dodge.width=0.9), size = 1, alpha = 0.5) +
          geom_boxplot(notch = FALSE, outlier.colour = NA, position = position_dodge(width=0.9), alpha = 0.3) +
          ylab("cor_") +
          xlab("Model parsimony") +
          ggtitle("Training performance (CV)") +
          ylim(ylim) +
          theme_bw() +
          #geom_hline(yintercept = unique(maj.class), col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")


        # overall testing accuracy learner results
        dat <- res$cv$scores$generalization.cor
        if(omit.na)
        {
          dat <- dat[rowSums(!is.na(dat))!=0,]
        }
        df <- data.frame(parsimony = rownames(dat),dat)
        df.melt <- melt(df, id.vars = "parsimony")
        df.melt$parsimony <- as.factor(df.melt$parsimony)
        df.melt$parsimony <- factor(df.melt$parsimony,
                                    levels = levels(df.melt$parsimony)[order(as.numeric(gsub("k_","",levels(df.melt$parsimony))))]
        )
        g.cor.gen.cv <- ggplot(data = df.melt, aes(y = value, x = parsimony)) +
          geom_point(aes(color = variable), position=position_jitterdodge(dodge.width=0.9), size = 1, alpha = 0.5) +
          geom_boxplot(notch = FALSE, outlier.colour = NA, position = position_dodge(width=0.9), alpha = 0.3) +
          ylab("cor_") +
          xlab("Model parsimony") +
          ggtitle("Testing performance (CV)") +
          ylim(ylim) +
          theme_bw() +
          #geom_hline(yintercept = unique(maj.class), col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

      }else
      {
        g.cor.emp.cv <- g.empty
        g.cor.gen.cv <- g.empty
      }

      # RHO Empirical
      v <- res$best$scores$cor_
      df <- data.frame(value = v, parsimony = names(v))
      df$parsimony <- factor(df$parsimony,
                             levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
      )
      g.cor.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
        geom_line(aes(color = "gray")) +
        geom_point(size = 2, alpha = 1) +
        scale_color_manual(values = "gray") +
        ylab("cor_") +
        xlab("Model parsimony") +
        ggtitle("Emprical performance") +
        labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$cor_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
        ylim(ylim) +
        theme_bw() +
        geom_hline(yintercept = mean(maj.class), col = "gray", linetype = "dashed") +
        theme(legend.position="bottom", legend.direction="horizontal") +
        guides(colour = "none")

      # RSQ Empirical (R squared)
      v <- res$best$scores$rsq_
      df <- data.frame(value = v, parsimony = names(v))
      df$parsimony <- factor(df$parsimony,
                             levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
      )
      g.rsq.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
        geom_line(aes(color = "gray")) +
        geom_point(size = 2, alpha = 1) +
        scale_color_manual(values = "gray") +
        ylab("rsq_") +
        xlab("Model parsimony") +
        ggtitle("Emprical performance") +
        labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$rsq_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
        ylim(ylim) +
        theme_bw() +
        geom_hline(yintercept = mean(maj.class), col = "gray", linetype = "dashed") +
        theme(legend.position="bottom", legend.direction="horizontal") +
        guides(colour = "none")

      # SER Empirical (Standar error of the regression)
      v <- res$best$scores$ser_
      df <- data.frame(value = v, parsimony = names(v))
      df$parsimony <- factor(df$parsimony,
                             levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
      )
      g.ser.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
        geom_line(aes(color = "gray")) +
        geom_point(size = 2, alpha = 1) +
        scale_color_manual(values = "gray") +
        ylab("ser_") +
        xlab("Model parsimony") +
        ggtitle("Emprical performance") +
        labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$ser_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
        #ylim(ylim) +
        theme_bw() +
        geom_hline(yintercept = mean(maj.class), col = "gray", linetype = "dashed") +
        theme(legend.position="bottom", legend.direction="horizontal") +
        guides(colour = "none")


      grid.arrange(g.cor.emp.cv, g.cor.gen.cv,
                   # empricial
                   g.cor.emp, g.rsq.emp,
                   g.ser.emp,
                   ncol = 2)

    }else # if classification
    {
      if(crossval)
      {
        # make an empty plot in case it does not work
        g.empty <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(ylim) +
          theme_bw() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
          ylab("") +
          xlab("Model parismony sparse") +
          ggtitle("") +
          geom_hline(aes(yintercept=1), lty=2, col="lightgray") +
          geom_hline(aes(yintercept=0.5), lty=2, col="lightgray")

        #-----------------------------------------------------
        # ACCURACY
        #-----------------------------------------------------
        # overall training accuracy learner results
        dat <- res$cv$scores$empirical.acc
        if(omit.na)
        {
          dat <- dat[rowSums(!is.na(dat))!=0,]
        }
        df <- data.frame(parsimony = rownames(dat),dat)
        df.melt <- melt(df, id.vars = "parsimony")
        df.melt$parsimony <- as.factor(df.melt$parsimony)
        df.melt$parsimony <- factor(df.melt$parsimony,
                                    levels = levels(df.melt$parsimony)[order(as.numeric(gsub("k_","",levels(df.melt$parsimony))))]
        )
        g.accuracy.emp.cv <- ggplot(data = df.melt, aes(y = value, x = parsimony)) +
          geom_point(aes(color = variable), position=position_jitterdodge(dodge.width=0.9), size = 1, alpha = 0.5) +
          geom_boxplot(notch = FALSE, outlier.colour = NA, position = position_dodge(width=0.9), alpha = 0.3) +
          ylab("accuracy_") +
          xlab("Model parsimony") +
          ggtitle("Training performance (CV)") +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = unique(maj.class), col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

        # overall testing accuracy learner results
        dat <- res$cv$scores$generalization.acc
        if(omit.na)
        {
          dat <- dat[rowSums(!is.na(dat))!=0,]
        }
        df <- data.frame(parsimony = rownames(dat),dat)
        df.melt <- melt(df, id.vars = "parsimony")
        df.melt$parsimony <- as.factor(df.melt$parsimony)
        df.melt$parsimony <- factor(df.melt$parsimony,
                                    levels = levels(df.melt$parsimony)[order(as.numeric(gsub("k_","",levels(df.melt$parsimony))))]
        )
        g.accuracy.gen.cv <- ggplot(data = df.melt, aes(y = value, x = parsimony)) +
          geom_point(aes(color = variable), position=position_jitterdodge(dodge.width=0.9), size = 1, alpha = 0.5) +
          geom_boxplot(notch = FALSE, outlier.colour = NA, position = position_dodge(width=0.9), alpha = 0.3) +
          ylab("accuracy_") +
          xlab("Model parsimony") +
          ggtitle("Testing performance (CV)") +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = unique(maj.class), col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

        #-----------------------------------------------------
        # AUC
        #-----------------------------------------------------
        # overall training accuracy learner results
        dat <- res$cv$scores$empirical.auc
        if(omit.na)
        {
          dat <- dat[rowSums(!is.na(dat))!=0,]
        }
        df <- data.frame(parsimony = rownames(dat),dat)
        df.melt <- melt(df, id.vars = "parsimony")
        df.melt$parsimony <- as.factor(df.melt$parsimony)
        df.melt$parsimony <- factor(df.melt$parsimony,
                                    levels = levels(df.melt$parsimony)[order(as.numeric(gsub("k_","",levels(df.melt$parsimony))))]
        )
        g.auc.emp.cv <- ggplot(data = df.melt, aes(y = value, x = parsimony)) +
          geom_point(aes(color = variable), position=position_jitterdodge(dodge.width=0.9), size = 1, alpha = 0.5) +
          geom_boxplot(notch = FALSE, outlier.colour = NA, position = position_dodge(width=0.9), alpha = 0.3) +
          ylab("auc_") +
          xlab("Model parsimony") +
          ggtitle("Training performance (CV)") +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = 0.5, col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

        # overall testing accuracy learner results
        dat <- res$cv$scores$generalization.auc
        if(omit.na)
        {
          dat <- dat[rowSums(!is.na(dat))!=0,]
        }
        df <- data.frame(parsimony = rownames(dat),dat)
        df.melt <- melt(df, id.vars = "parsimony")
        df.melt$parsimony <- as.factor(df.melt$parsimony)
        df.melt$parsimony <- factor(df.melt$parsimony,
                                    levels = levels(df.melt$parsimony)[order(as.numeric(gsub("k_","",levels(df.melt$parsimony))))]
        )
        g.auc.gen.cv <- ggplot(data = df.melt, aes(y = value, x = parsimony)) +
          geom_point(aes(color = variable), position=position_jitterdodge(dodge.width=0.9), size = 1, alpha = 0.5) +
          geom_boxplot(notch = FALSE, outlier.colour = NA, position = position_dodge(width=0.9), alpha = 0.3) +
          ylab("auc_") +
          xlab("Model parsimony") +
          ggtitle("Testing performance (CV)") +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = 0.5, col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")
      }else
      {
        g.accuracy.emp.cv <- g.empty
        g.accuracy.gen.cv <- g.empty
        g.auc.emp.cv <- g.empty
        g.auc.gen.cv <- g.empty
      }

      if(all(is.na(unlist(res$best$scores))))
      {
        g.accuracy.emp <- g.empty
        g.auc.emp <- g.empty
        g.recall.emp <- g.empty
        g.precision.emp <- g.empty
      }else
      {
        # ACCURACY Empirical
        v <- res$best$scores$accuracy_
        df <- data.frame(value = v, parsimony = names(v))
        df$parsimony <- as.factor(df$parsimony)
        df$parsimony <- factor(df$parsimony,
                               levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
        )
        g.accuracy.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
          geom_line(aes(color = "gray")) +
          geom_point(size = 2, alpha = 1) +
          scale_color_manual(values = "gray") +
          ylab("accuracy_") +
          xlab("Model parsimony") +
          ggtitle("Emprical performance") +
          labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$accuracy_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = mean(maj.class), col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")


        # AUC Empirical
        v <- res$best$scores$auc_
        df <- data.frame(value = v, parsimony = names(v))
        df$parsimony <- as.factor(df$parsimony)
        df$parsimony <- factor(df$parsimony,
                               levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
        )
        g.auc.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
          geom_line(aes(color = "gray")) +
          geom_point(size = 2, alpha = 1) +
          scale_color_manual(values = "gray") +
          ylab("auc_") +
          xlab("Model parsimony") +
          ggtitle("Emprical performance") +
          labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$auc_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = mean(maj.class), col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

        # RECALL Empirical
        v <- res$best$scores$recall_
        df <- data.frame(value = v, parsimony = names(v))
        df$parsimony <- as.factor(df$parsimony)
        df$parsimony <- factor(df$parsimony,
                               levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
        )
        g.recall.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
          geom_line(aes(color = "gray")) +
          geom_point(size = 2, alpha = 1) +
          scale_color_manual(values = "gray") +
          ylab("recall_") +
          xlab("Model parsimony") +
          ggtitle("Emprical performance") +
          labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$recall_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = 0.5, col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

        # PRECISION Empirical
        v <- res$best$scores$precision_
        df <- data.frame(value = v, parsimony = names(v))
        df$parsimony <- as.factor(df$parsimony)
        df$parsimony <- factor(df$parsimony,
                               levels = levels(df$parsimony)[order(as.numeric(gsub("k_","",levels(df$parsimony))))]
        )
        g.precision.emp <- ggplot(data = df, aes(x = parsimony, y = value, group = 1)) +
          geom_line(aes(color = "gray")) +
          geom_point(size = 2, alpha = 1) +
          scale_color_manual(values = "gray") +
          ylab("precision_") +
          xlab("Model parsimony") +
          ggtitle("Emprical performance") +
          labs(subtitle = paste("L:",obj$classifier$learner,"|F:",signif(res$best$model$precision_,4),"|k:",length(res$best$model$indices_[[1]]), sep="")) +
          ylim(ylim) +
          theme_bw() +
          geom_hline(yintercept = 0.5, col = "gray", linetype = "dashed") +
          theme(legend.position="bottom", legend.direction="horizontal") +
          guides(colour = "none")

      } # end existing scores

      grid.arrange(g.accuracy.emp.cv, g.accuracy.gen.cv,
                   g.auc.emp.cv, g.auc.gen.cv,
                   # empricial
                   g.accuracy.emp, g.auc.emp,
                   g.recall.emp, g.precision.emp,
                   ncol = 2)

    } # end classification
  }

  # return digested results
  return(res)

}

#' Get the models from a classifier result for each k-sparsity
#'
#' @description Get the N best models from a classifier result for each k-sparsity.
#' @param obj: the classifier result output from the function fit. This can also be a ModelCollection or Population object
#' @param significance: if TRUE, (default:FALSE) a statistical test will be applied to find the lowest threshold that will delimit the window
#' of the best models. If FALSE, the models will be selected according to the rest of the criteria.
#' @param by.k.sparsity: if TRUE (default:TRUE), the filtering will be performed for each sparsity level
#' @param k.penalty: (default:0), it will penalize the models with large sparsity if different, when by.k.sparsity is set to TRUE
#' @param n.best: the number of best models to be returned for each sparsity if by.k.sparsity is set to TRUE or for the whole population
#' otherwise (default:5).
#' @param nbest: the number of best models we wish to get from the population, per each sparsity or not. If there are less best models then this
#' number, less will be returned
#' @param single.best: if TRUE, this will return the best model of all (default:FALSE) and the n.best will be set to 1.
#' @param single.best.cv: if single.best is TRUE, we could chose the best model based on data from cross validation (default:TRUE) and in this
#' case obj should be an experiment or from empirical results not in CV.
#' @param single.best.k: if single.best is TRUE, we could chose the best model of a given sparsity that is specified by a number here.
#' If this value is specified (default:NULL), then this will de-actvate single.best.cv.
#' @param max.min.prevalence: if TRUE (default:FALSE), the best models will be selected based on their performance but also on the prevalence of
#' the features that compose it.
#' @param X: the dataset to be learned (default:NULL). This is neeeded when max.min.prevalence is set to TRUE.
#' @param verbose: provide more information about execution (default = FALSE)
#' @param evalToOrder: which attribute of the model object should we use to order the models and select them (default:fit_)
#' @param return.population: if set to TRUE (default:FALSE), the result will be send as a population of models
#' @param unique.control: if set to TRUE (default:TRUZ), we correct the population so that no dupplication of models takes place
#' @return a list of model objects or a model when it is a single one or a model collection
#' @export
getNBestModels <- function(obj,
                           significance = FALSE,
                           by.k.sparsity = TRUE,
                           k.penalty = 0,
                           n.best = 5,
                           single.best = FALSE,
                           single.best.cv = TRUE,
                           single.best.k = NULL,
                           max.min.prevalence = FALSE,
                           X = NULL,
                           verbose = FALSE,
                           evalToOrder = "fit_",
                           return.population = FALSE,
                           unique.control = TRUE
)
{

  # sanity check
  if(!isExperiment(obj) & !isModelCollection(obj) & !isPopulation(obj))
  {
    warning("getNBestModels: please provide a valid experiment, modelCollection or population object ... returning NULL")
    return(NULL)
  }

  # if an experiment
  if(isExperiment(obj = obj))
  {
    if(verbose) print(paste0("getNBestModels: the object is an experiment"))
    mc              <- obj$classifier$models
  }

  # convert to a model collection if it is not
  if(isPopulation(obj = obj)) # if a population
  {
    if(verbose) print(paste0("getNBestModels: the object is an population of models"))
    mc              <- listOfModels2ModelCollection(pop = obj)
  }
  # if a modelCollection
  if(isModelCollection(obj = obj))
  {
    if(verbose) print(paste0("getNBestModels: the object is a model collection"))
    mc              <- obj
  }

  if(!isModelCollection(mc))
  {
    warning("getNBestModels: the object is not a valid model collection. Returning empty handed.")
    return(NULL)
  }

  if(single.best)
  {
    n.best          <- 1
    if(verbose) print(paste0("getNBestModels: single best"))
  }

  # set up switch variables that are no needed to parameterize but that we could in the future
  penalty_by_kfold <- FALSE

  if(by.k.sparsity | single.best)
  {
    ####################################################################
    # # if by.k.sparsity
    ####################################################################

    if(verbose) print(paste0("getNBestModels: by k sparsity"))
    res <- list()
    pop.valids <- c()
    # for each k_sparsity
    for(i in 1:length(mc))
    {
      if(unique.control)
      {
        pop           <- unique(mc[[i]])
      }else
      {
        pop           <- mc[[i]]
      }

      if(verbose) print(paste0("getNBestModels: the population of sparsity ", i," contains ", length(pop), " models"))

      if(significance)
      {
        # restrict the population to the confidence interval
        pop         <- selectBestPopulation(pop = pop, score = evalToOrder, p = 0.05, k_penalty = k.penalty)
        if(verbose) print(paste0("getNBestModels: after significance selection with k.penalty ", k.penalty," it contains ", length(pop), " models"))
      }

      # if we wish to select best models with max min prevalence
      if(max.min.prevalence)
      {
        if(!is.null(X))
        {
          eval      <- populationGet_X(element2get = evalToOrder, toVec = TRUE, na.rm = FALSE)(pop)
          best.eval <- max(eval, na.rm = T)
          # epsilon is used to be able to select a window of best models
          epsilon   <- sqrt(best.eval * (1 - best.eval) / ncol(X))
          pop       <- pop[eval >= (best.eval - epsilon) & !is.na(eval)]
          pop       <- getMaxMinPrevalenceModel(pop = pop, X = X, selected = 0)
          if(verbose) print(paste0("getNBestModels: after max.min.prevalence it contains ", length(pop), " models"))
        }
      }

      pop           <- sortPopulation(pop = pop, evalToOrder = evalToOrder)
      pop           <- pop[1:min(n.best, length(pop))]
      if(verbose) print(paste0("getNBestModels: the final population contains ", length(pop), " models"))
      res[[i]]      <- pop
      # mark valididity
      pop.valids <- c(pop.valids, isPopulation(pop))

    } # end for loop
    names(pop.valids) <- names(mc)
    names(res)      <- names(mc)[pop.valids]

    mc <- mc[pop.valids]

    if(!isModelCollection(mc))
    {
      warning("digestModelCollection: after treating the mc object no result is available. Returning NULL")
      return(NULL)
    }

    if(single.best)
    {
      single.best.cv <- FALSE
      # set best model type switch
      if(isExperiment(obj) & !myAssertNotNullNorNa(single.best.k))
      {
        if(!is.null(obj$crossVal))
        {
          single.best.cv <- TRUE
        }
      }

      k_catalogue <- paste0("k_",obj$classifier$params$sparsity)
      spar        <- populationGet_X("eval.sparsity", toVec = TRUE, na.rm = FALSE)(modelCollectionToPopulation(res))

      # Here we are left with two options
      if(single.best.cv)
      {
        # get the best model from crossval information
        if(verbose) print(paste0("getNBestModels: single.best.cv mode ... returning the best model"))
        if(obj$classifier$params$objective == "auc" & !(evalToOrder == "accuracy_" | evalToOrder == "auc_"))
        {
          evalToOrder <- "accuracy_"
        }
        if(obj$classifier$params$objective == "cor" & !(evalToOrder == "cor_"))
        {
          evalToOrder <- "cor_"
        }

        # Abreviations for the results
        key <- data.frame(real=c("auc_","accuracy_","cor_"), abbrv=c("auc","acc","cor")); rownames(key) <- key$real
        emp.name <- paste("empirical", as.character(key[evalToOrder,]$abbrv), sep=".")
        gen.name <- paste("generalization", as.character(key[evalToOrder,]$abbrv), sep=".")
        # for each classifier
        emp.data <- obj$crossVal$scores[[emp.name]][k_catalogue, ]
        gen.data <- obj$crossVal$scores[[gen.name]][k_catalogue, ]
        # plot for debug
        # par(mfrow=c(2,1)); image(as.matrix(t(emp.data))); image(as.matrix(t(gen.data)))

        if(!is.null(emp.data) & !is.null(gen.data))
        {
          # compute the penalty data
          emp.data.penalty          <- emp.data
          k                         <- as.numeric(gsub("k_","",k_catalogue))


          # if we want to compute the penalty by k_fold
          if(penalty_by_kfold)
          {
            for(j in 1:nrow(emp.data))
            {
              emp.data.penalty[j,]  <- emp.data[j,] - penalty[i] * k[j]
            }
            # select the k_sparse for each k_fold
            ind <- apply(emp.data.penalty, 2, which.max)
            k_sparse <- rownames(emp.data.penalty)[ind]

            best_empirical <- diag(as.matrix(emp.data[ind,]))
            best_generalization <- diag(as.matrix(gen.data[ind,]))

          }else # otherwise we compute a meaned penalty
          {
            mean_score <- rowMeans(emp.data.penalty, na.rm = TRUE)
            mean_score_penalty  <- mean_score - k.penalty * k
            #plot(mean_score, mean_score_penalty, ylim=c(0,1),xlim=c(0.5,1))

            # make sure to be in the space of available sparsity models in the emperical models
            ind <- which.max(mean_score_penalty[names(mc)])
            k_sparse <- rep(rownames(emp.data.penalty[names(mc),])[ind], ncol(emp.data))
            best_empirical <- as.numeric(emp.data[names(mc),][ind,])
            best_generalization <- as.numeric(gen.data[names(mc),][ind,])

            # if no values are found in empirical
            if(length(ind) == 0)
            {
              best_empirical <- logical(0)
              best_generalization <- logical(0)
            }

            # => TEST if(all(is.na(best_empirical))) best_empirical <- logical(0)
            # => TEST if(all(is.na(best_generalization))) best_generalization <- logical(0)
            # plot(best_empirical, best_generalization, ylim=c(0.5,1),xlim=c(0.5,1))
            # boxplot(list(best_empirical,best_generalization), ylim=ylim)
          }
          res.k.cv <- data.frame(best_empirical, best_generalization, k_sparse)
        }
        else
        {
          res.k.cv <- data.frame(best_empirical = NA, best_generalization = NA)[-1,]
        }
        best.k <- as.numeric(gsub("k_","",as.character(unique(res.k.cv$k_sparse))))

      }else
      {
        # get the best model from empirical information
        if(verbose) print(paste0("getNBestModels: single.best mode ... returning the best model"))

        eval <- NULL
        for(i in 1:length(res))
        {
          eval.i <- populationGet_X(evalToOrder)(res[[i]])
          if(length(eval.i) > 0)
          {
            eval <- c(eval, eval.i)
          }else
          {
            eval <- c(eval, NA)
          }
        }
        # eval <- unlist(lapply(res, function(x){populationGet_X(evalToOrder)(x)[[1]]}))
        # apply the k_penalty
        eval <- eval - (k.penalty * spar)

        best.k <- as.numeric(gsub("k_","",names(eval[which.max(eval)])))
      }

      # select the best model for a given k
      if(!myAssertNotNullNorNa(single.best.k))
      {
        # set from above if it does not exist
        single.best.k <- best.k
      }else
      {
        if(single.best.k == 0)
        {
          # this is a special case, and means that there will not be a selection but the maximum number of variables will be taken into account
          #k   <- as.numeric(gsub("k_","",k_catalogue))
          single.best.k <- max(spar)
        }
      }

      k_spar <- paste0("k_", single.best.k)

      # otherwise when single.best.k is specified this will be the first choice
      if(length(single.best.k) == 1 & is.numeric(single.best.k))
      {
        if(k_spar %in% names(mc)) # check if we have it in the model collection
        {
          if(verbose) print(paste0("getNBestModels: single.best.k mode with k_spar ", k_spar, " returning the best model"))
          pop     <- mc[[k_spar]]
          eval    <- populationGet_X(element2get = evalToOrder, toVec = TRUE, na.rm = FALSE)(pop)
          mod     <- pop[[which.max(eval)]]
          if(return.population)
          {
            return(list(mod))
          }else
          {
            return(mod)
          }
        }else # not found
        {
          if(verbose) print(paste0("getNBestModels: single.best.k mode with k_spar ", k_spar, " not found in the results"))
          if(return.population)
          {
            return(list(NA))
          }else
          {
            return(NA)
          }
        }
      }else
      {
        print(paste0("getNBestModels: single.best.k mode but ",k_spar, " is not found in the model collection. Executing the default settings."))
      }
    } # end of single.best.k

    if(return.population)
    {
      if(verbose) print(paste0("getNBestModels: returning a population of models"))
      # Transform the model collection onto a population
      return(modelCollectionToPopulation(mod.collection = res))
    }else
    {
      if(verbose) print(paste0("getNBestModels: returning a model collection"))
      # a model collection
      return(res)
    }

  }else
  {
    ####################################################################
    # # else not by.k.sparsity
    ####################################################################
    if(verbose) print(paste0("getNBestModels: regardless of k sparsity"))
    # first convert the pop
    if(unique.control)
    {
      pop             <- unique(modelCollectionToPopulation(mc))
    }else
    {
      pop             <- modelCollectionToPopulation(mc)
    }

    if(verbose) print(paste0("getNBestModels: the population with all sparsities contains ", length(pop), " models"))

    if(significance)
    {
      # restrict the population to the confidence interval
      pop           <- selectBestPopulation(pop = pop, score = evalToOrder, p = 0.05, k_penalty = k.penalty)
      if(verbose) print(paste0("getNBestModels: after significance selection with k.penalty ", k.penalty," it contains ", length(pop), " models"))
    }

    # if we wish to select best models with max min prevalence
    if(max.min.prevalence)
    {
      if(!is.null(X))
      {
        eval        <- populationGet_X(element2get = evalToOrder, toVec = TRUE, na.rm = FALSE)(pop)
        k           <- populationGet_X(element2get = "eval.sparsity", toVec = TRUE, na.rm = TRUE)(pop)
        eval.penalty<- eval - (k*k.penalty)
        best.eval   <- max(eval.penalty)
        epsilon     <- sqrt(best.eval * (1 - best.eval) / ncol(X))
        pop         <- pop[eval.penalty >= (best.eval - epsilon)]
        pop         <- getMaxMinPrevalenceModel(pop = pop, X = X, selected = 0)
        if(verbose) print(paste0("getNBestModels: after max.min.prevalence it contains ", length(pop), " models"))
      }
    } # end max.min.prevalence

  } # end by.k.sparsity ifelse


  if(return.population)
  {
    if(verbose) print(paste0("getNBestModels: returning a population of models"))
    return(pop)
  }else
  {
    if(verbose) print(paste0("getNBestModels: returning a model collection"))
    return(listOfModels2ModelCollection(pop))
  }
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

#' Function to predict the class for each "Class vs ALL" combination.
#' @title predict_ova
#' @description Function to predict the class for each "Class vs ALL" combination.
#' @param y: the response vector
#' @param mod: object model
#' @return list of class predict
#' @export
predict_ova <- function(mod, class_names) {
  intercept_list <- mod$intercept_
  score_list <- mod$score_

  # List of prediction vectors for each combination.
  predictions_list <- lapply(seq_along(intercept_list), function(j) {
    class_name <- class_names[j]
    sapply(score_list[[j]], function(score) {
      ifelse(score > intercept_list[[j]], class_name, "ALL")
    })
  })

  return(predictions_list)
}


