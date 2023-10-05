
################################################################
# @script:  ovoExample.R
# @author:  Edi Prifti
# @author:  Fabien KAMBU MBUANGI
# @author:  Jean-Daniel Zucker
# @date:    Juin 2023
################################################################

#' Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function
#' @title getSign_ovo
#' @description Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function, one versus one.
#' @import foreach
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param parallel.local: weather or not to run in //
#' @return a list of +1 & -1 for each one-vs-all classification of our dataset
#' @export
#'
getSign_ovo <- function(X, y, clf = NULL, parallel.local = FALSE)
{
  #One-versus-one class distribution
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

  for(i in 1:(length(list_y))){
    Sign <- getSign(X=list_X[[i]], y=list_y[[i]], clf = NULL, parallel.local = FALSE)
    list_Sign[[i]] = Sign
  }
  res <- list_Sign

  return(res)
}



#' Evaluates the fitting scores of a model objects one versus one
#' @title evaluateIntercept_ovo
#' @description Evaluates fit scores of model objects one versus one.
#' @param mod : model objects
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @return list model objects with fit scores
evaluateIntercept_ovo <- function(mod, X, y, clf)
{
  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list()
  listmod <- list()
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

  listcoeffs <- clf$coeffs_
  list_mod <- mod

  for(i in 1:length(list_mod)){
    clf$coeffs_ <- listcoeffs[[i]]
    evalInter <- evaluateIntercept(mod=list_mod[[i]], X=list_X[[i]], y=list_y[[i]], clf)
    listmod[[i]] <- evalInter
  }
  mod <- listmod
  return(mod)
}

#' Computes the predected classification using a given model
#' @title evaluateYhat_ovo
#' @description This function evaluates the predicted classification using (1) a model object list that contains the intercept and the sign or (2) directly the attributes score, intercept, sign
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param score: the score passed directly
#' @param intercept: the intercept passed directly
#' @param sign: the sign passed directly
#' @return a list with the predicted classification of the samples
#'
evaluateYhat_ovo <- function(mod = NULL, X, y, clf, score=NULL, intercept=NULL, sign=NULL)
{
  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list()
  listyhat <- list()
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

  listcoeffs <- clf$coeffs_
  list_mod <- mod
  for(i in 1:(length(list_mod))){
    clf$coeffs_ <- listcoeffs[[i]]
    yha <- evaluateYhat(mod = list_mod[[i]], X = list_X[[i]], y = list_y[[i]], clf = clf)
    listyhat[[i]] <- yha
  }
  yhat <- listyhat

  return(yhat)
}



#' Evaluates the accuracy of a model
#' @title evaluateAccuracy_ovo
#' @description This function one versus one, evaluates the accuracy of either (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign.
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param force.re.evaluation: evaluate again all the elements needed for accuracy (default:FALSE)
#' @param mode: training or test mode. If training, the funciton maximizes accuracy.
#' @return either (1) list a model whose evaluation parameters are updated or (2) the accuracy
#' @export
evaluateAccuracy_ovo <- function(mod = NULL, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{
  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list()
  listmod <- list()
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

  listcoeffs <- clf$coeffs_
  list_mod <- mod
  for(i in 1:(length(list_mod))){
    clf$coeffs_ <- listcoeffs[[i]]
    modacc <- evaluateAccuracy(mod = list_mod[[i]], X= list_X[[i]], y= list_y[[i]], clf, force.re.evaluation = force.re.evaluation, mode = mode)
    listmod[[i]] <- modacc
  }
  mod <- listmod

  return(mod)
}




#' Computes the AUC of a model
#' @title evaluateAUC_ovo
#' @description Computes the AUC of a model
#' @param score: the ^y score of the model
#' @param y: the response vector
#' @param sign: in which direction to make the comparison? "auto" (default): automatically define in which group
#' the median is higher and take the direction accordingly. ">": if the predictor values for the control group
#' are higher than the values of the case group (controls > t >= cases). "<": if the predictor values for the
#' control group are lower or equal than the values of the case group (controls < t <= cases).
#' @return  a list of auc values
#' @importFrom pROC roc
evaluateAUC_ovo <- function(score, y, sign = '>')
{
  #One-versus-one class distribution
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
    au <- evaluateAUC(score, y = list_y[[i]], sign = sign)
    list_auc[[i]] = au
  }
  auc <- list_auc
  return(auc)
}


#' Compute other prediction scores such as precision, recall and f-score
#' @title evaluateAdditionnalMetrics_ovo
#' @description This function computes prediction scores based on the confusion matrix such as accuracy, precision, recall and f-score
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param mode: training or testing mode
#' @return a list of models whose evaluation parameters are updated
evaluateAdditionnalMetrics_ovo <- function(mod, X, y, clf, mode = "train")
{
  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list()
  listmod <- list()
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

  listcoeffs <- clf$coeffs_
  list_mod <- mod
  for(i in 1:(length(list_mod))){
    clf$coeffs_ <- listcoeffs[[i]]
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
evaluateModelRegression_ovo <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE)
{
  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list()
  listmod <- list()
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
#' @description Evaluates the fitting score of a model object, one versus one.
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
  #One-versus-one class distribution
  nClasse <- unique(y)
  list_mod <- list()
  listmod <- list()
  list_y <- list()
  list_X <- list()
  scorelist <- list()
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
  listcoeffs <- clf$coeffs_
  list_mod <- mod



  for(i in 1: length(list_mod)) {

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
  for(i in 1: length(list_mod)) {
    if(force.re.evaluation)
    {

      clf$coeffs_ <- listcoeffs[[i]]
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

  for(i in 1:length(list_mod)) {
    # if after all the above steps we still don't have a score than we kill the model.
    if(!myAssertNotNullNorNa(mod[[i]]$score_))
    {
      return(NULL)
    }
  }

  for(i in 1:length(list_mod)) {
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
             for(i in 1: length(list_mod)){
               if((mod[[i]]$language != "ter" & mod[[i]]$language != "bin") & !isModelSota(mod[[i]]))
               {
                 mod                <- evaluateIntercept_ovo(X = X, y = y, clf = clf, mod = mod)
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
                 for(j in 1:length(mod)){
                   scorelist[[j]] <- mod[[j]]$score
                 }
                 aucg                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = ">")
                 aucl                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = "<")
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$auc_

               }

               # in case it is accuracy
               if(clf$params$evalToFit == "accuracy_") # in this case the auc will be computed in evaluate other metrics
               {
                 mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$accuracy_
               }

               # otherwise compute the rest
               if(clf$params$evalToFit != "auc_" & clf$params$evalToFit != "accuracy_")
               {
                 mod <- evaluateAdditionnalMetrics_ovo(mod = mod, X = X, y = y, clf = clf, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]][[clf$params$evalToFit]]
                 # compte accuracy also
                 mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 # and auc, since these are helpful information


                 for(j in 1:length(mod)){
                   scorelist[[j]] <- mod[[j]]$score
                 }

                 aucg                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = ">")
                 aucl                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = "<")
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
               }
             }
           } # if test mode, we don't recompute the intercept
           else
           {
             for(i in 1:length(list_mod)) {
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
                 for(j in 1:length(mod)){
                   scorelist[[j]] <- mod[[j]]$score
                 }

                 aucg                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = ">")
                 aucl                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = "<")
                 mod[[i]]$auc_             <- max(aucg[[i]], aucl[[i]])
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$auc_
               }

               # in case it is accuracy
               if(clf$params$evalToFit == "accuracy_") # in this case the auc will be computed in evaluate other metrics
               {
                 mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]]$accuracy_
               }

               # otherwise compute the rest
               if(clf$params$evalToFit != "auc_" & clf$params$evalToFit != "accuracy_")
               {
                 mod <- evaluateAdditionnalMetrics_ovo(mod = mod, X = X, y = y, clf = clf, mode = mode)
                 mod[[i]]$unpenalized_fit_ <- mod[[i]][[clf$params$evalToFit]]
                 # compte accuracy also
                 mod <- evaluateAccuracy_ovo(mod, X, y, clf, force.re.evaluation = force.re.evaluation, mode = mode)
                 # and auc, since these are helpful information
                 for(j in 1:length(mod)){
                   scorelist[[j]] <- mod[[j]]$score
                 }

                 aucg                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = ">")
                 aucl                 <- evaluateAUC_ovo(score = scorelist, y = y, sign = "<")
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

             for(i in 1:length(list_mod)){
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

           for(i in 1:length(list_mod)){

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
  for(i in 1:length(list_mod)) {
    mod[[i]]$fit_ <- max(mod[[i]]$unpenalized_fit_ - clf$params$k_penalty * mod[[i]]$eval.sparsity, 0)
  }


  return(mod)
}

#' Evaluates the fitting score of a model object ovo
#' @title evaluateModel_ovo
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

evaluateModel_ovo <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE, estim.feat.importance = FALSE, mode = 'train')
{

  if(mode != "train" & mode != "test")
  {
    stop("evaluateModel: mode should be one of c('train','test')")
  }

  #One-versus-one class distribution
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
  listcoeffs <- clf$coeffs_
  mod.res <- list()

  for(i in 1:(length(list_X))){
    list_mod[[i]] = mod
  }


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
  mod.res <- list_mod

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
        mod.res <- evaluateModelRegression_ovo(mod = mod.res, X = X, y = y, clf = clf, eval.all = eval.all, force.re.evaluation = force.re.evaluation)
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
    for(i in 1:length(mod.res)){
      # At this stage this should not happen but for debug stop it
      if((!myAssertNotNullNorNa(mod.res[[i]]$intercept_) | !myAssertNotNullNorNa(mod.res[[i]]$sign_)) & !isModelSota(mod.res[[i]]))
      {
        if(clf$params$warnings) warning("evaluateModel: model without intercept at this stage is not normal.")
        return(NULL)
      }
    }

    mod.res  <- evaluateAdditionnalMetrics_ovo(mod = mod.res, X = X, y = y, clf = clf, mode = mode)
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
  mod.res <- mod.res

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
  eval.sparsity_ovo <- 0
  precision_ovo = 0
  recall_ovo = 0
  f1_ovo = 0
  confusionMatrix_ovo = 0
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
    intercept_ovo = intercept_ovo + mod.res[[i]]$intercept_
    eval.sparsity_ovo = eval.sparsity_ovo + mod.res[[i]]$eval.sparsity
    signn[[i]] =  mod.res[[i]]$sign_
    precision_ovo = precision_ovo + mod.res[[i]]$precision_
    recall_ovo = recall_ovo + mod.res[[i]]$recall_
    f1_ovo = f1_ovo + mod.res[[i]]$f1_
    scoree[[i]] = mod.res[[i]]$score_
    pos_scoree[[i]] = mod.res[[i]]$pos_score_
    neg_scoree[[i]] = mod.res[[i]]$neg_score_
    confusionMatrix_ovo = confusionMatrix_ovo + mod.res[[i]]$confusionMatrix_
  }


  # retrieve average of evaluation metric
  fit_ovo = fit_ovo/nClass
  unpenalized_fit_ovo = unpenalized_fit_ovo/nClass
  auc_ovo = auc_ovo/nClass
  accuracy_ovo = accuracy_ovo/nClass
  intercept_ovo = intercept_ovo/nClass
  eval.sparsity_ovo =  eval.sparsity_ovo/nClass
  precision_ovo = precision_ovo/nClass
  recall_ovo = recall_ovo/nClass
  f1_ovo = f1_ovo/nClass
  confusionMatrix_ovo = confusionMatrix_ovo

  #generate a single model output
  mod.res <- list()
  mod.res$learner <- learnerr
  mod.res$language <- languagee
  mod.res$objective <- objectivee
  mod.res$evalToFit <- evalToFitt
  mod.res$indices_ <- indicess[[1]]
  mod.res$names_ <- namess[[1]]
  mod.res$coeffs_ <- coeffss[[1]]
  mod.res$fit_ <- fit_ovo
  mod.res$unpenalized_fit_ <- unpenalized_fit_ovo
  mod.res$auc_ <-auc_ovo
  mod.res$accuracy_ <- accuracy_ovo
  mod.res$intercept_ <-  intercept_ovo
  mod.res$eval.sparsity <- eval.sparsity_ovo
  mod.res$precision_ <- precision_ovo
  mod.res$recall_ <- recall_ovo
  mod.res$f1_ <- f1_ovo
  mod.res$cor_ <- corr
  mod.res$aic_ <- aicc
  mod.res$sign_ <- signn[[1]]
  mod.res$rsq_ <- rsqq
  mod.res$ser_ <- serr
  mod.res$score_ <- scoree[[1]]
  mod.res$pos_score_ <- pos_scoree[[1]]
  mod.res$neg_score_ <- neg_scoree[[1]]
  mod.res$confusionMatrix_ <- confusionMatrix_ovo


  return(mod.res)
}


#' evaluate Model one versus one
#' @title evaluateModel_ovo
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
listOfSparseVecToListOfModels_ovo <- function(X, y, clf, v, lobj = NULL, eval.all = FALSE)
{
  #One-versus-one class distribution
  nClasse <- unique(y)
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
  listcoeffs <- clf$coeffs_
  y = list_y[[1]]
  X = list_X[[1]]
  clf$coeffs_ = listcoeffs[[1]]
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
    pop[[i]] <- sparseVecToModel(X, y, model, clf, eval.all = eval.all, obj = lobj[[i]])
  }

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

evaluateFeatureImportanceInPopulation_ovo <- function(pop, X, y, clf, score = "fit_", filter.ci = TRUE, method = "optimized",
                                                      seed = c(1:10), aggregation = "mean", verbose = TRUE)
{
 #One-versus-one class distribution
  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  k <- 1
  listcoeffs <- list()
  list_res <- list()

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
  listcoeffs <- clf$coeffs_

  if(!isPopulation(pop))
  {
    if(clf$params$warnings) warning("evaluateFeatureImportanceInPopulation: please masure to provide a valid population.")
    return(NULL)
  }

  # Sanity checks
  #check.X_y_w(X = X, y = y)

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
  pop <- evaluatePopulation_ovo(X = X, y = y, clf = clf, pop = pop, eval.all = TRUE, force.re.evaluation = TRUE, mode = "test")
  #pop <- evaluatePopulation_ovo(X = X, y = y, clf = clf, pop = pop, eval.all = TRUE, force.re.evaluation = TRUE)
  # compute the presence of features in the models
  for(h in 1:length(list_y)) {
    X = list_X[[h]]
    y = list_y[[h]]
    clf$coeffs_ <- listcoeffs[[h]]

    fa            <- makeFeatureAnnot(pop = pop, X = X, y = y, clf = clf)
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

