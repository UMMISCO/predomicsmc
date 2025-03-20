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

  list_Sign <- list() # List of different combinations of Sign
  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
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
  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
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
  list_y <- list() # list of y
  list_X <- list() # list of X
  sign = sign
  list_auc <- list()
  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
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

  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
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
  list_mod <- list()
  listmod <- list()
  list_y <- list()
  list_X <- list()
  listcoeffs <- list()

  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
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

  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X

  listcoeffs <- clf$coeffs_
  list_mod <- mod
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
evaluateModel_mc <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE, estim.feat.importance = FALSE, approch = "ovo", mode = 'train', aggregation_ = "voting") {

  # Validate mode parameter; it must be either 'train' or 'test'
  if (mode != "train" & mode != "test") {
    stop("evaluateModel: mode should be one of c('train','test')")
  }

  # Initialize variables and data structures
  list_mod <- list()   # List of models
  list_y <- list()     # List of target variable subsets
  list_X <- list()     # List of feature matrix subsets
  listcoeffs <- list()
  mod.res <- list()
  listX <- list()
  listXmin <- list()   # List of minimum values for features
  listXmax <- list()   # List of maximum values for features
  listy <- list()

  # Copy necessary data and coefficients from classifier (clf)
  listcoeffs <- clf$coeffs_
  listX <- clf$data$X
  listXmin <- clf$data$X.min
  listXmax <- clf$data$X.max
  listy <- clf$data$y
  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X

  # Handle test mode: prepare model structures for evaluation
  if (mode == "test") {
    combi <- generate_combinations_with_factors(y, X, approch = approch)
    list_y <- combi$list_y
    list_X <- combi$list_X
    n_combinations <- length(mod$list_intercept_)
    list_mod <- vector("list", n_combinations)

    for (i in 1:n_combinations) {
      # Populate list_mod with model components for each combination
      list_mod[[i]] <- list(
        learner = mod$learner,
        language = mod$language,
        objective = mod$objective,
        indices_ = mod$indices_[[i]],
        names_ = mod$names_[[i]],
        coeffs_ = mod$coeffs_[[i]],
        fit_ = mod$fit_,
        unpenalized_fit_ = mod$unpenalized_fit_,
        auc_ = mod$auc_,
        accuracy_ = mod$accuracy_,
        cor_ = mod$cor_,
        aic_ = mod$aic_,
        intercept_ = mod$list_intercept_[[i]],
        eval.sparsity = mod$eval.sparsity,
        precision_ = mod$precision_,
        recall_ = mod$recall_,
        f1_ = mod$f1_,
        sign_ = mod$sign_[[i]],
        rsq_ = mod$rsq_[[i]],
        ser_ = mod$ser_[[i]],
        score_ = mod$score_[[i]],
        pos_score_ = mod$pos_score_[[i]],
        neg_score_ = mod$neg_score_[[i]],
        confusionMatrix_ = mod$confusionMatrix_[[i]]
      )
    }
    mod.res <- list_mod
  }

  # Handle train mode: reset model attributes for recomputation
  if (mode == "train") {
    mod.res <- mod
    for (i in 1:length(mod.res)) {
      # Reset relevant attributes
      mod.res[[i]]$fit_ <- NA
      mod.res[[i]]$unpenalized_fit_ <- NA
      mod.res[[i]]$auc_ <- NA
      mod.res[[i]]$accuracy_ <- NA
      mod.res[[i]]$precision_ <- NA
      mod.res[[i]]$recall_ <- NA
      mod.res[[i]]$f1_ <- NA
      mod.res[[i]]$intercept_ <- NA
      mod.res[[i]]$sign_ <- NA
      mod.res[[i]]$cor_ <- NA
      mod.res[[i]]$rsq_ <- NA
      mod.res[[i]]$ser_ <- NA
      mod.res[[i]]$aic_ <- NA
      mod.res[[i]]$score_ <- NA
      mod.res[[i]]$pos_score_ <- NA
      mod.res[[i]]$neg_score_ <- NA
    }
  }

  # Regression-specific handling if objective is correlation
  if (clf$params$objective == "cor") {
    for (i in 1:length(mod.res)) {
      if (!isModelSota(mod.res[[i]])) {
        mod.res <- evaluateModelRegression_mc(
          mod = mod.res, X = X, y = y, clf = clf,
          eval.all = eval.all, force.re.evaluation = force.re.evaluation
        )
        return(mod.res)
      } else {
        if (clf$params$warnings) warning("evaluateModel: evaluating a sota model in correlation objective")
      }
    }
  }

  # Additional feature importance estimation for non-SOTA models
  for (i in 1:length(mod.res)) {
    if (isModelSota(mod.res[[i]])) {
      if (estim.feat.importance) {
        estim.feat.importance <- FALSE
      }
    }
  }

  # Handle sparsity differences in models
  for (i in 1:length(list_y)) {
    if (mod.res[[i]]$eval.sparsity != length(unique(mod.res[[i]]$indices_))) {
      if (clf$params$warnings) warning("evaluateModel: An individual has at least one indice replicated")
      values2keep <- which(mod.res[[i]]$indices_ == unique(mod.res[[i]]$indices_))
      mod.res[[i]]$indices_ <- mod.res[[i]]$indices_[values2keep]
      mod.res[[i]]$names_ <- mod.res[[i]]$names_[values2keep]
      mod.res[[i]]$coeffs_ <- mod.res[[i]]$coeffs[values2keep]
      mod.res[[i]]$eval.sparsity <- length(unique(mod.res[[i]]$indices_))
    }
  }

  # Evaluate fit and additional metrics if required
  mod.res <- evaluateFit_mc(
    mod = mod.res, X = X, y = y, clf = clf,
    force.re.evaluation = force.re.evaluation, approch = approch, mode = mode
  )
  if (eval.all) {
    mod.res <- evaluateAdditionnalMetrics_mc(mod = mod.res, X = X, y = y, clf = clf, approch = approch, mode = mode)
  }

  # Feature importance estimation for BTR models
  for (i in 1:length(mod.res)) {
    if (!isModelSota(mod.res[[i]]) && estim.feat.importance) {
      clf$coeffs_ <- listcoeffs[[i]]
      clf$data$X <- listX[[i]]
      clf$data$X.min <- listXmin[[i]]
      clf$data$X.max <- listXmax[[i]]
      clf$data$y <- listy[[i]]
      mod.res[[i]] <- estimateFeatureImportance(mod = mod.res[[i]], X = list_X[[i]], y = list_y[[i]], clf = clf, plot.importance = FALSE)
    }
  }

  # Final aggregation of model results
  mod.res <- evaluateModels_aggregation(
    mod = mod.res, y = y, X = X,
    force.re.evaluation = TRUE, clf = clf,
    approch = approch, aggregation_ = aggregation_
  )

  return(mod.res)
}


#' evaluate Population multi class
#' @title evaluatePopulation_mc
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
                                  aggregation_ = "voting",
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
                                                 aggregation_ =  aggregation_,
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
                                                 aggregation_ =  aggregation_,
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
                                   aggregation_ =  aggregation_,
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
#' @title listOfSparseVecToListOfModels_mc
#' @description Converts an list of "SparseVec" objects onto a list of predomics objects
#' @import snow
#' @param X: dataset
#' @param y: labels
#' @param clf: classifier
#' @param v: list of vectors of coeffs. For example, v=list( c(0.0,1.0,0.0,-1.0) , c(1.0,1.0,0.0,0.0) , c(0.0,1.0,1.0,-1.0) )
#' @param lobj: a list of objects to add as elements in the model objects if not null (default:NULL)
#' @param eval.all: evaluate population (default:FALSE)
#' @return an list of model object ovo and ova
#' @export
listOfSparseVecToListOfModels_mc <- function(X, y, clf, v, lobj = NULL, eval.all = FALSE, approch = "ovo")
{


  # Initialize lists to store data for each class or pair of classes
  list_y <- list() # list of target values
  list_X <- list() # list of feature matrices
  listcoeffs <- list() # list of coefficients

  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
  # Retrieve the list of coefficients from the classifier
  listcoeffs <- clf$coeffs_

  # Validate the input lobj (optional additional objects)
  if (!is.null(lobj)) {
    if (!is.list(lobj)) {
      stop("listOfDenseVecToListOfModels: lobj should be a list of objects.")
    }
    if (length(lobj) != length(v)) {
      stop("listOfDenseVecToListOfModels: lobj should be a list the same length as v")
    }
  }

  # Handle the case where the input vector list is empty
  if (length(v) == 0) {
    if (clf$params$warnings) warning("listOfSparseVecToListOfModels: empty list returning NULL")
    return(NULL)
  }

  # Initialize population lists for storing models
  pop <- list()
  list_pop <- list()

  # Generate models for each sparse vector in v
  for (i in 1:length(v)) {
    for (j in 1:length(list_y)) {
      # Assign coefficients for the current class or class pair
      clf$coeffs_ <- listcoeffs[[j]]

      # Convert the sparse vector into a model
      model <- v[[i]][[j]]
      pop[[j]] <- sparseVecToModel(X = list_X[[j]], y = list_y[[j]], model, clf, eval.all = eval.all, obj = lobj[[i]][[j]])
    }
    list_pop[[i]] <- pop
  }

  # Finalize the list of models
  pop <- list_pop

  # Return the generated models
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
                                                     seed = c(1:10), aggregation = "mean", approch = "ovo", verbose = TRUE, aggregation_ = "votingAggregation")
{
  list_y <- list()
  list_X <- list()
  listcoeffs <- list()
  list_res <- list()
  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
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
  pop <- evaluatePopulation_mc(X = X, y = y, clf = clf, pop = pop, eval.all = TRUE, force.re.evaluation = TRUE, approch = approch, mode = "test", aggregation_ = aggregation_)
  # compute the presence of features in the models
  listfa  <- makeFeatureAnnot_mc(pop = pop, X = X, y = y, clf = clf, approch = approch)

  populations <- list()

  for (i in 1:length(pop[[1]]$indices_)) {
    lista <- list()

    for (j in 1:length(pop)) {
      popp <- pop[[j]]
      popp$indices_ <- pop[[j]]$indices_[[i]]
      popp$names_ <- pop[[j]]$names_[[i]]
      popp$coeffs_ <- pop[[j]]$coeffs_[[i]]
      popp$intercept_ <- pop[[j]]$list_intercept_mc[[i]]
      popp$sign_ <- pop[[j]]$sign_[[i]]
      popp$score_ <- pop[[j]]$score_[[i]]
      popp$confusionMatrix_ <- pop[[j]]$confusionMatrix_[[i]]

      lista[[j]] <- popp
    }

    populations[[i]] <- lista
  }

  for(h in 1:length(list_y)) {
    X = list_X[[h]]
    y = list_y[[h]]
    clf$coeffs_ <- listcoeffs[[h]]
    clf$data$X <- listX[[h]]
    clf$data$X.min <- listXmin[[h]]
    clf$data$X.max <- listXmax[[h]]
    clf$data$y <- listy[[h]]
    pop = populations[[h]]
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
                                                      eval.all = TRUE, force.re.evaluation = TRUE, mode = "test")
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

#' This function predicts outcomes for a one-versus-all (OvA) classification model.
#' @title predictModel_ova
#' @description This function predicts outcomes for a one-versus-all (OvA) classification model.
#' @param y: The true class labels.
#' @param X: The feature matrix of the data to be predicted.
#' @param mod: The model object containing the OvA classification models.
#' @param clf: Object clf.
#' @param force.re.evaluation: Boolean to force re-evaluation of the model even if it is already evaluated.
#' @return This function returns the list of one-versus-all predictions and the list of normalized scores.
#' @export
#'
predictModel_ova <- function(mod, y, X, clf, force.re.evaluation = TRUE) {
  # Initialize empty lists for models, class pairs, predictions, and scores
  predictions_list <- list()
  scorelist <- list()
  #y = as.vector(y)
  #nClasse <- unique(y)
  list_y <- list()

  # Determine the number of combinations
  mods <- list()
  mods = mod
  mods$binary_scores_mods = mods$score_
  # Calculate scores for each model and store in scorelist
  for(i in 1:length(mods)){
    scorelist[[i]] <- getModelScore(mod = mods[[i]], X = X, clf = clf, force.re.evaluation = TRUE)
  }

  # Extract scores only
  score_list <- lapply(scorelist, function(x) x$score_)

  # List of prediction vectors for each combination
  predictions_list <- lapply(seq_along(score_list), function(j) {
    # Obtenir les niveaux (levels) de y
    class_name <- levels(as.factor(y))[j]

    # Calcul des prédictions pour la classe actuelle
    sapply(score_list[[j]], function(score) {
      ifelse(score > mods[[j]]$intercept_, class_name, "ALL")
    })
  })

  list_intercept <- list()
  list_intercept = lapply(mods, function(x) x$intercept_)
  # Identifier les seuils minimum et maximum
  seuils_min <- min(unlist(list_intercept))
  seuils_max <- max(unlist(list_intercept))

  # Normalisation des scores
  scores_normalises <- lapply(score_list, function(scores) {
    denominateur <- seuils_max - seuils_min
    denominateur[denominateur == 0] <- 1e-10  # Remplacer les zéros par une petite valeur
    scores <- as.numeric(unlist(scores))  # Conversion en type numérique
    seuils_min <- as.numeric(seuils_min)  # Conversion en type numérique
    (scores - seuils_min) / denominateur
  })

  # Remplacer les valeurs -Inf par 0
  scores_normalises <- lapply(scores_normalises, function(scores) {
    scores[is.infinite(scores)] <- 0  # Remplacer les valeurs -Inf par 0
    scores
  })
  for(i in 1: length(scores_normalises)){
    mods[[i]]$scores_predictions <- scores_normalises[[i]]
    mods[[i]]$predictions <- predictions_list[[i]]
  }
  mod <- list()
  mod <- mods

  # Return predicted class labels for each combination, the corresponding score vectors, and the distances
  return(mod)
}


#' This function predicts outcomes for a one-versus-one (OvO) classification model.
#' @title predictModel_ovo
#' @description This function predicts outcomes for a one-versus-one (OvO) classification model.
#' @param y: The true class labels
#' @param X: The feature matrix of the data to be predicted
#' @param mod: The model object containing the OvO classification models
#' @param clf: The clf object
#' @param force.re.evaluation: Boolean to force re-evaluation of the model even if it is already evaluated.
#' @return This function returns mod with a list of one-versus-one predictions and the list of normalized scores.
#' @export
#'
predictModel_ovo <- function(mod, y, X, clf, force.re.evaluation = TRUE ) {
  # Initialize empty lists for models, class pairs, predictions, and scores
  predictions_list <- list()
  scorelist <- list()
  #nClasse <- unique(y)
  list_y <- list()
  mods <- list()
  mods <- mod
  ##mods$binary_scores_mods = mods$score_

  # Calculate scores for each model and store in scorelist
  for(i in 1:length(mod)){
    scorelist[[i]] <- getModelScore(mod = mods[[i]], X = X, clf = clf, force.re.evaluation = TRUE)
  }

  # Extract scores only
  score_only_list <- lapply(scorelist, function(x) x$score_)

  # Decompose dataset into one-versus-one combinations
  list_y <- decompose_y_levels(y)

  # Get the number of combinations
  num_combinations <- length(mods)

  # Predict class labels for each combination
  for (comb_index in 1:num_combinations) {
    intercept <- mods[[comb_index]]$intercept_
    scores <- score_only_list[[comb_index]]
    num_samples <- length(scores)

    # Get unique classes
    class_values <- list_y[[comb_index]]
    unique_classes <- unique(class_values)

    # Initialize prediction vector
    predictions <- character(length = num_samples)

    # Make predictions based on decision values
    for (sample_index in 1:num_samples) {
      decision_value <- scores[sample_index]
      predictions[sample_index] <- ifelse(decision_value > intercept, unique_classes[2], unique_classes[1])
    }

    # Add predictions to list
    predictions_list[[comb_index]] <- predictions
  }

  # Calculate distances between scores and intercepts
  scores_distance <- list()

  # Check if 'score_list' contains only zeros
  if (all(sapply(score_only_list, function(scores_distance) all(scores_distance == 0)))) {
    # If 'score_list' contains only zeros, fill 'scores_distance' with zero lists of the same length
    scores_distance <- lapply(score_only_list, function(scores_distance) rep(0, length(scores_distance)))
  } else {
    # If 'score_list' contains other values, calculate the distances
    for(i in 1:length(mods)){
      # Calculate the absolute difference between 'score_list' and 'intercept'
      scores_distance[[i]] = abs((score_only_list[[i]]) - mods[[i]]$intercept)
    }
  }

  # Normalize function
  normalize <- function(x) {
    if (length(x) == 0 || max(x) == min(x)) {
      # If the vector has no variation, return the vector unchanged
      return(x)
    } else {
      # Otherwise, normalize the vector
      return((x - min(x)) / (max(x) - min(x)))
    }
  }

  # Apply the normalize function to each vector in the list
  normalized_scores <- lapply(scores_distance, normalize)

  # Invert the normalized scores
  new_scores <- lapply(normalized_scores, function(x) 1 - x)

  # Store the predictions and normalized scores
  for(i in 1:length(new_scores)){
    mods[[i]]$scores_predictions <- new_scores[[i]]
    mods[[i]]$predictions <- predictions_list[[i]]
  }

  mod <- mods  # Update mod with the new values

  # Return mod predicted class labels for each combination, the corresponding score vectors, and the distances
  return(mod)
}


# Function to aggregate one-versus-one predictions using majority voting
#' @title voting
#' @description Function to aggregate one-versus-one predictions using majority voting.
#' @param mod: The model object containing predictions and scores.
#' @return The function returns the model object with aggregated predictions.
#' @export
voting <- function(mod) {
  predictions_list <- lapply(mod, function(x) x$predictions)  # Extract predictions for each model

  num_samples <- length(predictions_list[[1]])  # Number of samples in one vector
  aggregated_vector <- character(num_samples)  # Initialize aggregated vector

  # Iterate over each sample
  for (i in 1:num_samples) {
    # Count votes for position i
    votes <- table(sapply(predictions_list, `[`, i))
    # Aggregate prediction for position i (select the most frequent class)
    aggregated_vector[i] <- names(sort(votes, decreasing = TRUE)[1])
  }

  # Aggregation for other model properties
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_ <- lapply(mod, function(x) x$accuracy_)
  model$cor_ <- NA
  model$aic_ <- NA
  model$list_intercept_ <- lapply(mod, function(x) x$intercept_)
  model$intercept_ <- mean(sapply(mod, function(x) x$intercept_))  # Mean of intercepts
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <- lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_vector
  model$method <- "voting"
  model$approach <- "ovo"

  return(model)
}


# Function to aggregate one-versus-one predictions using majority voting
#' @title Predomics_aggregation_ovo
#' @description Function to aggregate one-versus-one predictions using majority voting, incorporating the ability to break ties by selecting the class with the highest weighted score among the tied classes.
#' @param mod: The model object containing predictions and scores.
#' @return The function returns the model object with aggregated predictions.
#' @export
Predomics_aggregation_ovo <- function(mod) {
  predictions_list <- lapply(mod, function(x) x$predictions)
  scores_list <- lapply(mod, function(x) x$scores_predictions)
  num_samples <- length(predictions_list[[1]])  # Number of samples in one vector
  aggregated_vector <- character(num_samples)  # Initialize aggregated vector

  # Iterate over each sample
  for (i in 1:num_samples) {
    # Count votes for position i
    votes <- table(sapply(predictions_list, `[`, i))
    max_votes <- max(votes)

    # Get classes with the maximum number of votes
    top_classes <- names(votes[votes == max_votes])

    if (length(top_classes) > 1) {
      # If there is a tie, calculate the sum of scores for each tied class
      class_scores_sum <- sapply(top_classes, function(class) {
        sum(sapply(1:length(mod), function(j) {
          if (predictions_list[[j]][i] == class) {
            return(scores_list[[j]][i])
          } else {
            return(0)
          }
        }))
      })

      # Choose the class with the highest sum of scores
      best_class_index <- which.max(class_scores_sum)
      aggregated_vector[i] <- top_classes[best_class_index]
    } else {
      # If no tie, choose the class with the maximum votes
      aggregated_vector[i] <- top_classes[1]
    }
  }

  #### Aggrégations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_<- lapply(mod, function(x) x$accuracy_)
  model$cor_<- NA
  model$aic_<- NA
  model$list_intercept_<- lapply(mod, function(x) x$intercept_)
  model$intercept_<- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <-lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_vector
  mod <- list()
  mod <- model
  mod$method = "Predomics_aggregation_ovo"
  mod$approch = "ovo"
  return(mod)

}

#' This function performs weighted aggregation of predictions.
#' @title weighted
#' @description Function to aggregate one-versus-one predictions using weighted voting.
#' @param mod: The model object containing predictions and scores.
#' @return The function returns the model object with aggregated predictions.
#' @export
#'
weighted <- function(mod) {
  predictions_list <- list()
  scores_list <- list()
  predictions_list <- lapply(mod, function(x) x$predictions)
  scores_list <- lapply(mod, function(x) x$scores_predictions)

  num_predictions <- length(predictions_list)
  num_samples <- length(predictions_list[[1]])

  aggregated_vector <- character(num_samples)

  # Iterate over each sample
  for (i in 1:num_samples) {
    # Extract scores for the current sample
    class_scores <- sapply(scores_list, function(score_vector) score_vector[i])
    # Extract predictions for the current sample
    prediction_ <- sapply(predictions_list, function(prediction_vector) prediction_vector[i])

    # Get unique classes in the predictions
    classes <- unique(prediction_)
    class_scores_sum <- numeric(length(classes))

    # Calculate the sum of scores for each unique class
    for (j in 1:length(classes)) {
      class_ <- classes[j]
      class_scores_sum[j] <- sum(scores_list[[j]][prediction_ == class_])
    }

    # Find the index of the class with the highest sum of scores
    best_class_index <- which.max(class_scores_sum)
    best_class <- classes[best_class_index]

    # Assign the predicted class with the highest sum of scores to the aggregated vector
    aggregated_vector[i] <- best_class
  }

  #### Aggrégations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_<- lapply(mod, function(x) x$accuracy_)
  model$cor_<- NA
  model$aic_<- NA
  model$list_intercept_<- lapply(mod, function(x) x$intercept_)
  model$intercept_<- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <-lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_vector
  mod <- list()
  mod <- model
  mod$predictions_aggre <- aggregated_vector
  mod$method = "weighted_voting"
  mod$approach = "ovo"
  return(mod)
}







#' This function performs weighted aggregation of predictions, weighted by AUC and F1 score.
#' @title weighted_f1_auc
#' @description Function to aggregate one-versus-one predictions using weighted voting with AUC and F1 score.
#' @param mod: The model object containing predictions and scores.
#' @return The function returns the model object with aggregated predictions.
#' @export
#'
weighted_f1_auc <- function(mod) {
  predictions_list <- list()
  scores_list <- list()
  f1_list <- lapply(mod, function(x) x$f1_)
  auc_list <- lapply(mod, function(x) x$auc_)
  predictions_list <- lapply(mod, function(x) x$predictions)
  scores_list <- lapply(mod, function(x) x$scores_predictions)

  num_predictions <- length(predictions_list)
  num_samples <- length(predictions_list[[1]])

  aggregated_vector <- character(num_samples)

  # Iterate over each sample
  for (i in 1:num_samples) {
    # Extract scores for the current sample and multiply by F1 * AUC
    weighted_scores <- sapply(seq_along(scores_list), function(j) scores_list[[j]][i] * f1_list[[j]] * auc_list[[j]])

    # Extract predictions for the current sample
    prediction_ <- sapply(predictions_list, function(prediction_vector) prediction_vector[i])

    # Get unique classes in the predictions
    classes <- unique(prediction_)

    # Handle empty classes case
    if (length(classes) == 0) {
      warning(paste("No classes found for sample", i))
      next
    }

    class_scores_sum <- numeric(length(classes))

    # Calculate the sum of scores for each unique class
    for (j in 1:length(classes)) {
      class_ <- classes[j]
      class_scores_sum[j] <- sum(weighted_scores[prediction_ == class_], na.rm = TRUE)
    }

    # Find the index of the class with the highest sum of scores
    if (all(is.na(class_scores_sum))) {
      warning(paste("All scores are NA for sample", i))
      next
    }

    best_class_index <- which.max(class_scores_sum)

    # Handle case where best_class_index is empty
    if (length(best_class_index) == 0) {
      warning(paste("No best class found for sample", i))
      next
    }

    best_class <- classes[best_class_index]

    # Assign the predicted class with the highest sum of scores to the aggregated vector
    aggregated_vector[i] <- best_class
  }

  #### Aggregations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- auc_list
  model$accuracy_ <- lapply(mod, function(x) x$accuracy_)
  model$cor_ <- NA
  model$aic_ <- NA
  model$list_intercept_ <- lapply(mod, function(x) x$intercept_)
  model$intercept_ <- mean(sapply(mod, function(x) x$intercept_), na.rm = TRUE)
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- f1_list
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- predictions_list
  model$scores_predictions <- scores_list
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_vector
  mod <- list()
  mod <- model
  mod$predictions_aggre <- aggregated_vector
  mod$method <- "weighted_f1_auc"
  mod$approach <- "ovo"
  return(mod)
}



#' This function performs weighted aggregation of predictions, weighted by Accuracy.
#' @title weighted_accuracy
#' @description Function to aggregate one-versus-one predictions using weighted voting with Accuracy.
#' @param mod: The model object containing predictions and scores.
#' @return The function returns the model object with aggregated predictions.
#' @export
#'
weighted_accuracy <- function(mod) {
  predictions_list <- list()
  scores_list <- list()
  accuracy_list <- lapply(mod, function(x) x$accuracy_)
  predictions_list <- lapply(mod, function(x) x$predictions)
  scores_list <- lapply(mod, function(x) x$scores_predictions)

  num_predictions <- length(predictions_list)
  num_samples <- length(predictions_list[[1]])

  aggregated_vector <- character(num_samples)

  # Iterate over each sample
  for (i in 1:num_samples) {
    # Extract scores for the current sample and multiply by accuracy
    weighted_scores <- sapply(seq_along(scores_list), function(j) scores_list[[j]][i] * accuracy_list[[j]])
    # Extract predictions for the current sample
    prediction_ <- sapply(predictions_list, function(prediction_vector) prediction_vector[i])

    # Get unique classes in the predictions
    classes <- unique(prediction_)
    class_scores_sum <- numeric(length(classes))

    # Calculate the sum of scores for each unique class
    for (j in 1:length(classes)) {
      class_ <- classes[j]
      class_scores_sum[j] <- sum(weighted_scores[prediction_ == class_])
    }

    # Find the index of the class with the highest sum of scores
    best_class_index <- which.max(class_scores_sum)
    best_class <- classes[best_class_index]

    # Assign the predicted class with the highest sum of scores to the aggregated vector
    aggregated_vector[i] <- best_class
  }

  #### Aggregations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_ <- accuracy_list
  model$cor_ <- NA
  model$aic_ <- NA
  model$list_intercept_ <- lapply(mod, function(x) x$intercept_)
  model$intercept_ <- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- predictions_list
  model$scores_predictions <- scores_list
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_vector
  mod <- list()
  mod <- model
  mod$predictions_aggre <- aggregated_vector
  mod$method <- "weighted_accuracy"
  mod$approach <- "ovo"
  return(mod)
}



#' Aggregation function of one-versus-all predictions using search and score max.
#' @title Predomics_aggregation_ova
#' @description  Function to aggregate one-versus-all predictions using New approach.
#' @param mod: The model object containing predictions and scores.
#' @param y: true class.
#' @return The function returns the model object with aggregated predictions.
#' @export
Predomics_aggregation_ova <- function(mod, y) {
  # Initialize the vector to hold the aggregated predictions with the appropriate length
  classes_list <- list()
  score_list <- list()
  classes_list = lapply(mod, function(x) x$predictions)
  score_list = lapply(mod, function(x) x$scores_predictions)
  aggregated_predictions <- character(length = length(classes_list[[1]]))

  # Create a character vector of the unique classes from y
  y = as.factor(y)
  names_class <- levels(y)

  names_class <- as.character(names_class)

  # Iterate over each position in the aggregated predictions
  for (i in seq_along(aggregated_predictions)) {
    # Extract the classes and scores for the current position from each list
    current_classes <- sapply(classes_list, function(class_vector) class_vector[i])
    scores <- sapply(score_list, function(score_vector) score_vector[i])

    # Exclude 'ALL' and retrieve corresponding scores
    valid_classes_indices <- which(current_classes != "ALL")
    valid_classes <- current_classes[valid_classes_indices]
    valid_scores <- scores[valid_classes_indices]

    # If all scores are zero, randomly choose a class from the list of unique classes
    if (all(scores == 0)) {
      predicted_class <- sample(names_class, 1)
    } else if (length(valid_classes) == 1) {
      # If there is only one valid class, predict that class
      predicted_class <- valid_classes
    } else if (length(valid_classes) > 1) {
      # If there are multiple valid classes, choose the class with the highest score
      max_score_index <- which.max(valid_scores)
      predicted_class <- valid_classes[max_score_index]
    } else {
      # If there are no valid classes (all were 'ALL'), choose the class from 'ALL' with the highest score
      max_score <- max(scores)  # first, find the max score from the original scores
      max_score_indices <- which(scores == max_score)  # then find all the indices with max score
      predicted_class <- names_class[max_score_indices[1]]  # choose the class for the first index with max score
    }

    # Fill the aggregation vector at position i with the predicted class
    aggregated_predictions[i] <- predicted_class
  }

  #### Aggrégations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_<- lapply(mod, function(x) x$accuracy_)
  model$cor_<- NA
  model$aic_<- NA
  model$list_intercept_<- lapply(mod, function(x) x$intercept_)
  model$intercept_<- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <-lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_predictions
  mod <- list()
  mod <- model
  # Return the final vector of aggregated predictions
  mod$method = "Predomics_aggregation_ova"
  mod$approach = "ova"
  return(mod)
}

#' Aggregation function of one-versus-all predictions using maximum score.
#' @title maximization
#' @description  Function to aggregate one-versus-all predictions using maximization Aggregation.
#' @param mod: The model object containing predictions and scores.
#' @param y: True class.
#' @return  The function returns the model object with aggregated predictions.
#' @export
maximization <- function(mod, y) {
  # Initialize the vector to hold the aggregated predictions with the appropriate length
  classes_list <- list()
  score_list <- list()
  classes_list <- lapply(mod, function(x) x$predictions)
  score_list  <- lapply(mod, function(x) x$scores_predictions)
  aggregated_predictions <- character(length = length(classes_list[[1]]))
  y = as.factor(y)
  current_classes <- levels(y)
  # Iterate over each position in the aggregated predictions
  for (i in seq_along(aggregated_predictions)) {
    scores <- sapply(score_list, function(score_vector) score_vector[i])

    # Find the index of the maximum score
    max_score_index <- which.max(scores)

    # Fill the aggregation vector at position i with the predicted class with the maximum score
    aggregated_predictions[i] <- current_classes[max_score_index]
  }

  #### Aggrégations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_<- lapply(mod, function(x) x$accuracy_)
  model$cor_<- NA
  model$aic_<- NA
  model$list_intercept_<- lapply(mod, function(x) x$intercept_)
  model$intercept_<- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <-lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_predictions
  mod <- list()
  mod <- model
  mod$method = "maximization"
  mod$approach = "ova"
  return(mod)
}



#' Aggregation function of one-versus-all predictions using maximum score, weighted by AUC and F1 score.
#' @title maximization_accuracy
#' @description  Function to aggregate one-versus-all predictions using maximization aggregation, weighted by AUC and F1 score.
#' @param mod: The model object containing predictions and scores.
#' @param y: True class.
#' @return  The function returns the model object with aggregated predictions.
#' @export
maximization_accuracy <- function(mod, y) {
  # Initialize the vector to hold the aggregated predictions with the appropriate length
  classes_list <- lapply(mod, function(x) x$predictions)
  score_list <- lapply(mod, function(x) x$scores_predictions)
  accuracy_list <- lapply(mod, function(x) x$accuracy_) # Get the accuracy of each submodel

  aggregated_predictions <- character(length = length(classes_list[[1]]))
  y <- as.factor(y)
  current_classes <- levels(y)

  # Iterate over each position in the aggregated predictions
  for (i in seq_along(aggregated_predictions)) {
    # Multiply each score by the corresponding accuracy
    scores <- sapply(seq_along(score_list), function(j) score_list[[j]][i] * accuracy_list[[j]])

    # Find the index of the maximum score
    max_score_index <- which.max(scores)

    # Fill the aggregation vector at position i with the predicted class with the maximum score
    aggregated_predictions[i] <- current_classes[max_score_index]
  }

  #### Aggregations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_ <- lapply(mod, function(x) x$accuracy_)
  model$cor_ <- NA
  model$aic_ <- NA
  model$list_intercept_ <- lapply(mod, function(x) x$intercept_)
  model$intercept_ <- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <- lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_predictions
  mod <- list()
  mod <- model
  mod$method <- "maximization_accuracy"
  mod$approach <- "ova"
  return(mod)
}



#' Aggregation function of one-versus-all predictions using ranking score.
#' @title ranking
#' @description Function to aggregate one-versus-all predictions using ranking Aggregation.
#' @param mod: The model object containing predictions and scores.
#' @param y: True class.
#' @return  The function returns the model object with aggregated predictions.
#' @export
ranking <- function(mod, y) {
  # Initializing the vector to store aggregated predictions with appropriate length
  classes_list <- list()
  score_list <- list()
  classes_list <- lapply(mod, function(x) x$predictions)
  score_list  <- lapply(mod, function(x) x$scores_predictions)
  aggregated_predictions <- character(length = length(classes_list[[1]]))
  y = as.factor(y)
  current_classes <- levels(y)

  # Iterating over each position in the aggregated predictions
  for (i in seq_along(aggregated_predictions)) {
    # Extract the scores for the current position from each list
    scores <- sapply(score_list, function(score_vector) score_vector[i])

    # Ranking the scores and assigning the corresponding rank to each score
    rank_scores <- rank(-scores, ties.method = "min")

    # Selecting the class corresponding to the highest score
    best_class_index <- which(rank_scores == 1)
    aggregated_predictions[i] <- current_classes[best_class_index]
  }
  #### Aggregations
  model <- list()
  model$learner <- mod[[1]]$learner
  model$language <- mod[[1]]$language
  model$objective <- mod[[1]]$objective
  model$indices_ <- lapply(mod, function(x) x$indices_)
  model$names_ <- lapply(mod, function(x) x$names_)
  model$coeffs_ <- lapply(mod, function(x) x$coeffs_)
  model$fit_ <- lapply(mod, function(x) x$fit_)
  model$unpenalized_fit_ <- lapply(mod, function(x) x$unpenalized_fit_)
  model$auc_ <- lapply(mod, function(x) x$auc_)
  model$accuracy_<- lapply(mod, function(x) x$accuracy_)
  model$cor_<- NA
  model$aic_<- NA
  model$list_intercept_<- lapply(mod, function(x) x$intercept_)
  model$intercept_<- mean(sapply(mod, function(x) x$intercept_))
  model$eval.sparsity <- mod[[1]]$eval.sparsity
  model$precision_ <- lapply(mod, function(x) x$precision_)
  model$recall_ <- lapply(mod, function(x) x$recall_)
  model$f1_ <- lapply(mod, function(x) x$f1_)
  model$sign_ <- lapply(mod, function(x) x$sign_)
  model$rsq_ <- lapply(mod, function(x) x$rsq_)
  model$ser_ <- lapply(mod, function(x) x$ser_)
  model$score_ <- lapply(mod, function(x) x$score_)
  model$predictions <- lapply(mod, function(x) x$predictions)
  model$scores_predictions <-lapply(mod, function(x) x$scores_predictions)
  model$pos_score_ <- lapply(mod, function(x) x$pos_score_)
  model$neg_score_ <- lapply(mod, function(x) x$neg_score_)
  model$confusionMatrix_ <- lapply(mod, function(x) x$confusionMatrix_)
  model$predictions_aggre <- aggregated_predictions
  mod <- list()
  mod <- model
  mod$method = "ranking"
  mod$approach = "ova"
  return(mod)
}


#' This function evaluates the aggregated model's performance.
#' @title evaluateModel_aggregation
#' @param mod: The model object containing aggregated predictions and true labels.
#' @param y: The true class labels.
#' @return The function returns the model object with evaluation metrics.
#' @export
#'
evaluateModel_aggregation <- function(mod, y) {
  # Calculation of the confusion matrix
  confusion_matrix <- table(mod$predictions_aggre, y)
  # Calculation of the overall accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  # Initialization of variables for precision, recall, and F1 score
  class_precision <- numeric(length = nrow(confusion_matrix))
  class_recall <- numeric(length = nrow(confusion_matrix))
  # Calculation of precision, recall, and F1 score for each class
  for (i in 1:nrow(confusion_matrix)) {
    tp <- confusion_matrix[i, i] # True Positives
    fp <- sum(confusion_matrix[i, ]) - tp # False Positives
    fn <- sum(confusion_matrix[, i]) - tp # False Negatives
    class_precision[i] <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    class_recall[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  }
  # Calculation of averages for precision, recall, and F1 score
  mean_precision <- mean(class_precision, na.rm = TRUE)
  mean_recall <- mean(class_recall, na.rm = TRUE)
  mean_f1 <- 2 * (mean_precision * mean_recall) / (mean_precision + mean_recall)
  # Creation of the model object containing all metrics
  mod$fit_ <- accuracy
  auc <- mean(unlist(mod$auc_))
  mod$auc_ = auc
  mod$unpenalized_fit_ <- accuracy
  mod$accuracy_ <- accuracy
  mod$precision_ <- mean_precision
  mod$recall_ <- mean_recall
  mod$f1_ <-  mean_f1
  mod$Multi_confusionMatrix_ <- confusion_matrix

  return(mod)
}

#' This function evaluates the aggregated models' performance.
#' @title evaluateModel_aggregation
#' @param mod models objects.
#' @param y The true class labels.
#' @param X The feature matrix of the data to be predicted.
#' @param force.re.evaluation Boolean to force re-evaluation of the model even if it is already evaluated.
#' @param clf Object clf.
#' @param approach Type of approach to be used ("ovo" or "ova").
#' @param aggregation Type of aggregation method to be used ("votingAggregation", "weightedAggregation", "Predomics_aggregation_ova", "maximizationAggregation", "rankingAggregation").
#' @return The function returns a list containing evaluated models with their respective metrics.
#' @export
evaluateModels_aggregation <- function(mod, y, X, force.re.evaluation = TRUE, clf, approch = "ovo", aggregation_ = "votingAggregation") {
  # Initializing an empty list to store the overall evaluation of each model.
  mod_evaluate <- NULL


  if (approch == "ovo") {
    if (aggregation_ == "voting") {
      mod_predict <- predictModel_ovo(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_voting <- voting(mod = mod_predict)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_voting, y = y)
    } else if (aggregation_ == "weighted") {
      mod_predict <- predictModel_ovo(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_weighted <- weighted(mod = mod_predict)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_weighted, y = y)
    } else if (aggregation_ == "Predomics_aggregation_ovo") {
      mod_predict <- predictModel_ovo(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_prodomics_ovo <- Predomics_aggregation_ovo(mod = mod_predict)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_prodomics_ovo, y = y)
    }  else if (aggregation_ == "weighted_f1_auc") {
      mod_predict <- predictModel_ovo(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_weighted_f1_auc <- weighted_f1_auc(mod = mod_predict)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_weighted_f1_auc, y = y)
    }  else if (aggregation_ == "weighted_accuracy") {
      mod_predict <- predictModel_ovo(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_weighted_accuracy <- weighted_accuracy(mod = mod_predict)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_weighted_accuracy, y = y)
    }else if (aggregation_ == "weighted_f1") {
      mod_predict <- predictModel_ovo(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_weighted_f1 <- weighted_f1(mod = mod_predict)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_weighted_f1, y = y)
    }

  } else if (approch == "ova") {
    if (aggregation_ == "Predomics_aggregation_ova") {
      mod_predict <- predictModel_ova(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_Predomics <- Predomics_aggregation_ova(mod = mod_predict, y = y)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_Predomics, y = y)
    } else if (aggregation_ == "maximization") {
      mod_predict <- predictModel_ova(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_maximization <- maximization(mod = mod_predict, y = y)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_maximization, y = y)
    } else if (aggregation_ == "ranking") {
      mod_predict <- predictModel_ova(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_ranking <- ranking(mod = mod_predict, y = y)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_ranking, y = y)
    } else if (aggregation_ == "maximization_accuracy") {
      mod_predict <- predictModel_ova(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_maximization_accuracy <- maximization_accuracy(mod = mod_predict, y = y)
      mod_evaluate <- evaluateModel_aggregation(mod = mod_maximization_accuracy , y = y)
    }else if (aggregation_ == "maximization_f1") {
      mod_predict <- predictModel_ova(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_maximization_f1 <- maximization_f1(mod = mod_predict, y = y)
      mod_evaluate <- evaluateModel_aggregation(mod =  mod_maximization_f1 , y = y)
    } else if (aggregation_ == "maximization_f1_auc") {
      mod_predict <- predictModel_ova(mod = mod, y = y, X = X, clf = clf, force.re.evaluation = TRUE)
      mod_maximization_f1_auc <-  maximization_f1_auc(mod = mod_predict, y = y)
      mod_evaluate <- evaluateModel_aggregation(mod =  mod_maximization_f1_auc , y = y)
    }

  }

  # Return the list containing evaluated models with their respective metrics.
  return(mod_evaluate)
}



#' Regenerate the `clf` object with updated data and coefficients.
#'
#' This function regenerates the `clf` object by initializing its data components, decomposing the dataset based on the specified approach, and computing the coefficients for the model.
#'
#' @title regenerate_clf
#' @param clf Object representing the model to be regenerated. It should contain the fields required for updating, such as `feature.cor`.
#' @param X Matrix of input data (features) used for model training and testing.
#' @param y Vector of target labels corresponding to the rows of `X`.
#' @param approch Type of approach to be used for dataset decomposition. Accepts "ovo" (one-versus-one) or "ova" (one-versus-all).
#' @return The function returns the updated `clf` object with new data and computed coefficients.
#' @export
regenerate_clf <- function(clf, X, y, approch) {
  # Initialize lists
  clf$data <- list()
  list_features <- list()
  clf$data$features <- list_features
  names(clf$data$features) <- clf$data$features
  list_XX <- list()  # List of X combinations
  list_min <- list() # List min of X
  list_max <- list() # List max of X

  feature.cor <- list()  # List of different combinations of feature.cor
  list_y <- list()       # List of different combinations of y
  list_X <- list()       # List of different combinations of X

  list_y <- list() #  List of different combinations of y
  list_X <- list() #  List of different combinations of X
  combi <- generate_combinations_with_factors(y, X, approch = approch)
  list_y <- combi$list_y
  list_X <- combi$list_X
  max.nb.features <- nrow(X)

  for (i in 1:length(list_X)) {
    Xi <- list_X[[i]][rownames(clf$feature.cor[[i]])[1:max.nb.features], ]

    # Check if Xi is non-empty and does not contain only NAs
    if (length(Xi) > 0 && !all(is.na(Xi))) {
      mino <- min(Xi, na.rm = TRUE)
      maxo <- max(Xi, na.rm = TRUE)
    } else {
      mino <- NA  # Assign NA if the condition is met
      maxo <- NA  # Assign NA if the condition is met
    }

    list_XX[[i]] <- Xi
    list_min[[i]] <- mino
    list_max[[i]] <- maxo
  }

  clf$data$X <- list_XX
  clf$data$X.min <- list_min
  clf$data$X.max <- list_max
  clf$data$y <- list_y

  # Compute the coefficients once for all to improve performance
  cat("... Computing ternary coefficients for speedup\n")
  coeffs <- getSign_mc(X = X, y = y, clf = clf, approch = approch, parallel.local = FALSE)
  clf$coeffs_ <- coeffs  # Add them to the clf

  return(clf)
}


# Create a function that transforms a population of model objects onto a dataframe to be plotted
#' populationToDataFrame
#'
#' @description For each model in the list of models it will extract each attribute and create a dataframe needed for further exploration
#' @param pop: a list of model objects, (i.e a population of models)
#' @param attributes: the list of attributes that we wish to have in the data.frame (default:"learner","language","fit_", "unpenalized_fit_", "auc_", "accuracy_", "cor_", "aic_", "intercept_", "eval.sparsity", "sign_","precision_", "recall_","f1_")
#' @return an data frame with attributes for each model
#' @export
populationToDataFrame_mc <- function(
    pop,
    attributes = c("learner","language","fit_", "unpenalized_fit_",
                   "auc_", "accuracy_", "cor_", "aic_", "intercept_",
                   "eval.sparsity", "sign_","precision_", "recall_","f1_")
){
  list_pop <- list()
  list_df <- list()

  # Loop for each sub-model in the first population
  for (j in 1:length(pop[[1]]$names_)) {
    list_mod <- list()

    # Loop for each model in the population
    for (i in 1:length(pop)) {
      list_mod[[i]] <- list()

      # Assigning ‘mod’ elements to sub-models
      list_mod[[i]]$learner <- pop[[i]]$learner
      list_mod[[i]]$language <- pop[[i]]$language
      list_mod[[i]]$objective <- pop[[i]]$objective
      list_mod[[i]]$indices_ <- pop[[i]]$indices_[[j]]
      list_mod[[i]]$names_ <- pop[[i]]$names_[[j]]
      list_mod[[i]]$coeffs_ <- pop[[i]]$coeffs_[[j]]
      list_mod[[i]]$fit_ <- pop[[i]]$fit_
      list_mod[[i]]$unpenalized_fit_ <- pop[[i]]$unpenalized_fit_
      list_mod[[i]]$auc_ <- pop[[i]]$auc_
      list_mod[[i]]$accuracy_ <- pop[[i]]$accuracy_
      list_mod[[i]]$cor_ <- pop[[i]]$cor_
      list_mod[[i]]$aic_ <- pop[[i]]$aic_
      list_mod[[i]]$intercept_ <- pop[[i]]$list_intercept_[[j]]
      list_mod[[i]]$eval.sparsity <- pop[[i]]$eval.sparsity
      list_mod[[i]]$precision_ <- pop[[i]]$precision_
      list_mod[[i]]$recall_ <- pop[[i]]$recall_
      list_mod[[i]]$f1_ <- pop[[i]]$f1_
      list_mod[[i]]$sign_ <- pop[[i]]$sign_[[j]]
      list_mod[[i]]$rsq_ <- pop[[i]]$rsq_[[j]]
      list_mod[[i]]$ser_ <- pop[[i]]$ser_[[j]]
      list_mod[[i]]$score_ <- pop[[i]]$score_[[j]]
      list_mod[[i]]$mda.cv_ <- pop[[i]]$mda.cv_[[j]]
      list_mod[[i]]$prev.cv_ <- pop[[i]]$prev.cv_[[j]]
      list_mod[[i]]$mda_ <- pop[[i]]$mda_[[j]]
    }

    # Add the list of sub-models to the list of populations
    list_pop[[j]] <- list_mod
  }

  # Transform each sub-list into a dataframe
  for (i in 1:length(list_pop)) {
    # Call populationToDataFrame on each sub-model
    list_df[[i]] <- populationToDataFrame(pop = list_pop[[i]],  attributes = attributes)
  }

  return(list_df)
}


#' Generate Class Combinations for Multiclass Classification
#'
#' This function generates combinations of classes and their corresponding data for binary classification approaches such as One-vs-One (OVO) and One-vs-All (OVA).
#'
#' @param y A numeric or character vector representing the dependent variable with multiple classes.
#' @param X A matrix or data frame where each column corresponds to the predictors associated with the values in `y`.
#' @param approch A character string specifying the approach to use: "ovo" for One-vs-One or "ova" for One-vs-All.
#' @return A list containing:
#' @export
generate_combinations_with_factors <- function(y, X, approch) {
  # Convert y to a factor to extract its levels
  y_factor <- as.factor(y)
  levels_classes <- levels(y_factor) # Extract the levels of the factor

  list_y <- list()
  list_X <- list()

  # Decomposition phase
  if (approch == "ovo") {
    k <- 1
    for (i in 1:(length(levels_classes) - 1)) {
      for (j in (i + 1):length(levels_classes)) {
        class_i <- levels_classes[i]
        class_j <- levels_classes[j]

        # Select samples for classes i and j
        indices <- which(y_factor %in% c(class_i, class_j))
        y_pair <- y_factor[indices]
        X_pair <- X[, indices]

        # Store the decomposed data
        list_y[[k]] <- as.vector(y_pair)
        list_X[[k]] <- X_pair
        k <- k + 1
      }
    }
  } else if (approch == "ova") {
    for (i in 1:length(levels_classes)) {
      class_i <- levels_classes[i]

      # Transform labels for OVA
      y_temp <- ifelse(y_factor == class_i, as.character(class_i), "All")
      list_y[[i]] <- as.vector(y_temp)
      list_X[[i]] <- X
    }
  }

  # Return the combinations
  return(list(list_y = list_y, list_X = list_X))
}


#' Decompose Class Levels for Pairwise Combinations
#'
#' This function generates pairwise combinations of class levels from a dependent variable `y` and sorts the samples within each pair.
#' @param y A numeric or character vector representing the dependent variable with multiple classes.
#' @return A list where each element contains the sorted samples for a specific pair of class levels.
#' @export
decompose_y_levels <- function(y) {
  # Convert y to a factor to extract its levels
  y_factor <- as.factor(y)
  levels_classes <- levels(y_factor) # Extract the unique levels

  # Initialize a list to store combinations
  list_y <- list()
  k <- 1

  for (i in 1:(length(levels_classes) - 1)) {
    for (j in (i + 1):length(levels_classes)) {
      class_i <- levels_classes[i]
      class_j <- levels_classes[j]

      # Select indices for classes i and j
      indices <- which(y_factor == class_i | y_factor == class_j)

      # Extract samples for the current pair
      y_pair <- y_factor[indices]

      # Sort samples for each pair
      y_pair_sorted <- sort(as.vector(y_pair))

      # Store the sorted combinations
      list_y[[k]] <- y_pair_sorted
      k <- k + 1
    }
  }

  # Return the list of combinations
  return(list_y)
}




#' Sort Data by Dependent Variable
#' This function sorts a dependent variable `y` and its corresponding predictors `X` based on the ascending order of `y` values.
#' @param y A numeric vector representing the dependent variable.
#' @param X A matrix or data frame where each column corresponds to the predictors associated with the values in `y`.
#' @return A list containing:
#' @export
sort_data <- function(y, X) {
  # Sorts the indices based on the values of y
  indices_sorted <- order(y)

  # Sorts y using the indices
  y_sorted <- y[indices_sorted]

  # Sorts X according to the indices
  X_sorted <- X[, indices_sorted]

  # Returns the sorted y and the sorted dimensions of X
  return(list(y = y_sorted, X = X_sorted))
}






#' Merge a list of cross validation scores form digest results
#'
#' @import reshape2
#' @import ggplot2
#' @title mergeMeltImportanceCV_mc
#' @description mergeMeltImportanceCV_mc returns a list containing lists of data frames for each class, with the feature importance for the different learners, without any consideration for sparsity.
#' @param list.results.digest: a list of digest objects one for each learner used. For example, list(res.terda.digest, res.terga.digest, res.terbeam.digest)
#' @param filter.cv.prev: filter variable for each learner based on the appearence prevalence in the cross validation.
#' @param min.kfold.nb: wether we should restrict all experiments in the smallest number of k-folds of a comparative analyses (default = FALSE)
#' @param type: the type of importance "mda (mean decreased accuracy)" or "pda (prevalence decreased accuracy)" (default = mda)
#' @param learner.grep.pattern: select a subset of learners using a grep pattern (default:"*")
#' @param nb.top.features: the number of top features to focus on the plot
#' @param feature.selection: the names of the features to be selected (default:NULL)
#' @param fixed.order: if the order of features in the plot should follow the feature selection one (default = FALSE)
#' @param scaled.importance: the scaled importance is the importance multipied by the prevalence in the folds. If (default = TRUE) this will be used, the mean mda
#' will be scaled by the prevalence of the feature in the folds and ordered subsequently
#' @param make.plot: make a plot for all the learners
#' @param main: should add the title to the graph for correct alignment (default:FALSE)
#' @param cv.prevalence: wether or not to plot the distribution of the prevalence of the feature in the top-models for each k-fold in the graph (default:FALSE)
#' @return a list of several data.frames and a ggplot object
#' @export
mergeMeltImportanceCV_mc <- function(list.results, filter.cv.prev = 0.5, min.kfold.nb = FALSE,
                                     type = "mda",
                                     learner.grep.pattern = "*",
                                     nb.top.features = 25, feature.selection = NULL, fixed.order = FALSE,
                                     scaled.importance = TRUE,
                                     make.plot = TRUE, main = FALSE,
                                     cv.prevalence = TRUE)
{

  list_fa <- list()
  list_fa = feature.selection
  feature.selection = NULL
  list_res <- list()
  list_X <- list()
  list_y <- list()
  list_X = list.results$terBeam$classifier$data$X
  list_y = list.results$terBeam$classifier$data$y
  for (numb in seq_along(list.results$terBeam$classifier$coeffs_)){

    fa = list_fa[[numb]]
    feature.selection = rownames(fa$pop.noz)

    # sanity checks
    valid.efip <- unlist(lapply(list.results, function(x){!is.null(x$crossVal$fip[[1]])}))
    if(!any(valid.efip))
    {
      warning(paste("mergeMeltImportanceCV: Some learners does not have feature importance data!",paste(names(valid.efip)[valid.efip], collapse = ", ")))
    }

    if(length(which(!valid.efip)) == length(list.results))
    {
      warning("mergeMeltImportanceCV: stopping since thre were no data on feature importance for none of the learners")
      return(NULL)
    }

    if(filter.cv.prev < 0 | filter.cv.prev > 1)
    {
      stop("mergeMeltImportanceCV: please provide a valid filter prevalence value percentage [0, 1]")
    }

    # restrict to the valid efip learners
    list.results      <- list.results[valid.efip]

    e.nb              <- length(list.results) # number of classifiers
    e.name            <- names(list.results) # names of the classifiers

    # get the number of k-folds for each experiment, which can be differnt from one to another.
    nbfolds           <- rep(NA, e.nb)
    names(nbfolds)    <- e.name
    X =
      for(i in 1:e.nb)
      {
        nbfolds[i]      <- ncol(list.results[[i]]$crossVal$scores$empirical.auc)
      }

    inds.folds        <- NULL
    if(min(nbfolds) != max(nbfolds) & min.kfold.nb)
    {
      inds.folds <- list()
      for(i in 1:e.nb)
      {
        set.seed(1234)
        inds.folds[[i]]           <- sample(x = seq_len(nbfolds[i]), size = min(nbfolds), replace = FALSE)
      }
    }else
    { # else just get the increasing index
      inds.folds      <- list()
      for(i in 1:e.nb)
      {
        inds.folds[[i]]           <- seq_len(nbfolds[i])
      }
    }

    # build a list with results on each classifier.
    res.split         <- list()
    res.split.fprev   <- list()
    for(i in 1:e.nb) # for each classifier
    {
      if(!is.null(feature.selection))
      {
        ind <- match(feature.selection, rownames(list.results[[i]]$crossVal$fip$mda))
        if(any(is.na(ind)))
        {
          print("mergeMeltImportanceCV: the following features  are not found in the importance data... skiping")
          print(feature.selection[is.na(ind)])
          if(sum(is.na(ind)) == length(feature.selection))
          {
            warning(paste("mergeMeltBestScoreCV: no feature found, returning NULL"))
            return(NULL)
          }
          # features <- rownames(list.results[[i]]$crossVal$fip$mda[features, inds.folds[[i]]])
          features <- feature.selection[!is.na(ind)]
        }else
        {
          features <- feature.selection
        }
      }else
      {
        features <- rownames(list.results[[i]]$crossVal$fip$mda)
      }

      # for each classifier
      if(type == "mda")
      {
        imp.data                    <- list.results[[i]]$crossVal$fip$mda[features, inds.folds[[i]]]
      }

      if(type == "pda")
      {
        imp.data                    <- list.results[[i]]$crossVal$fip$pda[features, inds.folds[[i]]]
      }

      if(type != "pda" & type != "mda")
      {
        stop("mergeMeltImportanceCV: please provide a valid type: either pda or mda")
      }

      # filter by feature prevalence in the crossval final populations
      imp.data.prev               <- rowSums(!is.na(imp.data)) / ncol(imp.data)
      imp.data.prev               <- imp.data.prev[order(imp.data.prev, decreasing = TRUE)]
      # filter to 50 % of the crossval
      if(any(imp.data.prev/ncol(imp.data) > filter.cv.prev))
      {
        imp.data.prev             <- imp.data.prev[imp.data.prev / ncol(imp.data) >= filter.cv.prev]
      }else{
        print("mergeMeltImportanceCV: filter.cv.prev too strong, canceling the filtering step...")
      }

      imp.data                    <- imp.data[names(imp.data.prev),]
      colnames(imp.data)          <- paste("fold_", seq_len(ncol(imp.data)), sep="")

      # scale the mda by mutiplying each value of by the prevalence in the folds
      imp.data.sc <- imp.data * imp.data.prev

      if(scaled.importance)
      {
        # use the scaled importance
        imp.data <- imp.data.sc
      }

      imp.data.melt <- melt(data.frame(feature = rownames(imp.data), imp.data), measure.vars = colnames(imp.data))
      colnames(imp.data.melt)     <- c("feature","folds","value")
      res.split[[i]]              <- data.frame(imp.data.melt)
      res.split.fprev[[i]]        <- imp.data.prev
    }
    names(res.split)              <- e.name
    names(res.split.fprev)        <- e.name


    if(all(unlist(lapply(res.split, nrow)) == 0))
    {
      warning(paste("mergeMeltBestScoreCV: all the results are empty, returning NULL"))
      return(NULL)
    }

    if(any(unlist(lapply(res.split, nrow)) == 0))
    {
      warning(paste("mergeMeltBestScoreCV: some results are empty, omitting them"))
      res.split                   <- res.split[-which(unlist(lapply(res.split, nrow)) == 0)]
    }

    # melt the list together
    if(length(grep(learner.grep.pattern, names(res.split))) == 0)
    {
      warning(paste("mergeMeltBestScoreCV: pattern",learner.grep.pattern,"did not find any results"))
      return(NULL)
    }

    # PREVALENCE data.
    # Similarly melt the data on prevalence on the folds, which will be added to the same plot
    # select a subset of learners based on pattern and melt them together
    if(length(list.results) == 1)
    {
      fprev.melt                    <- melt(res.split.fprev[grep(learner.grep.pattern, names(res.split.fprev))], level="feature")
      fprev.melt                    <- data.frame(rownames(fprev.melt), fprev.melt)
      # rename melted data frame
      colnames(fprev.melt)          <- c("feature","value","method")
      fprev.melt$method             <- factor(fprev.melt$method, levels = names(list.results))
      # Transform in percentage
      fprev.melt$value              <- fprev.melt$value * 100

    }else
    {
      res.split.fprev               <- lapply(res.split.fprev, as.data.frame)
      for(i in 1:length(res.split.fprev))
      {
        res.split.fprev[[i]]        <- data.frame(feature = rownames(res.split.fprev[[i]]), res.split.fprev[[i]])
      }
      fprev.melt                    <- melt(res.split.fprev[grep(learner.grep.pattern, names(res.split.fprev))], level="feature")
      fprev.melt                    <- fprev.melt[,-2]
      # rename melted data frame
      colnames(fprev.melt)          <- c("feature","value","method")
      fprev.melt$method             <- factor(fprev.melt$method, levels = names(list.results))
      # Transform in percentage
      fprev.melt$value              <- fprev.melt$value * 100
    }

    # IMPORTANCE data
    # select a subset of learners based on pattern and melt them together
    cv.res.melt                   <- melt(res.split[grep(learner.grep.pattern, names(res.split))], level="feature")
    # rename melted data frame
    colnames(cv.res.melt)         <- c("feature","folds","variable","value","method")
    # omit a column that is not needed
    cv.res.melt                   <- cv.res.melt[,-3]
    # reset the factor order
    cv.res.melt$method            <- factor(cv.res.melt$method, levels = names(list.results))
    # Transform in percentage
    cv.res.melt$value             <- cv.res.melt$value * 100
    # summarize by feature and method to prepare for the graphic
    cv.res.summary                <- summarySE(data=cv.res.melt, measurevar="value", groupvars=c("feature","method"), na.rm = TRUE)
    # reorder features by value
    cv.res.summary.ord            <- summarySE(data=cv.res.melt, measurevar="value", groupvars=c("feature"), na.rm = TRUE)

    # Set the order of the features in the plot
    if(fixed.order)
    {
      # get the order by feature.selection
      lev.ord                       <- match(features, cv.res.summary.ord$feature)
      names(lev.ord)                <- features
    }else
    {
      # get the order by mean importance
      lev.ord                       <- order(cv.res.summary.ord$value, decreasing = TRUE)
      names(lev.ord)                <- as.character(cv.res.summary.ord$feature)
    }

    # get the names of the top features
    if(is.null(nb.top.features))
    {
      nb.top.features <- NA
    }

    # if the feature mask is specified than we can not select a subset based on number
    if(!is.null(feature.selection) & !is.null(nb.top.features))
    {
      nb.top.features <- NA
    }

    features.tokeep               <- as.character(cv.res.summary.ord$feature[lev.ord])[1:min(nb.top.features, length(lev.ord), na.rm = TRUE)]
    # restrict data to these top features
    cv.res.summary                <- cv.res.summary[as.character(cv.res.summary$feature) %in% features.tokeep, ]
    # reorder the factor by the top
    cv.res.summary$feature        <- factor(cv.res.summary$feature, levels = rev(features.tokeep))
    features_present <- rownames(list_X[[numb]])
    features_required <- as.character(cv.res.summary$feature)

    # Identifie les fonctionnalités disponibles et manquantes
    available_features <- intersect(features_required, features_present)
    missing_features <- setdiff(features_required, features_present)

    if (length(available_features) > 0) {
      # Extraction des données disponibles pour les fonctionnalités présentes
      X_subset <- list_X[[numb]][available_features, ]

      # Complète avec NA si des fonctionnalités manquent
      if (length(missing_features) > 0) {
        # Ajouter les fonctionnalités manquantes avec NA
        missing_rows <- matrix(NA, nrow = length(missing_features), ncol = ncol(list_X[[numb]]))
        rownames(missing_rows) <- missing_features
        colnames(missing_rows) <- colnames(list_X[[numb]])
        X_subset <- rbind(X_subset, missing_rows)
      }

      # Réorganise les lignes pour correspondre à l'ordre des fonctionnalités requises
      X_ordered <- X_subset[features_required, ]

      # Calcul des signes avec getSign
      cv.res.summary$sign <- as.character(getSign(X = X_ordered, y = list_y[[numb]]))

    } else {
      print(paste("Aucune fonctionnalité disponible pour numb =", numb))
    }

    # same for prevalence
    # restrict data to these top features
    fprev.melt                    <- fprev.melt[as.character(fprev.melt$feature) %in% features.tokeep, ]
    # reorder the factor by the top
    fprev.melt$feature            <- factor(fprev.melt$feature, levels = rev(features.tokeep))

    res <- list()
    res$split <- res.split
    res$melt <- cv.res.melt
    res$summary <- cv.res.summary
    res$fprev <- fprev.melt

    if(make.plot)
    {
      order_cbn = order(unique(list_y[[numb]]))
      cbn = unique(list_y[[numb]])
      cbn = cbn[order_cbn]

      pd <- position_dodge(0.3) # move them .05 to the left and right

      # fix color space
      col <- c("deepskyblue1","firebrick1")
      col.n <- c("-1","1")
      tab.v <- table(cv.res.summary$sign)
      if(length(tab.v) < 2) col <- col[col.n %in% names(tab.v)]

      if(cv.prevalence)
      {
        g <- ggplot(cv.res.summary, aes(x=feature, y=value)) +
          # the prevalence data on the bottom
          geom_bar(data = fprev.melt, stat="identity", position="identity", aes(fill = "1")) +
          geom_hline(yintercept = min(0, cv.res.summary$value, na.rm = TRUE), col="gray") +
          ylab("Feature importance & prevalence (CV)") +
          xlab("") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          theme_bw() +
          coord_flip() +
          scale_fill_manual("Dataset", values = c("gray90","gray90")) +
          scale_color_manual("Dataset", values = col) +
          # the importance data
          geom_errorbar(aes(ymin = value - se, ymax = value + se, color = sign), width=.1, position=pd) +
          #geom_line(aes(group = feature, color = sign), position=pd) +
          geom_point(position = pd, size=2, shape=19, aes(color = sign)) + # 21 is filled circle
          guides(colour = "none", fill = "none")
      }else
      {
        g <- ggplot(cv.res.summary, aes(x=feature, y=value, color = sign)) +
          # the prevalence data on the bottom
          #geom_bar(data = fprev.melt, stat="identity", position="identity", aes(fill = "1", color = "1")) +
          geom_hline(yintercept = min(0, cv.res.summary$value, na.rm = TRUE), col="gray") +
          # the importance data
          geom_errorbar(aes(ymin = value - se, ymax = value + se), width=.1, position=pd) +
          #geom_line(aes(group = feature, color = "1"), position=pd) +
          geom_point(position = pd, size=2, shape=19) + # 21 is filled circle
          ylab("Feature importance") +
          xlab("") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
          theme_bw() +
          coord_flip() +
          scale_color_manual("Dataset", values = col) +
          guides(colour = "none", fill = "none")
      }
      # if only one learner
      if(length(list.results) == 1)
      {
        # add or not the title
        if(!main)
        {
          if(type == "mda")
          {
            g <- g + ggtitle(paste("mda|cv", cbn[1], " = Blue;", cbn[2], " = Red"))
          }
          if(type == "pda")
          {
            g <- g + ggtitle(paste("pda|cv", cbn[1], " = Blue;", cbn[2], " = Red"))
          }
        }
      }else
      {
        g <- g + facet_grid(. ~ method, scales = "free")
      }


      res$g                         <- g
    }

    list_res[[numb]] <- res
  }

  # Extraire les plots depuis les objets dans list_res
  plots <- lapply(list_res, function(x) x$g)
  # Vérifier que tous les éléments sont des objets ggplot
  if (all(sapply(plots, function(p) inherits(p, "ggplot")))) {
    # Combiner tous les plots en une seule grille
    do.call(grid.arrange, c(plots, ncol = 4)) # Ajustez ncol selon vos besoins
  } else {
    print("Certains éléments dans list_res[[i]]$g ne sont pas des objets ggplot.")
  }
  return(list_res)
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


