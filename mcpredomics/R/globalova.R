#' Computes the predected classification using a given model.one versus one.
#'
#' @description This function evaluates the predicted classification either using (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign. one versus all
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param score: the score passed directly
#' @param intercept: the intercept passed directly
#' @param sign: the sign passed directly
#' @return a list vector with the predicted classification of the samples
evaluateYhat_ova <- function(mod = NULL, X, y, clf, score=NULL, intercept=NULL, sign=NULL)
{
  unique_y <- unique(y)
  yhat_list <- list()
  ova <- list()
  #list of y one versus all
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova
  }
  for(i in 1:length(ova)){
    yha <- evaluateYhat(mod = NULL, X, y=ova[[i]], clf, score=NULL, intercept=NULL, sign=NULL)
    yhat_list[[i]] = yha
  }
  yhat = yhat_list

  return(yhat)
}


#' Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function
#'
#' @description Evaluates the sign for a given feature this is the old getMgsVsTraitSignDiscr function.
#' @import foreach
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param parallel.local: weather or not to run in //
#' @return a list of +1 & -1 for each one-vs-all classification of our dataset
#' @export
getSign_ova <- function(X, y, clf = NULL, parallel.local = FALSE)
{
  unique_y <- unique(y)
  list_Sign <- list()
  ova <- list()
  #list of y one versus all
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova
  }
  for(i in 1:length(ova)){
    Sign <- getSign(X, y=ova[[i]], clf = NULL, parallel.local = FALSE)
    list_Sign[[i]] = Sign
  }
  res <- list_Sign

  return(res)
}



' One Versus All, Computes the AUC of a model
#'
#' @description One Versus All, Computes the AUC of a model
#' @param score: the ^y score of the model
#' @param y: the response vector
#' @param sign: in which direction to make the comparison? "auto" (default): automatically define in which group
#' the median is higher and take the direction accordingly. ">": if the predictor values for the control group
#' are higher than the values of the case group (controls > t >= cases). "<": if the predictor values for the
#' control group are lower or equal than the values of the case group (controls < t <= cases).
#' @return a list auc value
#' @importFrom pROC roc
evaluateAUC_ova <- function(score, y, sign = '>')
{
  list_auc = list() #vector one versus all of auc
  unique_y <- unique(y)
  ova <- list() #
  # list of y according to the number of classes.
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova

  }
  # list values auc_ one versus all
  for(i in 1:length(ova)){
    au <- evaluateAUC(score, y = ova[[i]], sign = '>')
    list_auc[[i]] = au
  }
  auc <- list_auc
  return(auc)
}

#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @return a list model object with the fitting score
evaluateIntercept_ova <- function(mod, X, y, clf)
{
  list_Intercept = list() #vector one versus all of auc
  unique_y <- unique(y)
  ova <- list() #
  # list of y according to the number of classes.
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova

  }

  # list evaluate intercept one versus all
  for(i in 1:length(ova)){
    eva <- evaluateIntercept(mod, X, y=ova[[i]], clf)
    list_Intercept[[i]] = eva
  }

  mod <- list_Intercept
  return(mod)
}


#' @description Evaluates an entire population of models, that be predomics objects or individuals
#'
#' @import foreach
#' @title evaluatePopulation_ova
#' @name evaluatePopulation_ova
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
#' @return an list individual object
#' @export
evaluatePopulation_ova <- function(X, y, clf, pop, eval.all = FALSE,
                                   force.re.evaluation = FALSE,
                                   estim.feat.importance = FALSE,
                                   mode = "train",
                                   delete.null.models = TRUE,
                                   lfolds = NULL)
{

  list_population = list() #list population  one versus all of auc
  unique_y <- unique(y)
  ova <- list() #
  # list of y according to the number of classes.
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova

  }

  # list evaluate population one versus all
  for(i in 1:length(ova)){
    evaPop <- evaluatePopulation(X, y=ova[[i]], clf, pop, eval.all = FALSE,
                                 force.re.evaluation = FALSE,
                                 estim.feat.importance = FALSE,
                                 mode = "train",
                                 delete.null.models = TRUE,
                                 lfolds = NULL)
    list_population[[i]] = evaPop
  }
  res <- list_population

  return(res)
}


#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod: a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param eval.all: should the function evaluate all the scores (default:FALSE)
#' @param force.re.evaluation: re-evaluate all the scores even if they exist (default:FALSE)
#' @param estim.feat.importance: evaluate the importance in the model object (default:FALSE)
#' @param mode: A choice from c("train", "test") indicates wether we wish to learn the threthold
#' of the model (default:"train") or not "test" for the c("terinter","bininter","ratio") languages
#' @return a list model object with the fitting scores evaluated
#' @export
evaluateModel_ova <- function(mod, X, y, clf, eval.all = FALSE, force.re.evaluation = FALSE, estim.feat.importance = FALSE, mode = 'train')
{
  list_model = list() #list population  one versus all of auc
  unique_y <- unique(y)
  ova <- list() #
  # list of y according to the number of classes.
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova

  }

  # list evaluate model one versus all
  for(i in 1:length(ova)){
    evamodel <- evaluateModel(mod, X, y=ova[[i]], clf, eval.all = FALSE, force.re.evaluation = FALSE, estim.feat.importance = FALSE, mode = 'train')
    list_model[[i]] = evamodel
  }
  mod.res <- list_model
  return(mod.res)
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
#' @return a list model object with the fitting score
evaluateFit_ova <- function(mod, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{
  list_Fit = list() #list population  one versus all of auc
  unique_y <- unique(y)
  ova <- list() #
  # list of y according to the number of classes.
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova

  }

  # list evaluate fit one versus all
  for(i in 1:length(ova)){
    evafit <-  evaluateFit(mod, X, y=ova[[i]], clf, force.re.evaluation = FALSE, mode = "train")
    list_Fit[[i]] = evafit
  }
  mod <- list_Fit

  return(mod)
}

#' Selects a the top k features that are significantly associated with the class to predict
#'
#' @description Runs statistics on the data and selects a subset of k features that are the most significant.
#' An accelerated version is implemented based on the BioQC package for the Mann-Whittney tests. Besides filtering
#' this function can be used in a more larger statistical context.
#' @import BioQC
#' @param data: the dataset X
#' @param trait: is the equivalent of y (class, or numerical)
#' @param k: the number of features (default:10)
#' @param type: the statistics to be run (default:wilcoxon)
#' @param restrict: Run the statistics in a subset of the dataset (default: a vector of all TRUE)
#' @param multiple.adjust: the multiple testing adjustment method (default:BH)
#' @param paired: wether paired statistics should be run (default:FALSE)
#' @param sort: return variables sorted by p-value significance (default:TRUE)
#' @param verbose: print out information indicating progress (default:FALSE)
#' @param verbose.step: Showing a 1 percent progress.
#' @param return.data: if (default: FALSE) this returns a list of the statistics of X of each approach one against all, else the restricted data subset
#' @param accelarate: use a turbo method developped by bioQC (default:FALSE). There is an issue when executing in batch.
#' @export
filterfeaturesK_ova <- function(data,
                                trait,
                                k = 10,
                                type = "wilcoxon",
                                restrict = rep(TRUE, ncol(data)),
                                multiple.adjust = "BH",
                                paired = FALSE,
                                sort = TRUE,
                                verbose = FALSE,
                                verbose.step = NULL,
                                return.data = FALSE,
                                accelerate = FALSE
)
{
  unique_y <- unique(y)
  list_res <- list()
  ova <- list()
  nClass <- length(unique_y)
  #list of y one versus all
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova
  }

  for(i in 1:length(ova)){
    res     <- filterfeaturesK(data = X,
                               trait = ova[[i]],
                               k = max.nb.features,
                               type = "wilcoxon",
                               sort = TRUE,
                               verbose = clf$params$verbose,
                               return.data = FALSE) # to avoid having to recompute this all the time

    list_res[[i]] = res
  }
  res <- list_res

  return(res)
}



#' One Versus All, Evaluates the accuracy of a model one versus all
#'
#' @description One Versus All, This function evaluates the accuracy of either (1) a model object one versus all that contains intercept and sign or (2) directly the attributes score, intercept, sign
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param force.re.evaluation: evaluate again all the elements needed for accuracy (default:FALSE)
#' @param mode: training or test mode. If training, the funciton maximizes accuracy.
#' @return either (1) a model whose evaluation parameters are updated or (2) the accuracy
#' @export
evaluateAccuracy_ova <- function(mod = NULL, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{
  unique_y <- unique(y)
  list_mod_accuracy <- list()
  ova <- list()
  #list of y one versus all
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova
  }
  #evaluate accuracy each model one versus all
  for(i in 1:length(ova)){
    mod_ <- evaluateAccuracy(mod = NULL, X, y=ova[[i]], clf, force.re.evaluation = FALSE, mode = "train")
    list_mod_accuracy[[i]] = mod_
  }

  mod <- list_mod_accuracy

  return(mod)
}


#' One Versus All, Compute other prediction scores such as precision, recall and f-score a model
#'
#' @description One Versus All, This function computes prediction scores based on the confusion matrix such as accuracy, precision, recall and f-score a model one versus all
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param mode: training or testing mode
#' @return a model whose evaluation parameters are updated
evaluateAdditionnalMetrics_ova <- function(mod, X, y, clf, mode = "train")
{
  unique_y <- unique(y)
  list_add_metrics <- list()
  ova <- list()
  #list of y one versus all
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova
  }

  #evaluate additional metrics  each model one versus all
  for(i in 1:length(ova)){
    mod_ <- evaluateAdditionnalMetrics_ova(mod, X, y=ova[[i]], clf, mode = "train")
    list_add_metrics[[i]] = mod_
  }
  mod <- list_add_metrics
  return(mod)
}


#' evaluates the feature importance in a population of models one versus all
#'
#' @description One Versus All, This function perturbes the dataset by shuffling one at a time a subset of features that appear in a population of models
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
evaluateFeatureImportanceInPopulation_ova <- function(pop, X, y, clf, score = "fit_", filter.ci = TRUE, method = "optimized", seed = c(1:10), aggregation = "mean", verbose = TRUE)
{
  unique_y <- unique(y)
  list_res <- list()
  ova <- list()
  #list of y one versus all
  for(i in 1: length(unique_y)){
    y_ova <- ifelse(y== unique_y[i], 1,-1)
    ova[[i]] <- y_ova
  }
  #evaluate feature importance in population each model one versus all
  for(i in 1:length(ova)){
    res_ <- evaluateFeatureImportanceInPopulation(pop=pop, X, y=ova[[i]], clf, score = "fit_",
                                                  filter.ci = TRUE, method = "optimized",
                                                  seed = c(1:10), aggregation = "mean",
                                                  verbose = TRUE)

    list_res[[i]] = res_
  }

  res <- list_res

  return(res)
}
