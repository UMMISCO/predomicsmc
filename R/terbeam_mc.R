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
# @script: terbeam_mc.R
# @author: Fabien Kambu Mbuangi
# @author: Edi Prifti
# @date: November 2023
################################################################


#' terbeam: ternary beam searching algorithm
#'
#' @title terbeam_ova
#' @description one versus one terbeam is a model search algorithm on a beam search approach.
#' @param sparsity: number of features in a given model. This is a vector with multiple lengths.
#' @param maxNbOfModels: number of models to be explored for a given k_sparsity. This is equivalent to a population size in terga.
#' @param nbVeryBest: is the number of features to be kept that appear in the very best models. They will be kept even if they are not frequent in the best models (default: 1 percent of maxNbOfModels).
#' @param nbBest: is the number of features that will be used to build the k+1 sparsity combinations (default: 10 percent of maxNbOfModels).
#' @param final.pop.perc: a percentage of nbVeryBest translates in a number of models to be kept for k_sparsity.
#' @param popSaveFile: (??)
#' @param saveFiles: ??
#' @param language is the language that is used by the different algorithms {bin, bininter, ter, terinter, ratio}, (default:"terinter")
#' @param scoreFormula: a Function that contains the ratio Formula or other specific ones
#' @param epsilon: a small value to be used with the ratio language (useCustomLanguage) (default: NULL). When null it is going to be calculated by the minimum value of X divided by 10.
#' @param objective: this can be auc, cor or aic. Terga can also predict regression, other than class prediction. (default:auc)
#' @param max.nb.features: focuses only on the subset of top most significant features (default:1000)
#' @param estimate_coefs: non ternary solution for the aic objective (default:FALSE)
#' @param evalToFit: The model performance attribute to use as fitting score (default:"fit_"). Other choices are c("auc_","accuracy_","precision_","recall_","f_score_")
#' @param k_penalty: Penalization of the fit by the k_sparsity (default: 0)
#' @param intercept: (??) (default:NULL)
#' @param testAllSigns: ??
#' @param plot: Plot different graphics (default:FALSE).
#' @param verbose: print out information on the progress of the algorithm (default:TRUE)
#' @param warnings: Print out warnings when runnig (default:FALSE).
#' @param debug: print debug information (default:FALSE)
#' @param print_ind_method: One of c("short","graphical") indicates how to print a model and subsequently a population during the run (default:"short").
#' @param nCores: the number of cores to execute the program. If nCores=1 than the program runs in a non parallel mode
#' @param parallelize.folds: parallelize folds when cross-validating (default:TRUE)
#' @param seed: the seed to be used for reproductibility. If seed=NULL than it is not taken into account (default:NULL).
#### TODO check
#' @param experiment.id: The id of the experiment that is to be used in the plots and comparitive analyses (default is the learner's name, when not specified)
#' @param experiment.description: A longer description of the experiment. This is important when many experiments are run and can also be printed in by the printExperiment function.
#' @param experiment.save: Data from an experiment can be saved with different levels of completness, with options to be selected from c("nothing", "minimal", "full"), default is "minimal"
#' @param parallel: parallel
#' @return an object containing a list of parameters for this classifier
#' @export
terBeam_mc <- function(sparsity = 1:5, max.nb.features = 1000,
                       # maxNbOfModels corresponds to the width of the beam search in terms of models
                       maxNbOfModels = 10000, # population size
                       # nbBest is the number of models to keep such that the most frequent feature in the best models are kept
                       nbBest = round(maxNbOfModels/10),
                       # nbVeryBest
                       nbVeryBest = round(maxNbOfModels/100),
                       final.pop.perc = 100,  # percentage of models in the returned results
                       # population options
                       popSaveFile = "NULL", saveFiles = FALSE,
                       # language in {bin, bininter, ter, terinter, ratio}
                       language = "terinter",
                       # language options
                       scoreFormula=scoreRatio, epsilon = "NULL",
                       # evaluation options
                       objective = "auc", k_penalty=0, evalToFit = 'auc_', estimate_coefs=FALSE, intercept = "NULL", testAllSigns = FALSE,
                       # output options
                       plot = FALSE, verbose = TRUE, warnings = FALSE, debug = FALSE, print_ind_method = "short", parallelize.folds = TRUE,
                       # computing options
                       nCores = 4, seed = "NULL", #maxTime = Inf,
                       # experiment options
                       experiment.id = "NULL", experiment.description = "NULL", experiment.save = "nothing")
{
  # standard means that we use the standard heuristics
  clf <- list()
  clf$learner <- "terBeam_mc"
  clf$params  <- list()
  clf$experiment <- list()                        # information about the experiment
  clf$params$objective          <- objective
  clf$params$estimate_coefs     <- estimate_coefs
  clf$params$sparsity           <- sparsity
  clf$params$max.nb.features    <- max.nb.features
  #  clf$params$maxBeam           <- maxBeam
  #  clf$params$FILENAME          <- FILENAME
  #  clf$params$PREFIX            <- PREFIX
  clf$params$saveFiles          <- saveFiles      # It would be interesting to add this in the future
  #  clf$params$pathSave          <- pathSave

  #  clf$params$size_pop          <- size_pop
  clf$params$maxNbOfModels      <- maxNbOfModels
  clf$params$nbBest             <- nbBest
  clf$params$nbVeryBest         <- nbVeryBest

  # print out intermediary results
  clf$params$plot               <- plot           # print out logs.
  clf$params$verbose            <- verbose        # print out logs.
  clf$params$warnings           <- warnings       # print out warnings
  clf$params$debug              <- debug          # print out debugging information.
  clf$params$print_ind_method   <- print_ind_method # method to print individual

  # Computing options
  clf$params$nCores             <- nCores         # parallel computing
  clf$params$parallel           <- nCores > 1     # parallel computing
  clf$params$parallelize.folds  <- parallelize.folds
  clf$params$parallel.local     <- FALSE
  clf$params$seed               <- seed           # fix the seed to be able to reproduce results

  clf$params$testAllSigns       <- testAllSigns
  clf$params$final.pop.perc     <- final.pop.perc
  clf$params$evalToFit          <- evalToFit
  clf$params$k_penalty          <- k_penalty

  clf$params$language           <- language
  clf$params$popSaveFile        <- popSaveFile
  clf$params$epsilon            <- epsilon
  clf$params$scoreFormula       <- scoreFormula
  clf$params$intercept          <- intercept

  # Experiment information
  if(!(experiment.id=="NULL"))
  {
    clf$experiment$id           <- experiment.id
  }else
  {
    clf$experiment$id           <- clf$learner
  }

  if(!(experiment.description=="NULL"))
  {
    clf$experiment$description  <- experiment.description
  } else
  {
    clf$experiment$description  <- paste(clf$learner, date() , sep = " ")
  }
  #match.arg(experiment.save)
  clf$experiment$save           <- experiment.save

  return(clf)
}


terBeam_fit_mc <- function(X, y, clf,approch = "ovo", aggregation_ = "votingAggregation")
{
  # Setting the language environment
  switch(clf$params$language,
         ter=
           {
             # ternary language without intercept (maximize the auc)
             if(clf$params$verbose){print("Setting environment for the language 'ter'")}
             if(clf$params$objective == "cor")
             {
               clf$params$evalToFit <- "cor_"
             }else
             {
               # note that here with the ter language we could not use auc to fit since the intercept should be 0
               clf$params$intercept = 0
               if(clf$params$evalToFit == "auc_")
               {
                 clf$params$evalToFit <- "accuracy_"
                 warning("terga1_fit: changing evalToFit from auc_ to accuracy_ because of the language.")
               }
             }
           },
         terinter=
           {
             # ternary language without intercept (maximize the accuracy)
             if(clf$params$verbose){print("Setting environment for the language 'terinter'")}
             if(clf$params$objective == "cor")
             {
               clf$params$evalToFit <- "cor_"
             }
           },
         bin=
           {
             # ternary language without intercept (maximize the auc)
             if(clf$params$verbose){print("Setting environment for the language 'bin'")}
             if(clf$params$objective == "cor")
             {
               clf$params$evalToFit <- "cor_"
             }else
             {
               # note that here with the ter language we could not use auc to fit since the intercept should be 0
               clf$params$intercept = 0
               if(clf$params$evalToFit == "auc_")
               {
                 clf$params$evalToFit <- "accuracy_"
                 warning("terga1_fit: changing evalToFit from auc_ to accuracy_ because of the language.")
               }
             }
           },
         bininter=
           {
             # ternary language without intercept (maximize the auc)
             if(clf$params$verbose){print("Setting environment for the language 'bininter'")}
             if(clf$params$objective == "cor")
             {
               clf$params$evalToFit <- "cor_"
             }
           },
         ratio=
           {
             # ternary language without intercept (maximize the auc)
             if(clf$params$verbose){print("Setting environment for the language 'ratio'")}
             if(clf$params$objective == "cor")
             {
               clf$params$evalToFit <- "cor_"
             }
           },
         {
           stop(paste("The language",clf$params$language, "is not implemented !"))
         }
  )

  if(clf$params$verbose) print(paste("... ... parameters are checked and set"))
  # Print the experiment configuration
  if(clf$params$verbose) printClassifier(obj = clf)
  # Rprof("Profiling_terbeam", line.profiling = TRUE)

  # Rprof(NULL)
  # summaryRprof("Profiling_terbeam", lines = "show")
  # store all the features to keep
  features.to.keep <- allFeatures <- rownames(X)
  features.to.keep_ <- allFeatures <- rownames(X)
  fullPop <- list()
  res.mod.coll <- list()
  nb.selected.features <- list()
  nb.selected.features.pos <- list()
  nb.selected.features.neg <- list()
  listcoeff <- list()
  features.pool = list()
  features.pool.coeffs <- list()
  neg.ind <- list()
  pos.ind <- list()
  ind.features <- list()
  selected.features.neg <- list()
  selected.features.pos <- list()
  selected.features <- list()
  list_features.to.keep <- list()
  l_features.to.keep <- list()
  feature.cor <- list()
  feature.cor <-  clf$feature.cor
  clf$coeffss_ <-  list()
  clf$coeffss_ = clf$coeffs_
  list_best <- list()
  list_veryBest <- list()
  list_pop <- list()
  kl = 0
  #features.to.keep <- list()
  # for each sparsity
  for(k in clf$params$sparsity)
  {
    if(k == 1)
    {
      for(j in 1:(length(feature.cor))){
        # For k = 1 we generate every possible Model with only one feature
        if(!is.null(clf$feature.cor[[1]]))
        {
          # for the ration language, force to have the same number of negative and positive features selected
          if((clf$params$language == "ratio" | clf$params$language == "ter" | clf$params$language == "terinter") & clf$params$objective == "auc")
          {
            # select the best features here no need to compute all
            nb.selected.features[[j]] <- min(nrow(clf$feature.cor[[j]]),clf$params$maxNbOfModels)
            nb.selected.features.neg[[j]] <- nb.selected.features.pos[[j]] <- round(nb.selected.features[[j]]/2)
            # get the pool of features
            features.pool[[j]] <- as.character(rownames(clf$feature.cor[[j]]))[order(clf$feature.cor[[j]]$p)]
            coeffss = clf$coeffss_[[j]]
            features.pool_ = features.pool[[j]]
            #features.pool_ = unlist(features.pool_)
            features.pool.coeffs[[j]] <- coeffss[features.pool_]
            # negative
            features.pool.coeffs_ = features.pool.coeffs[[j]]
            neg.ind[[j]] <- features.pool.coeffs_ == "-1" & !is.na(features.pool.coeffs_)
            neg.ind_ = neg.ind[[j]]
            nb.selected.features.neg_ = nb.selected.features.neg[[j]]
            selected.features.neg[[j]] <- features.pool_[neg.ind_][1:min(sum(neg.ind_), nb.selected.features.neg_)]
            # positive
            pos.ind[[j]] <- features.pool.coeffs_ == "1" & !is.na(features.pool.coeffs_)
            pos.ind_ = pos.ind[[j]]
            selected.features.pos[[j]] <- features.pool_[pos.ind_][1:min(sum(pos.ind_), nb.selected.features.neg_)]
            selected.features.neg_ = selected.features.neg[[j]]
            selected.features.pos_ = selected.features.pos[[j]]
            selected.features[[j]] <- c(selected.features.neg_, selected.features.pos_)
          }else
          {
            nb.selected.features[[j]] <- min(nrow(clf$feature.cor[[j]]), clf$params$maxNbOfModels)
            # get the pool of features
            features.pool[[j]] <- rownames(clf$feature.cor[[j]])[order(clf$feature.cor[[j]]$p)]
            # select the best features here no need to compute all
            selected.features[[j]] <- features.pool_[1:nb.selected.features[[j]]]
          }
          selected.features_ = selected.features[[j]]
          # get the index in the rownames
          ind.features[[j]]    <- which(rownames(X) %in% selected.features_)
          if(clf$params$verbose) print(paste("... ... generating only best single feature models"))
        }else
        {
          if(clf$params$verbose) print(paste("... ... generating all single feature models"))
          stop("terBeam_fit: clf$feature.cor is missing")
          ind.features[[j]]    <- seq_len(nrow(X))
        }

        pop               <- generateAllSingleFeatureModel_mc(X, y, clf, ind.sub = ind.features, approch = approch)

      }

    } else
    {
      ### Get the features to keep for next k
      nbCombinaisons    <- choose(n = c(1:length(features.to.keep)), k = k)


      # for the ration language, force to have the same number of negative and positive features selected
      if((clf$params$language == "ratio" | clf$params$language == "ter" | clf$params$language == "terinter") & clf$params$objective == "auc")
      {
        for(j in 1:(length(feature.cor))){
          # select the best features here no need to compute all
          nb.selected.features[[j]] <- max(which(nbCombinaisons < clf$params$maxNbOfModels))
          nb.selected.features.neg[[j]] <- nb.selected.features.pos[[j]] <- round(nb.selected.features[[j]]/2)
          # get the pool of features
          features.pool[[j]] <- intersect(as.character(rownames(clf$feature.cor[[j]]))[order(clf$feature.cor[[j]]$p)], features.to.keep_)
          coeffss = clf$coeffss_[[j]]
          features.pool_ = features.pool[[j]]
          #features.pool_ = unlist(features.pool_)
          features.pool.coeffs[[j]] <- coeffss[features.pool_]
          # negative
          features.pool.coeffs_ = features.pool.coeffs[[j]]
          nb.selected.features.neg_ = nb.selected.features.neg[[j]]
          neg.ind[[j]] <- features.pool.coeffs_ == "-1" & !is.na(features.pool.coeffs_)
          neg.ind_ = neg.ind[[j]]
          selected.features.neg[[j]] <- features.pool_[neg.ind_][1:min(sum(neg.ind_), nb.selected.features.neg_)]
          # positive

          pos.ind[[j]] <- features.pool.coeffs_ == "1" & !is.na(features.pool.coeffs_)
          pos.ind_ =  pos.ind[[j]]
          selected.features.neg_ = selected.features.neg[[j]]
          selected.features.pos[[j]] <- features.pool_[pos.ind_][1:min(sum(pos.ind_), nb.selected.features.neg_)]
          selected.features.pos_ = selected.features.pos[[j]]
          l_features.to.keep[[j]] <- selected.features[[j]] <- c(selected.features.neg_, selected.features.pos_)
        }
      }else
      {
        for(j in 1:(length(feature.cor))){
          nb.selected.features[[j]] <- max(which(nbCombinaisons < clf$params$maxNbOfModels))
          # get the pool of features
          features.pool[[j]] <- intersect(as.character(rownames(clf$feature.cor[[j]]))[order(clf$feature.cor[[j]]$p)], features.to.keep)

          # select the best features here no need to compute all
          features.pool_ = features.pool[[j]]
          features.to.keep <- selected.features <- features.pool_[1:min(nb.selected.features[[j]], length(features.pool[[j]]))]
        }
      }


      # # nbCombinaisons contains the maximum number of models that would be generated for a given number of features
      # features.to.keep  <- features.to.keep[1:max(which(nbCombinaisons < clf$params$maxNbOfModels))]

      ### For k > 1 we generate every possible combinations of features of size k
      list_ind.features.to.keep <- list()
      for(j in 1:(length(l_features.to.keep))){
        list_ind.features.to.keep[[j]] <- which(allFeatures %in% l_features.to.keep[[1]])
      }
      if(length(list_ind.features.to.keep) >= k)
      {
        pop               <- generateAllCombinations_mc(X = X, y = y, clf = clf,
                                                        ind.features.to.keep = list_ind.features.to.keep,
                                                        sparsity = k,
                                                        allFeatures = allFeatures,approch=approch)
      }else
      {
        break
      }

    }

    # Evaluate the population

    pop                 <- evaluatePopulation_mc(X = X, y = y, clf = clf, pop = pop,
                                                 eval.all = TRUE,
                                                 force.re.evaluation = TRUE,
                                                 estim.feat.importance = FALSE,
                                                 mode = "train",
                                                 approch = approch,
                                                 aggregation_ = aggregation_,
                                                 delete.null.models = TRUE)

    # Sort the population according to the clf$params$evalToFit attribute
    pop                 <- sortPopulation(pop, evalToOrder = "fit_")


    # it may happen that the population is empty, in this case we break and return
    if(is.null(pop))
    {
      next
    }

    # Sample the best and veryBest population
    best                <- pop[1:min(clf$params$nbBest,length(pop))]
    veryBest            <- pop[1:min(clf$params$nbVeryBest,length(pop))]
    # features.to.keep
    ### Evaluate the apperance of every features in the best and veryBest models
    featuresApperance   <- countEachFeatureApperance_mc(clf, allFeatures, pop, best, veryBest,approch=approch)
    features.to.keep    <- getFeatures2Keep_mc(clf, featuresApperance,approch=approch)
    listf <- list()
    listf <- features.to.keep[[1]]
    features.to.keep <- list()
    features.to.keep <-  listf

    if(clf$params$verbose)
    {
      if(isModel(pop[[1]]))
      {
        try(printModel_mc(mod = pop[[1]], method = clf$params$print_ind_method, score = "fit_"), silent = TRUE)
      }
    }

    #EP: keep only the verybest
    #fullPop[(length(fullPop) +1):(length(fullPop) + length(pop))] <- pop
    minsize <- min(length(pop), clf$params$nbVeryBest)
    fullPop[(length(fullPop) +1):(length(fullPop) + minsize)] <- pop[1:minsize]

    # save populatio in a file
    if(!(clf$params$popSaveFile=="NULL"))
    {
      savePopulation(fullPop, paste("resulstsForSparsity", k, clf$params$popSaveFile, sep = "_"))
    }

    # stopping testing
    if((length(features.to.keep) < k + 1) & (k != 1)) # If we exhausted all the combinations
    {
      break
    } # end if stopping test

  } # end loop sparsity

  if(clf$params$verbose) print(paste("... ... models are created"))

  return.perc       <- clf$params$final.pop.perc
  if(return.perc > 100)
  {
    return.perc = 100 # upper bound
    warning("terBeam_fit: clf$params$final.pop.perc can not be greater than 100")
  }

  #fullPop           <- sortPopulation(fullPop, evalToOrder = "fit_")
  fullPop           <- unique(fullPop) # keep only unique models

  if(return.perc == 100)
  {
    # transform the population onto a model collection
    res.mod.coll    <- listOfModels2ModelCollection(pop = fullPop)
  } else # if smaller percentage
  {
    #if(clf$params$final.pop.perc>100)
    nBest           <- round(return.perc * clf$params$nbVeryBest / 100)
    res.mod.coll    <- listOfModels2ModelCollection(pop = fullPop, nBest = nBest)
  }

  if(clf$params$verbose) print(paste("... ... models are coverted onto a model collection"))
  return(res.mod.coll)
}
