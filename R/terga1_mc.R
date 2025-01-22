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
# @script: terga5.R
# @author: Edi Prifti
# @author: Fabien KAMBU MBUANGI
# @date: Juin 2023
################################################################


#' terga1: Model search algorithm based on genetic algorithms (GA)
#'
#' @title terga1_mc
#' @description terga1_mc is a model multi class search algorithm based on genetic algorithms (GA). A “genome” or “individual” in this context is a combination of features that will be associated together to compute a score that will be the prediction model. Depending on the type of fitting function that is maximized the fatures are weighed by specific coefficients. In short the algorithm is based on different operations such as crossing, mutating and evolving different “individuals” and evaluating their fitness to the “environment” which is represented by the variable to be predicted.
#' @param sparsity: number of features in a given model. This is a vector with multiple lengths.
#' @param size_pop: the number of individuals in a population to be evolved.
#' @param size_world: this is the number of features in the dataset.
#' @param max.nb.features: focuses only on the subset of top most significant features (default:1000)
#' @param popSourceFile: A population of models that can start as a first generation to be evolved (default:NULL).
#' @param popSaveFile: (??)
#' @param language is the language that is used by the different algorithms {bin, bininter, ter, terinter, ratio}, (default:"terinter")
#' @param scoreFormula: a Function that contains the ratio Formula or other specific ones
#' @param epsilon: a small value to be used with the ratio language (default: NULL). When null it is going to be calculated by the minimum value of X divided by 10.
#' @param unique_vars: logical (default: FALSE) indicates weather unique variables can be used in a model or population.
#' @param objective: this can be auc, cor or aic. Terga can also predict regression, other than class prediction. (default:auc)
#### TODO UPDATE THIS AS c(classification, regression)
#' @param estimate_coefs: non ternary solution for the aic objective (default:FALSE)
#' @param intercept: (Interceot for the a given model) (default:NULL)
#' @param evalToFit: The model performance attribute to use as fitting score (default:"fit_"). Other choices are c("auc_","accuracy_","precision_","recall_","f_score_")
#' @param k_penalty: Penalization of the fit by the k_sparsity (default: 0)
#' @param select_type: the selection operator type. can be mixed, elite or tournoi (default: mixed)
#' @param select_perc1: percentage of individuals to be selected with elite
#' @param select_perc2: percentage of individuals to be selected with tournoi
#' @param perc_best_ancestor: percentage of best ancentors as seeding in the new population
#' @param mutate_size: percentage of individuals in the population to be mutated
#' @param mutate_rate: percentage of features in an individual to be mutated
#' @param plot: plot graphics indicating the evolution of the simulation (default:FALSE)
#### TOCHECK is this still needed for tergaV2
#' @param convergence: should the algorithm converge when the best individual is not improving (default:TRUE).
#' @param convergence_steps: the number of generations after which we consider convergence (default:10).
#' @param evolve_k1: weather or not to evaluate exhaustively the features for k_sparse=1. This will take a lot of time if the dataset is large, thus the possibility to evolve this using the GA. (default:TRUE)
#' @param verbose: print out information on the progress of the algorithm (default:TRUE)
#' @param warnings: Print out warnings when runnig (default:FALSE).
#' @param debug: print debug information (default:FALSE)
#' @param print_ind_method: One of c("short","graphical") indicates how to print a model and subsequently a population during the run (default:"short").
#' @param parallelize.folds: parallelize folds when cross-validating (default:TRUE)
#' @param nb_generations: maximum number of generations to evolve the population.
#' @param nCores: the number of cores to execute the program. If nCores=1 than the program runs in a non parallel mode
#' @param seed: the seed to be used for reproductibility. If seed=NULL than it is not taken into account (default:NULL).
#### TODO check
#' @param experiment.id: The id of the experiment that is to be used in the plots and comparitive analyses (default is the learner's name, when not specified)
#' @param experiment.description: A longer description of the experiment. This is important when many experiments are run and can also be printed in by the printExperiment function.
#' @param experiment.save: Data from an experiment can be saved with different levels of completness, with options to be selected from c("nothing", "minimal", "full"), default is "minimal"
#' @return an object containing a list of parameters for this classifier
#' @export
terga1_mc <- function(sparsity = c(1:10),
                      # population options
                      size_pop = 100, size_world = "NULL", max.nb.features = 1000, popSourceFile = "NULL", popSaveFile = "NULL",
                      # language in {bin, bininter, ter, terinter, ratio}
                      language = "terinter",
                      # language options
                      scoreFormula=scoreRatio, epsilon = "NULL",
                      # individual options
                      unique_vars = FALSE,
                      # evaluation options
                      objective = "auc", k_penalty=0, evalToFit = "fit_", estimate_coefs = FALSE, intercept = "NULL",
                      # selection options
                      select_type = "mixed", select_perc1 = 20, select_perc2 = 30, perc_best_ancestor = 10,
                      # mutation options
                      mutate_size = 70, mutate_rate = 50,
                      # evolution options
                      nb_generations = 100, convergence = TRUE, convergence_steps = 10, evolve_k1 = TRUE,
                      # output options
                      plot = FALSE, verbose = TRUE, warnings = FALSE, debug = FALSE, print_ind_method = "short", parallelize.folds = TRUE,
                      # computing options
                      nCores = 4, seed = "NULL",
                      # experiment options
                      experiment.id = "NULL", experiment.description = "NULL", experiment.save = "nothing")
{
  clf <- list() # create a classifier object
  clf$learner <- "terga1_mc" # name of the method
  clf$params <- list() # parameter list
  clf$experiment <- list() # information about the experiment

  # POPULATION
  clf$params$sparsity           <- sparsity # number of non zero variables in the model
  clf$params$current_sparsity   <- NA # number of non zero variables in the model
  clf$params$size_pop           <- size_pop # how many models in the population to evolve
  clf$params$size_world         <- size_world # total number of variables
  clf$params$max.nb.features    <- max.nb.features
  clf$params$nb_generations     <- nb_generations # number of generation to evolve
  clf$params$unique_vars        <- unique_vars # weather in a model we can have one variable more than once
  clf$params$popSourceFile      <- popSourceFile
  clf$params$popSaveFile        <- popSaveFile

  # FITTING
  clf$params$objective          <- objective # prediction (roc) or regression (cor)
  clf$params$estimate_coefs     <- estimate_coefs # integer or real estimated coefficients.
  # SELECTION
  clf$params$select_type        <- select_type # selection method (mixte, elitist, tournoi)
  clf$params$select_perc1       <- select_perc1 # selection percentage for elitist
  clf$params$select_perc2       <- select_perc2 # selection percentage for tournoi
  clf$params$perc_best_ancestor <- perc_best_ancestor # percentage of best k-1 models to be used as seed for the k simulation
  # MUTATION
  clf$params$mutate_size        <- mutate_size # what percentage of models in the population are mutated
  clf$params$mutate_rate        <- mutate_rate # what percentage of the variables in the model are mutated
  # CONVERGENCE
  clf$params$convergence        <- convergence # what should the simulation stop when convergence ?
  clf$params$convergence_steps  <- convergence_steps # after how many steps without improvement do we consider convergence?
  clf$params$evolve_k1          <- evolve_k1 # weather to evolve models with k_1 or to search them exhaustively.
  # print out intermediary results
  clf$params$plot               <- plot           # plot results?
  clf$params$verbose            <- verbose        # print out logs.
  clf$params$warnings           <- warnings       # print out warnings
  clf$params$debug              <- debug          # print out debugging information.
  clf$params$print_ind_method   <- print_ind_method # method to print individual

  # Computing options
  clf$params$nCores             <- nCores # parallel computing
  clf$params$parallel           <- nCores > 1 # parallel computing
  clf$params$parallelize.folds  <- parallelize.folds
  clf$params$parallel.local     <- FALSE
  clf$params$seed               <- seed

  # Evaluation options
  clf$params$evalToFit          <- evalToFit
  clf$params$k_penalty          <- k_penalty

  clf$params$language           <- language
  #clf$params$useCustomLanguage  <- useCustomLanguage
  clf$params$epsilon            <- epsilon
  clf$params$scoreFormula       <- scoreFormula
  clf$params$intercept          <- intercept

  # Experiment information
  if(!(experiment.id=="NULL"))
  {
    clf$experiment$id          <- experiment.id
  }else
  {
    clf$experiment$id          <- clf$learner
  }

  if(!(experiment.description=="NULL"))
  {
    clf$experiment$description <- experiment.description
  } else
  {
    clf$experiment$description <- paste(clf$learner, date() , sep = " ")
  }
  #match.arg(experiment.save)
  clf$experiment$save          <- experiment.save

  return(clf)
}


# Launch the fit classifier routine

#' @title terga1_mc_fit
#' @description #' This function fits a classifier using a genetic algorithm-based approach.
#' It evolves populations of models and selects the best models according to a specified evaluation metric.
#' The classifier can be adapted to different types of language (binary, ternary, etc.),
#' and the function ensures that the evaluation metric matches the classifier's requirements.
#' @param X A data frame or matrix containing the features of the dataset.
#'          Each row corresponds to a sample, and each column corresponds to a feature.
#' @param y A vector containing the labels or target values. It must have the same length as the number of rows in `X`.
#' @param clf A classifier object that contains the model to be fitted. It must have a parameter list (`params`)
#'            that includes the classifier's configuration (e.g., language, objective, evaluation metric).
#' @param approch A character string indicating the approach to use for model evaluation.
#'                Default is "ova" (one-vs-all), but other options may be available depending on the classifier.
#' @param aggregation_ A character string specifying the aggregation method to be used. Default is "Predomics_aggregation_ova".
#' @param constrained A logical value indicating whether the classifier should be constrained in some way during fitting.
#'                    Default is FALSE.
#'
#' @return A fitted classifier model based on the genetic algorithm approach. The result can be used for predictions
#'         and further analysis.

#'
terga1_mc_fit <- function(X, y, clf, approch="ova", aggregation_ = "Predomics_aggregation_ova", constrained = FALSE) {

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



  # set the size of the world
  clf$params$size_world <- nrow(X)

  # Print the experiment configuration
  if(clf$params$verbose) printClassifier(obj = clf)

  res <- list() # the final object containing the evolved models
  for(i in clf$params$sparsity) # sparsity is = k, i.e. the number of features in a model
  {
    cat("\t\tResolving problem with\t", i, "\tvariables ...\n")

    # set the current_sparsity
    clf$params$current_sparsity <- i


    # test for
    if (clf$params$current_sparsity == 1 & !clf$params$evolve_k1) # if we want to evolve features for k_sparse=1 we create a normal population
    {
      pop_last      <- as.list(1:nrow(X)) # create population with k_sparse = 1

    }else
    {
      best_ancestor = NULL

      if (exists("pop_last"))
      {
        if(length(evaluation) != 0)
        {
          best_ancestor = pop_last[[which.max(evaluation)]]
        }

        # build a new population seeded by the last one
        pop         <- population(clf = clf,
                                  size_ind = i,
                                  #size_world = nrow(list_X[[1]])
                                  size_world = nrow(X),
                                  best_ancestor = best_ancestor,
                                  size_pop = clf$params$size_pop,
                                  seed = clf$params$current_seed)
      }else
      {
        # build a new population from scratch
        pop         <- population(clf = clf,
                                  size_ind = i,
                                  size_world = nrow(X),
                                  best_ancestor = best_ancestor,
                                  size_pop = clf$params$size_pop,
                                  seed = clf$params$current_seed)
      }

      # if this is population with model objects we transform in an index population
      if(isPopulation(obj = pop))
      {
        pop <- listOfModelsToListOfSparseVec(list.models = pop)
      }
      # then we evolve
      pop_last      <- evolve_mc(X, y, clf, pop, seed = clf$params$current_seed,approch = approch)
      # Initialiser la liste des indices
      list_indices <- list()

      # Convert 'constrained' to logical type if it's not already
      constrained <- as.logical(constrained)

      if (constrained == FALSE) {
        # Loop through the indices of the first list in 'pop_last'
        for (i in 1:length(pop_last[[1]])) {
          # Initialize the list of indices for each iteration
          list_indices[[i]] <- list()

          # Loop through the different lists in 'pop_last'
          for (j in 1:length(pop_last)) {
            if (i <= length(pop_last[[j]])) {
              # Add the corresponding element from 'pop_last' to 'list_indices'
              list_indices[[i]][[j]] <- pop_last[[j]][[i]]
            } else {
              # Reuse the previous element if the index exceeds the length of the sublist
              list_indices[[i]][[j]] <- list_indices[[i - 1]][[j]]
            }
          }
        }
        # Update 'pop_last' with the new list of indices
        pop_last <- list_indices

      } else if (constrained == TRUE) {
        # Case where 'constrained' is TRUE
        for (i in 1:length(pop_last[[1]])) {
          # Initialize the list of indices for each iteration
          list_indices[[i]] <- list()

          # Loop through the different lists in 'pop_last'
          for (j in 1:length(pop_last)) {
            if (i <= length(pop_last[[j]])) {
              # Add the corresponding element from 'pop_last' to 'list_indices'
              list_indices[[i]][[j]] <- pop_last[[j]][[i]]
            } else {
              # Reuse the previous element if the index exceeds the length of the sublist
              list_indices[[i]][[j]] <- list_indices[[i - 1]][[j]]
            }
          }
        }

        # Initialize the output list 'new_list'
        new_list <- list()

        # Fill 'new_list' with the first element of each sublist in 'list_indices'
        for (jj in seq_along(list_indices)) {
          # Get the first element of the sublist from 'list_indices'
          first_element <- list_indices[[jj]][[1]]

          # Create a sublist in 'new_list' with this first element repeated
          new_list[[jj]] <- rep(list(first_element), length(list_indices[[jj]]))
        }

        # Update 'pop_last' with the new list
        pop_last <- new_list
      }

    }

    # evaluate the fitting function for all the models of the populaion
    # transform to a population of model objects
    pop_last.mod <- listOfSparseVecToListOfModels_mc(X, y , clf = clf, v = pop_last,approch = approch)
    # evaluate the population
    pop.last.eval <- evaluatePopulation_mc(X , y, clf, pop_last.mod, force.re.evaluation = TRUE, eval.all = TRUE, approch=approch, aggregation_ = aggregation_)
    # get the evaluation vector
    evaluation    <- as.numeric(populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = TRUE)(pop = pop.last.eval))
    # get the evaluation
    evaluation.ord <- order(abs(evaluation), decreasing = TRUE)
    # order by best in front
    evaluation <- evaluation[evaluation.ord]
    pop_ordered_mod <- pop.last.eval[evaluation.ord] # we gain speed

    # get the best individuals (should be the first)
    best_individual_index <- which.max(abs(evaluation))
    best_individual <- pop_ordered_mod[[best_individual_index]]

    # print it out
    if(clf$params$verbose)
    {
      if(isModel(best_individual))
      {
        try(cat(paste("gen =",i,"\t", printModel_mc(mod = best_individual, method = clf$params$print_ind_method, score = "fit_"),"\n")), silent = TRUE)
      }
    }

    # transform the indexes into models
    if(!isPopulation(obj = pop_ordered_mod))
    {
      pop_ordered_mod <- evaluatePopulation_mc(X, y, clf, pop_ordered_mod, force.re.evaluation = TRUE, approch = approch, aggregation_ = aggregation_,eval.all = TRUE)
    }
    # keep only models that are unique
    pop_ordered_mod <- unique(pop_ordered_mod)

    if(!(clf$params$popSaveFile=="NULL"))
    {
      #pop2Save      <- evaluatePopulation(X, y, clf, pop_ordered_mod, eval.all = TRUE)
      pop2Save      <- pop_ordered_mod
      savePopulation(pop2Save, paste("generation", i, clf$params$popSaveFile, sep = "_"))
    }

    # save the whole list of models ordered by fitting score. The best is the first
    res[[paste("k",i,sep = "_")]] <- pop_ordered_mod
  }

  return(res)
}

#' Creates new combinations of features based from a parents.
#' @title evolve_mc
#' @description This function is used in terga1 and is the main engine of the algorithm that allows to cross, mutate and select individuals from one generation to the next (one versus one).
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @param pop: A population (i.e. list) of index vectors
#' @param seed: For reproductibility purpose to fix the random generator number.
#' @return a list population of models, containing parents and children(one versus one)
#' @export
evolve_mc <- function(X, y, clf, pop, seed = NULL, approch="ovo")
{
  nClasse <- unique(y)
  list_evolved_pop <- list() # List of different combinations of evolved
  list_y <- list() # List of different combinations of y
  list_X <- list() # List of different combinations of X
  listcoeffs <- list() # List of different combinations of coeffs
  listX <- list()
  listXmin <- list() # List min of X
  listXmax <- list() # List max of X
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
  for (i in 1:length(list_X)) {
    clf$coeffs_ <- listcoeffs[[i]]
    clf$data$X <- listX[[i]]
    clf$data$X.min <- listXmin[[i]]
    clf$data$X.max <- listXmax[[i]]
    clf$data$y <- listy[[i]]
    list_evolved_pop[[i]] <- evolve(X = list_X[[i]], y = list_y[[i]], clf, pop, seed = NULL)
  }

  evolved_pop <- list_evolved_pop

  return(evolved_pop)
}
