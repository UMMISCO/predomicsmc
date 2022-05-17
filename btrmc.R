mcterga1 <- function(sparsity = c(1:10),
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
  clf$learner <- "terga1" # name of the method
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


# load the data
data("cir_train")
str(cir_train, max.level = 1)

# Filter the non informative variables
X <- cir_train$X; y <- cir_train$y # set global variables
X <- X[rowSums(X)!=0,]; dim(X) # filter out variables with only zero values

# Vector containing 3 positive classes and one negative class
mo = c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5,2,2,2,2,2,3,3,3,3,3)
# affectation vecteur 4 classes dans y
y <-mo

# first combination one vs all
cb1 <-y
vo<-which(cb1 !=2)
wo <- which(cb1 == 2)
cb1 <- replace(cb1,vo, -1)
cb1 <- replace(cb1,wo, 1)
cb1

# second combination one vs all
cb2 <- y
vs<-which(cb2 !=4)
ws <- which(cb2 == 4)
cb2 <- replace(cb2,vs, -1)
cb2 <- replace(cb2,ws, 1)
cb2


# third  combination one vs all

cb3 <- y
vv<-which(cb3 !=3)
wv <- which(cb3 == 3)
cb3 <- replace(cb3,vv, -1)
cb3 <- replace(cb3,wv, 1)
cb3

# third  combination one vs all

cb4 <- y
vl<-which(cb4 !=5)
wl <- which(cb4 == 5)
cb4 <- replace(cb4,vv, -1)
cb4 <- replace(cb4,wv, 1)
cb4

# creation first binary classifier one vs all of our dataset








# Launch the fit classifier routine
mcterga1_fit <- function(X, y,cb1, cb2, cb3, cb4,clf) {

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
                 warning("mcterga1_fit: changing evalToFit from auc_ to accuracy_ because of the language.")
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
                 warning("mcterga1_fit: changing evalToFit from auc_ to accuracy_ because of the language.")
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
      pop_last1      <- evolve(X, cb1, clf, pop, seed = clf$params$current_seed)
      pop_last2      <- evolve(X, cb2, clf, pop, seed = clf$params$current_seed)
      pop_last3      <- evolve(X, cb3, clf, pop, seed = clf$params$current_seed)
      pop_last4      <- evolve(X, cb4, clf, pop, seed = clf$params$current_seed)
      
    }

    # evaluate the fitting function for all the models of the populaion
    # transform to a population of model objects
    pop_last.modcb1 <- listOfSparseVecToListOfModels(X = X, cb1 = cb1, clf = clf, v = pop_lastcb1)
    pop_last.modcb2 <- listOfSparseVecToListOfModels(X = X, cb2 = cb2, clf = clf, v = pop_lastcb2)
    pop_last.modcb3 <- listOfSparseVecToListOfModels(X = X, cb3= cb3, clf = clf, v = pop_lastcb3)
    pop_last.modcb4 <- listOfSparseVecToListOfModels(X = X, cb4= cb4, clf = clf, v = pop_lastcb4)
    # evaluate the population
    pop.last.evalcb1 <- evaluatePopulation(X, cb1, clf, pop_last.modcb1, force.re.evaluation = TRUE, eval.all = TRUE)
    pop.last.evalcb2 <- evaluatePopulation(X, cb2, clf, pop_last.modcb2, force.re.evaluation = TRUE, eval.all = TRUE)
    pop.last.evalcb3 <- evaluatePopulation(X, cb3, clf, pop_last.modcb3, force.re.evaluation = TRUE, eval.all = TRUE)
    pop.last.evalcb4 <- evaluatePopulation(X, cb4, clf, pop_last.modcb4, force.re.evaluation = TRUE, eval.all = TRUE)
    # get the evaluation vector
    evaluationcb1    <- as.numeric(populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = TRUE)(pop = pop.last.evalcb1))
    evaluationcb2   <- as.numeric(populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = TRUE)(pop = pop.last.evalcb2))
    evaluationcb3    <- as.numeric(populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = TRUE)(pop = pop.last.evalcb3))
    evaluationcb4    <- as.numeric(populationGet_X(element2get = "fit_", toVec = TRUE, na.rm = TRUE)(pop = pop.last.evalcb4))
    # get the evaluation
    evaluationcb1.ord <- order(abs(evaluationcb1), decreasing = TRUE)
    evaluationcb2.ord <- order(abs(evaluationcb2), decreasing = TRUE)
    evaluationcb3.ord <- order(abs(evaluationcb3), decreasing = TRUE)
    evaluationcb4.ord <- order(abs(evaluationcb4), decreasing = TRUE)
    # order by best in front
    evaluationcb1 <- evaluationcb1[evaluationcb1.ord]
    evaluationcb2 <- evaluationcb2[evaluationcb2.ord]
    evaluationcb3 <- evaluationcb3[evaluationcb3.ord]
    evaluationcb4 <- evaluationcb4[evaluationcb4.ord]
    pop_ordered_modcb1 <- pop.last.evalcb1[evaluationcb1.ord] # we gain speed
    pop_ordered_modcb2 <- pop.last.evalcb2[evaluationcb2.ord] # we gain speed
    pop_ordered_modcb3 <- pop.last.evalcb3[evaluationcb3.ord] # we gain speed
    pop_ordered_modcb4 <- pop.last.evalcb4[evaluationcb4.ord] # we gain speed

    # get the best individuals (should be the first)
    best_individual_indexcb1 <- which.max(abs(evaluationcb1))
    best_individual_indexcb2 <- which.max(abs(evaluationcb2))
    best_individual_indexcb3 <- which.max(abs(evaluationcb3))
    best_individual_indexcb4 <- which.max(abs(evaluationcb4))
    best_individualcb1 <- pop_ordered_modcb1[[best_individual_indexcb1]]
    best_individualcb2 <- pop_ordered_modcb2[[best_individual_indexcb2]]
    best_individualcb3 <- pop_ordered_modcb3[[best_individual_indexcb3]]
    best_individualcb4 <- pop_ordered_modcb4[[best_individual_indexcb4]]

    # print it out
    if(clf$params$verbose)
    {
      if(isModel((best_individualcb1) & (best_individualcb2) & (best_individualcb3) & (best_individualcb4)))
      {
        try(cat(paste("gencb1 =",i, "\t", printModel(mod = best_individualcb1, method = clf$params$print_ind_method, score = "fit_"),"\n")), silent = TRUE)
        try(cat(paste("gencb2 =",i, "\t", printModel(mod = best_individualcb2, method = clf$params$print_ind_method, score = "fit_"),"\n")), silent = TRUE)
        try(cat(paste("gencb3 =",i, "\t", printModel(mod= best_individualcb3, method = clf$params$print_ind_method, score = "fit_"),"\n")), silent = TRUE)
        try(cat(paste("gencb4 =",i, "\t", printModel(mod = best_individualcb4, method = clf$params$print_ind_method, score = "fit_"),"\n")), silent = TRUE)
      }
    }

    # transform the indexes into models
    if(!isPopulation((objcb1= pop_ordered_modcb1)&(objcb2 = pop_ordered_modcb2)&(objcb3 = pop_ordered_modcb3)&(objcb4 = pop_ordered_modcb4)))
    {
      pop_ordered_modcb1 <- evaluatePopulation(X, cb1, clf, pop_ordered_mod, force.re.evaluation = TRUE, eval.all = TRUE)
      pop_ordered_modcb2 <- evaluatePopulation(X, cb2, clf, pop_ordered_mod, force.re.evaluation = TRUE, eval.all = TRUE)
      pop_ordered_modcb3 <- evaluatePopulation(X, cb3, clf, pop_ordered_mod, force.re.evaluation = TRUE, eval.all = TRUE)
      pop_ordered_modcb4 <- evaluatePopulation(X, cb4, clf, pop_ordered_mod, force.re.evaluation = TRUE, eval.all = TRUE)
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

































