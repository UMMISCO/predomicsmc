################################################################
# PRINTING DIFFERENT, OBJECTS
################################################################

#' Prints a model object as text.
#'
#' @description Prints a model object as text
#' @param mod: a model to plot
#' @param method: an object containing the different parameters of the classifier
#' @param score: which score to show in the fit (default:fit_)
#' @export
printModel_mc <- function(mod, method = "short", score = "fit_")
{
  if(!isModel(obj = mod))
  {
    print("printModel: please provide a valid predomics model object.")
    return(NULL)
  }

  if(!score %in% names(mod))
  {
    print("printModel: please provide a valid score that is found as an attribute in the model object.")
    return(NULL)
  }

  list_res <- list()
  list_matrices <- list()
  listindices <- mod$indices_
  listnames <- mod$names_
  listcoeffs <- mod$coeffs_
  listsign <- mod$sign_
  listscore <- mod$score_
  listpos <- mod$pos_score_
  listneg <- mod$neg_score_
  list_intercept <- mod$list_intercept_
  list_matrices <- mod$confusionMatrix_
  list_accura <- list()
  accura <- mod$accuracy
  for(km in 1:length(listindices)){
    mod$indices_ <- listindices[[km]]
    mod$names_ <- listnames[[km]]
    mod$coeffs_ <- listcoeffs[[km]]
    mod$sign_ <-  listsign[[km]]
    mod$score_ <- listscore[[km]]
    mod$pos_score_ <- listpos[[km]]
    mod$neg_score_ <- listneg[[km]]
    mod$intercept_ <- list_intercept[[km]]
    mod$fit_ <- accura

    switch(method,
           short={
             if(!isModelSota(mod))
             {
               if(all(sapply(mod$coeffs_, myAssertNotNullNorNa)))
               {
                 ind.pos <- sapply(mod$coeffs_, function(coeffs) sign(coeffs) == 1)
                 ind.neg <- sapply(mod$coeffs_, function(coeffs) sign(coeffs) == -1)

                 if(mod$language == "ratio")
                 {
                   signs <- rep("+", length(mod$coeffs_))
                   term.pos <- ifelse(all(!ind.pos), "0", paste("(", paste(signs[ind.pos], mod$indices_[ind.pos], sep = "", collapse = ""), ")"))
                   term.neg <- ifelse(all(!ind.neg), "0", paste("(", paste(signs[ind.neg], mod$indices_[ind.neg], sep = "", collapse = ""), ")"))
                   res <- paste(term.pos, "/", term.neg)
                   mod.inter <- paste(mod$sign_, signif(mod$intercept_, 2))
                   mod.fit <- mod[[score]]
                   if(!is.na(mod.fit)) res <- paste("|", res, " ", mod.inter, decision, "| (F=", signif(mod.fit, 4), sep = "")
                   res <- paste(res, "|K=", mod$eval.sparsity, "|Le=", mod$learner, "|La=", mod$language, ")", sep = "")
                 }else{
                   signs <- rep("+", length(mod$coeffs_))
                   term.pos <- ifelse(all(!ind.pos), "0", paste("(", paste(signs[ind.pos], mod$indices_[ind.pos], sep = "", collapse = ""), ")"))
                   term.neg <- ifelse(all(!ind.neg), "0", paste("(", paste(signs[ind.neg], mod$indices_[ind.neg], sep = "", collapse = ""), ")"))
                   res <- paste(term.pos, " - ", term.neg)
                   mod.inter <- paste(mod$sign_, signif(mod$intercept_, 2))
                   mod.fit <- mod[[score]]
                   if(!is.na(mod.fit)) res <- paste("|", res, " ", mod.inter, "| (F=", signif(mod.fit, 4), sep = "")
                   res <- paste(res, "|K=", mod$eval.sparsity, "|Le=", mod$learner, "|La=", mod$language, ")", sep = "")
                 }
               }
             }else{
               if(!is.null(mod$coeffs_))
               {
                 coeffs <- signif(unlist(mod$coeffs_), 2)
               }else{
                 coeffs <- ""
               }
               res <- unlist(mod$indices_)
               res <- paste(coeffs, res, sep = " * ")
               res <- paste(res, collapse = " ")
               res <- paste(res, mod$learner, sep = "|")
               mod.fit <- mod[[score]]
               if(!is.na(mod.fit)) res <- paste("|", res, " ", "| (F=", signif(mod.fit, 4), sep = "")
               res <- paste(res, "|K=", mod$eval.sparsity, "|Le=", mod$learner, "|La=", mod$language, ")", sep = "")
             }
           },
           long={
             if(!isModelSota(mod))
             {
               if(all(sapply(mod$coeffs_, myAssertNotNullNorNa)))
               {
                 ind.pos <- sapply(mod$coeffs_, function(coeffs) sign(coeffs) == 1)
                 ind.neg <- sapply(mod$coeffs_, function(coeffs) sign(coeffs) == -1)

                 if(mod$language == "ratio")
                 {
                   signs <- rep("+ ", length(mod$coeffs_))
                   term.pos <- ifelse(all(!ind.pos), "0", paste("(", paste(signs[ind.pos], mod$names_[ind.pos], sep = "", collapse = ""), ")"))
                   term.neg <- ifelse(all(!ind.neg), "0", paste("(", paste(signs[ind.neg], mod$names_[ind.neg], sep = "", collapse = ""), ")"))
                   res <- paste(term.pos, "/", term.neg)
                   mod.inter <- paste(mod$sign_, signif(mod$intercept_, 2))
                   mod.fit <- mod[[score]]
                   if(!is.na(mod.fit)) res <- paste("|", res, " ", mod.inter, "| (F=", signif(mod.fit, 4), sep = "")
                   res <- paste(res, "|K=", mod$eval.sparsity, "|Le=", mod$learner, "|La=", mod$language, ")", sep = "")
                 }else{
                   signs <- rep("+ ", length(mod$coeffs_))
                   term.pos <- ifelse(all(!ind.pos), "0", paste("(", paste(signs[ind.pos], mod$names_[ind.pos], sep = "", collapse = ""), ")"))
                   term.neg <- ifelse(all(!ind.neg), "0", paste("(", paste(signs[ind.neg], mod$names_[ind.neg], sep = "", collapse = ""), ")"))
                   res <- paste(term.pos, " - ", term.neg)
                   mod.inter <- paste(mod$sign_, signif(mod$intercept_, 2))
                   mod.fit <- mod[[score]]
                   if(!is.na(mod.fit)) res <- paste("|", res, " ", mod.inter, "| (F=", signif(mod.fit, 4), sep = "")
                   res <- paste(res, "|K=", mod$eval.sparsity, "|L=", mod$learner, "|La=", mod$language, ")", sep = "")
                 }
               }
             }else{
               if(!is.null(mod$coeffs_))
               {
                 coeffs <- signif(unlist(mod$coeffs_), 2)
               }else{
                 coeffs <- ""
               }
               res <- unlist(mod$names_)
               res <- paste(coeffs, res, sep = " * ")
               res <- paste(res, collapse = " ")
               res <- paste(res, mod$learner, sep = "|")
               mod.fit <- mod[[score]]
               res <- paste("|", res, " ", "| (F=", signif(mod.fit, 4), sep = "")
               res <- paste(res, "|K=", mod$eval.sparsity, "|L=", mod$learner, "|La=", mod$language, ")", sep = "")
             }
           },
           str={
             res <- str(mod)
           },
           {
             warning('This method does not exist! Try one of these: short, long or str')
           }
    )
    list_res[[km]] <- res
  }
  res <- list()
  res <- list_res
  return(res)
}

#' Prints a population of model objects as text.
#'
#' @description Prints a population of model objects as text
#' @param obj: a population of models to plot
#' @param method: if "digested" a short sumary (one line) will be printed, otherwise the method will contain the
#' specific way to print a model through the printModel() routine
#' @param score: which score to show in the fit (default:fit_)
#' @param indent: a string (default:'tab---') that will precede each element of the object.
#' @export
printPopulation_mc <- function(obj, method = "short", score = "fit_", indent="")
{
  # sanity check
  if(!isPopulation(obj))
  {
    return(NULL)
    warning("printPopulation: the object to print is not a valid Population of models")
  }

  switch(method,
         digested={
           spar <- populationGet_X("eval.sparsity")(obj)
           pop.name <- unique(spar)
           if(length(pop.name)==1)
           {
             attribute.name <- paste0("k_",pop.name)
           }else
           {
             attribute.name <- "k_mixed"
           }
           ptdf <- populationToDataFrame(obj) # convert model population to a dataframe for easy access
           #attribute.name <- ""
           attribute.value <- paste(length(obj), "models ...",
                                    paste(paste(ptdf$learner, ptdf$language, signif(ptdf$fit_,2), ptdf$eval.sparsity, sep="_")[1:min(5,length(obj))],collapse = "; "))
           cat(paste(indent,attribute.name,": ",attribute.value, "\n",sep=""))
         },
         short =,
         str =,
         long ={
           for(i in 1:length(obj))
           {
             mod <- obj[[i]]
             print(paste(i, printModel_mc(mod = mod, method = method, score = score), sep=":"))
           }
         },
         {
           print('printPopulation: please provide a valid method (digested/short/long/str)')
         }
  )
}



#' Prints as text the detail on a given ModelCollection object
#'
#' @description This function prints a ModelCollection object. For each k_sparsity it will show some detail of
#' the maximum first models
#' @param obj: a ModelCollection object
#' @param indent: a string (default:'tab---') that will precede each element of the object for the "long" method.
#' @param method: the output method (default:long) will print for each k_sparsity a short information of the population of models,
#' while the short method will output the number of models for each k_sparsity
#' @return NULL if the object is not a valid ModelCollection.
#' @export
printModelCollection_mc <- function(obj, indent = "\t--- ", method = "long")
{
  if(!isModelCollection(obj))
  {
    return(NULL)
    warning("printModelCollection: the object to print is not a valid experiment.")
  }

  switch(method,
         short={
           paste(names(obj), unlist(lapply(obj,length)), sep=": ")
         },
         long={
           # for each k-sparsity show the number of models and some information on the first ones.
           for(i in 1:length(obj))
           {
             printPopulation_mc(obj = obj[[i]], method = "digested", indent = indent)
           }
         },
         {
           print('printModelCollection: please provide a valid method (short/long)')
         }
  )
}


#' Prints as text the detail on a given Experiment object
#'
#' @description This function prints a summary of an Experiment object.
#' @param obj: an Experiment object
#' @param indent: a string (default:'tab---') that will precede each element of the object.
#' @return NULL if the object is not a valid Experiment
#' @export
printExperiment_mc <- function(obj, indent = "\t--- ")
{
  if(!isExperiment(obj))
  {
    return(NULL)
    warning("printExperiment: the object to print is not a valid experiment.")
  }

  cat("========== Experiment ==========\n")
  for(i in 1:length(obj$classifier$experiment))
  {
    attribute.name <- names(obj$classifier$experiment)[i]
    attribute.value <- obj$classifier$experiment[[i]]
    cat(paste(indent,attribute.name,": ",attribute.value, "\n",sep=""))
  }

  if(!is.null(obj$crossVal))
  {
    cat("========== Cross validation ==========\n")
    cat(paste(indent, "total folds",": ",ncol(obj$crossVal$scores$empirical.auc), "\n",sep=""))
    cat(paste(indent, "folds",": ",ncol(obj$crossVal$scores$empirical.auc)/length(obj$classifier$params$seed), "\n",sep=""))
    cat(paste(indent, "times",": ",length(obj$classifier$params$seed), "\n",sep=""))
    cat(paste(indent, "seeds",": ",paste(obj$classifier$params$seed, collapse = ", "), "\n",sep=""))
  }else
  {
    cat("========== Cross validation ==========\n")
    cat(paste(indent, "total folds",": ",0, "\n",sep=""))
    cat(paste(indent, "folds",": ",0, "\n",sep=""))
    cat(paste(indent, "times",": ",length(obj$classifier$params$seed), "\n",sep=""))
    cat(paste(indent, "seeds",": ",paste(obj$classifier$params$seed, collapse = ", "), "\n",sep=""))
  }

  # Detailed learner options
  if(!is.null(obj$classifier$params))
  {
    cat("========== Learner ==========\n")
    cat(paste(indent,"learner: ", obj$classifier$learner, "\n",sep=""))
    for(i in 1:length(obj$classifier$params))
    {
      attribute.name <- names(obj$classifier$params)[i]
      attribute.value <- obj$classifier$params[[i]]
      if(length(attribute.value) > 1) # for sparsity for instance
      {
        if(!is.list(attribute.value))
        {
          cat(paste(indent, attribute.name,": ",paste(attribute.value, collapse = ","), "\n",sep=""))
        }
      }else
      {
        if(is.function(attribute.value)) # In case of functions
        {
          cat(paste(indent, attribute.name,": function","\n",sep=""))
        }else
        {
          cat(paste(indent, attribute.name,": ",attribute.value, "\n",sep=""))
        }
      }
    }
    if(obj$classifier$learner == "metal")
    {
      nclf <- length(obj$classifier$params$list.clfs)-1
      for(i in 1:nclf)
      {
        cat(paste(indent, obj$classifier$params$list.clfs[[i]]$experiment$id,"\n",sep = ""))
      }
    }
  }

  # Detailed learner options
  if(isModelCollection(obj$classifier$models))
  {
    cat("========== Model Collection ==========\n")
    printModelCollection_mc(obj$classifier$models)
  }
}

#' Prints as text the detail on a given Classifier object
#'
#' @description This function prints a summary of a Classifier object.
#' @param obj: a Classifier object
#' @param indent: a string (default:'tab---') that will precede each element of the object.
#' @return NULL if the object is not a valid Classifier
#' @export
printClassifier_mc <- function(obj, indent="\t--- ")
{
  # sanity check
  if(!isClf(obj))
  {
    return(NULL)
    warning("printClassifier: the object to print is not a valid Classifier")
  }

  # Global experiment information
  if(!is.null(obj$experiment))
  {
    cat("========== Experiment ==========\n")
    for(i in 1:length(obj$experiment)){
      attribute.name <- names(obj$experiment)[i]
      attribute.value <- obj$experiment[[i]]
      cat(paste(indent,attribute.name,": ",attribute.value, "\n",sep=""))
    }
  }

  # Detailed learner options
  if(!is.null(obj$params))
  {
    cat("========== Learner ==========\n")
    cat(paste(indent,"learner: ", obj$learner, "\n",sep=""))
    for(i in 1:length(obj$params))
    {
      attribute.name <- names(obj$params)[i]
      attribute.value <- obj$params[[i]]
      if(length(attribute.value)>1) # for sparsity for instance
      {
        if(!is.list(attribute.value))
        {
          cat(paste(indent, attribute.name,": ",paste(attribute.value, collapse = ","), "\n",sep=""))
        }
      }else
      {
        if(is.function(attribute.value)) # In case of functions
        {
          cat(paste(indent, attribute.name,": function","\n",sep=""))
        }else
        {
          cat(paste(indent, attribute.name,": ",attribute.value, "\n",sep=""))
        }
      }
    }
    if(obj$learner=="metal")
    {
      nclf <- length(obj$params$list.clfs)-1
      for(i in 1:nclf)
      {
        cat(paste(indent, obj$params$list.clfs[[i]]$experiment$id,"\n",sep = ""))
      }
    }
  }

  # Detailed learner options
  if(isModelCollection(obj$models))
  {
    cat("========== Model Collection ==========\n")
    printModelCollection_mc(obj$models)
  }

}


#' Prints as text the detail on a given object from the predomics package.
#'
#' @description This function will summarize any of the predomics package objects such as can be an Experiment,
#' a Model, a Population of models or a ModelCollection
#' @param obj: an object from the predomics object
#' @return NULL
#' @export
printy_mc <- function(obj)
{
  type = NA
  if(isModel(obj))
  {
    type <- "model"
  }
  if(isPopulation(obj))
  {
    type <- "population"
  }
  if(isClf(obj))
  {
    type <- "classifier"
  }
  if(isExperiment(obj))
  {
    type <- "experiment"
  }
  if(isModelCollection(obj))
  {
    type <- "model.collection"
  }

  switch(type,
         model={
           print(paste("Summary of Model object"))
           printModel_mc(mod = obj, method = "long")
         },
         population={
           print(paste("Summary of a population of models with",length(obj),"models"))
           printPopulation_mc(obj = obj[1:min(5,length(obj))], method = "long")
           if(length(obj) > 5) print("...")
         },
         model.collection={
           print(paste("Summary of a ModelCollection object with",length(obj),"populations of models"))
           printModelCollection_mc(obj = obj, method = "long")
         },
         experiment={
           print(paste("Summary of Experiment object"))
           printExperiment_mc(obj)
         },
         classifier={
           print(paste("Summary of Classifier object"))
           printClassifier_mc(obj)
         },
         {
           print('printy: please provide valid predomics model')
         }
  )

}

################################################################
# Multi-class Model Plots for Predomics
################################################################

#' @title Plots a model or a population of model objectsas barplots of scaled coefficients.
#'
#' @description Plots a model or a population of models as a barplots, representing each feature, the length being the coefficient
#' @import ggplot2
#' @param mod: a model to plot
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the class vector
#' @param sort.features: wether the features need to be sorted by correlation with 'y' or not (default: TRUE)
#' @param sort.ind: computing sorting can take time if computed for every model and can be computed outside the function and passed as a parameter
#' @param feature.name: show the name of the features (default:FALSE)
#' @param col.sign: the colors of the cofficients based on the sign of the coefficients (default: -1=deepskyblue1, 1:firebrick1)
#' @param main: possibility to change the title of the function (default:"")
#' @param slim: plot without axis information (default:FALSE)
#' @param importance: the importance (mda) of the features in crossval
#' @param res_clf: the result of the learning process (default:NULL). If provided information on MDA will be extracted for the importance graphic.
#' @export
plotModel_mc <- function(mod, X, y,
                         sort.features = FALSE,
                         sort.ind = NULL,
                         feature.name = FALSE,
                         col.sign = c("deepskyblue1", "firebrick1"),
                         main = "",
                         slim = FALSE,
                         importance = FALSE,
                         res_clf = NULL, approch = "ova") {


  list_mod <- list()
  plot_sub_model <- list()

  # Retrieve the mod elements to create the main title
  alg = mod$learner
  lang = mod$language
  k = mod$eval.sparsity

  # Loop to fill the list of sub-models
  for (i in 1:length(mod$names_)) {
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
      mda.cv_ = mod$mda.cv_[[i]],
      prev.cv_ = mod$prev.cv_[[i]],
      mda_ = mod$mda_[[i]]
    )
  }

  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()

  if (approch == "ovo") {
    f <- 1
    for (i in 1:(length(nClasse) - 1)) {
      for (j in (i + 1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)
        y_pair <- y[indices]
        X_pair <- X[, indices]
        list_y[[f]] <- as.vector(y_pair)
        list_X[[f]] <- X_pair
        f <- f + 1
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

  # Loop to generate sub-models with custom titles
  for (i in 1:length(list_y)) {
    # Determine the main title with alg, lang, and k
    combination_title <- paste("alg =", alg, "\nlang =", lang, "\nk =", k)

    # Addition of a specific title for each sub-model
    if (approch == "ovo") {
      combination_title <- paste(combination_title, "\n", paste(nClasse[i], "vs", nClasse[j]))
    } else {
      combination_title <- paste(combination_title, "\n", paste(nClasse[i], "vs All"))
    }

    plot_sub_model[[i]] <- plotModel(mod = list_mod[[i]], X = list_X[[i]], y = list_y[[i]],
                                     sort.features = sort.features,
                                     feature.name = feature.name,
                                     importance = importance,
                                     main = combination_title)
  }

  return(plot_sub_model)
}


#' Analyze the results from a list of classifiers
#'
#' @description Analyze the results from a list of classifiers.
#' @param scores: a list where each element is a vector of scores from a given models
#' @param y: the class to be predicted
#' @param main: title of the graph
#' @param ci: the point shape for the graph
#' @param percent: color for the graph
#' @return a list of roc objects
#' @export
plotAUC_mc <- function(scores, y, approch = "ovo", main = "", ci = TRUE, percent = TRUE) {

  classes <- unique(y)
  list_y <- list()
  list_scores <- list()
  plot_list <- list()

  # Convert approch to lower case for verification
  approch <- tolower(approch)

  if (approch == "ovo") {
    combinations <- combn(classes, 2, simplify = FALSE)
    k <- 1
    for (comb in combinations) {
      class_i <- comb[1]
      class_j <- comb[2]
      indices <- which(y == class_i | y == class_j)
      y_pair <- y[indices]

      if (length(y_pair) < length(scores[[k]])) {
        missing_len <- length(scores[[k]]) - length(y_pair)
        y_random <- sample(y_pair, missing_len, replace = TRUE)
        y_pair <- c(y_pair, y_random)
      }

      list_y[[k]] <- as.vector(y_pair)
      list_scores[[k]] <- scores[[k]]
      k <- k + 1
    }
  } else if (approch == "ova") {
    for (i in seq_along(classes)) {
      class_i <- classes[i]
      y_temp <- ifelse(y == class_i, as.character(class_i), "All")

      if (length(y_temp) < length(scores[[i]])) {
        missing_len <- length(scores[[i]]) - length(y_temp)
        y_random <- sample(y_temp, missing_len, replace = TRUE)
        y_temp <- c(y_temp, y_random)
      }

      list_y[[i]] <- as.vector(y_temp)
      list_scores[[i]] <- scores[[i]]
    }
  } else {
    stop("Invalid approach: choose 'ova' or 'ovo'")
  }

  # Loop to generate graphs for each combination of classes
  for (i in seq_along(list_y)) {
    class_label <- if (approch == "ovo") {
      paste(classes[i], "vs", classes[i + 1])
    } else {
      paste(classes[i], "vs ALL")
    }

    plot_list[[class_label]] <- plotAUC(score = list_scores[[i]], y = list_y[[i]],
                                        main = paste(main, class_label),
                                        ci = ci, percent = percent)
  }

  return(plot_list)
}

#' Plots the prevalence of a list of features in the whole dataset and per each class
#'
#' @description Plots the abundance of a given number of features for each class and tests significance
#' @import reshape2
#' @import ggplot2
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
plotAbundanceByClass_mc <- function(features, X, y, approch = "OVA",
                                    main = "", plot = TRUE,
                                    col.pt = c("deepskyblue4", "firebrick4"),
                                    col.bg = c("deepskyblue1", "firebrick1")) {

  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  plot_list <- list()

  # Determine class combinations based on the chosen approach
  if (approch == "OVO") {
    k <- 1
    for (i in 1:(length(nClasse) - 1)) {
      for (j in (i + 1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)
        y_pair <- y[indices]
        X_pair <- X[, indices, drop = FALSE]

        # Convert y to 1 for class_i and -1 for class_j
        list_y[[k]] <- ifelse(y_pair == class_i, 1, -1)
        list_X[[k]] <- X_pair
        k <- k + 1
      }
    }
  } else if (approch == "OVA") {
    for (i in 1:length(nClasse)) {
      class_i <- nClasse[i]
      indices <- which(y == class_i | y != class_i)

      # Convert y to 1 for the selected class and -1 for others
      list_y[[i]] <- ifelse(y[indices] == class_i, 1, -1)
      list_X[[i]] <- X[, indices, drop = FALSE]
    }
  } else {
    stop("Invalid approach: choose 'OVA' or 'OVO'")
  }

  # Generate the graphs for each combination.
  for (i in seq_along(list_y)) {
    class_label <- if (approch == "OVA") {
      paste(nClasse[i], "vs ALL")
    } else {
      class_pair <- combn(nClasse, 2, simplify = TRUE)[, i]
      paste(class_pair[1], "vs", class_pair[2])
    }

    plot_list[[class_label]] <- plotAbundanceByClass(
      features = rownames(features[[i]]$pop.noz),
      X = list_X[[i]],
      y = list_y[[i]],
      main = paste(main, class_label),
      plot = plot,
      col.pt = col.pt,
      col.bg = col.bg
    )
  }

  return(plot_list)
}

#' Plots the prevalence of a list of features in the whole dataset and per each class
#'
#' @description Plots the prevalence of a given number of features
#' @import ggplot2
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
plotPrevalence_mc <- function(features, X, y, approch = "OVA", topdown = TRUE,
                              main = "", plot = TRUE,
                              col.pt = c("deepskyblue4", "firebrick4"),
                              col.bg = c("deepskyblue1", "firebrick1")) {

  classes <- unique(y)
  list_y <- list()
  list_X <- list()
  plot_list <- list()

  # Determine class combinations based on the chosen approach
  combinations <- if (approch == "OVA") {
    paste0(classes, "_vs_ALL")
  } else if (approch == "OVO") {
    combn(classes, 2, function(x) paste0(x[1], "_vs_", x[2]), simplify = TRUE)
  } else {
    stop("Invalid approach: choose 'OVA' or 'OVO'")
  }

  # Fill the lists list_y and list_X.
  for (i in seq_along(combinations)) {
    combination <- combinations[i]

    if (approch == "OVA") {
      class_i <- gsub("_vs_ALL", "", combination)
      indices <- which(y == class_i | y != class_i)

      # Create the vector y for OVA
      list_y[[i]] <- ifelse(y[indices] == class_i, 1, -1)
      list_X[[i]] <- X[, indices, drop = FALSE]

    } else {  # OVO approach
      class_i <- strsplit(combination, "_vs_")[[1]][1]
      class_j <- strsplit(combination, "_vs_")[[1]][2]
      indices <- which(y == class_i | y == class_j)

      # Set y to 1 for class_i and -1 for class_j
      list_y[[i]] <- ifelse(y[indices] == class_i, 1, -1)
      list_X[[i]] <- X[, indices, drop = FALSE]
    }
  }

  # Generate the graphs with legends.
  for (i in seq_along(combinations)) {
    class_combination <- combinations[i]
    class_1 <- if (approch == "OVA") gsub("_vs_ALL", "", class_combination) else strsplit(class_combination, "_vs_")[[1]][1]
    class_minus1 <- if (approch == "OVA") "ALL" else strsplit(class_combination, "_vs_")[[1]][2]

    # Plot with the legend
    p <- plotPrevalence(
      features = rownames(features[[i]]$pop.noz),
      X = list_X[[i]],
      y = list_y[[i]],
      topdown = topdown,
      main = paste(main, class_combination),
      plot = plot,
      col.pt = col.pt,
      col.bg = col.bg
    )

    # Add legend annotation at the bottom of the plot
    legend_text <- paste0(class_1, ": Red | ", class_minus1, ": Blue")  # Use class names instead of firebrick4 and deepskyblue4

    # Adjust the plot with the legend text at the bottom
    p <- p + annotate(
      "text", x = Inf, y = -Inf, label = legend_text,
      vjust = -1, hjust = 1, size = 4, color = "black"
    ) +
      theme(
        plot.margin = unit(c(1, 1, 2, 1), "cm"),  # Extra margin at the bottom for legend
        plot.caption = element_text(hjust = 0.5)  # Center the caption
      ) +
      labs(caption = legend_text)  # Use caption to add the legend text

    plot_list[[class_combination]] <- p
  }

  return(plot_list)
}




#' Plots the prevalence of a list of features in the whole dataset and per each class
#'
#' @description Plots the coefficients of subset of features in the models where they are found
#' @importFrom reshape2 melt
#' @param feat.model.coeffs: feature vs. model coeffient table
#' @param topdown: showing features from top-down or the other way around (default:TRUE)
#' @param col: colors to be used for the coeffients (default: -1 = deepskyblue1, 0 = white, 1 = firebrick1)
#' @param vertical.label: wether the x-axis labels should be vertical or not (default:TRUE)
#' @return a list ggplot object
#' @export
plotFeatureModelCoeffs_mc <- function(feat.model.coeffs, y, approch = "OVA", topdown = TRUE, col = c("deepskyblue1", "white", "firebrick1"), vertical.label = TRUE) {

  # Determine the class combinations based on the selected approach
  classes = unique(y)
  if (approch == "OVA") {
    combinations <- paste0(classes, "_vs_ALL")
  } else if (approch == "OVO") {
    combinations <- combn(classes, 2, function(x) paste0(x[1], "_vs_", x[2]))
  } else {
    stop("Invalid approach: choose 'OVA' or 'OVO'")
  }

  # Initialize a list to store the plots
  plot_list <- list()

  # Loop over each combination to create a plot
  for (i in seq_along(combinations)) {
    combination <- combinations[i]
    data <- feat.model.coeffs[[i]]$pop.noz  # Retrieve the coefficients for the i-th combination

    # Prepare the data for visualization
    if (topdown) {
      data <- t(data[nrow(data):1, ])
    } else {
      data <- t(data)
    }

    # Convert the data to long format for ggplot
    data.m <- reshape2::melt(data)
    colnames(data.m) <- c("models", "feature", "value")

    # Define colors based on the values (-1, 0, 1)
    col.n <- c("-1", "0", "1")
    tab.v <- table(data.m$value)
    if (length(tab.v) < 3) {
      col <- col[col.n %in% names(tab.v)]
    }

    # Create the plot for this combination
    p <- ggplot(data.m, aes(models, feature)) +
      geom_tile(aes(fill = value), colour = "darkgray") +
      theme_bw() +
      scale_fill_gradientn(colours = col) +
      ggtitle(paste(combination))  # Add the combination as the title

    # Adjust vertical or horizontal labels
    if (vertical.label) {
      p <- p + theme(legend.position = "none", axis.text = element_text(size = 9), axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      p <- p + theme(legend.position = "none", axis.text = element_text(size = 9))
    }

    # Add this plot to the list
    plot_list[[combination]] <- p
  }

  # Return the list of plots
  return(plot_list)
}

