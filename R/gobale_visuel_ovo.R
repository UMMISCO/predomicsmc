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
 # for(i in 1: length(list_matrices)){
   # list_accura[[i]] =  sum(diag(list_matrices[[i]])) / sum(list_matrices[[i]])
 # }


  for(km in 1:length(listindices)){

    mod$indices_ <- listindices[[km]]
    mod$names_ <- listnames[[km]]
    mod$coeffs_ <- listcoeffs[[km]]
    mod$sign_ <-  listsign[[km]]
    mod$score_ <- listscore[[km]]
    mod$pos_score_ <- listpos[[km]]
    mod$neg_score_ <- listneg[[km]]
    mod$intercept_ <- list_intercept[[km]]
    ###mod$fit_ <- list_accura[[km]]
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
                         res_clf = NULL,approch = "ova")
{
  # Création de la liste vide pour stocker les sous-modèles
  list_mod <- list()
  plot_sub_model <- list()

  # Boucle pour remplir la liste de sous-modèles
  for (i in 1:length(mod$names_)) {
    # Initialiser une liste pour chaque sous-modèle
    list_mod[[i]] <- list()

    # Assigner les éléments de 'mod' aux sous-modèles
    list_mod[[i]]$learner <- mod$learner
    list_mod[[i]]$language <- mod$language
    list_mod[[i]]$objective <- mod$objective
    list_mod[[i]]$indices_ <- mod$indices_[[i]]
    list_mod[[i]]$names_ <- mod$names_[[i]]
    list_mod[[i]]$coeffs_ <- mod$coeffs_[[i]]
    list_mod[[i]]$fit_ <- mod$fit_
    list_mod[[i]]$unpenalized_fit_ <-mod$unpenalized_fit_
    list_mod[[i]]$auc_ <- mod$auc_
    list_mod[[i]]$accuracy_ <- mod$accuracy_
    list_mod[[i]]$cor_ <- mod$cor_
    list_mod[[i]]$aic_ <- mod$aic_
    list_mod[[i]]$intercept_ <- mod$list_intercept_[[i]]
    list_mod[[i]]$eval.sparsity <- mod$eval.sparsity
    list_mod[[i]]$precision_ <- mod$precision_
    list_mod[[i]]$recall_ <- mod$recall_
    list_mod[[i]]$f1_ <- mod$f1_
    list_mod[[i]]$sign_ <- mod$sign_[[i]]
    list_mod[[i]]$rsq_ <- mod$rsq_[[i]]
    list_mod[[i]]$ser_ <- mod$ser_[[i]]
    list_mod[[i]]$score_ <- mod$score_[[i]]
    list_mod[[i]]$mda.cv_ <- mod$mda.cv_[[i]]
    list_mod[[i]]$prev.cv_ <- mod$prev.cv_[[i]]
    list_mod[[i]]$mda_ <- mod$mda_[[i]]
  }

  nClasse <- unique(y)
  list_y <- list()   # List for different combinations of y
  list_X <- list()   # List for different combinations of X

  if (approch == "ovo") {
    k <- 1
    for (i in 1:(length(nClasse) - 1)) {
      for (j in (i + 1):length(nClasse)) {
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

  for (i in 1:length(list_y)){
    plot_sub_model[[i]] <- plotModel(mod = list_mod[[i]], X = list_X[[i]], y=list_y[[i]],
                                     sort.features = sort.features,
                                     feature.name = feature.name, importance = importance)
  }

  return(plot_sub_model)
}


#' Analyze the results from a list of classifiers
#'
#' @description Analyze the results from a list of classifiers.
#' @param scores: a list where each element is a vector of scores from a given model
#' @param y: the class to be predicted
#' @param main: title of the graph
#' @param ci: the point shape for the graph
#' @param percent: color for the graph
#' @return a list of roc objects
#' @export
plotAUC_mc <- function(scores, y, main="", ci = TRUE, percent = TRUE, approch = "ova")
{
  require(pROC)

  # Initialize an empty list to store ROC objects
  roc_objects <- list()

  # Create a plot with appropriate title
  plot(NULL, xlim=c(0, 100), ylim=c(0, 100), xlab="False Positive Rate", ylab="True Positive Rate", main=main, type="n")

  # Loop through each set of scores
  for (i in seq_along(scores)) {
    score <- scores[[i]]

    # Calculate ROC object
    rocobj <- roc(response = y, predictor = score, percent = percent, ci = ci, of = "se", sp = seq(0, 100, 5))
    roc_objects[[i]] <- rocobj

    # Plot ROC curve
    plot(rocobj, add=TRUE, col=i)
  }

  # Add legend
  legend("bottomright", legend = paste("Model", seq_along(scores)), col = seq_along(scores), lty = 1)

  # Add information on the threshold for each model
  for (i in seq_along(roc_objects)) {
    rocobj2 <- roc_objects[[i]]
    resa <- coords(rocobj2, x = "best", input = "threshold", best.method = "youden")
    abline(v=resa[2], col="red", lty=2)
    abline(h=resa[3], col="red", lty=2)
  }

  return(roc_objects)
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
plotAbundanceByClass_mc <- function(features, X, y, topdown = TRUE,
                                    main = "", plot = TRUE,
                                    col.pt = c("deepskyblue4", "firebrick4"),
                                    col.bg = c("deepskyblue1", "firebrick1"), approch = "ovo")
{
  nClasse <- unique(y)  # Récupère les classes uniques
  list_y <- list()   # Liste pour les différentes combinaisons de y
  list_X <- list()   # Liste pour les différentes combinaisons de X
  list_plot <- list()  # Liste pour stocker les graphiques

  # Si l'approche est one-vs-one (ovo)
  if (approch == "ovo") {
    k <- 1
    for (i in 1:(length(nClasse) - 1)) {
      for (j in (i + 1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)  # Sélection des indices pour les deux classes
        y_pair <- y[indices]  # Récupère les labels pour les deux classes
        X_pair <- X[, indices]  # Récupère les données correspondantes
        list_y[[k]] <- as.vector(y_pair)  # Ajoute le vecteur y dans la liste
        list_X[[k]] <- X_pair  # Ajoute les données X dans la liste
        k <- k + 1
      }
    }
  } else {  # Pour les autres approches (e.g. one-vs-all)
    for (i in 1:length(nClasse)) {
      class_i <- nClasse[i]
      y_temp <- ifelse(y == class_i, as.character(class_i), "All")  # Crée un vecteur one-vs-all
      list_y[[i]] <- as.vector(y_temp)  # Ajoute ce vecteur à la liste
      list_X[[i]] <- X  # Ajoute toutes les données X (elles ne changent pas pour one-vs-all)
    }
  }

  # Générer les graphiques
  for (i in 1:length(list_y)) {
    list_plot[[i]] <- plotAbundanceByClass(
      features = rownames(features[[i]]$pop.noz),
      X = list_X[[i]],
      y = list_y[[i]],
      topdown = topdown,  # Transmet l'argument topdown
      main = main,  # Transmet l'argument main
      plot = plot,  # Transmet l'argument plot
      col.pt = col.pt,  # Transmet les couleurs pour les points
      col.bg = col.bg   # Transmet les couleurs pour le fond
    )
  }

  return(list_plot)  # Retourne la liste des graphiques
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
plotPrevalence_mc <- function(features, X, y, topdown = TRUE, main = "", plot = TRUE,
                           col.pt = c("deepskyblue4", "firebrick4"),
                           col.bg = c("deepskyblue1", "firebrick1"),
                           zero.value = 0, approch="ovo")
{
  nClasse <- unique(y)  # Récupère les classes uniques
  list_y <- list()   # Liste pour les différentes combinaisons de y
  list_X <- list()   # Liste pour les différentes combinaisons de X
  list_plot <- list()  # Liste pour stocker les graphiques

  # Si l'approche est one-vs-one (ovo)
  if (approch == "ovo") {
    k <- 1
    for (i in 1:(length(nClasse) - 1)) {
      for (j in (i + 1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)  # Sélection des indices pour les deux classes
        y_pair <- y[indices]  # Récupère les labels pour les deux classes
        X_pair <- X[, indices]  # Récupère les données correspondantes
        list_y[[k]] <- as.vector(y_pair)  # Ajoute le vecteur y dans la liste
        list_X[[k]] <- X_pair  # Ajoute les données X dans la liste
        k <- k + 1
      }
    }
  } else {  # Pour les autres approches (e.g. one-vs-all)
    for (i in 1:length(nClasse)) {
      class_i <- nClasse[i]
      y_temp <- ifelse(y == class_i, as.character(class_i), "All")  # Crée un vecteur one-vs-all
      list_y[[i]] <- as.vector(y_temp)  # Ajoute ce vecteur à la liste
      list_X[[i]] <- X  # Ajoute toutes les données X (elles ne changent pas pour one-vs-all)
    }
  }

  # Générer les graphiques
  for (i in 1:length(list_y)) {
    list_plot[[i]] <- plotPrevalence(
      features = rownames(features[[i]]$pop.noz),
      X = list_X[[i]],
      y = list_y[[i]],
      topdown = topdown,  # Transmet l'argument topdown
      main = main,  # Transmet l'argument main
      plot = plot,  # Transmet l'argument plot
      col.pt = col.pt,  # Transmet les couleurs pour les points
      col.bg = col.bg,  # Transmet les couleurs pour le fond
      zero.value = 0
    )
  }

  return(list_plot)  # Retourne la liste des graphiques
}

