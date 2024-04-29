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
  listindices <- mod$indices_
  listnames <- mod$names_
  listcoeffs <- mod$coeffs_
  listsign <- mod$sign_
  listscore <- mod$score_
  listpos <- mod$pos_score_
  listneg <- mod$neg_score_
  list_intercept <- mod$intercept_

  for(km in 1:length(listindices)){

    mod$indices_ <- listindices[[km]]
    mod$names_ <- listnames[[km]]
    mod$coeffs_ <- listcoeffs[[km]]
    mod$sign_ <-  listsign[[km]]
    mod$score_ <- listscore[[km]]
    mod$pos_score_ <- listpos[[km]]
    mod$neg_score_ <- listneg[[km]]
    mod$intercept_ <- list_intercept[[km]]

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
