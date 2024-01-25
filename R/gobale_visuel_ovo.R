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
                      res_clf = NULL, approch="ovo")
{

  list_y <- list()
  list_X <- list()
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
      y_temp <- ifelse(y == class_i, as.character(class_i), "Rest")
      list_y[[i]] <- as.vector(y_temp)
      list_X[[i]] <- X
    }
  }

  list_g <- list()
  listindices <- list()
  listnames <- list()
  listcoeffs <- list()
  listsign <- list()
  listscore <- list()
  listpos <- list()
  listneg <- list()
  listconf <- list()
  listindices <- mod$indices_
  listnames <- mod$names_
  listcoeffs <- mod$coeffs_
  listsign <- mod$sign_
  listscore <- mod$score_
  listpos <- mod$pos_score_
  listneg <- mod$neg_score_
  listconf <- mod$confusionMatrix_



  for(km in 1:length(listindices)){

    mod$indices_ <- listindices[[km]]
    mod$names_ <- listnames[[km]]
    mod$coeffs_ <- listcoeffs[[km]]
    mod$sign_ <-  listsign[[km]]
    mod$score_ <- listscore[[km]]
    mod$pos_score_ <- listpos[[km]]
    mod$neg_score_ <- listneg[[km]]
    mod$confusionMatrix_ <- listconf[[km]]
    y = list_y[[km]]
    X = list_X[[km]]



  # test model validity
  if(!isModel(obj = mod))
  {
    print("plotModel: The model object is not valid!")
    return(NULL)
  }

  if(length(col.sign) != 2)
  {
    print("plotModel: please provide 2 colors for the ternary coefficients excluding zero")
    return(NULL)
  }

  # disable importance for SOTA
  if(isModelSota(mod) & importance)
  {
    importance <- FALSE
    print("plotModel: importance graphic is disabled for SOTA models")
  }

  if(!isModelSotaRF(mod))
  {

    # Fix display names
    if(isModelSotaSVM(mod))
    {
      mod$learner <- "sota"
    }

    # reset model attributes for glmnet for proper viewing
    if(mod$learner == "terda" & mod$language == "logreg")
    {
      mod$learner <- "sota"
      mod$language <- "glmnet"
    }

    if(sort.features)
    {
      if(is.null(sort.ind))
      {
        # get the order of the features in terms of discriminance compared to the class.
        ind       <- order(filterfeaturesK(data = X, trait = y, k=nrow(X), sort = FALSE)$p, decreasing = FALSE)
      }else
      {
        ind       <- sort.ind
      }
    }else # no order (use the default X ordering)
    {
      ind         <- c(1:nrow(X))
    }

    # get the normalized coefficients
    coeffsl <- normModelCoeffs(mod = mod, X = X, y = y, sort.features = sort.features, sort.ind = ind)
    # and the sparsity
    k_sparsity <- mod$eval.sparsity
    #k_sparsity <- k_sparsity[!unlist(lapply(coeffsl,is.null))]
    #tmp <- unlist(lapply(coeffsl,length))
    coeffs.name <- c()
    coeffs.nr   <- c()

    coeffs.name  <- rep(mod$learner,length(coeffsl))
    coeffs.nr    <- c(1:length(coeffsl))

    coeffs.data         <- data.frame(coeffsl, coeffs.name, coeffs.nr)
    colnames(coeffs.data) <- c("coeff","classifier","feature")
    coeffs.data$coeff   <- as.numeric(coeffs.data$coeff)
    coeffs.data$sign    <- sign(coeffs.data$coeff)
    coeffs.data$sign[coeffs.data$sign == 0] <- NA
    coeffs.data$col     <- col.sign[factor(coeffs.data$sign, levels = c(-1,1))]

    # add information on importance
    rownames(coeffs.data) <- rownames(X)[ind]
    coeffs.data$importance <- 0
    coeffs.data$importance.col <- NA

    if(!is.null(mod$mda.cv_))
    {
      coeffs.data[mod$names_,]$importance <- mod$mda.cv_
      coeffs.data[mod$names_,]$importance.col <- "black"
    }

    # get the features
    features <- rownames(coeffs.data)[which(!is.na(coeffs.data$sign))]

    names(col.sign) <- c("-1","1")
    col.sign <- as.character(col.sign[names(table(sign(coeffs.data$coeff[coeffs.data$coeff != 0])))])

    # make the main plot
    g1 <- ggplot(coeffs.data, aes(feature, coeff, fill=col)) +
      geom_bar(stat="identity", position="dodge") + ylim(-1, 1) +
      xlab("") +
      theme(legend.position="none", axis.text=element_text(size=9)) +
      scale_fill_manual("Sign", values = col.sign) +
      geom_hline(yintercept = 0, col="gray") +
      theme_bw() + guides(fill = "none") +
      coord_flip() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "white", fill = "white", linewidth = 0.3),
        strip.text.x = element_text(colour = "darkred", size = 10))

    if(feature.name)
    {
      g1 <- g1 + scale_x_continuous(breaks=which(!is.na(coeffs.data$sign)), labels = features) + theme(aspect.ratio = 2)
    }else
    {
      g1 <- g1 + scale_x_continuous(breaks=which(!is.na(coeffs.data$sign)))
    }

    if(!slim)
    {
      if(main == "")
      {
        main <- paste("alg:", mod$learner, " | lang:",mod$language," | k:",mod$eval.sparsity, sep="")
      }
      g1 <- g1 +
        ggtitle(main)
    }else
    {
      if(main == "")
      {
        main <- paste(mod$learner, "|",mod$language,"|",mod$eval.sparsity, sep="")
      }

      g1 <- g1 +
        ggtitle(main) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank())
    }

    # make the plot on importance
    if(!is.null(mod$mda.cv_) & importance)
    {
      importance.se <- FALSE
      if(!is.null(res_clf))
      {
        if(!is.null(res_clf$crossVal$fip))
        {
          importance.se <- TRUE
        }
      }

      if(importance.se)
      {

        mdacv <- mergeMeltImportanceCV(list.results = list(exp = res_clf),
                                       filter.cv.prev = 0,
                                       min.kfold.nb = FALSE,
                                       learner.grep.pattern = "*",
                                       nb.top.features = NULL,
                                       feature.selection = features,
                                       scaled.importance = FALSE,
                                       make.plot = TRUE,
                                       main = TRUE,
                                       cv.prevalence = FALSE)
        g2 <- mdacv$g + theme(aspect.ratio = 2) + ggtitle("mda|cv")

      }else
      {
        g2 <- ggplot(coeffs.data, aes(feature, importance, fill=importance.col)) +
          geom_bar(stat="identity", position="dodge") +
          theme(legend.position="none", axis.text=element_text(size=9)) +
          scale_fill_manual("Sign", values = "black") +
          xlab("") +
          geom_hline(yintercept = 0, col="gray") +
          theme_bw() + guides(fill = "none") +
          coord_flip() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.background = element_rect(colour = "white", fill = "white", linewidth = 0.3),
            strip.text.x = element_text(colour = "darkred", size = 10)
          )

        if(feature.name )
        {
          g2 <- g2 + scale_x_continuous(breaks=which(!is.na(coeffs.data$sign)), labels = features) + theme(aspect.ratio = 2) +
            ggtitle("mda|cv")

        }else
        {
          g2 <- g2 + scale_x_continuous(breaks=which(!is.na(coeffs.data$sign)))

          if(slim)
          {
            g2 <- g2 +
              ggtitle("mda|cv") +
              ylim(0, 1) +
            ##theme(panel.grid.major = element_rect(size = 0.5))
              theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                strip.background = element_rect(colour = "white", fill = "white", linewidth =  0.3),
                strip.text.x = element_text(colour = "darkred", size = 10),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.text.y=element_blank()
              )
          }else
          {
            g2 <- g2 +
              ggtitle("mda|cv")
          }

        } # end zoom in features
      } # end CV mda plot type

      #return(grid.arrange(g, g2, ncol=2, widths = c(2,1)))
      return(g2)
    }else # end else plot importance
    {
      warning("plotModel: importance is not available in this model, returning model plot")
      return(g1)
    }
  }else
  {
    # RANDOM FOREST MODEL
    mod$learner <- "sota"

    if(main == "")
    {
      main <- paste(mod$learner, "|",mod$language,"|",mod$eval.sparsity, sep="")
    }

    if(!is.null(mod$obj))
    {
      g <- tree_func(final_model = mod$obj, tree_num = 1, main = main, node.text.color = "black")
    }else
    {
      # return an empty graph
      df <- data.frame(
        x = rep(1, 1),
        y = rep(1, 1),
        z = rep(1, 1)
      )

      g <- ggplot(df, aes(x, y, fill = "black")) +
        geom_tile() +
        ggtitle(main) +
        #theme(legend.position="none", axis.text=element_text(size=9)) +
        scale_fill_manual("Sign", values = "lightgray") +
        theme_bw() + guides(fill = "none") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              #panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank()#,
              #plot.background=element_blank()
        ) +
        coord_flip()
    }
  }

    list_g[[km]] <- g

  }
  g <- list()
  g <- list_g
  return(g)
}













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

  # if(isModelSota(mod))
  # {
  #   method <- "sota"
  # }
  list_res <- list()
  listindices <- list()
  listnames <- list()
  listcoeffs <- list()
  listsign <- list()
  listscore <- list()
  listpos <- list()
  listneg <- list()
  listconf <- list()
  listindices <- mod$indices_
  listnames <- mod$names_
  listcoeffs <- mod$coeffs_
  listsign <- mod$sign_
  listscore <- mod$score_
  listpos <- mod$pos_score_
  listneg <- mod$neg_score_
  listconf <- mod$confusionMatrix_

  for(km in 1:length(listindices)){

    mod$indices_ <- listindices[[km]]
    mod$names_ <- listnames[[km]]
    mod$coeffs_ <- listcoeffs[[km]]
    mod$sign_ <-  listsign[[km]]
    mod$score_ <- listscore[[km]]
    mod$pos_score_ <- listpos[[km]]
    mod$neg_score_ <- listneg[[km]]
    mod$confusionMatrix_ <- listconf[[km]]

  switch(method,
         short={
           if(!isModelSota(mod))
           {
             if(all(myAssertNotNullNorNa(mod$coeffs_)))
             {
               # Bin(inter) or Ter(inter)
               ind.pos <- sign(mod$coeffs_) == 1
               ind.neg <- sign(mod$coeffs_) == -1

               # BTR
               if(mod$language == "ratio")
               {
                 signs <- rep("+",length(mod$coeffs_))
                 if(all(!ind.pos))
                 {
                   term.pos <- "0"
                 }else
                 {
                   term.pos <- paste("(",paste(signs[ind.pos], mod$indices_[ind.pos], sep = "", collapse = ""),")",sep="")
                 }

                 if(all(!ind.neg))
                 {
                   term.neg <- "0"
                 }else
                 {
                   term.neg <- paste("(",paste(signs[ind.neg], mod$indices_[ind.neg], sep = "", collapse = ""),")",sep="")
                 }
                 res <- paste(term.pos, "/", term.neg)
                 mod.inter <- paste(mod$sign_, signif(mod$intercept_,2))
                 decision <- paste(" then class =", colnames(mod$confusionMatrix_)[2])
                 mod.fit <- mod[[score]]
                 if(!is.na(mod.fit)) res <- paste("|",res," ", mod.inter, decision, "| (F=",signif(mod.fit,4),sep="")
                 res <- paste(res,"|K=",mod$eval.sparsity,"|Le=",  mod$learner,"|La=", mod$language,")",sep="")
               }else
               {
                 # Bin(inter) or Ter(inter)
                 signs <- rep("+",length(mod$coeffs_))
                 if(all(!ind.pos))
                 {
                   term.pos <- "0"
                 }else
                 {
                   term.pos <- paste("(",paste(signs[ind.pos], mod$indices_[ind.pos], sep = "", collapse = ""),")",sep="")
                 }

                 if(all(!ind.neg))
                 {
                   term.neg <- "0"
                 }else
                 {
                   term.neg <- paste("(",paste(signs[ind.neg], mod$indices_[ind.neg], sep = "", collapse = ""),")",sep="")
                 }
                 res <- paste(term.pos, " - ", term.neg)
                 mod.inter <- paste(mod$sign_, signif(mod$intercept_,2))
                 decision <- paste(" then class =",colnames(mod$confusionMatrix_)[2])
                 mod.fit <- mod[[score]]
                 if(!is.na(mod.fit)) res <- paste("|",res," ", mod.inter, decision, "| (F=",signif(mod.fit,4),sep="")
                 res <- paste(res,"|K=",mod$eval.sparsity,"|Le=",  mod$learner,"|La=", mod$language,")",sep="")
               }
             } # end exist coeffs
           }else
           {
             # SOTA
             if(!is.null(mod$coeffs_))
             {
               coeffs <- signif(as.numeric(mod$coeffs_),2)
             }else{
               coeffs <- ""
             }
             res <- mod$indices_
             res <- paste(coeffs, res, sep = " * ")
             res <- paste(res, collapse = " ")
             res <- paste(res, mod$learner, sep="|")
             mod.fit <- mod[[score]]
             if(!is.na(mod.fit)) res <- paste("|",res," ","| (F=",signif(mod.fit,4),sep="")
             res <- paste(res,"|K=",mod$eval.sparsity,"|Le=",  mod$learner,"|La=", mod$language,")",sep="")
           }
         },
         long={
           if(!isModelSota(mod))
           {
             if(all(myAssertNotNullNorNa(mod$coeffs_)))
             {
               # Bin(inter) or Ter(inter)
               ind.pos <- sign(mod$coeffs_) == 1
               ind.neg <- sign(mod$coeffs_) == -1

               # BTR
               if(mod$language == "ratio")
               {
                 signs <- rep("+ ",length(mod$coeffs_))
                 if(all(!ind.pos))
                 {
                   term.pos <- "0"
                 }else
                 {
                   term.pos <- paste("(",paste(signs[ind.pos], mod$names_[ind.pos], sep = "", collapse = ""),")",sep="")
                 }

                 if(all(!ind.neg))
                 {
                   term.neg <- "0"
                 }else
                 {
                   term.neg <- paste("(",paste(signs[ind.neg], mod$names_[ind.neg], sep = "", collapse = ""),")",sep="")
                 }
                 res <- paste(term.pos, "/", term.neg)
                 mod.inter <- paste(mod$sign_, signif(mod$intercept_,2))
                 decision <- paste(" then class =", colnames(mod$confusionMatrix_)[2])
                 mod.fit <- mod[[score]]
                 if(!is.na(mod.fit)) res <- paste("|",res," ", mod.inter, decision, "| (F=",signif(mod.fit,4),sep="")
                 res <- paste(res,"|K=",mod$eval.sparsity,"|Le=",  mod$learner,"|La=", mod$language,")",sep="")
               }else
               {
                 # Bin(inter) or Ter(inter)
                 signs <- rep("+ ",length(mod$coeffs_))
                 if(all(!ind.pos))
                 {
                   term.pos <- "0"
                 }else
                 {
                   term.pos <- paste("(",paste(signs[ind.pos], mod$names_[ind.pos], sep = "", collapse = ""),")",sep="")
                 }

                 if(all(!ind.neg))
                 {
                   term.neg <- "0"
                 }else
                 {
                   term.neg <- paste("(",paste(signs[ind.neg], mod$names_[ind.neg], sep = "", collapse = ""),")",sep="")
                 }
                 res <- paste(term.pos, " - ", term.neg)
                 mod.inter <- paste(mod$sign_, signif(mod$intercept_,2))
                 decision <- paste(" then class =",colnames(mod$confusionMatrix_)[2])
                 mod.fit <- mod[[score]]
                 if(!is.na(mod.fit)) res <- paste("|",res," ", mod.inter, decision, "| (F=",signif(mod.fit,4),sep="")
                 res <- paste(res,"|K=",mod$eval.sparsity,"|L=",  mod$learner,"|La=", mod$language,")",sep="")
               }
             } # end exist coeffs
           }else
           {
             # SOTA
             if(!is.null(mod$coeffs_))
             {
               coeffs <- signif(as.numeric(mod$coeffs_),2)
             }else{
               coeffs <- ""
             }
             res <- mod$names_
             res <- paste(coeffs, res, sep = " * ")
             res <- paste(res, collapse = " ")
             res <- paste(res, mod$learner, sep="|")
             mod.fit <- mod[[score]]
             res <- paste("|",res," ","| (F=",signif(mod.fit,4),sep="")
             res <- paste(res,"|K=",mod$eval.sparsity,"|L=",  mod$learner,"|La=", mod$language,")",sep="")
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













