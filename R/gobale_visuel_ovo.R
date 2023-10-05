################################################################
# SINGLE MODEL PLOTS ONE VERSUS ONE
################################################################

#' @title Plots a model or a population of model objectsas barplots of scaled coefficients.
#'
#' @description Plots a model or a population one versus one of models as a barplots, representing each feature, the length being the coefficient
#' @param mod: a model to plot
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the class vector multi class
#' @param sort.features: wether the features need to be sorted by correlation with 'y' or not (default: TRUE)
#' @param sort.ind: computing sorting can take time if computed for every model and can be computed outside the function and passed as a parameter
#' @param feature.name: show the name of the features (default:FALSE)
#' @param col.sign: the colors of the cofficients based on the sign of the coefficients (default: -1=deepskyblue1, 1:firebrick1)
#' @param main: possibility to change the title of the function (default:"")
#' @param slim: plot without axis information (default:FALSE)
#' @param importance: the importance (mda) of the features in crossval
#' @param res_clf: the result of the learning process (default:NULL). If provided information on MDA will be extracted for the importance graphic.
#' @export
plotModel_ovo <- function(mod, X, y,
                      sort.features = FALSE,
                      sort.ind = NULL,
                      feature.name = FALSE,
                      col.sign = c("deepskyblue1", "firebrick1"),
                      main = "",
                      slim = FALSE,
                      importance = FALSE,
                      res_clf = NULL)
{
#One-versus-one class distribution
  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
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

  # we recover the first output to apply the plot
  X <- list_X[[1]]
  y <- list_y[[1]]

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
        strip.background = element_rect(colour = "white", fill = "white", size = 0.3),
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
            strip.background = element_rect(colour = "white", fill = "white", size = 0.3),
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
              theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                strip.background = element_rect(colour = "white", fill = "white", size = 0.3),
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

  return(g)
}

