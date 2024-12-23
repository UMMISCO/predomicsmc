plotPrevalence_mmc <- function(features, X, y, approch = "ova", topdown = TRUE,
                              main = "", plot = TRUE,
                              col.pt = c("deepskyblue4", "firebrick4"),
                              col.bg = c("deepskyblue1", "firebrick1")) {
  # Sort data
  resul <- sort_data(y, X)
  y <- resul$y
  X <- resul$X
  classes <- unique(y)
  list_y <- list()
  list_X <- list()
  plot_list <- list()

  # Determine class combinations based on the chosen approach
  combinations <- if (approch == "ova") {
    paste0(classes, "_vs_ALL")
  } else if (approch == "ovo") {
    combn(classes, 2, function(x) paste0(x[1], "_vs_", x[2]), simplify = TRUE)
  } else {
    stop("Invalid approach: choose 'ova' or 'ovo'")
  }

  # Fill the lists list_y and list_X
  for (i in seq_along(combinations)) {
    combination <- combinations[i]

    if (approch == "ova") {
      class_i <- gsub("_vs_ALL", "", combination)
      indices <- which(y == class_i | y != class_i)

      # Create the vector y for ova
      list_y[[i]] <- ifelse(y[indices] == class_i, 1, -1)
      list_X[[i]] <- X[, indices, drop = FALSE]
    } else {  # ovo approach
      class_i <- strsplit(combination, "_vs_")[[1]][1]
      class_j <- strsplit(combination, "_vs_")[[1]][2]
      indices <- which(y == class_i | y == class_j)

      # Set y to 1 for class_i and -1 for class_j
      list_y[[i]] <- ifelse(y[indices] == class_i, 1, -1)
      list_X[[i]] <- X[, indices, drop = FALSE]
    }
  }

  # Generate plots for each combination
  for (i in seq_along(combinations)) {
    class_combination <- combinations[i]
    class_1 <- if (approch == "ova") gsub("_vs_ALL", "", class_combination) else strsplit(class_combination, "_vs_")[[1]][1]
    class_minus1 <- if (approch == "ova") "ALL" else strsplit(class_combination, "_vs_")[[1]][2]

    # Generate the plot
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

    # Customize legend annotation
    legend_text <- paste0(class_1, ": Red | ", class_minus1, ": Blue")

    # Add annotations for the legend
    p <- p + annotate(
      "text", x = Inf, y = -Inf, label = legend_text,
      vjust = -1, hjust = 1, size = 4, color = "black"
    ) +
      theme(
        plot.margin = unit(c(1, 1, 2, 1), "cm"),  # Extra margin at the bottom for legend
        plot.caption = element_text(hjust = 0.5)  # Center the caption
      ) +
      labs(caption = legend_text)

    # Store the plot in the list
    plot_list[[class_combination]] <- p
  }

  # Arrange all plots in a grid
  arranged_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = ceiling(sqrt(length(plot_list))))

  return(arranged_plot)
}



plotAbundanceByClass_mmc <- function(features, X, y, approch = "ova",
                                    main = "", plot = TRUE,
                                    col.pt = c("deepskyblue4", "firebrick4"),
                                    col.bg = c("deepskyblue1", "firebrick1")) {

  # Sort data
  resul = sort_data(y, X)
  y <- resul$y
  X <- resul$X
  nClasse <- unique(y)
  list_y <- list()
  list_X <- list()
  plot_list <- list()

  # Determine class combinations based on the chosen approach
  if (approch == "ovo") {
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
  } else if (approch == "ova") {
    for (i in 1:length(nClasse)) {
      class_i <- nClasse[i]
      indices <- which(y == class_i | y != class_i)

      # Convert y to 1 for the selected class and -1 for others
      list_y[[i]] <- ifelse(y[indices] == class_i, 1, -1)
      list_X[[i]] <- X[, indices, drop = FALSE]
    }
  } else {
    stop("Invalid approach: choose 'ova' or 'ovo'")
  }

  # Generate the graphs for each combination with legends
  for (i in seq_along(list_y)) {
    class_label <- if (approch == "ova") {
      paste(nClasse[i], "vs ALL")
    } else {
      class_pair <- combn(nClasse, 2, simplify = TRUE)[, i]
      paste(class_pair[1], "vs", class_pair[2])
    }

    # Plot with the legend
    p <- plotAbundanceByClass(
      features = rownames(features[[i]]$pop.noz),
      X = list_X[[i]],
      y = list_y[[i]],
      main = paste(main, class_label),
      plot = plot,
      col.pt = col.pt,
      col.bg = col.bg
    )

    # Add legend annotation at the bottom of the plot
    legend_text <- if (approch == "ova") {
      paste0(nClasse[i], ": Red | ALL: Blue")
    } else {
      class_pair <- strsplit(class_label, " vs ")[[1]]
      paste0(class_pair[1], ": Red | ", class_pair[2], ": Blue")
    }

    # Adjust the plot with the legend text at the bottom
    p <- p + annotate(
      "text", x = Inf, y = -Inf, label = legend_text,
      vjust = -1, hjust = 1, size = 4, color = "black"
    ) +
      theme(
        plot.margin = unit(c(1, 1, 2, 1), "cm"),  # Extra margin at the bottom for the legend
        plot.caption = element_text(hjust = 0.5)  # Center the caption
      ) +
      labs(caption = legend_text)  # Use caption to add the legend text

    plot_list[[i]] <- p
  }

  # Use the number of combinations to set ncol for horizontal layout
  ncol <- 2  # You can adjust this based on how many plots you want in a row

  # Create the arranged grid of plots in one figure
  arranged_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = ncol)

  return(arranged_plot)
}











plotAUC_mcc <- function(scores, y, main = "", ci = TRUE, percent = TRUE, approch = "ovo") {
  require(pROC)

  # Initialize an empty list to store ROC objects
  roc_list <- list()
  scores_list <- list()
  scores_list = scores
  indices_sorted <- order(y)
  # Sort y using the indices
  y <- y[indices_sorted]
  nClasse <- unique(y)
  list_y <- list() # List of different combinations of y

  # Dataset decomposition phase using one-versus-one and one-versus-all approaches
  if (approch == "ovo") {
    k <- 1
    for (i in 1:(length(nClasse)-1)) {
      for (j in (i+1):length(nClasse)) {
        class_i <- nClasse[i]
        class_j <- nClasse[j]
        indices <- which(y == class_i | y == class_j)
        y_pair <- y[indices]
        list_y[[k]] <- as.vector(y_pair)
        k <- k + 1
      }
    }
  } else {
    for (i in 1:length(nClasse)) {
      class_i <- nClasse[i]
      y_temp <- ifelse(y == class_i, as.character(class_i), "All")
      list_y[[i]] <- as.vector(y_temp)
    }
  }

  # Setup plotting in a single figure
  par(mfrow = c(ceiling(sqrt(length(scores_list))), ceiling(length(scores_list) / ceiling(sqrt(length(scores_list))))))

  # Loop through the lists of scores and responses
  for (i in seq_along(scores_list)) {
    # Extract scores and responses
    score <- unlist(scores_list[[i]])
    y <- unlist(list_y[[i]])
    y <- as.factor(y)

    # Verify matching lengths
    if (length(score) != length(y)) {
      stop(paste("Error: The lengths of scores and responses for class", i, "do not match."))
    }

    # Compute the ROC for the current class
    rocobj <- roc(response = y, predictor = score, percent = percent, ci = ci, of = "se", sp = seq(0, 100, 5))

    # Obtain the label from unique values in list_y[[i]]
    class_label <- paste(unique(list_y[[i]]), collapse = " vs ")  # Create a label like "Bact1 vs Bact2"

    # Plot the ROC curve
    plot(rocobj, ci.type = "shape", ci.col = "grey80", main = paste(main, "Model:", class_label), col = "blue")

    # Compute optimal threshold information
    rocobj2 <- roc(response = y, predictor = score, percent = percent, ci = TRUE, of = "auc")
    resa <- coords(rocobj2, x = "best", input = "threshold", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))

    # Extract numerical values from `resa`
    if (is.data.frame(resa)) {  # If `resa` is a data.frame, extract the columns
      threshold <- as.numeric(resa$threshold[1])
      sensitivity <- as.numeric(resa$sensitivity[1])
      specificity <- as.numeric(resa$specificity[1])
    } else if (is.list(resa)) {  # If `resa` is a list
      threshold <- as.numeric(resa[["threshold"]])
      sensitivity <- as.numeric(resa[["sensitivity"]])
      specificity <- as.numeric(resa[["specificity"]])
    } else {
      stop("Error: Unexpected format of the `coords()` output.")
    }

    # Retrieve the confidence interval for AUC
    auc_ci <- ci.auc(rocobj)  # Calculate AUC confidence interval
    auc_ci_text <- if (ci) {
      paste0(signif(auc_ci[1], 3), " - ", signif(auc_ci[3], 3))
    } else {
      "N/A"
    }

    # Add threshold lines to the graph
    if (!is.na(threshold) && !is.na(sensitivity)) {
      abline(v = threshold, col = "red", lty = 2)  # Vertical line for the threshold
      abline(h = sensitivity, col = "red", lty = 2)  # Horizontal line for sensitivity
    }

    # Add a legend with AUC, CI, and threshold information (formatted in 3 lines)
    legend("bottomright", legend = c(
      paste("AUC:", signif(rocobj$auc, 3)),
      paste("CI:", auc_ci_text),
      paste("Threshold:", ifelse(!is.na(threshold), signif(threshold, 3), "N/A")),
      paste(class_label)   # Use the labels obtained from list_y[[i]]
    ), bty = "n")  # Remove the border around the legend

    # Store the ROC object for later analysis
    roc_list[[i]] <- rocobj
  }

  # Reset plot layout to default (single plot)
  par(mfrow = c(1, 1))

  # Return the list of ROC objects
  return(roc_list)
}





plotFeatureModelCoeffs_mmc <- function(feat.model.coeffs, y, approch = "ova",
                                      topdown = TRUE, col = c("deepskyblue1", "white", "firebrick1"),
                                      vertical.label = TRUE) {

  # Determine the class combinations based on the selected approach
  indices_sorted <- order(y)
  # Sorts y using the indices
  y <- y[indices_sorted]
  classes = unique(y)

  if (approch == "ova") {
    combinations <- paste0(classes, "_vs_ALL")
  } else if (approch == "ovo") {
    combinations <- combn(classes, 2, function(x) paste0(x[1], "_vs_", x[2]), simplify = TRUE)
  } else {
    stop("Invalid approach: choose 'ova' or 'ovo'")
  }

  # Initialize a list to store the plots
  plot_list <- list()

  # Loop over each combination to create a plot
  for (i in seq_along(combinations)) {
    combination <- combinations[i]
    data <- feat.model.coeffs[[i]]$pop.noz  # Retrieve the coefficients for the i-th combination

    # Prepare the data for visualization
    if (topdown) {
      data <- t(data[nrow(data):1, ])  # Reversing the row order for top-down display
    } else {
      data <- t(data)  # No reversal, use original order
    }

    # Convert the data to long format for ggplot
    data.m <- reshape2::melt(data)
    colnames(data.m) <- c("models", "feature", "value")

    # Define colors based on the values (-1, 0, 1)
    col.n <- c("-1", "0", "1")
    tab.v <- table(data.m$value)
    if (length(tab.v) < 3) {
      col <- col[col.n %in% names(tab.v)]  # Adjust colors based on the available values
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

    # Define the legend text for the combination
    legend_text <- if (approch == "ova") {
      paste0(classes[i], ": Red | ALL: Blue")
    } else {
      class_pair <- strsplit(combination, "_vs_")[[1]]
      paste0(class_pair[1], ": Red | ", class_pair[2], ": Blue")
    }

    # Add the legend as a caption at the bottom of the plot
    p <- p + theme(
      plot.margin = unit(c(1, 1, 2, 1), "cm"),  # Extra margin at the bottom for the legend
      plot.caption = element_text(hjust = 0.5)  # Center the caption
    ) +
      labs(caption = legend_text)  # Use caption to add the legend text

    # Add this plot to the list
    plot_list[[combination]] <- p
  }

  # Use the number of combinations to set ncol for horizontal layout
  ncol <- ceiling(sqrt(length(plot_list)))  # Use square root of the number of plots to determine columns

  # Create the arranged grid of plots
  arranged_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = ncol)

  # Return the arranged plot
  return(arranged_plot)
}



#' This function evaluates the models tests.
#' @title evaluatesBestsModelsTest
#' @param experiences experiences.
#' @param y y
#' @param X X
#' @param force.re.evaluation Boolean to force re-evaluation of the model even if it is already evaluated.
#' @param clf Object clf.
#' @param approach Type of approach to be used ("ovo" or "ova").
#' @param aggregation Type of aggregation method to be used ("votingAggregation", "weightedAggregation", "Predomics_aggregation_ova", "maximizationAggregation", "rankingAggregation").
#' @return The function returns a list best modesls test evaluated
#' @export
evaluatesBestsModelsTest <- function(experiences, approch = "ovo", aggregation_ = "votingAggregation", clf = clf, y = y, X = X) {

  bests_models_test <- list()
  clf <- regenerate_clf(clf, X, y, approch = approch)
  for (j in 1:length(experiences$classifier$models)) {

    lst_mod <- experiences$classifier$models[[j]]
    list_ <- list()

    for (i in 1:length(lst_mod)) {

      mod_test <- evaluateModel_mc(
        mod = lst_mod[[i]],
        X = X.test,
        y = y.test,
        clf = clf,
        eval.all = TRUE,
        force.re.evaluation = TRUE,
        approch = approch,
        aggregation_ = aggregation_,
        mode = "test"
      )

      list_[[i]] <- mod_test
    }

    accuracies <- sapply(list_, function(x) x$accuracy_)
    sorted_indices <- order(accuracies, decreasing = TRUE)
    sorted_models <- list_[sorted_indices]
    bests_models_test[[j]] <- sorted_models[[1]]
  }

  return(bests_models_test)
}

summarySE <- function(data, measurevar, groupvars, na.rm = FALSE, conf.interval = .95) {
  # Ensure that measurevar and groupvars are non-null and valid
  if (is.null(data) || is.null(measurevar) || is.null(groupvars)) {
    stop("Data, measurevar, and groupvars must be provided.")
  }

  data %>%
    dplyr::group_by(across(all_of(groupvars))) %>%
    dplyr::summarise(
      N = sum(!is.na(.data[[measurevar]]), na.rm = na.rm),

      sd = sd(.data[[measurevar]], na.rm = na.rm),
      value = mean(.data[[measurevar]], na.rm = na.rm),
      .groups = 'drop' # This will drop the grouping after summarise
    ) %>%
    dplyr::mutate(
      se = sd / sqrt(N),
      ci = if_else(N > 1,
                   se * qt(conf.interval/2 + 0.5, N - 1),
                   NA_real_  # Or your chosen default value for CI when N <= 1
      )
    )
}


#' listOfModels2ModelCollection
#'
#' @description Structures a list of predomics objects into a structured collection by k_sparsity.
#' @param pop: is population (a list) of predomics objects
#' @param nBest: number of elements to return for each sparsity (default:NA)
#' @return an model collection object
#' @export
listOfModels2ModelCollection <- function(pop, nBest = NA)
{
  # this is the old select_nBest_BySparsity while the first listOfModels2ModelCollection is deactivated
  spar <- populationGet_X(element2get = "eval.sparsity", toVec = TRUE, na.rm = TRUE)(pop)

  # get for every sparsity, the indices of the individuals that have this sparsity
  real.sparsity <- as.numeric(names(table(spar)))
  real.sparsity <- real.sparsity[real.sparsity != 0] # delete sparsity 0 if any

  # get the index of samples with that sparsity
  indivBySparsity <- lapply(real.sparsity,
                            function(x, spar)
                            {
                              which(spar == x)
                            }, spar)

  res <- lapply(seq_along(indivBySparsity),
                function(x, indivBySparsity)
                {
                  pop[indivBySparsity[[x]]]
                },
                indivBySparsity)
  names(res) <- lapply(real.sparsity, function(x) paste("k", x, sep = "_"))


  # selection
  if(!is.na(nBest))
  {
    res <- lapply(res, function(x) # for each k_sparsity
    {
      x <- sortPopulation(pop = x, evalToOrder = "fit_", decreasing = TRUE)

      # filter by number of elements we would like to keep
      if(length(x) > nBest)
        x <- x[1:nBest]
      return(x)
    })
  }

  return(res)
}


#' sparseVecToModel
#'
#' @description Builds a model object based on model that is in the sparse (short) format.
#' @param X: dataset
#' @param y: labels
#' @param v: A vector of indexes (example v=c(1,11))
#' @param clf: classifier information
#' @param eval.all: Should the model be evaluated (default:FALSE)
#' @param obj: an object model to add to the model (default:NULL)
#' @return an model object
sparseVecToModel <- function(X, y, v, clf, eval.all=FALSE, obj = NULL)
{
  if(is.null(v))
  {
    return(NULL)
  }
  res <- individual(X, y, clf, ind = v, eval.all = eval.all, obj = obj)
  return(res)
}

#' Computes the predected classification using a given model
#'
#' @description This function evaluates the predicted classification either using (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param score: the score passed directly
#' @param intercept: the intercept passed directly
#' @param sign: the sign passed directly
#' @return a vector with the predicted classification of the samples
evaluateYhat <- function(mod = NULL, X, y, clf, score=NULL, intercept=NULL, sign=NULL)
{
  if(!isModel(mod))
  {
    stop("evaluateYhat: please provide a valid model object BTR or SOTA.")
  }

  if(isModelSotaRF(mod))
  {
    yhat <- predict(object = mod$obj, t(X[mod$indices_,]))
  }

  if(isModelSotaSVM(mod))
  {
    yhat <- predict(object = mod$obj, t(X[mod$indices_,]))
  }

  if(isModelSotaGLMNET(mod))
  {
    yhat <- predict(object = mod$obj, s = mod$lambda, newx = t(X), type="class")[,1]
  }

  if(!isModelSota(mod)) # if BTR
  {
    # score_
    if(!myAssertNotNullNorNa(mod$score_))
    {
      scorelist <- getModelScore(mod = mod, X = X, clf = clf, force.re.evaluation = FALSE) # compute the score of the model
      score <- scorelist$score_

      if(is.null(score))
      {
        return(NULL)
      }

    } else
    {
      score       <- mod$score_
    }

    if(!myAssertNotNullNorNa(mod$intercept_) | !myAssertNotNullNorNa(mod$sign_))
    {
      mod <- evaluateIntercept(mod = mod, X = X, y = y, clf = clf)
      intercept <- mod$intercept_
      sign <- mod$sign_
    }else{
      intercept <- mod$intercept_
      sign <- mod$sign_
    }

    if(!myAssertNotNullNorNa(score, "missing score from evaluateYhat", stop = FALSE)) return(NULL)
    if(!myAssertNotNullNorNa(intercept, "missing intercept from evaluateYhat", stop = FALSE)) return(NULL)
    if(!myAssertNotNullNorNa(sign, "missing sign from evaluateYhat", stop = FALSE)) return(NULL)

    lev   <- levels(as.factor(clf$data$y))

    # NOTE: in the score we may have infite values that come for instance from the ratio language
    # This means that whatever the intercept these examples are positive ones. As such we can omit
    # them when computing the intercept.
    ind.infinite  <- is.infinite(mod$score_)
    ind.nan       <- is.nan(mod$score_)

    # For the ratio language
    # CASE 1
    # a/b > teta
    # a > teta * b
    # if b is infinite than whatever the teta the inequation is true
    # CASE 2
    # a/b < teta
    # a < teta * b
    # if b is infinite than whatever the teta the inequation is false

    if(mod$sign==">")
    {
      yhat.bool <- (score - intercept > 0)
      yhat.bool[ind.infinite] <- TRUE
      yhat.bool[ind.nan] <- FALSE # since 0 is not > than 0
      # compute the class
      yhat  <- factor(lev[as.factor(yhat.bool)], levels=lev)

    }else
    {
      yhat.bool <- (score - intercept < 0)
      yhat.bool[ind.infinite] <- FALSE
      yhat.bool[ind.nan] <- FALSE # since 0 is not < than 0
      # compute the class
      yhat  <- factor(lev[as.factor(yhat.bool)], levels=lev)
    }
  }

  return(yhat)
}

#' Compute other prediction scores such as precision, recall and f-score
#'
#' @description This function computes prediction scores based on the confusion matrix such as accuracy, precision, recall and f-score
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param mode: training or testing mode
#' @return a model whose evaluation parameters are updated
evaluateAdditionnalMetrics <- function(mod, X, y, clf, mode = "train")
{

  # if the following attributes are selected, then we need to fix it since they are derivates of a score
  if(clf$params$objective == "auc" & (clf$params$evalToFit != "fit_" | clf$params$evalToFit != "unpenalized_fit_"))
  {
    clf$params$evalToFit <- "accuracy_"
  }

  if(clf$params$evalToFit == "accuracy_") # additional metrics is auc_
  {
    # compute the auc
    aucg                 <- evaluateAUC(score = mod$score, y = y, sign = ">")
    aucl                 <- evaluateAUC(score = mod$score, y = y, sign = "<")
    mod$auc_             <- max(aucg, aucl)
  }

  if(clf$params$evalToFit == "auc_") # additional metrics is auc_
  {
    # evalute the accuracy that is not measured
    mod <- evaluateAccuracy(mod = mod, X = X, y = y, clf = clf, force.re.evaluation = TRUE, mode = mode)
  }

  if(!myAssertNotNullNorNa(mod$confusionMatrix_))
  {
    # visit this website for more information on the measures https://en.wikipedia.org/wiki/Precision_and_recall
    mod$confusionMatrix_ <- computeConfusionMatrix(mod, X, y, clf)

    if(is.null(mod$confusionMatrix_))
    {
      return(NULL)
    }
  }

  # precision = tp/(tp+fp)
  # cm = confusion matrix (2,2 is the positive class)
  if(all(dim(mod$confusionMatrix) == 2)) # if confusion matrix is OK (2x2)
  {
    mod$precision_ <- mod$confusionMatrix[2, 2] / (mod$confusionMatrix[2, 2] + mod$confusionMatrix[2, 1])
    #mod$precision_ <- mod$confusionMatrix[1, 1] / (mod$confusionMatrix[1, 1] + mod$confusionMatrix[2, 1])
    # recall = tp/(tp+fn), aka sensitivity
    #mod$recall_    <- mod$confusionMatrix[1, 1] / (mod$confusionMatrix[1, 1] + mod$confusionMatrix[1, 2])
    mod$recall_    <- mod$confusionMatrix[2, 2] / (mod$confusionMatrix[2, 2] + mod$confusionMatrix[1, 2])
    mod$f1_       <- 2 * (mod$precision_ * mod$recall_) / (mod$precision_ + mod$recall_)

  }else # otherwise we don't compute it
  {
    mod$precision_  <- NA
    mod$recall_     <- NA
    mod$f1_         <- NA
  }

  return(mod)
}

#' Computes the AUC of a model
#'
#' @description Computes the AUC of a model
#' @param score: the ^y score of the model
#' @param y: the response vector
#' @param sign: in which direction to make the comparison? "auto" (default): automatically define in which group
#' the median is higher and take the direction accordingly. ">": if the predictor values for the control group
#' are higher than the values of the case group (controls > t >= cases). "<": if the predictor values for the
#' control group are lower or equal than the values of the case group (controls < t <= cases).
#' @return an auc value
#' @importFrom pROC roc
evaluateAUC <- function(score, y, sign = '>')
{
  # NOTE: in the score we may have infite values that come for instance from the ratio language
  # This means that whatever the intercept these examples are positive ones. As such we can omit
  # them when computing the intercept.
  ind.infinite  <- is.infinite(score)
  ind.nan       <- is.nan(score)
  ind.filter    <- ind.infinite | ind.nan

  # if all the observations are not numbers return NA
  if(sum(ind.filter) == length(y))
  {
    return(NA)
  }

  score <- score[!ind.filter]
  y     <- y[!ind.filter]

  # if y does not contain exactly 2 levels, and if these don't have at least
  # 1 count then we don't compute the AUC
  if(length(table(y)) != 2 | any(table(y)==0))
  {
    auc <- NA

  }else # otherwise we compute it
  {
    rocobj <- suppressMessages(roc(response = y, predictor = score, direction = sign))
    auc <- as.numeric(rocobj$auc)
  }

  if(sign == "auto")
  {
    resg <- evaluateAUC(score = score, y = y, sign = ">")
    resl <- evaluateAUC(score = score, y = y, sign = "<")
    auc <- max(resg, resl)
  }

  return(auc)
}

#' Evaluates the accuracy of a model
#'
#' @description This function evaluates the accuracy of either (1) a model object that contains intercept and sign or (2) directly the attributes score, intercept, sign
#' @param mod: a model object to be used in the class prediction
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @param force.re.evaluation: evaluate again all the elements needed for accuracy (default:FALSE)
#' @param mode: training or test mode. If training, the funciton maximizes accuracy.
#' @return either (1) a model whose evaluation parameters are updated or (2) the accuracy
#' @export
evaluateAccuracy <- function(mod = NULL, X, y, clf, force.re.evaluation = FALSE, mode = "train")
{

  #If mod is not a valid model
  if(!isModel(obj = mod))
  {
    stop("evaluateAccuracy: please provide a valid model object BTR or SOTA")
  }else
  {
    # test if the confusion matrix exists
    if(!myAssertNotNullNorNa(mod$confusionMatrix_) | force.re.evaluation)
    {
      # NOTE: we consider that evaluateFit is the main function where we would have computed the score and intercept if force.re.evaluation.
      # here we need to update the confusionMatrix_

      # compute the score if it does not exist
      if(!myAssertNotNullNorNa(mod$score_))
      {
        scorelist       <- getModelScore(mod = mod, X = X, clf, force.re.evaluation = force.re.evaluation) # compute the score of the model
        mod$score_      <- scorelist$score_
        mod$pos_score_  <- scorelist$pos_score_
        mod$neg_score_  <- scorelist$neg_score_

        if(is.null(mod$score_))
        {
          return(NULL)
        }
      }

      # compute the intercept and/or the sign if they do not exist
      if((!myAssertNotNullNorNa(mod$intercept_) | !myAssertNotNullNorNa(mod$sign_)) & !isModelSota(mod))
      {
        mod <- evaluateIntercept(mod = mod, X = X, y = y, clf = clf)
      }

      if(!isModelSota(mod))
      {
        if(!myAssertNotNullNorNa(mod$intercept_)) return(NULL)
        if(!myAssertNotNullNorNa(mod$sign_)) return(NULL)
      }

      # compute the confusion matrix
      mod$confusionMatrix_ <- computeConfusionMatrix(mod, X, y, clf)

      if(is.null(mod$confusionMatrix_))
      {
        return(NULL)
      }
    } # end re-evaluation or missing confusion matrix

    # EXPERIMENTAL TODO: add other accuraciy (weighted)
    # compute a confusionMatrix that is weighted for a better AUC
    #props <- prop.table(table(y))
    #confusionMatrix_.prop <- apply(mod$confusionMatrix_, 1, fun <- function(x){x*rev(props)})
    # accuracy = (tp+tn)/(tp+fp+tn+fn)
    #a1 <- sum(diag(confusionMatrix_.prop)) / sum(confusionMatrix_.prop)
    # EXPERIMENTAL

    if(mode == "train")
    {
      a1 <- sum(diag(mod$confusionMatrix_)) / sum(mod$confusionMatrix_)
      a2 <- sum(diag(apply(mod$confusionMatrix_, 2, rev))) / sum(mod$confusionMatrix_)

      if(a1 < a2  & !isModelSota(mod))
      {
        # inverse the sign
        if(mod$sign_ == ">")
        {
          mod$sign_ <- "<"
        }else
        {
          mod$sign_ <- ">"
        }

        # and recompute everything. This works with a recursive calling but due to R limits recursive calling in some deep cases
        # throws errors. This is why we are recoding another solution
        # mod <- evaluateAccuracy(mod = mod, X = X, y = y, force.re.evaluation = force.re.evaluation, mode = mode)

        # recompute the confusion matrix
        mod$confusionMatrix_ <- computeConfusionMatrix(mod, X, y, clf)
        if(is.null(mod$confusionMatrix_))
        {
          return(NULL)
        }

        # and accuracy
        mod$accuracy_ <- sum(diag(mod$confusionMatrix_)) / sum(mod$confusionMatrix_)
      }else
      {
        mod$accuracy_ <- a1
      }
    }else
    {
      mod$accuracy_ <- sum(diag(mod$confusionMatrix_)) / sum(mod$confusionMatrix_)
    }
  } # end train/test

  return(mod)
}





#' Evaluates the fitting score of a model object
#'
#' @description Evaluates the fitting score of a model object.
#' @param mod : a model object
#' @param X: the data matrix with variables in the rows and observations in the columns
#' @param y: the response vector
#' @param clf: the classifier parameter object
#' @return a model object with the fitting score
evaluateIntercept <- function(mod, X, y, clf)
{
  if(!isModel(mod))
  { # if model is not an object
    if(is.character(mod))
    { # if model is of the form of variable names
      mod <- index2names(X, mod)
    }
    mod <- individual(X, y, clf = clf, ind = mod)
  }

  if(isModelSota(mod))
  {
    if(clf$params$warnings) warning("evaluateIntercept: no intercept for sota models. Returning unchanged.")
    return(mod)
  }

  if((clf$params$intercept=="NULL"))
  {

    # compute the fitting scores depending on the method
    if(!myAssertNotNullNorNa(mod$score_))
    {
      scorelist <- getModelScore(mod = mod, X = X, clf = clf)
      mod$score_ <- scorelist$score_
      mod$pos_score_ <- scorelist$pos_score_
      mod$neg_score_ <- scorelist$neg_score_

      if(is.null(mod$score_))
      {
        return(NULL)
      }
    }

    # NOTE: in the score we may have infite values that come for instance from the ratio language
    # This means that whatever the intercept these examples are positive ones. As such we can omit
    # them when computing the intercept.
    ind.infinite  <- is.infinite(mod$score_)
    ind.nan       <- is.nan(mod$score_)

    score         <- mod$score_
    # the ind.infinite scores should be replaced by a big value let say max(X)+1
    try(score[ind.infinite] <- clf$data$X.max + 1, silent = TRUE)
    # the ind.nan should be replaced by a small value for instance min(X)-1
    try(score[ind.nan] <- clf$data$X.min - 1, silent = TRUE)


    switch(clf$params$objective,
           auc={ # THE AUC objective
             interc             <- computeIntercept(score = score, y = y, clf)
             mod$intercept_     <- interc$intercept
             mod$sign_          <- interc$sign
           },
           cor={
             mod$intercept_     <- "NULL"
             mod$sign_          <- "NULL"
           },
           {
             if(clf$params$warnings) warning("evaluateIntercept: This objective does not exist !")
           }
    )
  } else
  {
    mod$intercept_ <- clf$params$intercept
    mod$sign_ <- ">"
  }

  return(mod)
}


#' Evaluates the confusion Matrix of the predicted class and the class to predict
#'
#' @description This function evaluates the accuracy of a model
#' @param mod: a model object to be evaluated
#' @param X: dataset to classify
#' @param y: variable to predict
#' @param clf: an object containing the different parameters of the classifier
#' @return a confusion matrix
computeConfusionMatrix <- function(mod, X, y, clf)
{
  yhat <- evaluateYhat(mod = mod, X = X, y = y, clf = clf)
  if(is.null(yhat))
  {
    return(NULL)
  }
  # yhat <- factor(yhat, levels = names(table(clf$data$y)))

  if(length(yhat) != length(y))
  {
    return(NULL)
  }
  cm <- table(y, yhat, dnn = c("y", "yhat"))

  return(cm)
}


