# Load required libraries
library(mcpredomics)
library(predomics)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(randomForest)
library(caret)
library(gtools)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tibble)
library(knitr)
library(kableExtra)
library(DT)
library(e1071)
library(glmnet)
library(testthat)

# Load dataset included in the mcpredomics package
data("mc.input", package = "mcpredomics")

# ------------------------ Preprocessing ------------------------
set.seed(123)
yvec <- mc.input$y$Enterotype[match(colnames(mc.input$X), rownames(mc.input$y))]

# Remove columns with zero sum and apply signal filtering
X_general <- mc.input$X[, colSums(mc.input$X) != 0, drop = FALSE]
X_general <- filterNoSignal(X = X_general, side = 1, threshold = "auto", verbose = FALSE)

# Assign final input data
set.seed(42)
y <- as.vector(yvec)
X <- X_general

# Function to balance the number of samples per class
balance_classes <- function(y, X, n_per_class, seed = 123) {
  classes <- unique(y)
  selected_indices <- integer(0)
  for (cl in classes) {
    cl_idx <- which(y == cl)
    set.seed(seed)
    selected_indices <- c(selected_indices, sample(cl_idx, n_per_class))
  }
  selected_indices <- sort(selected_indices)
  list(y = y[selected_indices], X = X[, selected_indices, drop = FALSE])
}

# Perform class balancing using the minimum sample size per class
n_samples_per_class <- min(table(y))
balanced_data <- balance_classes(y, X, n_samples_per_class)
y_balanced <- balanced_data$y
X_balanced  <- balanced_data$X

# Create training and test datasets
set.seed(42)
split_indices <- createDataPartition(y_balanced, p = 0.8, list = FALSE)
y_train <- y_balanced[split_indices]
y_test  <- y_balanced[-split_indices]
X_train <- X_balanced[,  split_indices, drop = FALSE]
X_test  <- X_balanced[, -split_indices, drop = FALSE]

# Configure the multiclass classifier
clf <- terBeam_mc(
  sparsity = 4:10,
  max.nb.features = 1000,
  seed = 1,
  nCores = 1,
  evalToFit = "accuracy_",
  objective = "auc",
  experiment.id = "terBeam_mc",
  experiment.save = "nothing"
)

# Generate interpretable signatures
approch  <- "ovo"
coeffss  <- getSign_mc(X = X_train, y = y_train, clf = clf, parallel.local = FALSE, approch = approch)

# ------------------------ Unit Tests ------------------------

# 1) Load pretrained model if available (not shipped because ~500 MB)
test_that("Pretrained model can be loaded locally", {
  filename   <- "res_clf_mc_enterotype_Majority_Voting_with_Tie_Breaking_ovo.rda"
  model_file <- tryCatch(testthat::test_path("testdata", filename), error = function(e) "")

  # Fallback: useful when running locally from package root
  if (!nzchar(model_file) || !file.exists(model_file)) {
    model_file <- file.path(getwd(), "tests", "testthat", "testdata", filename)
  }

  skip_if_not(nzchar(model_file) && file.exists(model_file),
              paste("Model file not found in testdata/:", filename))

  load(model_file, envir = globalenv())
  expect_true(exists("res_clf_mc_enterotype_Majority_Voting_with_Tie_Breaking_ovo", envir = globalenv()))
})

# 2) Check structure of signatures (partially independent from the heavy model)
test_that("getSign_mc returns the expected structure", {
  expect_length(coeffss, 6)
  expect_true(length(coeffss[[1]]) >= 1)

  skip_if_not(exists("res_clf_mc_enterotype_Majority_Voting_with_Tie_Breaking_ovo", envir = globalenv()),
              "Pretrained model not loaded (heavy file not shipped).")
  expect_length(res_clf_mc_enterotype_Majority_Voting_with_Tie_Breaking_ovo, 2)
})

# 3) Check classifier regeneration
test_that("regenerate_clf returns a valid structure", {
  clf_tmp <- regenerate_clf(clf, X_train, y_train, approch = "ovo")
  expect_true(is.list(clf_tmp))
  expect_true(length(clf_tmp) >= 1)
})

# 4) Evaluate best model (requires pretrained model file)
test_that("evaluateModel_mc computes correct metrics", {
  skip_if_not(exists("res_clf_mc_enterotype_Majority_Voting_with_Tie_Breaking_ovo", envir = globalenv()),
              "Pretrained model not loaded (heavy file not shipped).")

  pop <- modelCollectionToPopulation(
    res_clf_mc_enterotype_Majority_Voting_with_Tie_Breaking_ovo$classifier$models
  )
  fbm <- selectBestPopulation(pop)
  expect_true(length(fbm) >= 1)
  expect_true(!is.null(fbm[[1]]))

  # Recreate classifier configuration bound to train data
  clf2 <- regenerate_clf(clf, X_train, y_train, approch = "ovo")

  best_model_test <- evaluateModel_mc(
    mod = fbm[[1]],
    X = X_test,
    y = y_test,
    clf = clf2,
    eval.all = TRUE,
    force.re.evaluation = TRUE,
    aggregation_ = "Majority_Voting_with_Tie_Breaking",
    constraint_factor = "semi_constrained",
    mode = "test",
    approch = "ovo"
  )

  # Print evaluation metrics
  cat("\n--- TEST METRICS ---\n")
  cat("Accuracy  :", best_model_test$accuracy_, "\n")
  cat("Precision :", best_model_test$precision_, "\n")
  cat("Recall    :", best_model_test$recall_, "\n")
  cat("F1-score  :", best_model_test$f1_, "\n")

  # Unit tests: expected metric values (tolerance allows small fluctuations)
  expect_length(best_model_test, 31)
  expect_equal(best_model_test$accuracy_,  0.67, tolerance = 1e-2)
  expect_equal(best_model_test$precision_, 0.71, tolerance = 1e-2)
  expect_equal(best_model_test$recall_,    0.68, tolerance = 1e-2)
  expect_equal(best_model_test$f1_,        0.69, tolerance = 1e-2)
})

