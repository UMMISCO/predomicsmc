# Load necessary libraries
library(mcpredomics)
library(predomics)
library(ggplot2)
library(gridExtra)
library(pROC)
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

# Load the dataset
chemin_du_dataset <- system.file("data", "mc.input.Rda", package = "mcpredomics")
experiences <- system.file("vignette", "terbeam_predomics_aggregation_ova_unconstrained.rda", package = "mcpredomics")
load(chemin_du_dataset)
load(experiences)

# Create the model
clf <- terBeam_mc(
  sparsity = c(2,3,4,5,6,7,8,9,10),
  max.nb.features = 1000,
  seed = 1,
  nCores = 1,
  evalToFit = "accuracy_",
  objective = "auc",
  experiment.id = "terBeam_mc",
  experiment.save = "nothing"
)

# Load the dataset again
chemin_du_dataset <- system.file("data", "mc.input.Rda", package = "mcpredomics")
load(chemin_du_dataset)
set.seed(123)
yvec <- mc.input$y$Enterotype[match(colnames(mc.input$X), rownames(mc.input$y))]

# Filter null values
X_general <- mc.input$X[, colSums(mc.input$X) != 0]
X_general <- filterNoSignal(X = X_general, side = 1, threshold = "auto", verbose = FALSE)

set.seed(42)
y = as.vector(yvec)
X = X_general
# Check distribution after balancing
set.seed(42)
indices_division <- createDataPartition(y, p = 0.8, list = FALSE)

# Split balanced data into 80% for training and 20% for testing
y <- as.vector(y[indices_division])
y.test <- as.vector(y[-indices_division])
X <- X[, indices_division, drop = FALSE]
X.test <- X[, -indices_division, drop = FALSE]

# Approach
approch = "ova"

# Execute the function that generates coefficients
coeffss <- getSign_mc(X = X, y = y, clf = clf, parallel.local = FALSE, approch = approch)

# Check the length of coeffss and res_clf
test_that('function getSign_mc', {
  expect_length(coeffss, 4)
  expect_length(coeffss[[1]], 3385)
  expect_length(terbeam_predomics_aggregation_ova_unconstrained, 2)
})

# Convert model collection to population
pop <- modelCollectionToPopulation(terbeam_predomics_aggregation_ova_unconstrained$classifier$models)
fbm <- selectBestPopulation(pop)
clf <- regenerate_clf(clf, X, y, approch = "ova")

# Check the length of coeffss and res_clf
test_that('function regenerate_clf', {
  expect_length(clf, 5)
})

# Test the function evaluateModel_mc and check metrics
test_that("evaluateModel_mc returns correct metrics", {
  # Call the function to test
  best.model.test <- evaluateModel_mc(
    mod = fbm[[1]],
    X = X.test,
    y = y.test,
    clf = clf,
    eval.all = TRUE,
    force.re.evaluation = TRUE,
    aggregation_ = "Predomics_aggregation_ova",
    mode = "test",
    approch = "ova"
  )

  # Check that each metric is correct with increased tolerance
  expect_length(best.model.test, 31)
  expect_equal(best.model.test$accuracy_, 0.705, tolerance = 1e-3) # Increased tolerance
  expect_equal(best.model.test$precision_, 0.734, tolerance = 1e-3)
  expect_equal(best.model.test$recall_, 0.712, tolerance = 1e-3)
  expect_equal(best.model.test$f1_, 0.723, tolerance = 1e-3)
})
