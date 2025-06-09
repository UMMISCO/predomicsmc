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
##chemin_du_dataset <- system.file("data", "mc.input.Rda", package = "mcpredomics")

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

# Determine the number of samples per class
nombre_echantillons_par_classe <- min(table(y))

# Function to balance classes and maintain order
equilibrer_classes <- function(y, X, nombre_echantillons_par_classe, seed = 123) {
  classes <- unique(y)
  indices_equilibres <- integer(0)

  for (classe in classes) {
    indices_classe <- which(y == classe)
    set.seed(seed)
    indices_equilibres <- c(indices_equilibres, sample(indices_classe, nombre_echantillons_par_classe))
  }

  # Sort balanced indices to maintain original order
  indices_equilibres <- sort(indices_equilibres)
  return(list(y = y[indices_equilibres], X = X[, indices_equilibres]))
}

# Get balanced data
donnees_equilibrees <- equilibrer_classes(y, X, nombre_echantillons_par_classe)
y_equilibre <- donnees_equilibrees$y
X_equilibre <- donnees_equilibrees$X

# Check distribution after balancing
set.seed(42)
indices_division <- createDataPartition(y_equilibre, p = 0.8, list = FALSE)

# Split balanced data into 80% for training and 20% for testing
y <- as.vector(y_equilibre[indices_division])
y.test <- as.vector(y_equilibre[-indices_division])
X <- X_equilibre[, indices_division, drop = FALSE]
X.test <- X_equilibre[, -indices_division, drop = FALSE]




experiences <- system.file("vignette", "Terga1_Balance_voting_unconstrained.rda", package = "mcpredomics")
load(chemin_du_dataset)
load(experiences)

# Create the model
clf <- terga1_mc(nCores = 1,
                 seed = 1,
                 plot = TRUE
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
approch = "ovo"

# Execute the function that generates coefficients
coeffss <- getSign_mc(X = X, y = y, clf = clf, parallel.local = FALSE, approch = approch)

# Check the length of coeffss and res_clf
test_that('function getSign_mc', {
  expect_length(coeffss, 6)
  expect_length(coeffss[[1]], 3385)
  expect_length(Terga1_Balance_voting_unconstrained, 2)
})

# Convert model collection to population
pop <- modelCollectionToPopulation(Terga1_Balance_voting_unconstrained$classifier$models)
fbm <- selectBestPopulation(pop)
clf <- regenerate_clf(clf, X, y, approch = "ovo")

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
    aggregation_ = "voting",
    constraint_factor = "semi_constrained",
    mode = "test",
    approch = "ovo"
  )

  # --- Affichage des métriques TEST ---
  cat("\n--- Résultats des métriques (TEST) ---\n")
  cat("Accuracy  :", best.model.test$accuracy_, "\n")
  cat("Precision :", best.model.test$precision_, "\n")
  cat("Recall    :", best.model.test$recall_, "\n")
  cat("F1-score  :", best.model.test$f1_, "\n")
  cat("--------------------------------------\n")

  # --- Affichage des métriques TRAIN (si disponibles dans l'objet mod) ---
  if (!is.null(fbm[[1]])) {
    cat("\n--- Résultats des métriques (TRAIN) ---\n")
    cat("Accuracy  :", fbm[[1]]$accuracy_, "\n")
    cat("Precision :", fbm[[1]]$precision_, "\n")
    cat("Recall    :", fbm[[1]]$recall_, "\n")
    cat("F1-score  :", fbm[[1]]$f1_, "\n")
    cat("---------------------------------------\n")
  } else {
    cat("\n--- Aucune métrique TRAIN disponible dans mod ---\n")
  }

  # --- Vérifications des métriques test ---
  expect_length(best.model.test, 31)
  expect_equal(best.model.test$accuracy_, 0.74, tolerance = 1e-2)
  expect_equal(best.model.test$precision_, 0.76, tolerance = 1e-2)
  expect_equal(best.model.test$recall_, 0.74, tolerance = 1e-2)
  expect_equal(best.model.test$f1_, 0.75, tolerance = 1e-2)
})

