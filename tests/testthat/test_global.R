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

# Charger les données
chemin_du_dataset <- system.file("data", "mc.input.Rda", package = "mcpredomics")
experiences <- system.file("vignette", "terga1_voting_unconstrained.rda", package = "mcpredomics")
load(chemin_du_dataset)
load(experiences)

# Créer le modèle
clf <- terga1_mc(nCores = 1,
                 seed = 1,
                 plot = TRUE
)
printy(clf) # print the object for more information
isClf(clf)  # test whether the object is a classifier
class(clf)

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

# Approche
approch = "ovo"

# Exécutez la fonction qui génère les coefficients
coeffss <- getSign_mc(X = X, y = y, clf = clf, parallel.local = FALSE, approch = approch)

# Vérifiez la longueur de coeffss et de res_clf
test_that('function getSign_mc', {
  expect_length(coeffss, 6)
  expect_length(coeffss[[1]], 3385)
  expect_length(terga1_voting_unconstrained, 2)
})

pop <- modelCollectionToPopulation(terga1_voting_unconstrained$classifier$models)
fbm <- selectBestPopulation(pop)
clf <- regenerate_clf(clf, X, y, approch = "ovo")
# Vérifiez la longueur de coeffss et de res_clf
test_that('function regenerate_clf ', {
  expect_length(clf, 5)
})

test_that("evaluateModel_mc returns correct metrics", {
  # Appel de la fonction que vous souhaitez tester
  best.model.test <- evaluateModel_mc(
    mod = fbm[[1]],
    X = X.test,
    y = y.test,
    clf = clf,
    eval.all = TRUE,
    force.re.evaluation = TRUE,
    aggregation_ = "voting",
    mode = "test",
    approch = "ovo"
  )

  # Vérification que chaque métrique est correcte avec tolérance
  expect_length(best.model.test, 31)
  expect_equal(best.model.test$accuracy_, 0.6364, tolerance = 1e-4) # Ajuster la tolérance si nécessaire
  expect_equal(best.model.test$precision_, 0.6569, tolerance = 1e-4)
  expect_equal(best.model.test$recall_, 0.6364, tolerance = 1e-4)
  expect_equal(best.model.test$f1_, 0.6465, tolerance = 1e-4)

})


