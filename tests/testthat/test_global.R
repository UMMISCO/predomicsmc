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
experiences <- system.file("vignette", "res_clf.rda", package = "mcpredomics")
load(chemin_du_dataset)
load(experiences)

# Créer le modèle
clf <- terBeam_mc(
  sparsity = c(2, 3, 4, 5, 6, 7, 8, 9, 10),
  max.nb.features = 1000,
  seed = 1,
  nCores = 1,
  evalToFit = "accuracy_",
  objective = "auc",
  experiment.id = "terBeam_mc",
  experiment.save = "nothing"
)

set.seed(123)
yvec <- mc.input$y$Enterotype[match(colnames(mc.input$X), rownames(mc.input$y))]
indices_tries <- order(yvec)
yvec_trie <- yvec[indices_tries]
X_general  <- mc.input$X[,indices_tries]
# Create an index vector for data partitioning

X_general <- X_general[rowSums(X_general)!=0,]; dim(X_general) # filter out variables with only zero values
X_general <- filterNoSignal(X = X_general, side = 1, threshold = "auto", verbose = FALSE); dim(X_general)

set.seed(42)
y = as.vector(yvec_trie)
X = X_general

# Number of desired samples in each class
nombre_echantillons_par_classe <- min(table(y))

# Function to balance the classes
equilibrer_classes <- function(y, X, nombre_echantillons_par_classe,seed =123) {
  classes <- unique(y)
  indices_equilibres <- integer(0)

  for (classe in classes) {
    indices_classe <- which(y == classe)
    set.seed(seed)
    indices_equilibres <- c(indices_equilibres, sample(indices_classe, nombre_echantillons_par_classe))
  }

  return(list(y = y[indices_equilibres], X = X[, indices_equilibres]))
}

donnees_equilibrees <- equilibrer_classes(y, X, nombre_echantillons_par_classe)
y_equilibre <- donnees_equilibrees$y
X_equilibre <- donnees_equilibrees$X

# Verify the distribution after balancing

set.seed(42)
indices_division <- createDataPartition(y_equilibre, p = 0.8, list = FALSE)

# Split yvec_trie into 80% train and 20% test
y <- as.vector(y_equilibre[indices_division])
y.test <- as.vector(y_equilibre[-indices_division])
X <- X_equilibre[,indices_division]
X.test <- X_equilibre[,-indices_division]


# Approche
approch = "ova"

# Exécutez la fonction qui génère les coefficients
coeffss <- getSign_mc(X = X, y = y, clf = clf, parallel.local = FALSE, approch = approch)

# Vérifiez la longueur de coeffss et de res_clf
test_that('function getSign_mc', {
  expect_length(coeffss, 4)
  expect_length(coeffss[[1]], 3389)
  expect_length(res_clf, 2)
})

pop <- modelCollectionToPopulation(res_clf$classifier$models)
fbm <- selectBestPopulation(pop)
clf <- regenerate_clf(clf, X, y, approch = "ova")
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
    aggregation_ = "Predomics_aggregation_ova",
    mode = "test",
    approch = "ova"
  )

  # Vérification que chaque métrique est correcte avec tolérance
  expect_length(best.model.test, 28)
  expect_equal(best.model.test$accuracy_, 0.5530303, tolerance = 1e-4) # Ajuster la tolérance si nécessaire
  expect_equal(best.model.test$precision_, 0.5827553, tolerance = 1e-4)
  expect_equal(best.model.test$recall_, 0.5530303, tolerance = 1e-4)
  expect_equal(best.model.test$f1_, 0.5675038, tolerance = 1e-4)
})

