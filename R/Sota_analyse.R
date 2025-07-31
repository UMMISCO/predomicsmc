#
# # SVM NATIVE
#
# # Import necessary libraries
# library(e1071)          # For SVM model
# library(caret)          # For cross-validation and metrics
# library(dplyr)          # For data manipulation
# library(tibble)         # For advanced data frames
#
# # Ensure y is a factor and preprocess X
# y <- as.factor(y)
# X <- t(X)
#
# # Remove features with zero variance
# zero_var_features <- nearZeroVar(X, saveMetrics = TRUE)
# X <- X[, !zero_var_features$zeroVar]  # Keep only non-zero variance features
#
# # Normalize features
# preProcValues <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProcValues, X)
#
# # Stratified 10-fold cross-validation setup
# set.seed(123)
# folds <- createFolds(y, k = 10, list = TRUE)
#
# # Initialize a tibble to store metrics for each fold
# svm_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Function to calculate precision, recall, and F1-score
# compute_metrics <- function(conf_matrix) {
#   if (nrow(conf_matrix) < 2 | ncol(conf_matrix) < 2) {
#     return(c(NA, NA, NA))  # Return NA if not computable
#   }
#
#   TP <- conf_matrix[1, 1]
#   FP <- ifelse(ncol(conf_matrix) > 1, conf_matrix[1, 2], 0)
#   FN <- ifelse(nrow(conf_matrix) > 1, conf_matrix[2, 1], 0)
#   TN <- ifelse(nrow(conf_matrix) > 1 & ncol(conf_matrix) > 1, conf_matrix[2, 2], 0)
#
#   precision <- TP / (TP + FP + 1e-10)  # Avoid division by zero
#   recall <- TP / (TP + FN + 1e-10)
#   f1 <- 2 * (precision * recall) / (precision + recall + 1e-10)
#
#   return(c(precision, recall, f1))
# }
#
# # Perform cross-validation over 10 folds
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Extract training and validation sets
#   index_train <- unlist(folds[-i])  # Keep 9 folds for training
#   index_val <- folds[[i]]  # Use the 10th fold for validation
#
#   X_train <- X_scaled[index_train, , drop = FALSE]
#   y_train <- y[index_train]
#   X_val <- X_scaled[index_val, , drop = FALSE]
#   y_val <- y[index_val]
#
#   # Ensure training set has at least two classes
#   if (length(unique(y_train)) < 2) {
#     cat("Skipping Fold", fold_idx, ": Only one class present in training set\n")
#     next
#   }
#
#   # Train SVM model
#   svm_model <- svm(x = X_train, y = y_train, kernel = 'linear', probability = TRUE)
#
#   # Predict on training and validation sets
#   y_train_pred <- predict(svm_model, X_train)
#   y_val_pred <- predict(svm_model, X_val)
#
#   # Ensure y_val_pred and y_val have the same length
#   if (length(y_val_pred) != length(y_val)) {
#     cat("Skipping Fold", fold_idx, ": Size mismatch between predictions and actual values\n")
#     next
#   }
#
#   # Generate confusion matrices
#   conf_train <- table(Predicted = y_train_pred, Actual = y_train)
#   conf_val <- table(Predicted = y_val_pred, Actual = y_val)
#
#   # Compute empirical metrics
#   accuracy_empirical <- sum(diag(conf_train)) / sum(conf_train)
#   metrics_empirical <- compute_metrics(conf_train)
#
#   # Compute generalization metrics
#   accuracy_generalization <- sum(diag(conf_val)) / sum(conf_val)
#   metrics_generalization <- compute_metrics(conf_val)
#
#   # Store results
#   svm_metrics_results <- svm_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = metrics_empirical[1],
#     Precision.generalization = metrics_generalization[1],
#     Recall.empirique = metrics_empirical[2],
#     Recall.generalization = metrics_generalization[2],
#     F1.empirique = metrics_empirical[3],
#     F1.generalization = metrics_generalization[3],
#     Methods = "SVM",
#     Features = ncol(X_scaled)
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
# # Display results
# print(svm_metrics_results)
#
# # Save the results
# sota_svm_metrics_results_T2D <- svm_metrics_results
# save(sota_svm_metrics_results_T2D , file = "sota_svm_metrics_results_T2D.rda")
#
# ######################################################################################################################
#
# # Arbre de décision NATIVE
#
# # ## Multiple decision trees
# #
# # Charger les bibliothèques nécessaires
# library(rpart)  # Pour les arbres de décision
# library(caret)  # Pour la validation croisée et les métriques
# library(dplyr)  # Pour la manipulation de données
# library(tibble) # Pour la manipulation avancée des dataframes
#
# # S'assurer que y est un facteur
# y <- as.factor(y)
#
# # S'assurer que X a des noms de colonnes valides avant la transposition
# colnames(X) <- make.names(colnames(X))
#
# # Transposer X
# X <- t(X)
#
# set.seed(123)
#
# # Initialiser un tibble pour stocker les résultats des métriques
# dt_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Boucle sur chaque fold pour la validation croisée
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Définir les indices d'entraînement et de validation
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#
#   X_train <- as.data.frame(X[index_train, , drop = FALSE])  # Assurer le format dataframe
#   y_train <- y[index_train]
#   X_val <- as.data.frame(X[-index_train, , drop = FALSE])
#   y_val <- y[-index_train]
#
#   # Vérifier la structure des données après la transposition
#   if (!all(rownames(X_train) %in% rownames(X))) {
#     stop(paste("Problème dans la structure des données après la transposition de X au fold", fold_idx))
#   }
#
#   # Créer et entraîner le modèle d'arbre de décision
#   dt_model <- rpart(y_train ~ ., data = cbind(y_train = y_train, X_train), method = "class")
#
#   # Faire les prédictions sur l'ensemble d'entraînement (empirique)
#   y_train_pred <- predict(dt_model, X_train, type = "class")
#
#   # Faire les prédictions sur l'ensemble de validation (généralisation)
#   y_val_pred <- predict(dt_model, X_val, type = "class")
#
#   # Vérification des tailles des prédictions et des vraies valeurs
#   if(length(y_train) == length(y_train_pred) & length(y_val) == length(y_val_pred)) {
#     # Calcul des métriques empiriques
#     cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#     accuracy_empirical <- cm_train$overall['Accuracy']
#     precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#     recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#     f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#     # Calcul des métriques de généralisation
#     cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#     accuracy_generalization <- cm_val$overall['Accuracy']
#     precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#     recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#     f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#     # Stocker les résultats
#     dt_metrics_results <- dt_metrics_results %>% add_row(
#       Fold = paste0("Fold", fold_idx),
#       Accuracy.empirique = accuracy_empirical,
#       Accuracy.generalization = accuracy_generalization,
#       Precision.empirique = precision_empirical,
#       Precision.generalization = precision_generalization,
#       Recall.empirique = recall_empirical,
#       Recall.generalization = recall_generalization,
#       F1.empirique = f1_empirical,
#       F1.generalization = f1_generalization,
#       Methods = "Decision Tree",
#       Features = ncol(X)  # Mettre le bon nombre de features
#     )
#   } else {
#     warning(paste("Les tailles des prédictions et des vraies valeurs ne correspondent pas au fold", fold_idx))
#   }
#
#   fold_idx <- fold_idx + 1
# }
#
# # Afficher le dataframe des résultats
# print(dt_metrics_results)
#
# # Save the results
# sota_dt_metrics_results_T2D = dt_metrics_results
# save(sota_dt_metrics_results_T2D, file = "sota_dt_metrics_results_T2D.rda")
#
# ####################################################################################################################
#
#
# # ## Random Forest NATIVE
# #
# #
# # Charger les bibliothèques nécessaires
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # S'assurer que y est un facteur
# y <- as.factor(y)
# # Transposer X
# X <- t(X)
# set.seed(123)  # Fixer le seed pour la reproductibilité
#
# # Initialiser un tibble pour stocker les résultats
# forest_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Créer les indices de validation pour 10 folds
# folds <- createFolds(y, k = 10, list = TRUE)
#
# # Boucle sur chaque fold
# for (fold_idx in seq_along(folds)) {
#
#   # Définition des indices d'entraînement et de validation
#   index_val <- folds[[fold_idx]]
#   index_train <- setdiff(seq_len(length(y)), index_val)
#
#   # Découpage des données
#   X_train <- X[index_train, , drop = FALSE]
#   y_train <- y[index_train]
#   X_val <- X[index_val, , drop = FALSE]
#   y_val <- y[index_val]
#
#   # Vérification de la distribution des classes
#   cat("\n==== Fold", fold_idx, "====\n")
#   print(table(y_train))
#   print(table(y_val))
#
#   # Entraînement du modèle Random Forest
#   forest_model <- randomForest(x = X_train, y = y_train, ntree = 1000)
#
#   # Prédictions sur l'ensemble d'entraînement (empirique)
#   y_train_pred <- predict(forest_model, X_train)
#
#   # Prédictions sur l'ensemble de validation (généralisation)
#   y_val_pred <- predict(forest_model, X_val)
#
#   # Vérifier la correspondance des dimensions
#   if (length(y_train) == length(y_train_pred) & length(y_val) == length(y_val_pred)) {
#
#     # Calcul des métriques empiriques
#     cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#     accuracy_empirical <- cm_train$overall['Accuracy']
#     precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#     recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#     f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#     # Calcul des métriques de généralisation
#     cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#     accuracy_generalization <- cm_val$overall['Accuracy']
#     precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#     recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#     f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#     # Stocker les résultats
#     forest_metrics_results <- forest_metrics_results %>% add_row(
#       Fold = paste0("Fold", fold_idx),
#       Accuracy.empirique = accuracy_empirical,
#       Accuracy.generalization = accuracy_generalization,
#       Precision.empirique = precision_empirical,
#       Precision.generalization = precision_generalization,
#       Recall.empirique = recall_empirical,
#       Recall.generalization = recall_generalization,
#       F1.empirique = f1_empirical,
#       F1.generalization = f1_generalization,
#       Methods = "Random Forest",
#       Features = ncol(X)  # Mettre le vrai nombre de features
#     )
#
#   } else {
#     warning(paste("Problème de correspondance des tailles au fold", fold_idx))
#   }
# }
#
# # Afficher le DataFrame des résultats
# print(forest_metrics_results)
#
# # Sauvegarder les résultats
# sota_forest_metrics_results_T2D <- forest_metrics_results
# save(sota_forest_metrics_results_T2D, file = "sota_forest_metrics_results_T2D.rda")
#
# ########################################################################################################################
#
#
# # KNN NATIVE
#
# ### Charger les bibliothèques nécessaires
# library(class)     # Pour KNN
# library(caret)     # Pour la validation croisée et les métriques
# library(dplyr)     # Pour la manipulation des données
# library(tibble)    # Pour une manipulation avancée des data frames
#
# # S'assurer que la variable cible (y) est un facteur
# y <- as.factor(y)
# X <- t(X)  # Transposition de X si nécessaire
# set.seed(123)  # Pour la reproductibilité
#
# # Retirer les colonnes constantes (variance nulle)
# X_non_constant <- X[, apply(X, 2, var, na.rm = TRUE) != 0]
#
# # Vérification des valeurs manquantes et imputation si nécessaire
# if (any(is.na(X_non_constant))) {
#   # Imputation simple par la moyenne des colonnes
#   X_non_constant[is.na(X_non_constant)] <- colMeans(X_non_constant, na.rm = TRUE)
# }
#
# # Initialisation d'un tibble pour stocker les métriques par pli
# knn_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()  # Ajouter la colonne Nbre_Features
# )
#
# # Validation croisée k-fold (k=10)
# fold_idx <- 1
# for (i in 1:10) {
#   # Créer les indices d'entraînement et de validation pour le pli actuel
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#
#   # Diviser les données en ensembles d'entraînement et de validation
#   X_train <- X_non_constant[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X_non_constant[-index_train, ]
#   y_val <- y[-index_train]
#
#   # Entraînement du modèle KNN
#   knn_model <- knn(
#     train = X_train, test = X_train, cl = y_train, k = 3
#   )
#
#   # Prédictions sur l'ensemble d'entraînement (prédictions empiriques)
#   y_train_pred <- knn(
#     train = X_train, test = X_train, cl = y_train, k = 3
#   )
#
#   # Prédictions sur l'ensemble de validation (prédictions de généralisation)
#   y_val_pred <- knn(
#     train = X_train, test = X_val, cl = y_train, k = 3
#   )
#
#   # Assurez-vous que les prédictions sont des facteurs avec les mêmes niveaux que les valeurs réelles
#   y_train_pred <- factor(y_train_pred, levels = levels(y_train))
#   y_val_pred <- factor(y_val_pred, levels = levels(y_val))
#
#   # Vérification des tailles des prédictions et des véritables valeurs
#   if (length(y_train_pred) != length(y_train)) {
#     stop(paste("La longueur des prédictions d'entraînement ne correspond pas à celle des véritables valeurs pour le pli", fold_idx))
#   }
#   if (length(y_val_pred) != length(y_val)) {
#     stop(paste("La longueur des prédictions de validation ne correspond pas à celle des véritables valeurs pour le pli", fold_idx))
#   }
#
#   # Calcul des métriques pour l'ensemble d'entraînement
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   accuracy_empirical <- cm_train$overall['Accuracy']
#   precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#   recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#   f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#   # Calcul des métriques pour l'ensemble de validation
#   cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#   accuracy_generalization <- cm_val$overall['Accuracy']
#   precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#   recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#   f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#   # Stockage des résultats pour le pli actuel
#   knn_metrics_results <- knn_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "KNN",
#     Features = 2049  # Ajouter la valeur de Nbre_Features
#   )
#
#   # Incrémenter l'indice du pli
#   fold_idx <- fold_idx + 1
# }
#
# # Afficher le tableau des résultats
# print(knn_metrics_results)
# sota_knn_metrics_results_T2D = knn_metrics_results
# # Sauvegarder les résultats dans un fichier
# save(sota_knn_metrics_results_T2D, file = "sota_knn_metrics_results_T2D.rda")
#
# #######################################################################################################################
#
#
# # Regression Logistique NATIVE
#
#
# library(glmnet)   # For logistic regression with cross-validation
# library(caret)    # For data partitioning and evaluation metrics
# library(dplyr)    # For data manipulation
# library(tibble)   # For structured data storage
#
# # Ensure the target variable (y) is a factor
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)  # Set seed for reproducibility
#
# # Vérifier les valeurs manquantes et les imputer si nécessaire
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[, j][is.na(X[, j])] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # Initialiser la tibble pour stocker les métriques
# logreg_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# set.seed(123)  # Fixe la graine pour rendre les folds reproductibles
# folds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
#
# # Boucle sur les 10 folds
# for (fold_idx in seq_along(folds)) {
#   index_val <- folds[[fold_idx]]
#   index_train <- setdiff(seq_along(y), index_val)
#
#   X_train <- X[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X[index_val, ]
#   y_val <- y[index_val]
#
#   # Entraîner le modèle de régression logistique multinomiale
#   logreg_model <- cv.glmnet(as.matrix(X_train), y_train, family = "multinomial", alpha = 1)
#
#   # Prédiction sur les données d'entraînement
#   y_train_pred <- predict(logreg_model, newx = as.matrix(X_train), s = "lambda.min", type = "class")
#   y_train_pred <- factor(y_train_pred, levels = levels(y))
#
#   # Prédiction sur les données de validation
#   y_val_pred <- predict(logreg_model, newx = as.matrix(X_val), s = "lambda.min", type = "class")
#   y_val_pred <- factor(y_val_pred, levels = levels(y))
#
#   # Métriques sur les données d'entraînement
#   cm_train <- confusionMatrix(y_train_pred, y_train)
#   accuracy_empirical <- cm_train$overall['Accuracy']
#   precision_empirical <- mean(cm_train$byClass[, 'Precision'], na.rm = TRUE)
#   recall_empirical <- mean(cm_train$byClass[, 'Recall'], na.rm = TRUE)
#   f1_empirical <- mean(cm_train$byClass[, 'F1'], na.rm = TRUE)
#
#   # Métriques sur les données de validation
#   cm_val <- confusionMatrix(y_val_pred, y_val)
#   accuracy_generalization <- cm_val$overall['Accuracy']
#   precision_generalization <- mean(cm_val$byClass[, 'Precision'], na.rm = TRUE)
#   recall_generalization <- mean(cm_val$byClass[, 'Recall'], na.rm = TRUE)
#   f1_generalization <- mean(cm_val$byClass[, 'F1'], na.rm = TRUE)
#
#   # Ajouter les résultats
#   logreg_metrics_results <- logreg_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "Logistic Regression (glmnet)",
#     Features = ncol(X)
#   )
# }
#
# # Affichage des résultats
# print(logreg_metrics_results)
# Sota_RL_T2D <- logreg_metrics_results
# save(Sota_RL_T2D, file = "Sota_RL_T2D.rda")
#
# ######################################################################################################################
#
#
# ## Approche de Binarisation avec SOTA
#
#
# ### RANDOM FOREST OVA + MAXIMIZATION
#
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # Exemple X, y (déjà transposé si nécessaire)
# # X : matrice features (échantillons en lignes, variables en colonnes)
# # y : vecteur classe multiclasse
#
# # S'assurer que y est facteur
# y <- as.factor(y)
# X <- t(X)  # Si vos données sont encore en colonnes = échantillons
#
# classes <- levels(y)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # Résultats finaux
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# # === Cross-validation OvA ===
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X)), test_idx)
#
#   X_train <- X[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X[test_idx, ]
#   y_test <- y[test_idx]
#
#   # Matrices de scores pour train et test
#   score_train <- matrix(NA, nrow = length(train_idx), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = length(test_idx),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   # Entraîner un modèle OvA pour chaque classe
#   for (i in seq_along(classes)) {
#     current_class <- classes[i]
#     binary_y_train <- factor(ifelse(y_train == current_class, "Yes", "No"))
#
#     rf_model <- randomForest(x = X_train, y = binary_y_train, ntree = 500)
#
#     # Prédiction probabiliste
#     prob_train <- predict(rf_model, X_train, type = "prob")[, "Yes"]
#     prob_test  <- predict(rf_model, X_test,  type = "prob")[, "Yes"]
#
#     score_train[, i] <- prob_train
#     score_test[, i]  <- prob_test
#   }
#
#   # Agrégation (maximization)
#   y_pred_train <- apply(score_train, 1, function(row) classes[which.max(row)])
#   y_pred_test  <- apply(score_test,  1, function(row) classes[which.max(row)])
#
#   y_pred_train <- factor(y_pred_train, levels = classes)
#   y_pred_test  <- factor(y_pred_test,  levels = classes)
#
#   # Évaluations
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
# }
#
# # Résumé
# print(results)
# ova_RF_T2D <- results
# save(ova_RF_T2D, file = "ova_RF_T2D.rda")
#
# ###################################################################################################################
#
#
#
# ### RANDOM FOREST OVO + VOTING TIE
#
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # pour combn()
#
# # Données
# y <- as.factor(y)
# X <- t(X)  # Transposer si nécessaire
#
# classes <- levels(y)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # Tibble des résultats
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# # === Fonction de départage ===
# resolve_tie <- function(votes_row, classes) {
#   max_vote <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) {
#     return(classes[candidates])
#   } else {
#     # Tie-breaking aléatoire
#     return(sample(classes[candidates], 1))
#   }
# }
#
# # === Cross-validation OvO ===
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X)), test_idx)
#
#   X_train <- X[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X[test_idx, ]
#   y_test <- y[test_idx]
#
#   # Génération des paires OvO
#   class_pairs <- combn(classes, 2, simplify = FALSE)
#   votes_train <- matrix(0, nrow = length(train_idx), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = length(test_idx),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   # Entraînement et votes
#   for (pair in class_pairs) {
#     class1 <- pair[1]
#     class2 <- pair[2]
#
#     pair_idx_train <- which(y_train %in% c(class1, class2))
#     pair_y_train <- droplevels(y_train[pair_idx_train])
#     pair_X_train <- X_train[pair_idx_train, ]
#
#     rf_model <- randomForest(x = pair_X_train, y = pair_y_train, ntree = 500)
#
#     pred_train <- predict(rf_model, X_train)
#     pred_test  <- predict(rf_model, X_test)
#
#     for (i in seq_along(train_idx)) {
#       pred_class <- as.character(pred_train[i])
#       if (pred_class %in% classes) {
#         votes_train[i, pred_class] <- votes_train[i, pred_class] + 1
#       }
#     }
#
#     for (i in seq_along(test_idx)) {
#       pred_class <- as.character(pred_test[i])
#       if (pred_class %in% classes) {
#         votes_test[i, pred_class] <- votes_test[i, pred_class] + 1
#       }
#     }
#   }
#
#   # Agrégation avec départage en cas d'égalité
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test, 1, resolve_tie, classes = classes), levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
# }
#
# # Affichage et sauvegarde
# print(results)
# ovo_RF_T2D <- results
# save(ovo_RF_T2D, file = "ovo_RF_T2D.rda")
#
# ######################################################################################################################
#
# ### SVM OVA + MAXIMIZATION
#
# # === Librairies ===
# library(e1071)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # === Données ===
# y <- as.factor(y)
# X <- t(X)  # Transposer si nécessaire (échantillons en lignes)
# classes <- levels(y)
#
# # === Prétraitement ===
# zero_var_features <- nearZeroVar(X, saveMetrics = TRUE)
# X <- X[, !zero_var_features$zeroVar]
#
# preProc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProc, X)
#
# # === Cross-validation stratifiée ===
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # === Résultats + Modèles ===
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# fold_models <- list()  # pour stocker les modèles SVM OvA par fold
#
# # === Cross-validation OvA ===
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   ova_fold <- list()
#   score_train <- matrix(NA, nrow = length(train_idx), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = length(test_idx),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#
#     model <- svm(
#       x = X_train,
#       y = y_bin_train,
#       kernel = "linear",
#       probability = TRUE,
#       type = "C-classification"
#     )
#
#     ova_fold[[cls]] <- model
#
#     prob_train <- attr(predict(model, X_train, probability = TRUE), "probabilities")[, "Yes"]
#     prob_test  <- attr(predict(model, X_test,  probability = TRUE), "probabilities")[, "Yes"]
#
#     score_train[, cls] <- prob_train
#     score_test[, cls]  <- prob_test
#   }
#
#   y_pred_train <- apply(score_train, 1, function(row) classes[which.max(row)])
#   y_pred_test  <- apply(score_test,  1, function(row) classes[which.max(row)])
#   y_pred_train <- factor(y_pred_train, levels = classes)
#   y_pred_test  <- factor(y_pred_test,  levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ova_fold
# }
#
# # === Résultats Cross-validation ===
# print(results)
# ova_svm_T2D = results
# save(ova_svm_T2D, file = "ova_svm_T2D.rda")
#
# # === Modèle OvA final sur 100 % des données ===
# final_ova_svm_model <- list()
# for (cls in classes) {
#   y_bin <- factor(ifelse(y == cls, "Yes", "No"))
#
#   model <- svm(
#     x = X_scaled,
#     y = y_bin,
#     kernel = "linear",
#     probability = TRUE,
#     type = "C-classification"
#   )
#
#   final_ova_svm_model[[cls]] <- model
# }
#
# # === Sauvegarde modèle final ===
# save(final_ova_svm_model, preProc, file = "final_ova_svm_model.rda")
#
#
# # Charger modèle final
# load("final_ova_svm_model.rda")  # contient final_ova_svm_model et preProc
#
# # Nouvelle matrice X_new (features en colonnes)
# X_new <- t(X_new)  # transpose si besoin
# X_new_scaled <- predict(preProc, X_new)
#
# # Prédiction
# scores_new <- sapply(classes, function(cls) {
#   attr(predict(final_ova_svm_model[[cls]], X_new_scaled, probability = TRUE), "probabilities")[, "Yes"]
# })
#
# y_pred_new <- apply(scores_new, 1, function(row) classes[which.max(row)])
# y_pred_new <- factor(y_pred_new, levels = classes)
#
# print(y_pred_new)
#
# ######################################################################################################################
#
# ### SVM OVO + VOTING TIE
#
# # === Librairies ===
# library(e1071)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)
#
# # === Données ===
# y <- as.factor(y)
# X <- t(X)  # Transposer si besoin
# classes <- levels(y)
#
# # === Prétraitement ===
# zero_var_features <- nearZeroVar(X, saveMetrics = TRUE)
# X <- X[, !zero_var_features$zeroVar]
#
# preProc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProc, X)
#
# # === Fonction de tie-breaking ===
# resolve_tie <- function(votes_row, classes) {
#   max_vote <- max(votes_row)
#   tied <- which(votes_row == max_vote)
#   if (length(tied) == 1) {
#     return(classes[tied])
#   } else {
#     return(sample(classes[tied], 1))
#   }
# }
#
# # === Cross-validation stratifiée ===
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# fold_models <- list()
#
# # === Boucle de CV OvO ===
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   class_pairs <- combn(classes, 2, simplify = FALSE)
#   ovo_fold <- list()
#
#   votes_train <- matrix(0, nrow = nrow(X_train), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   for (pair in class_pairs) {
#     class1 <- pair[1]
#     class2 <- pair[2]
#
#     idx_pair_train <- which(y_train %in% c(class1, class2))
#     pair_X_train <- X_train[idx_pair_train, ]
#     pair_y_train <- droplevels(y_train[idx_pair_train])
#
#     model <- svm(
#       x = pair_X_train,
#       y = pair_y_train,
#       kernel = "linear",
#       type = "C-classification"
#     )
#
#     ovo_fold[[paste(class1, class2, sep = "_vs_")]] <- model
#
#     pred_train <- predict(model, X_train)
#     pred_test  <- predict(model, X_test)
#
#     for (i in seq_along(pred_train)) {
#       pred_class <- as.character(pred_train[i])
#       if (pred_class %in% classes) {
#         votes_train[i, pred_class] <- votes_train[i, pred_class] + 1
#       }
#     }
#
#     for (i in seq_along(pred_test)) {
#       pred_class <- as.character(pred_test[i])
#       if (pred_class %in% classes) {
#         votes_test[i, pred_class] <- votes_test[i, pred_class] + 1
#       }
#     }
#   }
#
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test,  1, resolve_tie, classes = classes), levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test, y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ovo_fold
# }
#
# # === Résultats cross-validation ===
# print(results)
# ovo_svm_balance = results
# save(ovo_svm_balance, file = "ovo_svm_balance.rda")
#
# final_ovo_svm_model <- list()
# class_pairs <- combn(classes, 2, simplify = FALSE)
#
# for (pair in class_pairs) {
#   class1 <- pair[1]
#   class2 <- pair[2]
#
#   idx_pair <- which(y %in% c(class1, class2))
#   X_pair <- X_scaled[idx_pair, ]
#   y_pair <- droplevels(y[idx_pair])
#
#   model <- svm(
#     x = X_pair,
#     y = y_pair,
#     kernel = "linear",
#     type = "C-classification"
#   )
#
#   final_ovo_svm_model[[paste(class1, class2, sep = "_vs_")]] <- model
# }
#
# # Sauvegarde
# save(final_ovo_svm_model, preProc, file = "final_ovo_svm_tiebreak_model.rda")
#
# # Charger modèle
# load("final_ovo_svm_tiebreak_model.rda")  # contient final_ovo_svm_model et preProc
#
# # Nouvelle donnée : X_new (features en colonnes)
# X_new <- t(X_new)
# X_new_scaled <- predict(preProc, X_new)
#
# votes_new <- matrix(0, nrow = nrow(X_new_scaled), ncol = length(classes))
# colnames(votes_new) <- classes
#
# for (pair_name in names(final_ovo_svm_model)) {
#   model <- final_ovo_svm_model[[pair_name]]
#   pred <- predict(model, X_new_scaled)
#
#   for (i in seq_along(pred)) {
#     pred_class <- as.character(pred[i])
#     if (pred_class %in% classes) {
#       votes_new[i, pred_class] <- votes_new[i, pred_class] + 1
#     }
#   }
# }
#
# y_pred_new <- apply(votes_new, 1, resolve_tie, classes = classes)
# y_pred_new <- factor(y_pred_new, levels = classes)
#
# print(y_pred_new)
#
# ####################################################################################################
#
# ### LOGISTIQUE REGRESSION
#
#
# ### Regression logistique OVA + MAXIMIZATION
#
# # === Librairies ===
# library(glmnet)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # === Données ===
# y <- as.factor(y)
# X <- t(X)
# classes <- levels(y)
#
# # === Gestion des valeurs manquantes ===
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[, j][is.na(X[, j])] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # === Normalisation ===
# preProc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProc, X)
#
# # === Folds CV ===
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # === Stockage résultats ===
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# fold_models <- list()
#
# # === Cross-validation OvA ===
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   ova_fold <- list()
#   score_train <- matrix(NA, nrow = nrow(X_train), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#     model <- cv.glmnet(x = as.matrix(X_train), y = y_bin_train, family = "binomial", alpha = 1, type.measure = "class")
#     ova_fold[[cls]] <- model
#
#     prob_train <- predict(model, newx = as.matrix(X_train), s = "lambda.min", type = "response")
#     prob_test  <- predict(model, newx = as.matrix(X_test),  s = "lambda.min", type = "response")
#
#     score_train[, cls] <- prob_train
#     score_test[, cls]  <- prob_test
#   }
#
#   y_pred_train <- apply(score_train, 1, function(row) classes[which.max(row)])
#   y_pred_test  <- apply(score_test,  1, function(row) classes[which.max(row)])
#
#   y_pred_train <- factor(y_pred_train, levels = classes)
#   y_pred_test  <- factor(y_pred_test,  levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test, y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ova_fold
# }
#
# # === Résultats ===
# print(results)
# ova_RL_T2D = results
# save(ova_RL_T2D, file = "ova_RL_T2D.rda")
#
# final_ova_logreg_model <- list()
#
# for (cls in classes) {
#   y_bin <- factor(ifelse(y == cls, "Yes", "No"))
#   model <- cv.glmnet(x = as.matrix(X_scaled), y = y_bin, family = "binomial", alpha = 1)
#   final_ova_logreg_model[[cls]] <- model
# }
#
# save(final_ova_logreg_model, preProc, file = "final_ova_logreg_model.rda")
#
# # Charger modèle
# load("final_ova_logreg_model.rda")
#
# # Nouvelle donnée X_new : features en colonnes
# X_new <- t(X_new)
# X_new_scaled <- predict(preProc, X_new)
#
# # Score pour chaque classe
# scores_new <- sapply(classes, function(cls) {
#   predict(final_ova_logreg_model[[cls]], newx = as.matrix(X_new_scaled), s = "lambda.min", type = "response")
# })
#
# # Classe prédite = max(prob)
# y_pred_new <- apply(scores_new, 1, function(row) classes[which.max(row)])
# y_pred_new <- factor(y_pred_new, levels = classes)
#
# print(y_pred_new)
#
#
# ##################
#
# ### REGRESSION LOGISTIQUE OVO + VOTING TIE
#
# # === Librairies ===
# library(glmnet)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # Pour combn()
#
# # === Données ===
# y <- as.factor(y)
# X <- t(X)
# classes <- levels(y)
#
# # === Gestion des valeurs manquantes ===
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[, j][is.na(X[, j])] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # === Normalisation ===
# preProc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProc, X)
#
# # === Folds CV ===
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # === Fonction de départage en cas d’égalité ===
# resolve_tie <- function(votes_row, classes) {
#   max_vote <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) {
#     return(classes[candidates])
#   } else {
#     return(sample(classes[candidates], 1))  # départage aléatoire
#   }
# }
#
# # === Résultats + stockage modèles ===
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# fold_models <- list()
# class_pairs <- combn(classes, 2, simplify = FALSE)
#
# # === Cross-validation OvO ===
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   ovo_fold <- list()
#   votes_train <- matrix(0, nrow = nrow(X_train), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   for (pair in class_pairs) {
#     class1 <- pair[1]
#     class2 <- pair[2]
#
#     idx_train_pair <- which(y_train %in% c(class1, class2))
#     X_pair <- X_train[idx_train_pair, ]
#     y_pair <- factor(y_train[idx_train_pair], levels = c(class1, class2))
#
#     model <- cv.glmnet(as.matrix(X_pair), y_pair, family = "binomial", alpha = 1)
#     ovo_fold[[paste(class1, class2, sep = "_vs_")]] <- model
#
#     pred_train <- predict(model, newx = as.matrix(X_train), s = "lambda.min", type = "class")
#     pred_test  <- predict(model, newx = as.matrix(X_test),  s = "lambda.min", type = "class")
#
#     for (i in seq_along(pred_train)) {
#       votes_train[i, as.character(pred_train[i])] <- votes_train[i, as.character(pred_train[i])] + 1
#     }
#
#     for (i in seq_along(pred_test)) {
#       votes_test[i, as.character(pred_test[i])] <- votes_test[i, as.character(pred_test[i])] + 1
#     }
#   }
#
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test, 1, resolve_tie, classes = classes), levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test, y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ovo_fold
# }
#
# # === Résumé
# print(results)
# ovo_RL_balance = results
# save(ovo_RL_balance, file = "ovo_RL_balance.rda")
#
#
#
# final_ovo_logreg_model <- list()
#
# for (pair in class_pairs) {
#   class1 <- pair[1]
#   class2 <- pair[2]
#
#   idx_pair <- which(y %in% c(class1, class2))
#   X_pair <- X_scaled[idx_pair, ]
#   y_pair <- factor(y[idx_pair], levels = c(class1, class2))
#
#   model <- cv.glmnet(as.matrix(X_pair), y_pair, family = "binomial", alpha = 1)
#   final_ovo_logreg_model[[paste(class1, class2, sep = "_vs_")]] <- model
# }
#
# save(final_ovo_logreg_model, preProc, file = "final_ovo_logreg_model.rda")
#
# # Charger modèle
# load("final_ovo_logreg_model.rda")
#
# # Nouvelle donnée
# X_new <- t(X_new)
# X_new_scaled <- predict(preProc, X_new)
#
# votes_new <- matrix(0, nrow = nrow(X_new_scaled), ncol = length(classes))
# colnames(votes_new) <- classes
#
# for (pair in class_pairs) {
#   class1 <- pair[1]
#   class2 <- pair[2]
#   model <- final_ovo_logreg_model[[paste(class1, class2, sep = "_vs_")]]
#
#   pred <- predict(model, newx = as.matrix(X_new_scaled), s = "lambda.min", type = "class")
#   for (i in seq_along(pred)) {
#     votes_new[i, as.character(pred[i])] <- votes_new[i, as.character(pred[i])] + 1
#   }
# }
#
# y_pred_new <- factor(apply(votes_new, 1, resolve_tie, classes = classes), levels = classes)
# print(y_pred_new)
#
#
#
# ######################################################################################################################
#
# ##Arbre de decision
#
# ### Arbre de decision OVA + MAXIMIZATION
#
# library(rpart)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # Données
# y <- as.factor(y)
# X <- t(X)
# colnames(X) <- make.names(colnames(X))  # Pour éviter les noms invalides
#
# # Normalisation
# preProc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProc, X)
#
# # Cross-validation
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
# classes <- levels(y)
#
# # Stockage des modèles par fold
# fold_models <- list()
#
# # Résultats
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# # Boucle sur les folds
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   ova_models <- list()
#   score_train <- matrix(NA, nrow = nrow(X_train), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#     data_train <- data.frame(X_train, y_bin_train = y_bin_train)
#
#     model <- rpart(y_bin_train ~ ., data = data_train, method = "class")
#     ova_models[[cls]] <- model
#
#     prob_train <- predict(model, newdata = as.data.frame(X_train), type = "prob")[, "Yes"]
#     prob_test  <- predict(model, newdata = as.data.frame(X_test),  type = "prob")[, "Yes"]
#
#     score_train[, cls] <- prob_train
#     score_test[, cls]  <- prob_test
#   }
#
#   y_pred_train <- apply(score_train, 1, function(row) classes[which.max(row)])
#   y_pred_test  <- apply(score_test,  1, function(row) classes[which.max(row)])
#
#   y_pred_train <- factor(y_pred_train, levels = classes)
#   y_pred_test  <- factor(y_pred_test,  levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test, y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ova_models
# }
#
# # Résumé
# print(results)
# ova_dt_T2D = results
# save(ova_dt_T2D,file = "ova_dt_T2D.rda")
#
#
#
# final_ova_dt_model <- list()
#
# for (cls in classes) {
#   y_bin <- factor(ifelse(y == cls, "Yes", "No"))
#   data_all <- data.frame(X_scaled, y_bin = y_bin)
#   model <- rpart(y_bin ~ ., data = data_all, method = "class")
#   final_ova_dt_model[[cls]] <- model
# }
#
# save(final_ova_dt_model, preProc, file = "final_ova_dt_model.rda")
#
# # Charger
# load("final_ova_dt_model.rda")
#
# # X_new est en colonnes (features)
# X_new <- t(X_new)
# X_new_scaled <- predict(preProc, X_new)
#
# scores_new <- sapply(classes, function(cls) {
#   predict(final_ova_dt_model[[cls]], newdata = as.data.frame(X_new_scaled), type = "prob")[, "Yes"]
# })
#
# y_pred_new <- apply(scores_new, 1, function(row) classes[which.max(row)])
# y_pred_new <- factor(y_pred_new, levels = classes)
#
# print(y_pred_new)
#
# ##################################################################
# ### Arbre de decision OVO + VOTING TIE
#
#
#
# library(rpart)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # Pour combn()
#
# # Données
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# classes <- levels(y)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # Résultats
# ovo_dt_results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# # Fonction de départage
# resolve_tie <- function(votes_row, classes) {
#   max_vote <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) {
#     return(classes[candidates])
#   } else {
#     return(sample(classes[candidates], 1))  # aléatoire
#   }
# }
#
# # Cross-validation
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X)), test_idx)
#
#   X_train <- as.data.frame(X[train_idx, ])
#   y_train <- y[train_idx]
#   X_test <- as.data.frame(X[test_idx, ])
#   y_test <- y[test_idx]
#
#   # Préparation des matrices de vote
#   votes_train <- matrix(0, nrow = length(train_idx), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = length(test_idx),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   # Toutes les paires (OvO)
#   class_pairs <- combn(classes, 2, simplify = FALSE)
#
#   for (pair in class_pairs) {
#     class1 <- pair[1]
#     class2 <- pair[2]
#
#     idx_pair_train <- which(y_train %in% pair)
#     idx_pair_test  <- which(y_test %in% pair)
#
#     X_pair_train <- X_train[idx_pair_train, ]
#     y_pair_train <- droplevels(y_train[idx_pair_train])
#
#     model <- rpart(y_pair_train ~ ., data = cbind(y_pair_train = y_pair_train, X_pair_train), method = "class")
#
#     # Prédiction sur ensemble d'entraînement complet
#     pred_train <- predict(model, X_train, type = "class")
#     pred_test  <- predict(model, X_test,  type = "class")
#
#     for (i in seq_along(pred_train)) {
#       if (pred_train[i] %in% pair) {
#         votes_train[i, as.character(pred_train[i])] <- votes_train[i, as.character(pred_train[i])] + 1
#       }
#     }
#
#     for (i in seq_along(pred_test)) {
#       if (pred_test[i] %in% pair) {
#         votes_test[i, as.character(pred_test[i])] <- votes_test[i, as.character(pred_test[i])] + 1
#       }
#     }
#   }
#
#   # Résolution finale par vote majoritaire avec tie-breaking
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test,  1, resolve_tie, classes = classes), levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   ovo_dt_results <- ovo_dt_results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
# }
#
# # Affichage
# print(ovo_dt_results)
# ovo_dt_balance = ovo_dt_results
# # Sauvegarde
# save(ovo_dt_balance, file = "ovo_dt_balance.rda")
#
#
#
# # Créer tous les modèles OvO sur l'ensemble des données pour usage futur
# ovo_dt_model <- list()
#
# class_pairs <- combn(classes, 2, simplify = FALSE)
#
# for (pair in class_pairs) {
#   class1 <- pair[1]
#   class2 <- pair[2]
#
#   idx_pair <- which(y %in% pair)
#   X_pair <- as.data.frame(X[idx_pair, ])
#   y_pair <- droplevels(y[idx_pair])
#
#   model <- rpart(y_pair ~ ., data = cbind(y_pair = y_pair, X_pair), method = "class")
#
#   key <- paste(class1, class2, sep = "_vs_")
#   ovo_dt_model[[key]] <- model
# }
#
# # Sauvegarde du modèle final + preprocessing
# save(ovo_dt_model, classes, file = "final_ovo_DT_model.rda")
#
# # Charger le modèle
# load("final_ovo_DT_model.rda")
#
# # Suppose que X_new est un data.frame avec échantillons en lignes et mêmes colonnes que X
# X_new <- t(X_new)
# X_new <- as.data.frame(X_new)
#
# # Initialiser les votes
# votes_new <- matrix(0, nrow = nrow(X_new), ncol = length(classes))
# colnames(votes_new) <- classes
#
# # Faire voter chaque classifieur OvO
# for (key in names(ovo_dt_model)) {
#   pair <- unlist(strsplit(key, "_vs_"))
#   model <- ovo_dt_model[[key]]
#
#   pred <- predict(model, X_new, type = "class")
#
#   for (i in seq_along(pred)) {
#     if (pred[i] %in% pair) {
#       votes_new[i, pred[i]] <- votes_new[i, pred[i]] + 1
#     }
#   }
# }
#
# # Résolution finale par vote majoritaire avec départage
# resolve_tie <- function(votes_row, classes) {
#   max_vote <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) {
#     return(classes[candidates])
#   } else {
#     return(sample(classes[candidates], 1))
#   }
# }
#
# y_pred_new <- factor(apply(votes_new, 1, resolve_tie, classes = classes), levels = classes)
#
# # Affichage
# print(y_pred_new)
#
# #####################################################################################################################
# ###### KNN
# ### KNN OVA + MAXIMIZATION
#
# library(class)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # Préparation des données
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# # Filtrage des colonnes constantes
# X <- X[, apply(X, 2, var, na.rm = TRUE) != 0]
#
# # Imputation des valeurs manquantes si besoin
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[, j][is.na(X[, j])] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # Normalisation
# preproc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preproc, X)
#
# # Définition des folds
# folds <- createFolds(y, k = 10, list = TRUE)
# classes <- levels(y)
#
# # Initialisation
# results <- tibble(
#   Fold = integer(),
#   Accuracy.train = numeric(),
#   Precision.train = numeric(),
#   Recall.train = numeric(),
#   F1.train = numeric(),
#   Accuracy.test = numeric(),
#   Precision.test = numeric(),
#   Recall.test = numeric(),
#   F1.test = numeric()
# )
#
# # Cross-validation OvA
# for (fold_idx in seq_along(folds)) {
#   test_idx <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   score_train <- matrix(NA, nrow = length(train_idx), ncol = length(classes))
#   score_test <- matrix(NA, nrow = length(test_idx), ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#
#     prob_train <- knn(train = X_train, test = X_train, cl = y_bin_train, k = 3, prob = TRUE)
#     prob_test <- knn(train = X_train, test = X_test, cl = y_bin_train, k = 3, prob = TRUE)
#
#     prob_train_scores <- attr(prob_train, "prob")
#     prob_train_scores <- ifelse(prob_train == "Yes", prob_train_scores, 1 - prob_train_scores)
#
#     prob_test_scores <- attr(prob_test, "prob")
#     prob_test_scores <- ifelse(prob_test == "Yes", prob_test_scores, 1 - prob_test_scores)
#
#     score_train[, cls] <- prob_train_scores
#     score_test[, cls] <- prob_test_scores
#   }
#
#   y_pred_train <- apply(score_train, 1, function(row) classes[which.max(row)])
#   y_pred_test <- apply(score_test, 1, function(row) classes[which.max(row)])
#
#   y_pred_train <- factor(y_pred_train, levels = classes)
#   y_pred_test <- factor(y_pred_test, levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test <- confusionMatrix(y_pred_test, y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train = mean(cm_train$byClass[, "Recall"], na.rm = TRUE),
#     F1.train = mean(cm_train$byClass[, "F1"], na.rm = TRUE),
#     Accuracy.test = cm_test$overall["Accuracy"],
#     Precision.test = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test = mean(cm_test$byClass[, "Recall"], na.rm = TRUE),
#     F1.test = mean(cm_test$byClass[, "F1"], na.rm = TRUE)
#   )
# }
#
# print(results)
#
# ova_knn_T2D = results
# save(ova_knn_T2D, file = "ova_knn_T2D.rda")
#
# # === Entraînement final sur 100 % des données ===
# final_knn_model <- list()
# for (cls in classes) {
#   y_bin <- factor(ifelse(y == cls, "Yes", "No"))
#   final_knn_model[[cls]] <- list(
#     X_train = X_scaled,
#     y_bin = y_bin
#   )
# }
#
# # Sauvegarde
# save(final_knn_model, preproc, classes, file = "final_ova_KNN_model.rda")
#
# # === Prédiction future ===
# # Suppose que X_new est défini (avec échantillons en colonnes)
# # X_new <- t(X_new)
# # X_new <- X_new[, colnames(X_scaled), drop = FALSE]
# # X_new_scaled <- predict(preproc, X_new)
#
# # scores_new <- sapply(classes, function(cls) {
# #   model <- final_knn_model[[cls]]
# #   pred <- knn(train = model$X_train, test = X_new_scaled, cl = model$y_bin, k = 3, prob = TRUE)
# #   ifelse(pred == "Yes", attr(pred, "prob"), 1 - attr(pred, "prob"))
# # })
#
# # y_pred_new <- apply(scores_new, 1, function(row) classes[which.max(row)])
# # y_pred_new <- factor(y_pred_new, levels = classes)
#
# # print(y_pred_new)
#
#
#
#
# ########
#
# ### KNN OVO + VOTING TIE
#
#
# library(class)
# library(caret)
# library(dplyr)
# library(tibble)
# library(gtools)  # Pour combinations
#
# # Données (X en lignes, y facteur)
# y <- as.factor(y)
# X <- t(X)
#
# # Nettoyage
# X <- X[, apply(X, 2, var, na.rm = TRUE) != 0]
# if (anyNA(X)) {
#   X[is.na(X)] <- colMeans(X, na.rm = TRUE)
# }
#
# # Normalisation
# preProc <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProc, X)
#
# # Paramètres
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
# classes <- levels(y)
# pairs <- combinations(length(classes), 2, classes)
#
# # Résultats initiaux
# knn_ovo_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Boucle des folds
# for (i in seq_along(folds)) {
#   test_idx <- folds[[i]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, ]
#   y_train <- y[train_idx]
#   X_test <- X_scaled[test_idx, ]
#   y_test <- y[test_idx]
#
#   # Votes
#   vote_train <- matrix(0, nrow = length(train_idx), ncol = length(classes), dimnames = list(NULL, classes))
#   vote_test  <- matrix(0, nrow = length(test_idx),  ncol = length(classes), dimnames = list(NULL, classes))
#
#   for (p in 1:nrow(pairs)) {
#     cls1 <- pairs[p, 1]
#     cls2 <- pairs[p, 2]
#
#     idx_pair <- y_train %in% c(cls1, cls2)
#     X_pair <- X_train[idx_pair, ]
#     y_pair <- y_train[idx_pair]
#
#     pred_train <- knn(X_pair, X_train, y_pair, k = 3)
#     pred_test  <- knn(X_pair, X_test,  y_pair, k = 3)
#
#     for (s in 1:length(pred_train)) {
#       vote_train[s, as.character(pred_train[s])] <- vote_train[s, as.character(pred_train[s])] + 1
#     }
#     for (s in 1:length(pred_test)) {
#       vote_test[s, as.character(pred_test[s])] <- vote_test[s, as.character(pred_test[s])] + 1
#     }
#   }
#
#   # Prédiction finale avec départage
#   predict_vote <- function(votes) {
#     apply(votes, 1, function(v) {
#       winners <- names(which(v == max(v)))
#       if (length(winners) > 1) winners[1] else winners
#     })
#   }
#
#   y_pred_train <- factor(predict_vote(vote_train), levels = classes)
#   y_pred_test  <- factor(predict_vote(vote_test),  levels = classes)
#
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   knn_ovo_results <- knn_ovo_results %>% add_row(
#     Fold = paste0("Fold", i),
#     Accuracy.empirique = cm_train$overall["Accuracy"],
#     Accuracy.generalization = cm_test$overall["Accuracy"],
#     Precision.empirique = mean(cm_train$byClass[,"Precision"], na.rm = TRUE),
#     Precision.generalization = mean(cm_test$byClass[,"Precision"], na.rm = TRUE),
#     Recall.empirique = mean(cm_train$byClass[,"Recall"], na.rm = TRUE),
#     Recall.generalization = mean(cm_test$byClass[,"Recall"], na.rm = TRUE),
#     F1.empirique = mean(cm_train$byClass[,"F1"], na.rm = TRUE),
#     F1.generalization = mean(cm_test$byClass[,"F1"], na.rm = TRUE),
#     Methods = "KNN_OvO",
#     Features = ncol(X_scaled)
#   )
# }
#
# print(knn_ovo_results)
# ovo_knn_balance = knn_ovo_results
# save(ovo_knn_balance, file = "ovo_knn_balance.rda")
#
# final_ovo_knn_model <- list()
#
# for (pair in pairs) {
#   idx_pair <- y %in% pair
#   final_ovo_knn_model[[paste(pair, collapse = "_vs_")]] <- list(
#     X = X_scaled[idx_pair, ],
#     y = y[idx_pair]
#   )
# }
#
# save(final_ovo_knn_model, preproc, classes, file = "final_ovo_KNN_model.rda")
#
# # Charger
# load("final_ovo_KNN_model.rda")
#
# # Nouvelle donnée
# X_new <- t(X_new)  # Assurez-vous que les lignes = échantillons
# X_new_scaled <- predict(preproc, X_new)
#
# # Vote
# vote_matrix <- matrix(NA, nrow = nrow(X_new_scaled), ncol = length(final_ovo_knn_model))
# colnames(vote_matrix) <- names(final_ovo_knn_model)
#
# for (j in seq_along(final_ovo_knn_model)) {
#   model <- final_ovo_knn_model[[j]]
#   vote_matrix[, j] <- knn(train = model$X, test = X_new_scaled, cl = model$y, k = 3)
# }
#
# # Vote majoritaire avec départage
# y_pred_new <- apply(vote_matrix, 1, function(votes) {
#   tab <- table(na.omit(votes))
#   if (length(tab) == 0) return(NA)
#   winners <- names(tab)[tab == max(tab)]
#   if (length(winners) == 1) return(winners)
#   sample(winners, 1)
# })
# y_pred_new <- factor(y_pred_new, levels = classes)
# print(y_pred_new)
