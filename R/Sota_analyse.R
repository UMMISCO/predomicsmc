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
# sota_svm_metrics_results_no_balance_3 <- svm_metrics_results
# save(sota_svm_metrics_results_no_balance_3, file = "sota_svm_metrics_results_no_balance_3.rda")
#
#
# ## Gradient Boosting Model (GBM)
#
# # Charger les bibliothèques nécessaires
# library(gbm)       # Pour la création de modèles GBM
# library(caret)     # Pour la validation croisée et les métriques
# library(dplyr)     # Pour la manipulation des données
# library(tibble)    # Pour une manipulation avancée des data frames
#
# # S'assurer que y est un facteur
# y <- as.factor(y)
#
# # Transposer X si nécessaire et convertir en data frame
# X <- as.data.frame(t(X))
#
# # Définir la graine pour assurer la reproductibilité
# set.seed(123)
#
# # Initialisation d'un tibble pour stocker les résultats des métriques par pli
# gbm_metrics_results <- tibble(
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
# # Boucle sur les 10 plis pour la validation croisée
# fold_idx <- 1
#
# for (i in 1:5) {
#   # Définir les indices d'entraînement et de validation
#   index_train <- createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)[[i]]
#   X_train <- as.data.frame(X[index_train, ])
#   y_train <- y[index_train]
#   X_val <- as.data.frame(X[-index_train, ])
#   y_val <- y[-index_train]
#
#   # Entraînement du modèle Gradient Boosting
#   gbm_model <- gbm.fit(
#     x = as.matrix(X_train),
#     y = as.numeric(y_train) - 1,  # Convertir les niveaux de facteur en valeurs numériques
#     distribution = "multinomial", # Distribution pour la classification multiclasse
#     n.trees = 100,               # Nombre total d'arbres
#     interaction.depth = 3,       # Profondeur maximale des arbres
#     shrinkage = 0.01,            # Taux d'apprentissage
#     n.minobsinnode = 10,         # Nombre minimum d'observations par nœud terminal
#     train.fraction = 1.0,        # Utiliser l'ensemble complet pour l'entraînement
#     verbose = FALSE              # Désactiver les messages d'entraînement
#   )
#
#   # Faire les prédictions sur l'ensemble d'entraînement
#   y_train_prob <- predict(gbm_model, as.matrix(X_train), n.trees = 100, type = "response")
#   y_train_pred <- apply(y_train_prob, 1, which.max)
#   y_train_pred <- factor(y_train_pred, levels = 1:length(levels(y)), labels = levels(y))
#
#   # Faire les prédictions sur l'ensemble de validation
#   y_val_prob <- predict(gbm_model, as.matrix(X_val), n.trees = 100, type = "response")
#   y_val_pred <- apply(y_val_prob, 1, which.max)
#   y_val_pred <- factor(y_val_pred, levels = 1:length(levels(y)), labels = levels(y))
#
#   # Vérifier la correspondance des tailles
#   if (length(y_train) == length(y_train_pred) & length(y_val) == length(y_val_pred)) {
#     # Calcul des métriques pour l'ensemble d'entraînement
#     cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#     accuracy_empirical <- cm_train$overall['Accuracy']
#     precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#     recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#     f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#     # Calcul des métriques pour l'ensemble de validation
#     cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#     accuracy_generalization <- cm_val$overall['Accuracy']
#     precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#     recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#     f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#     # Stockage des métriques dans le tibble
#     gbm_metrics_results <- gbm_metrics_results %>% add_row(
#       Fold = paste0("Fold", fold_idx),
#       Accuracy.empirique = accuracy_empirical,
#       Accuracy.generalization = accuracy_generalization,
#       Precision.empirique = precision_empirical,
#       Precision.generalization = precision_generalization,
#       Recall.empirique = recall_empirical,
#       Recall.generalization = recall_generalization,
#       F1.empirique = f1_empirical,
#       F1.generalization = f1_generalization,
#       Methods = "GBM",
#       Features = ncol(X_train)  # Ajout du nombre de features utilisées
#     )
#   } else {
#     warning(paste("Les tailles des prédictions et des vraies valeurs ne correspondent pas au fold", fold_idx))
#   }
#
#   fold_idx <- fold_idx + 1
# }
#
# # Afficher les résultats des métriques
# print(gbm_metrics_results)
#
# # Sauvegarder les résultats des métriques pour les modèles GBM
# sota_gbm_metrics_results_no_balance_3 <- gbm_metrics_results
# save(sota_gbm_metrics_results_no_balance_3, file = "sota_gbm_metrics_results_no_balance_3.rda")
#
# #
#
#
# ## Multiple decision trees
#
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
# for (i in 1:5) {
#   # Définir les indices d'entraînement et de validation
#   index_train <- createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)[[i]]
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
# sota_dt_metrics_results_no_balance_3 = dt_metrics_results
# save(sota_dt_metrics_results_no_balance_3, file = "sota_dt_metrics_results_no_balance_3.rda")
#
#
#
#
#
# ## Random Forest
#
#
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
# folds <- createFolds(y, k = 5, list = TRUE)
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
# sota_forest_metrics_results_no_balance_3 <- forest_metrics_results
# save(sota_forest_metrics_results_no_balance_3, file = "sota_forest_metrics_results_no_balance_3.rda")
#
#
#
#
# # Charger les bibliothèques nécessaires
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
# for (i in 1:5) {
#   # Créer les indices d'entraînement et de validation pour le pli actuel
#   index_train <- createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)[[i]]
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
# sota_knn_metrics_results_no_balance_3 = knn_metrics_results
# # Sauvegarder les résultats dans un fichier
# save(sota_knn_metrics_results_no_balance_3, file = "sota_knn_metrics_results_no_balance_3.rda")
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
# Sota_RL_CRC <- logreg_metrics_results
# save(Sota_RL_CRC, file = "Sota_RL_CRC.rda")













