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
# # Normalize features
# preProcValues <- preProcess(X, method = c("center", "scale"))
# X_scaled <- predict(preProcValues, X)
#
# # Stratified 10-fold cross-validation setup
# set.seed(123)
# train_control <- trainControl(method = "cv", number = 10)
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
#   k = numeric()
# )
#
# # Function to calculate precision, recall, and F1-score
# compute_metrics <- function(conf_matrix) {
#   # Extract confusion matrix components
#   TP <- conf_matrix[1, 1]  # True Positive
#   FP <- conf_matrix[1, 2]  # False Positive
#   FN <- conf_matrix[2, 1]  # False Negative
#   TN <- conf_matrix[2, 2]  # True Negative
#
#   # Compute metrics
#   precision <- TP / (TP + FP)
#   recall <- TP / (TP + FN)
#   f1 <- 2 * (precision * recall) / (precision + recall)
#
#   return(c(precision, recall, f1))
# }
#
# # Perform cross-validation over 10 folds
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Generate folds ensuring presence of all classes
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#   X_train <- X_scaled[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X_scaled[-index_train, ]
#   y_val <- y[-index_train]
#
#   # Train SVM model
#   svm_model <- svm(x = X_train, y = y_train, kernel = 'linear', probability = TRUE)
#
#   # Predict on training and validation sets
#   y_train_pred <- predict(svm_model, X_train)
#   y_val_pred <- predict(svm_model, X_val)
#
#   # Generate confusion matrices
#   conf_train <- table(Predicted = y_train_pred, Actual = y_train)
#   conf_val <- table(Predicted = y_val_pred, Actual = y_val)
#
#   # Compute empirical metrics
#   accuracy_empirical <- sum(diag(conf_train)) / sum(conf_train)
#   metrics_empirical <- compute_metrics(conf_train)
#   precision_empirical <- metrics_empirical[1]
#   recall_empirical <- metrics_empirical[2]
#   f1_empirical <- metrics_empirical[3]
#
#   # Compute generalization metrics
#   accuracy_generalization <- sum(diag(conf_val)) / sum(conf_val)
#   metrics_generalization <- compute_metrics(conf_val)
#   precision_generalization <- metrics_generalization[1]
#   recall_generalization <- metrics_generalization[2]
#   f1_generalization <- metrics_generalization[3]
#
#   # Store results
#   svm_metrics_results <- svm_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "SVM",
#     k = 3385
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
# # Display the results data frame
# print(svm_metrics_results)
#
# # Save the results
# sota_svm_metrics_results_balance <- svm_metrics_results
# save(sota_svm_metrics_results_balance, file = "sota_svm_metrics_results_balance.rda")
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
#   k = numeric()
# )
#
# # Boucle sur les 10 plis pour la validation croisée
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Définir les indices d'entraînement et de validation
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
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
#       k = 3385
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
# # Sauvegarder les résultats des métriques pour les modèles GBM
# sota_gbm_metrics_results_balance <- gbm_metrics_results
# save(sota_gbm_metrics_results_balance, file = "sota_gbm_metrics_results_balance.rda")
#
#
#
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
# X <- as.data.frame(t(X))  # Assurez-vous que X est un data frame et transposé si nécessaire
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
#   k = numeric()
# )
#
# # Boucle sur chaque fold pour la validation croisée
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Définir les indices d'entraînement et de validation
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#
#   X_train <- as.data.frame(X[index_train, ])
#   y_train <- y[index_train]
#   X_val <- as.data.frame(X[-index_train, ])
#   y_val <- y[-index_train]
#
#   # Créer et entraîner le modèle d'arbre de décision
#   dt_model <- rpart(y_train ~ ., data = data.frame(y_train, X_train), method = "class")
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
#       k = 3385
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
# sota_dt_metrics_results_balance = dt_metrics_results
# save(sota_dt_metrics_results_balance, file = "sota_dt_metrics_results_balance.rda")
#
#
#
#
#
# ## Random Forest
#
# # Charger les bibliothèques nécessaires
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # S'assurer que y est un facteur
# y <- as.factor(y)
# X <- t(X)  # Assurez-vous que X est transposé si nécessaire
# set.seed(123)
#
# # Initialiser un tibble pour stocker les résultats des métriques
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
#   k = numeric()
# )
#
# # Boucle sur chaque fold (validation croisée à 10 folds)
# fold_idx <- 1
# folds <- createFolds(y, k = 10, list = TRUE)
#
# for (i in 1:10) {
#   # Indices pour l'entraînement et la validation
#   index_train <- folds[[i]]
#   X_train <- X[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X[-index_train, ]
#   y_val <- y[-index_train]
#
#   # Entraîner le modèle Random Forest
#   forest_model <- randomForest(x = X_train, y = y_train, ntree = 1000)
#
#   # Prédictions sur l'ensemble d'entraînement (empirique)
#   y_train_pred <- predict(forest_model, X_train)
#
#   # Prédictions sur l'ensemble de validation (généralisation)
#   y_val_pred <- predict(forest_model, X_val)
#
#   # Vérifier que les tailles des prédictions et des vraies valeurs correspondent
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
#       k = 3385
#     )
#   } else {
#     warning(paste("La longueur des prédictions et des vraies valeurs ne correspond pas au fold", fold_idx))
#   }
#
#   fold_idx <- fold_idx + 1
# }
#
# # Afficher le DataFrame des résultats
# print(forest_metrics_results)
#
#
# sota_forest_metrics_results_balance = forest_metrics_results
#
# # Save the results
# save(sota_forest_metrics_results_balance, file = "sota_forest_metrics_results_balance.rda")
#
#
#
#
#
# ## Logistic Regression
#
# # Load necessary libraries
# library(glmnet)  # For logistic regression with cross-validation
# library(caret)   # For data partitioning and evaluation metrics
# library(dplyr)   # For data manipulation
# library(tibble)  # For structured data storage
#
# # Ensure the target variable (y) is a factor
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)  # Set seed for reproducibility
#
# # Remove constant columns (zero variance)
# X_non_constant <- X[, apply(X, 2, var, na.rm = TRUE) != 0]
#
# # Check for missing values and perform imputation if necessary
# if (any(is.na(X_non_constant))) {
#   # Simple mean imputation for missing values
#   X_non_constant[is.na(X_non_constant)] <- colMeans(X_non_constant, na.rm = TRUE)
# }
#
# # Initialize a tibble to store metrics for each fold
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
#   k = numeric()
# )
#
# # Perform k-fold cross-validation (k=10)
# fold_idx <- 1
# for (i in 1:10) {
#   # Create training and validation indices for the current fold
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#
#   # Split the data into training and validation sets
#   X_train <- X_non_constant[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X_non_constant[-index_train, ]
#   y_val <- y[-index_train]
#
#   # Train a multinomial logistic regression model using cross-validation
#   logreg_model <- cv.glmnet(as.matrix(X_train), y_train, family = "multinomial", alpha = 1)
#
#   # Predict on the training set (empirical predictions)
#   y_train_pred <- predict(logreg_model, newx = as.matrix(X_train), s = "lambda.min", type = "class")
#   y_train_pred <- factor(y_train_pred, levels = levels(y))  # Convert predictions to factor
#
#   # Predict on the validation set (generalization predictions)
#   y_val_pred <- predict(logreg_model, newx = as.matrix(X_val), s = "lambda.min", type = "class")
#   y_val_pred <- factor(y_val_pred, levels = levels(y))  # Convert predictions to factor
#
#   # Compute empirical metrics on the training set
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   accuracy_empirical <- cm_train$overall['Accuracy']
#   precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#   recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#   f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#   # Compute generalization metrics on the validation set
#   cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#   accuracy_generalization <- cm_val$overall['Accuracy']
#   precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#   recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#   f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#   # Store the results for the current fold
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
#     k = 3385
#   )
#
#   # Increment the fold index
#   fold_idx <- fold_idx + 1
# }
#
# # Display the results tibble
# print(logreg_metrics_results)
#
# # Save the results to a file
# sota_logreg_metrics_results_balance <- logreg_metrics_results
# save(sota_logreg_metrics_results_balance, file = "sota_logreg_metrics_results_balance.rda")
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
#   k = numeric()  # Ajouter la colonne Nbre_Features
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
#     k = 3385  # Ajouter la valeur de Nbre_Features
#   )
#
#   # Incrémenter l'indice du pli
#   fold_idx <- fold_idx + 1
# }
#
# # Afficher le tableau des résultats
# print(knn_metrics_results)
# sota_knn_metrics_results_balance = knn_metrics_results
# # Sauvegarder les résultats dans un fichier
# save(sota_knn_metrics_results_balance, file = "sota_knn_metrics_results_balance.rda")
#
# sota_ann_metrics_results_no_balance <- sota_ann_metrics_results_no_balance %>%
#   rename(k = Features)
#
# sota_ann_metrics_results_balance = sota_ann_metrics_results_no_balance
# # Afficher le dataframe modifié
# save(sota_ann_metrics_results_balance, file = "sota_ann_metrics_results_balance.rda")
#
#
#
#
#
# ####################### Predomics #######################################################
#
# #########################################################################################
#
#
# ## 1 Terbeam Maximization constrained
# num_folds <- 10  # Initialisation du nombre de plis
#
# # Création du data frame
# terbeam_maximization_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terbeam_maximization_constrained_balance",  # Méthode spécifiée
#   k = rep(4, num_folds)  # Ajouter la colonne Nbre_Features avec la valeur 4
# )
#
# # Vérification de la structure de 'terbeam_maximization_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_maximization_constrained") &&
#     !is.null(terbeam_maximization_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_maximization_constrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$Precision.empirique <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$Precision.generalization <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$Recall.empirique <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$Recall.generalization <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$F1.empirique <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_maximization_constrained_df$F1.generalization <- as.numeric(
#     terbeam_maximization_constrained$crossVal$scores$generalization.f1s["k_4", ]
#   )
#
# } else {
#   stop("L'objet 'terbeam_maximization_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_maximization_constrained_df)
# terbeam_maximization_constrained_df_balance = terbeam_maximization_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_maximization_constrained_df_balance, file = "terbeam_maximization_constrained_df_balance.rda")
#
#
#
# ## 2 Terbeam Maximization unconstrained
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle unconstrained
# terbeam_maximization_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terbeam_maximization_unconstrained_balance",  # Méthode spécifiée
#   k = rep(7, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_maximization_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_maximization_unconstrained") &&
#     !is.null(terbeam_maximization_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_maximization_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terbeam_maximization_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_maximization_unconstrained$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terbeam_maximization_unconstrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_maximization_unconstrained_df)
# terbeam_maximization_unconstrained_df_balance = terbeam_maximization_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_maximization_unconstrained_df_balance, file = "terbeam_maximization_unconstrained_df_balance.rda")
#
#
#
#
# ## 3. Terbeam ranking constrained
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle ranking constrained
# terbeam_ranking_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terbeam_ranking_constrained_balance",
#   k = rep(4, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_ranking_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_ranking_constrained") &&
#     !is.null(terbeam_ranking_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_ranking_constrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$Precision.empirique <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$Precision.generalization <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$Recall.empirique <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$Recall.generalization <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$F1.empirique <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_ranking_constrained_df$F1.generalization <- as.numeric(
#     terbeam_ranking_constrained$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("L'objet 'terbeam_ranking_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_ranking_constrained_df)
# terbeam_ranking_constrained_df_balance = terbeam_ranking_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_ranking_constrained_df_balance, file = "terbeam_ranking_constrained_df_balance.rda")
#
#
#
#
# #### 4
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle ranking unconstrained
# terbeam_ranking_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terbeam_ranking_unconstrained_balance",
#   k = rep(8, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_ranking_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_ranking_unconstrained") &&
#     !is.null(terbeam_ranking_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_ranking_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terbeam_ranking_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_ranking_unconstrained$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terbeam_ranking_unconstrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_ranking_unconstrained_df)
# terbeam_ranking_unconstrained_df_balance = terbeam_ranking_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_ranking_unconstrained_df_balance, file = "terbeam_ranking_unconstrained_df_balance.rda")
#
#
# #### 5
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle predomics_aggregation_ova_constrained
# terbeam_predomics_aggregation_ova_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terbeam_predomics_aggregation_ova_constrained_balance",
#   k = rep(4, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ova_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_predomics_aggregation_ova_constrained") &&
#     !is.null(terbeam_predomics_aggregation_ova_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ova_constrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$Precision.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$Precision.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$Recall.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$Recall.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$F1.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ova_constrained_df$F1.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_constrained$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ova_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ova_constrained_df)
# terbeam_predomics_aggregation_ova_constrained_df_balance = terbeam_predomics_aggregation_ova_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ova_constrained_df_balance, file = "terbeam_predomics_aggregation_ova_constrained_df_balance.rda")
#
#
#
# #### 6
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle predomics_aggregation_ova_unconstrained
# terbeam_predomics_aggregation_ova_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terbeam_predomics_aggregation_ova_unconstrained_balance" ,
#   k = rep(9, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ova_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_predomics_aggregation_ova_unconstrained") &&
#     !is.null(terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ova_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ova_unconstrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ova_unconstrained_df)
# terbeam_predomics_aggregation_ova_unconstrained_df_balance = terbeam_predomics_aggregation_ova_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ova_unconstrained_df_balance, file = "terbeam_predomics_aggregation_ova_unconstrained_df_balance.rda")
#
#
#
# #### 7
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle predomics_aggregation_ovo_constrained
# terbeam_predomics_aggregation_ovo_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terbeam_predomics_aggregation_ovo_constrained_balance",
#   k = rep(4, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ovo_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_predomics_aggregation_ovo_constrained") &&
#     !is.null(terbeam_predomics_aggregation_ovo_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ovo_constrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Precision.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Precision.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Recall.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Recall.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$F1.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$F1.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_constrained$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ovo_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ovo_constrained_df)
# terbeam_predomics_aggregation_ovo_constrained_df_balance = terbeam_predomics_aggregation_ovo_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ovo_constrained_df_balance, file = "terbeam_predomics_aggregation_ovo_constrained_df_balance.rda")
#
#
# #### 8
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle predomics_aggregation_ovo_unconstrained
# terbeam_predomics_aggregation_ovo_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terbeam_predomics_aggregation_ovo_unconstrained_balance",
#   k = rep(9, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ovo_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_predomics_aggregation_ovo_unconstrained") &&
#     !is.null(terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ovo_unconstrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ovo_unconstrained_df)
# terbeam_predomics_aggregation_ovo_unconstrained_df_balance = terbeam_predomics_aggregation_ovo_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ovo_unconstrained_df_balance, file = "terbeam_predomics_aggregation_ovo_unconstrained_df_balance.rda")
#
#
# #### 9
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terbeam_voting_constrained
# terbeam_voting_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terbeam_voting_constrained_balance" ,
#   k = rep(4, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_voting_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_voting_constrained") &&
#     !is.null(terbeam_voting_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_voting_constrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$Precision.empirique <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$Precision.generalization <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$Recall.empirique <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$Recall.generalization <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$F1.empirique <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_voting_constrained_df$F1.generalization <- as.numeric(
#     terbeam_voting_constrained$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("terbeam_voting_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_voting_constrained_df)
# terbeam_voting_constrained_df_balance = terbeam_voting_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_voting_constrained_df_balance, file = "terbeam_voting_constrained_df_balance.rda")
#
#
# #### 10
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terbeam_voting_unconstrained
# terbeam_voting_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terbeam_voting_unconstrained_balance",
#   k = rep(7, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_voting_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_voting_unconstrained") &&
#     !is.null(terbeam_voting_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_voting_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terbeam_voting_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terbeam_voting_unconstrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_voting_unconstrained_df)
# terbeam_voting_unconstrained_df_balance = terbeam_voting_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_voting_unconstrained_df_balance , file = "terbeam_voting_unconstrained_df_balance.rda")
#
# #### 11
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terbeam_weighted_constrained
# terbeam_weighted_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terbeam_weighted_constrained_balance" ,
#   k = rep(4, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_weighted_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_weighted_constrained") &&
#     !is.null(terbeam_weighted_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_weighted_constrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$Precision.empirique <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$Precision.generalization <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$Recall.empirique <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$Recall.generalization <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$F1.empirique <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_weighted_constrained_df$F1.generalization <- as.numeric(
#     terbeam_weighted_constrained$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("L'objet 'terbeam_weighted_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_weighted_constrained_df)
# terbeam_weighted_constrained_df_balance = terbeam_weighted_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_weighted_constrained_df_balance, file = "terbeam_weighted_constrained_df_balance.rda")
#
#
#
# #### 12
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terbeam_weighted_unconstrained
# terbeam_weighted_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terbeam_weighted_unconstrained_balance" ,
#   k = rep(9, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_weighted_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_weighted_unconstrained") &&
#     !is.null(terbeam_weighted_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_weighted_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terbeam_weighted_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_weighted_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terbeam_weighted_unconstrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_weighted_unconstrained_df)
# terbeam_weighted_unconstrained_df_balance = terbeam_weighted_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_weighted_unconstrained_df_balance, file = "terbeam_weighted_unconstrained_df_balance.rda")
#
#
#
#
#
#
#
#
#
#
#
# ################ Terga1
#
#
#
# ### 1
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_maximization_constrained
# terga1_maximization_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terga1_maximization_constrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_maximization_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_maximization_constrained") &&
#     !is.null(terga1_maximization_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_maximization_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$Precision.empirique <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$Precision.generalization <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$Recall.empirique <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$Recall.generalization <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$F1.empirique <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_maximization_constrained_df$F1.generalization <- as.numeric(
#     terga1_maximization_constrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_maximization_constrained_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_maximization_constrained_df)
# terga1_maximization_constrained_df_balance = terga1_maximization_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_maximization_constrained_df_balance, file = "terga1_maximization_constrained_df_balance.rda")
#
#
#
# ### 2
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_maximization_unconstrained
# terga1_maximization_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terga1_maximization_unconstrained_balance",
#   k = rep(9, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_maximization_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_maximization_unconstrained") &&
#     !is.null(terga1_maximization_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_maximization_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terga1_maximization_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_maximization_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terga1_maximization_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_maximization_unconstrained_df)
# terga1_maximization_unconstrained_df_balance = terga1_maximization_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_maximization_unconstrained_df_balance, file = "terga1_maximization_unconstrained_df_balance.rda")
#
#
#
# ### 3
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_ranking_constrained
# terga1_ranking_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terga1_ranking_constrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_ranking_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_ranking_constrained") &&
#     !is.null(terga1_ranking_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_ranking_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$Precision.empirique <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$Precision.generalization <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$Recall.empirique <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$Recall.generalization <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$F1.empirique <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_ranking_constrained_df$F1.generalization <- as.numeric(
#     terga1_ranking_constrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_ranking_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_ranking_constrained_df)
# terga1_ranking_constrained_df_balance = terga1_ranking_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_ranking_constrained_df_balance, file = "terga1_ranking_constrained_df_balance.rda")
#
# ### 4
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_ranking_unconstrained
# terga1_ranking_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terga1_ranking_unconstrained_balance" ,
#   k = rep(4, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_ranking_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_ranking_unconstrained") &&
#     !is.null(terga1_ranking_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_ranking_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terga1_ranking_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_ranking_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terga1_ranking_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_ranking_unconstrained_df)
# terga1_ranking_unconstrained_df_balance = terga1_ranking_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_ranking_unconstrained_df_balance, file = "terga1_ranking_unconstrained_df_balance.rda")
#
#
#
#
# ### 5
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_Predomics_aggregation_ova_constrained
# terga1_Predomics_aggregation_ova_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terga1_Predomics_aggregation_ova_constrained_balance" ,
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ova_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_Predomics_aggregation_ova_constrained") &&
#     !is.null(terga1_Predomics_aggregation_ova_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ova_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Precision.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Precision.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Recall.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Recall.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$F1.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$F1.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_constrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ova_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ova_constrained_df)
# terga1_Predomics_aggregation_ova_constrained_df_balance = terga1_Predomics_aggregation_ova_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ova_constrained_df_balance, file = "terga1_Predomics_aggregation_ova_constrained_df_balance.rda")
#
#
# ### 6
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_Predomics_aggregation_ova_unconstrained
# terga1_Predomics_aggregation_ova_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terga1_Predomics_aggregation_ova_unconstrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ova_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_Predomics_aggregation_ova_unconstrained") &&
#     !is.null(terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ova_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ova_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ova_unconstrained_df)
# terga1_Predomics_aggregation_ova_unconstrained_df_balance = terga1_Predomics_aggregation_ova_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ova_unconstrained_df_balance , file = "terga1_Predomics_aggregation_ova_unconstrained_df_balance.rda")
#
#
#
#
# ### 7
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_Predomics_aggregation_ovo_constrained
# terga1_Predomics_aggregation_ovo_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terga1_Predomics_aggregation_ovo_constrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ovo_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_Predomics_aggregation_ovo_constrained") &&
#     !is.null(terga1_Predomics_aggregation_ovo_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ovo_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Precision.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Precision.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Recall.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Recall.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$F1.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$F1.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ovo_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ovo_constrained_df)
# terga1_Predomics_aggregation_ovo_constrained_df_balance = terga1_Predomics_aggregation_ovo_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ovo_constrained_df_balance, file = "terga1_Predomics_aggregation_ovo_constrained_df_balance.rda")
#
# ### 8
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_Predomics_aggregation_ovo_unconstrained
# terga1_Predomics_aggregation_ovo_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terga1_Predomics_aggregation_ovo_unconstrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ovo_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_Predomics_aggregation_ovo_unconstrained") &&
#     !is.null(terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_unconstrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ovo_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ovo_unconstrained_df)
# terga1_Predomics_aggregation_ovo_unconstrained_df_balance = terga1_Predomics_aggregation_ovo_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ovo_unconstrained_df_balance, file = "terga1_Predomics_aggregation_ovo_unconstrained_df_balance.rda")
#
#
#
#
# ### 9
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_voting_constrained
# terga1_voting_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terga1_voting_constrained_balance",
#   k = rep(7, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_voting_constrained' et 'crossVal'
# if (exists("terga1_voting_constrained") &&
#     !is.null(terga1_voting_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_voting_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terga1_voting_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terga1_voting_constrained_df$Precision.empirique <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terga1_voting_constrained_df$Precision.generalization <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terga1_voting_constrained_df$Recall.empirique <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terga1_voting_constrained_df$Recall.generalization <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terga1_voting_constrained_df$F1.empirique <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terga1_voting_constrained_df$F1.generalization <- as.numeric(
#     terga1_voting_constrained$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terga1_voting_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_voting_constrained_df)
# terga1_voting_constrained_df_balance = terga1_voting_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_voting_constrained_df_balance, file = "terga1_voting_constrained_df_balance.rda")
#
# ### 10
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_voting_unconstrained
# terga1_voting_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terga1_voting_unconstrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_voting_unconstrained' et 'crossVal'
# if (exists("terga1_voting_unconstrained") &&
#     !is.null(terga1_voting_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_voting_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_voting_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_voting_unconstrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_voting_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_voting_unconstrained_df)
# terga1_voting_unconstrained_df_balance = terga1_voting_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_voting_unconstrained_df_balance, file = "terga1_voting_unconstrained_df_balance.rda")
#
#
#
#
# ### 11
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_weighted_constrained
# terga1_weighted_constrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "_terga1_weighted_constrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_weighted_constrained' et 'crossVal'
# if (exists("terga1_weighted_constrained") &&
#     !is.null(terga1_weighted_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_weighted_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$Precision.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$Precision.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$Recall.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$Recall.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$F1.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_weighted_constrained_df$F1.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_weighted_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_weighted_constrained_df)
# terga1_weighted_constrained_df_balance = terga1_weighted_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_weighted_constrained_df_balance, file = "terga1_weighted_constrained_df_balance.rda")
#
# ### 12
#
# num_folds <- 10
#
# # Initialisation du data frame pour le modèle terga1_weighted_unconstrained
# terga1_weighted_unconstrained_df <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy.empirique = numeric(num_folds),
#   Accuracy.generalization = numeric(num_folds),
#   Precision.empirique = numeric(num_folds),
#   Precision.generalization = numeric(num_folds),
#   Recall.empirique = numeric(num_folds),
#   Recall.generalization = numeric(num_folds),
#   F1.empirique = numeric(num_folds),
#   F1.generalization = numeric(num_folds),
#   Methods = "terga1_weighted_unconstrained_balance",
#   k = rep(10, num_folds)
#
# )
#
# # Vérification de la structure de 'terga1_weighted_unconstrained' et 'crossVal'
# if (exists("terga1_weighted_unconstrained") &&
#     !is.null(terga1_weighted_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_weighted_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_weighted_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_weighted_unconstrained$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_weighted_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_weighted_unconstrained_df)
# terga1_weighted_unconstrained_df_balance = terga1_weighted_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_weighted_unconstrained_df_balance, file = "terga1_weighted_unconstrained_df_balance.rda")
#
#
#
#
# analysis_table_final22 <- bind_rows(
#   terga1_Predomics_aggregation_ova_unconstrained_df,
#   terga1_Predomics_aggregation_ovo_unconstrained_df,
#   terga1_maximization_unconstrained_df,
#   terga1_ranking_unconstrained_df,
#   terga1_voting_unconstrained_df,
#   terga1_weighted_unconstrained_df,
#   terga1_Predomics_aggregation_ova_constrained_df,
#   terga1_Predomics_aggregation_ovo_constrained_df,
#   terga1_maximization_constrained_df,
#   terga1_ranking_constrained_df,
#   terga1_voting_constrained_df,
#   terga1_weighted_constrained_df,
#   terbeam_predomics_aggregation_ova_unconstrained_df,
#   terbeam_predomics_aggregation_ovo_unconstrained_df,
#   terbeam_maximization_unconstrained_df,
#   terbeam_ranking_unconstrained_df,
#   terbeam_voting_unconstrained_df,
#   terbeam_weighted_unconstrained_df,
#   terbeam_predomics_aggregation_ova_constrained_df,
#   terbeam_predomics_aggregation_ovo_constrained_df,
#   terbeam_maximization_constrained_df,
#   terbeam_ranking_constrained_df,
#   terbeam_voting_constrained_df,
#   terbeam_weighted_constrained_df,
#   sota_dt_metrics_results_balance,
#   sota_forest_metrics_results_balance,
#   sota_gbm_metrics_results_balance,
#   sota_knn_metrics_results_balance,
#   sota_svm_metrics_results_balance,
#   sota_logreg_metrics_results_balance,
#   sota_ann_metrics_results_balance
# )
# # Check the combined data frame
# print(head(analysis_table_final22))
# analysis_table_final_complet_balance = analysis_table_final22
# # Save the combined data frame to an RDA file
# save(analysis_table_final_complet_balance, file = "analysis_table_final_complet_balance.rda")
#
#
#
#
#
# ## Bon code remplacer le nom de la colonnes Features en K
#
#
#
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes (si nécessaire)
# analysis_table_final <- analysis_table_final22 %>%
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = -c(Fold, Methods, k),  # Inclure la colonne k
#     names_to = c("Metric", "Partition"),
#     names_sep = "\\.",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Partition = recode(Partition,
#                        'empirique' = 'train',
#                        'generalization' = 'test'
#     ),
#     Partition = factor(Partition, levels = c("train", "test")),
#     Value = as.numeric(Value),
#     Group = case_when(
#       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
#       TRUE ~ "SOTA"
#     )
#   )
#
# # Agréger les données pour le résumé
# df_summary <- df %>%
#   group_by(Methods, Partition, Metric, k) %>%  # Inclure k dans le groupement
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Vérification des valeurs de Group
# print(unique(df_summary$Group))
#
# # Fonction pour insérer un retour à la ligne après chaque 3e mot
# split_into_lines <- function(text, n = 3) {
#   words <- unlist(str_split(text, "_"))  # Séparer les mots sur les underscores
#   if (length(words) <= n) {
#     return(text)  # Si la chaîne a moins de 3 mots, ne rien changer
#   }
#   grouped_words <- paste0(words, collapse = " ")  # Remettre en forme avec des espaces
#   split_text <- str_wrap(grouped_words, width = n * 10, exdent = 1)  # Forcer le retour à la ligne
#   return(split_text)
# }
#
# # Modifier l'affichage des méthodes
# df_summary <- df_summary %>%
#   mutate(
#     Methods = sapply(Methods, split_into_lines),  # Appliquer la fonction
#     Methods = paste0(Methods, "\n k: ", k)  # Ajouter `k` après le nom
#   )
#
# # Vérification des résultats
# print(unique(df_summary$Methods))
#
# # Tracez le graphique
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +
#   ylab("Value") +
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = c("seagreen", "firebrick3")) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
