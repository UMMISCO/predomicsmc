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
#   Methods = character()
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
#     Methods = "SVM"
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
# # Display the results data frame
# print(svm_metrics_results)
#
# # Save the results
# sota_svm_metrics_results_no_balance <- svm_metrics_results
# save(sota_svm_metrics_results_no_balance, file = "sota_svm_metrics_results_no_balance.rda")
#
#
#
#
#
#
#
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
# # Assurez-vous que la variable cible y est un facteur
# y <- as.factor(y)
#
# # Transposer les données si nécessaire (assurez-vous que X a le bon format)
# X <- t(X)
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
#   Methods = character()
# )
#
# # Boucle sur les 10 plis pour la validation croisée
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Définir les indices d'entraînement et de validation pour le pli courant
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#   X_train <- X_scaled[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X_scaled[-index_train, ]
#   y_val <- y[-index_train]
#
#   # Entraînement du modèle Gradient Boosting
#   gbm_model <- gbm.fit(
#     x = X_train,
#     y = as.numeric(y_train) - 1,  # Convertir les niveaux de facteur en valeurs numériques (0, 1, 2, ...)
#     distribution = "multinomial", # Distribution pour la classification multiclasse
#     n.trees = 100,               # Nombre total d'arbres
#     interaction.depth = 3,       # Profondeur maximale des arbres
#     shrinkage = 0.01,            # Taux d'apprentissage
#     n.minobsinnode = 10,         # Nombre minimum d'observations par nœud terminal
#     train.fraction = 1.0,        # Utiliser l'ensemble complet pour l'entraînement
#     verbose = FALSE              # Désactiver les messages d'entraînement
#   )
#
#   # Prédictions sur l'ensemble d'entraînement (prédictions empiriques)
#   y_train_prob <- predict(gbm_model, X_train, n.trees = 100, type = "response")
#   y_train_pred <- apply(y_train_prob, 1, which.max) # Récupérer les classes avec la probabilité maximale
#   y_train_pred <- factor(y_train_pred, levels = 1:length(levels(y)), labels = levels(y))
#
#   # Prédictions sur l'ensemble de validation (prédictions de généralisation)
#   y_val_prob <- predict(gbm_model, X_val, n.trees = 100, type = "response")
#   y_val_pred <- apply(y_val_prob, 1, which.max)
#   y_val_pred <- factor(y_val_pred, levels = 1:length(levels(y)), labels = levels(y))
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
#   # Stockage des métriques dans le tibble
#   gbm_metrics_results <- gbm_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "GBM"
#   )
#
#   # Mise à jour de l'index du pli
#   fold_idx <- fold_idx + 1
# }
#
# # Afficher les résultats des métriques
# print(gbm_metrics_results)
#
# # Sauvegarder les résultats des métriques pour les modèles GBM
# sota_gbm_metrics_results_no_balance <- gbm_metrics_results
# save(sota_gbm_metrics_results_no_balance, file = "sota_gbm_metrics_results_no_balance.rda")
#
#
#
#
#
# ## Multiple decision trees
#
# # Load necessary libraries
# library(rpart)  # For decision trees
# library(caret)  # For cross-validation and metrics
# library(dplyr)  # For data manipulation
# library(tibble) # For advanced data frame manipulation
#
# # Ensure that y is a factor
# y <- as.factor(y)
# X <- as.data.frame(t(X))  # Ensure X is a data frame and correctly transposed if needed
# set.seed(123)
#
# # Initialize a tibble to store metrics for each fold
# dt_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirical = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirical = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirical = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirical = numeric(),
#   F1.generalization = numeric(),
#   Methods = character()
# )
#
# # Loop over each fold for cross-validation
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Define training and validation indices
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#
#   X_train <- as.data.frame(X[index_train, ])
#   y_train <- y[index_train]
#   X_val <- as.data.frame(X[-index_train, ])
#   y_val <- y[-index_train]
#
#   # Create and train the decision tree model
#   dt_model <- rpart(y_train ~ ., data = data.frame(y_train, X_train), method = "class")
#
#   # Make predictions on the training set (empirical)
#   y_train_pred <- predict(dt_model, X_train, type = "class")
#
#   # Make predictions on the validation set (generalization)
#   y_val_pred <- predict(dt_model, X_val, type = "class")
#
#   # Calculate empirical metrics
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#
#   accuracy_empirical <- cm_train$overall['Accuracy']
#   precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#   recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#   f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#   # Calculate generalization metrics
#   accuracy_generalization <- cm_val$overall['Accuracy']
#   precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#   recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#   f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#   # Store the results
#   dt_metrics_results <- dt_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirical = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirical = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirical = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirical = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "Decision Tree"
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
# # Display the results data frame
# print(dt_metrics_results)
#
# # Save the results
# sota_dt_metrics_results_no_balance = dt_metrics_results
# save(sota_dt_metrics_results_no_balance, file = "sota_dt_metrics_results_no_balance.rda")
#
#
#
#
#
# ## Random Forest
#
# # Load necessary libraries
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # Ensure that y is a factor
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# # Initialize a list to store metrics for each fold
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
#   Methods = character()
# )
#
# # Loop over each fold
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Define training and validation indices
#   index_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)[[i]]
#   X_train <- X_scaled[index_train, ]
#   y_train <- y[index_train]
#   X_val <- X_scaled[-index_train, ]
#   y_val <- y[-index_train]
#
#   # Create and train the Random Forest model (multiple decision trees)
#   forest_model <- randomForest(x = X_train, y = y_train, ntree = 100)
#
#   # Predictions on the training set (empirical)
#   y_train_pred <- predict(forest_model, X_train)
#
#   # Predictions on the validation set (generalization)
#   y_val_pred <- predict(forest_model, X_val)
#
#   # Compute empirical metrics
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#
#   accuracy_empirical <- cm_train$overall['Accuracy']
#   precision_empirical <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#   recall_empirical <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#   f1_empirical <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#   # Compute generalization metrics
#   accuracy_generalization <- cm_val$overall['Accuracy']
#   precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#   recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#   f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#   # Store the results
#   forest_metrics_results <- forest_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "Random Forest"
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
# # Display the results DataFrame
# print(forest_metrics_results)
# sota_forest_metrics_results_no_balance = forest_metrics_results
#
# # Save the results
# save(sota_forest_metrics_results_no_balance, file = "sota_forest_metrics_results_no_balance.rda")
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
#   Accuracy.empirical = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirical = numeric(),
#   Precision.generalization = numeric(),
#   Recall.empirical = numeric(),
#   Recall.generalization = numeric(),
#   F1.empirical = numeric(),
#   F1.generalization = numeric(),
#   Methods = character()
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
#     Accuracy.empirical = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirical = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirical = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirical = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "Logistic Regression (glmnet)"
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
# sota_logreg_metrics_results_no_balance <- logreg_metrics_results
# save(sota_logreg_metrics_results_no_balance, file = "sota_logreg_metrics_results_no_balance.rda")
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
#
#
#
# # Renommer les colonnes en français dans le dataframe
# sota_logreg_metrics_results_no_balance <- sota_logreg_metrics_results_no_balance %>%
#   rename(
#     Accuracy.empirique = Accuracy.empirical,
#     Precision.empirique = Precision.empirical,
#     Recall.empirique = Recall.empirical,
#     F1.empirique = F1.empirical
#   )
#
# # Afficher les résultats
# print(sota_logreg_metrics_results_no_balance)
#
#
#
# # Charger les bibliothèques nécessaires
# library(rpart)  # Pour les arbres de décision
# library(caret)  # Pour la validation croisée et les métriques
# library(dplyr)  # Pour la manipulation des données
# library(tibble) # Pour la manipulation avancée des dataframes
#
# # S'assurer que y est un facteur
# y <- as.factor(y)
# X <- as.data.frame(t(X))  # S'assurer que X est un dataframe et correctement transposé si nécessaire
# set.seed(123)
#
# # Initialiser un tibble pour stocker les métriques pour chaque pli
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
#   Methods = character()
# )
#
# # Boucle sur chaque pli pour la validation croisée
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
#   # Créer et entraîner le modèle de l'arbre de décision
#   dt_model <- rpart(y_train ~ ., data = data.frame(y_train, X_train), method = "class")
#
#   # Faire des prédictions sur l'ensemble d'entraînement (empirique)
#   y_train_pred <- predict(dt_model, X_train, type = "class")
#
#   # Faire des prédictions sur l'ensemble de validation (généralisation)
#   y_val_pred <- predict(dt_model, X_val, type = "class")
#
#   # Vérification des longueurs des prédictions et des labels
#   if(length(y_train_pred) != length(y_train)) {
#     stop("Les longueurs des prédictions et des labels d'entraînement ne correspondent pas.")
#   }
#
#   if(length(y_val_pred) != length(y_val)) {
#     stop("Les longueurs des prédictions et des labels de validation ne correspondent pas.")
#   }
#
#   # Convertir les prédictions et les labels en facteurs si nécessaire
#   y_train_pred <- factor(y_train_pred, levels = levels(y_train))
#   y_val_pred <- factor(y_val_pred, levels = levels(y_val))
#
#   # Calculer les métriques empiriques
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val <- caret::confusionMatrix(y_val_pred, y_val)
#
#   accuracy_empirique <- cm_train$overall['Accuracy']
#   precision_empirique <- mean(cm_train$byClass[,'Precision'], na.rm = TRUE)
#   recall_empirique <- mean(cm_train$byClass[,'Recall'], na.rm = TRUE)
#   f1_empirique <- mean(cm_train$byClass[,'F1'], na.rm = TRUE)
#
#   # Calculer les métriques de généralisation
#   accuracy_generalization <- cm_val$overall['Accuracy']
#   precision_generalization <- mean(cm_val$byClass[,'Precision'], na.rm = TRUE)
#   recall_generalization <- mean(cm_val$byClass[,'Recall'], na.rm = TRUE)
#   f1_generalization <- mean(cm_val$byClass[,'F1'], na.rm = TRUE)
#
#   # Stocker les résultats
#   dt_metrics_results <- dt_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirique,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirique,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirique,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirique,
#     F1.generalization = f1_generalization,
#     Methods = "Decision Tree"
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
# # Vérifier les résultats
# print(dt_metrics_results)
#
#
