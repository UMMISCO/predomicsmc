# # SVM NATIVE
#
# # Import necessary libraries
# library(e1071)   # For SVM model
# library(caret)   # For cross-validation and metrics
# library(dplyr)   # For data manipulation
# library(tibble)  # For advanced data frames
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
#   Precision.empirique = numeric(),       # (macro precision)
#   Precision.generalization = numeric(),  # (macro precision)
#   Recall.empirique = numeric(),          # (macro recall)
#   Recall.generalization = numeric(),     # (macro recall)
#   F1.empirique = numeric(),              # (macro F1)
#   F1.generalization = numeric(),         # (macro F1)
#   Methods = character(),
#   Features = numeric()
# )
#
# # --- Multiclass metrics (macro/micro/weighted) from a confusion matrix (compact) ---
# compute_metrics_multiclass <- function(conf_matrix) {
#   all_lvls <- union(rownames(conf_matrix), colnames(conf_matrix))
#   cm <- matrix(0, length(all_lvls), length(all_lvls),
#                dimnames = list(Predicted = all_lvls, Actual = all_lvls))
#   cm[rownames(conf_matrix), colnames(conf_matrix)] <- conf_matrix
#   TP <- diag(cm); FP <- rowSums(cm) - TP; FN <- colSums(cm) - TP
#   sup <- colSums(cm); eps <- 1e-12
#   prec <- TP / pmax(TP + FP, eps)
#   rec  <- TP / pmax(TP + FN, eps)
#   f1   <- 2 * prec * rec / pmax(prec + rec, eps)
#   macro <- c(precision = mean(prec, na.rm = TRUE),
#              recall    = mean(rec,  na.rm = TRUE),
#              f1        = mean(f1,   na.rm = TRUE))
#   micro_acc <- sum(TP) / sum(cm)  # equals micro-F1 in single-label multiclass
#   list(macro = macro, accuracy = micro_acc)
# }
#
# # Perform cross-validation over 10 folds
# fold_idx <- 1
#
# for (i in 1:10) {
#   # Extract training and validation sets
#   index_train <- unlist(folds[-i])      # 9 folds for training
#   index_val   <- folds[[i]]             # 1 fold for validation
#
#   X_train <- X_scaled[index_train, , drop = FALSE]
#   y_train <- y[index_train]
#   X_val   <- X_scaled[index_val, , drop = FALSE]
#   y_val   <- y[index_val]
#
#   # Ensure training set has at least two classes
#   if (length(unique(y_train)) < 2) {
#     next
#   }
#
#   # Train SVM model (native multiclass OvO)
#   svm_model <- svm(x = X_train, y = y_train, kernel = 'linear', probability = TRUE)
#
#   # Predict on training and validation sets
#   y_train_pred <- predict(svm_model, X_train)
#   y_val_pred   <- predict(svm_model, X_val)
#
#   # Size check (silent skip if mismatch)
#   if (length(y_val_pred) != length(y_val)) {
#     next
#   }
#
#   # Generate confusion matrices
#   conf_train <- table(Predicted = y_train_pred, Actual = y_train)
#   conf_val   <- table(Predicted = y_val_pred,   Actual = y_val)
#
#   # Compute empirical and generalization metrics (multiclass macro + accuracy)
#   m_emp <- compute_metrics_multiclass(conf_train)
#   m_gen <- compute_metrics_multiclass(conf_val)
#
#   # Store results (macro P/R/F1 + accuracy)
#   svm_metrics_results <- svm_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique      = m_emp$accuracy,
#     Accuracy.generalization = m_gen$accuracy,
#     Precision.empirique      = unname(m_emp$macro["precision"]),
#     Precision.generalization = unname(m_gen$macro["precision"]),
#     Recall.empirique         = unname(m_emp$macro["recall"]),
#     Recall.generalization    = unname(m_gen$macro["recall"]),
#     F1.empirique             = unname(m_emp$macro["f1"]),
#     F1.generalization        = unname(m_gen$macro["f1"]),
#     Methods = "SVM",
#     Features = ncol(X_scaled)
#   )
#
#   fold_idx <- fold_idx + 1
# }
# ##############################################################################################################################
#
#
# # --------------------------------------------------------------------------------------------------------------------
# # Decision Tree NATIVE  —  Multiclass CV (macro averages via caret::confusionMatrix), no prints
# # --------------------------------------------------------------------------------------------------------------------
#
# # Libraries
# library(rpart)   # decision trees
# library(caret)   # cross-validation and metrics
# library(dplyr)   # data manipulation
# library(tibble)  # tidy tables
#
# # Ensure labels are factors
# y <- as.factor(y)
#
# # Ensure valid column names before transpose
# colnames(X) <- make.names(colnames(X))
#
# # Transpose to have samples in rows, features in columns
# X <- t(X)
#
# set.seed(123)
#
# # Precompute stratified folds (train indices per fold)
# folds_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)
#
# # Initialize results container
# dt_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),       # macro precision (mean over classes)
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),          # macro recall
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),              # macro F1
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Cross-validation loop
# fold_idx <- 1
# for (i in seq_along(folds_train)) {
#   # Train/validation indices
#   index_train <- folds_train[[i]]
#   X_train <- as.data.frame(X[index_train, , drop = FALSE])
#   y_train <- y[index_train]
#   X_val   <- as.data.frame(X[-index_train, , drop = FALSE])
#   y_val   <- y[-index_train]
#
#   # Train decision tree (multiclass)
#   dt_model <- rpart(y_train ~ ., data = cbind(y_train = y_train, X_train), method = "class")
#
#   # Predictions
#   y_train_pred <- predict(dt_model, X_train, type = "class")
#   y_val_pred   <- predict(dt_model, X_val,   type = "class")
#
#   # Skip fold if size mismatch
#   if (!(length(y_train) == length(y_train_pred) && length(y_val) == length(y_val_pred))) {
#     next
#   }
#
#   # Metrics (caret returns per-class and overall; we macro-average byClass)
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val   <- caret::confusionMatrix(y_val_pred,   y_val)
#
#   accuracy_empirical       <- unname(cm_train$overall["Accuracy"])
#   precision_empirical      <- mean(cm_train$byClass[, "Precision"], na.rm = TRUE)
#   recall_empirical         <- mean(cm_train$byClass[, "Recall"],    na.rm = TRUE)
#   f1_empirical             <- mean(cm_train$byClass[, "F1"],        na.rm = TRUE)
#
#   accuracy_generalization  <- unname(cm_val$overall["Accuracy"])
#   precision_generalization <- mean(cm_val$byClass[, "Precision"], na.rm = TRUE)
#   recall_generalization    <- mean(cm_val$byClass[, "Recall"],    na.rm = TRUE)
#   f1_generalization        <- mean(cm_val$byClass[, "F1"],        na.rm = TRUE)
#
#   # Store
#   dt_metrics_results <- dt_metrics_results %>% add_row(
#     Fold = paste0("Fold", fold_idx),
#     Accuracy.empirique = accuracy_empirical,
#     Accuracy.generalization = accuracy_generalization,
#     Precision.empirique = precision_empirical,
#     Precision.generalization = precision_generalization,
#     Recall.empirique = recall_empirical,
#     Recall.generalization = recall_generalization,
#     F1.empirique = f1_empirical,
#     F1.generalization = f1_generalization,
#     Methods = "Decision Tree",
#     Features = ncol(X)   # number of features after transpose
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
#
# # --------------------------------------------------------------------------------------------------------------------
# # Random Forest NATIVE — Multiclass CV (macro averages via caret::confusionMatrix), no prints
# # --------------------------------------------------------------------------------------------------------------------
#
# # Libraries
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # Ensure labels are factors
# y <- as.factor(y)
#
# # Transpose to have samples in rows, features in columns
# X <- t(X)
#
# set.seed(123)
#
# # Prepare stratified folds (validation indices per fold)
# folds <- createFolds(y, k = 10, list = TRUE)
#
# # Results container
# forest_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),       # macro precision
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),          # macro recall
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),              # macro F1
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Cross-validation loop
# for (fold_idx in seq_along(folds)) {
#
#   # Train/validation split
#   index_val   <- folds[[fold_idx]]
#   index_train <- setdiff(seq_len(length(y)), index_val)
#
#   X_train <- X[index_train, , drop = FALSE]
#   y_train <- y[index_train]
#   X_val   <- X[index_val,   , drop = FALSE]
#   y_val   <- y[index_val]
#
#   # Train Random Forest (multiclass)
#   forest_model <- randomForest(x = X_train, y = y_train, ntree = 1000)
#
#   # Predictions
#   y_train_pred <- predict(forest_model, X_train)
#   y_val_pred   <- predict(forest_model, X_val)
#
#   # Skip fold if size mismatch
#   if (!(length(y_train) == length(y_train_pred) && length(y_val) == length(y_val_pred))) {
#     next
#   }
#
#   # Metrics (macro-averaged per class)
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val   <- caret::confusionMatrix(y_val_pred,   y_val)
#
#   accuracy_empirical       <- unname(cm_train$overall["Accuracy"])
#   precision_empirical      <- mean(cm_train$byClass[, "Precision"], na.rm = TRUE)
#   recall_empirical         <- mean(cm_train$byClass[, "Recall"],    na.rm = TRUE)
#   f1_empirical             <- mean(cm_train$byClass[, "F1"],        na.rm = TRUE)
#
#   accuracy_generalization  <- unname(cm_val$overall["Accuracy"])
#   precision_generalization <- mean(cm_val$byClass[, "Precision"], na.rm = TRUE)
#   recall_generalization    <- mean(cm_val$byClass[, "Recall"],    na.rm = TRUE)
#   f1_generalization        <- mean(cm_val$byClass[, "F1"],        na.rm = TRUE)
#
#   # Store results
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
#     Methods = "Random Forest",
#     Features = ncol(X)
#   )
# }
#
#
#
# # --------------------------------------------------------------------------------------------------------------------
# # KNN NATIVE — Multiclass CV (macro averages via caret::confusionMatrix), no prints
# # --------------------------------------------------------------------------------------------------------------------
#
# # Libraries
# library(class)   # KNN
# library(caret)   # CV and metrics
# library(dplyr)   # add_row
# library(tibble)  # tibble
#
# # Ensure target is a factor; transpose X -> samples in rows
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# # Remove constant (zero-variance) columns
# X_non_constant <- X[, apply(X, 2, var, na.rm = TRUE) != 0, drop = FALSE]
#
# # Simple NA imputation by column mean (if needed)
# if (any(is.na(X_non_constant))) {
#   cm <- colMeans(X_non_constant, na.rm = TRUE)
#   nas <- which(is.na(X_non_constant), arr.ind = TRUE)
#   X_non_constant[nas] <- cm[nas[,"col"]]
# }
#
# # Prepare stratified folds (train indices per fold)
# folds_train <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)
#
# # Results container
# knn_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),       # macro precision
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),          # macro recall
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),              # macro F1
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Cross-validation loop
# fold_idx <- 1
# for (i in seq_along(folds_train)) {
#   idx_train <- folds_train[[i]]
#   X_train <- X_non_constant[idx_train, , drop = FALSE]
#   y_train <- y[idx_train]
#   X_val   <- X_non_constant[-idx_train, , drop = FALSE]
#   y_val   <- y[-idx_train]
#
#   # KNN predictions (k=3)
#   y_train_pred <- class::knn(train = X_train, test = X_train, cl = y_train, k = 3)
#   y_val_pred   <- class::knn(train = X_train, test = X_val,   cl = y_train, k = 3)
#
#   # Force predicted factors to share levels with ground truth
#   y_train_pred <- factor(y_train_pred, levels = levels(y_train))
#   y_val_pred   <- factor(y_val_pred,   levels = levels(y_val))
#
#   # Skip fold if size mismatch
#   if (!(length(y_train_pred) == length(y_train) && length(y_val_pred) == length(y_val))) {
#     next
#   }
#
#   # Metrics (macro-averaged per class)
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val   <- caret::confusionMatrix(y_val_pred,   y_val)
#
#   accuracy_empirical       <- unname(cm_train$overall["Accuracy"])
#   precision_empirical      <- mean(cm_train$byClass[, "Precision"], na.rm = TRUE)
#   recall_empirical         <- mean(cm_train$byClass[, "Recall"],    na.rm = TRUE)
#   f1_empirical             <- mean(cm_train$byClass[, "F1"],        na.rm = TRUE)
#
#   accuracy_generalization  <- unname(cm_val$overall["Accuracy"])
#   precision_generalization <- mean(cm_val$byClass[, "Precision"], na.rm = TRUE)
#   recall_generalization    <- mean(cm_val$byClass[, "Recall"],    na.rm = TRUE)
#   f1_generalization        <- mean(cm_val$byClass[, "F1"],        na.rm = TRUE)
#
#   # Store results
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
#     Features = ncol(X_non_constant)
#   )
#
#   fold_idx <- fold_idx + 1
# }
#
#
#
#
# # ------------------------------------------------------------------------------------------------
# # Logistic Regression NATIVE (glmnet, multinomial) — 10-fold CV, no prints
# # ------------------------------------------------------------------------------------------------
#
# library(glmnet)  # multinomial logistic with CV
# library(caret)   # folds and metrics
# library(dplyr)   # add_row
# library(tibble)  # tibble
#
# # Ensure target is a factor; samples in rows
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# # Simple NA imputation by column mean (if needed)
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
#   }
# }
#
# # Results container
# logreg_metrics_results <- tibble(
#   Fold = character(),
#   Accuracy.empirique = numeric(),
#   Accuracy.generalization = numeric(),
#   Precision.empirique = numeric(),       # macro precision
#   Precision.generalization = numeric(),
#   Recall.empirique = numeric(),          # macro recall
#   Recall.generalization = numeric(),
#   F1.empirique = numeric(),              # macro F1
#   F1.generalization = numeric(),
#   Methods = character(),
#   Features = numeric()
# )
#
# # Stratified 10-fold (validation indices)
# set.seed(123)
# folds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
#
# # CV loop
# for (fold_idx in seq_along(folds)) {
#   index_val   <- folds[[fold_idx]]
#   index_train <- setdiff(seq_along(y), index_val)
#
#   X_train <- X[index_train, , drop = FALSE]
#   y_train <- y[index_train]
#   X_val   <- X[index_val,   , drop = FALSE]
#   y_val   <- y[index_val]
#
#   # Multinomial logistic regression with L1/L2 mixing (alpha=1 => Lasso)
#   logreg_model <- cv.glmnet(as.matrix(X_train), y_train, family = "multinomial", alpha = 1)
#
#   # Predictions (train/val) at lambda.min
#   y_train_pred <- predict(logreg_model, newx = as.matrix(X_train), s = "lambda.min", type = "class")
#   y_val_pred   <- predict(logreg_model, newx = as.matrix(X_val),   s = "lambda.min", type = "class")
#
#   # Align factor levels
#   y_train_pred <- factor(y_train_pred, levels = levels(y))
#   y_val_pred   <- factor(y_val_pred,   levels = levels(y))
#
#   # Metrics (macro-averaged per class)
#   cm_train <- caret::confusionMatrix(y_train_pred, y_train)
#   cm_val   <- caret::confusionMatrix(y_val_pred,   y_val)
#
#   accuracy_empirical       <- unname(cm_train$overall["Accuracy"])
#   precision_empirical      <- mean(cm_train$byClass[, "Precision"], na.rm = TRUE)
#   recall_empirical         <- mean(cm_train$byClass[, "Recall"],    na.rm = TRUE)
#   f1_empirical             <- mean(cm_train$byClass[, "F1"],        na.rm = TRUE)
#
#   accuracy_generalization  <- unname(cm_val$overall["Accuracy"])
#   precision_generalization <- mean(cm_val$byClass[, "Precision"], na.rm = TRUE)
#   recall_generalization    <- mean(cm_val$byClass[, "Recall"],    na.rm = TRUE)
#   f1_generalization        <- mean(cm_val$byClass[, "F1"],        na.rm = TRUE)
#
#   # Store
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
#
#
#
# ########################################################################################################################
#
# # Indirect Multiclass with SOTA Methods using the One-vs-One and One-vs-All approaches
#
# # ######################################################################################################################
#
#
# # ================================================
# # RANDOM FOREST OvA + MAXIMIZATION
# # ================================================
#
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # Example data (already transposed if needed)
# # X : feature matrix (samples in rows, variables in columns)
# # y : multiclass target vector
#
# # Ensure target is a factor
# y <- as.factor(y)
# X <- t(X)  # Transpose if samples are in columns
#
# classes <- levels(y)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # Initialize results container
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
#   X_test  <- X[test_idx, ]
#   y_test  <- y[test_idx]
#
#   # Score matrices for train and test
#   score_train <- matrix(NA, nrow = length(train_idx), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = length(test_idx),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   # Train one-vs-all models for each class
#   for (i in seq_along(classes)) {
#     current_class <- classes[i]
#     binary_y_train <- factor(ifelse(y_train == current_class, "Yes", "No"))
#
#     rf_model <- randomForest(x = X_train, y = binary_y_train, ntree = 500)
#
#     # Probabilistic predictions
#     prob_train <- predict(rf_model, X_train, type = "prob")[, "Yes"]
#     prob_test  <- predict(rf_model, X_test,  type = "prob")[, "Yes"]
#
#     score_train[, i] <- prob_train
#     score_test[, i]  <- prob_test
#   }
#
#   # Aggregation (maximization)
#   y_pred_train <- apply(score_train, 1, function(row) classes[which.max(row)])
#   y_pred_test  <- apply(score_test,  1, function(row) classes[which.max(row)])
#
#   y_pred_train <- factor(y_pred_train, levels = classes)
#   y_pred_test  <- factor(y_pred_test,  levels = classes)
#
#   # Evaluation
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
#
# # ======================================================
# # RANDOM FOREST OvO + VOTING WITH TIE-BREAKING
# # ======================================================
#
# library(randomForest)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # for combn()
#
# # Data
# y <- as.factor(y)
# X <- t(X)  # transpose if needed
#
# classes <- levels(y)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # Results container
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
# # === Tie-breaking function ===
# resolve_tie <- function(votes_row, classes) {
#   max_vote <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) {
#     return(classes[candidates])
#   } else {
#     # Random tie-breaking
#     return(sample(classes[candidates], 1))
#   }
# }
#
# # === Cross-validation OvO ===
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X)), test_idx)
#
#   X_train <- X[train_idx, ]
#   y_train <- y[train_idx]
#   X_test  <- X[test_idx, ]
#   y_test  <- y[test_idx]
#
#   # Generate all class pairs for OvO
#   class_pairs <- combn(classes, 2, simplify = FALSE)
#
#   # Vote matrices
#   votes_train <- matrix(0, nrow = length(train_idx), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = length(test_idx),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   # Train models and collect votes
#   for (pair in class_pairs) {
#     class1 <- pair[1]
#     class2 <- pair[2]
#
#     # Keep only samples belonging to the current pair
#     pair_idx_train <- which(y_train %in% c(class1, class2))
#     pair_y_train   <- droplevels(y_train[pair_idx_train])
#     pair_X_train   <- X_train[pair_idx_train, ]
#
#     rf_model <- randomForest(x = pair_X_train, y = pair_y_train, ntree = 500)
#
#     # Predictions (train and test)
#     pred_train <- predict(rf_model, X_train)
#     pred_test  <- predict(rf_model, X_test)
#
#     # Update votes
#     for (i in seq_along(train_idx)) {
#       pred_class <- as.character(pred_train[i])
#       if (pred_class %in% classes) {
#         votes_train[i, pred_class] <- votes_train[i, pred_class] + 1
#       }
#     }
#     for (i in seq_along(test_idx)) {
#       pred_class <- as.character(pred_test[i])
#       if (pred_class %in% classes) {
#         votes_test[i, pred_class] <- votes_test[i, pred_class] + 1
#       }
#     }
#   }
#
#   # Aggregation with tie-breaking
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test,  1, resolve_tie, classes = classes), levels = classes)
#
#   # Evaluate
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
#
# # ======================================================
# # SVM OvA + MAXIMIZATION
# # ======================================================
#
# # --- Libraries ---
# library(e1071)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # --- Data ---
# y <- as.factor(y)
# X <- t(X)  # transpose if samples are in columns
# classes <- levels(y)
#
# # --- Preprocessing ---
# zero_var_features <- nearZeroVar(X, saveMetrics = TRUE)
# X <- X[, !zero_var_features$zeroVar, drop = FALSE]
#
# preProc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preProc, X)
#
# # --- Stratified cross-validation ---
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # --- Results + Models ---
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
# fold_models <- list()  # store OvA SVM models per fold
#
# # --- OvA cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   # Score matrices (probability of "Yes" for each class model)
#   ova_fold    <- list()
#   score_train <- matrix(NA, nrow = length(train_idx), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = length(test_idx),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   # Train one-vs-all SVMs (linear kernel) and collect probabilities
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
#     ova_fold[[cls]] <- model
#
#     prob_train <- attr(predict(model, X_train, probability = TRUE), "probabilities")[, "Yes"]
#     prob_test  <- attr(predict(model, X_test,  probability = TRUE), "probabilities")[, "Yes"]
#
#     score_train[, cls] <- prob_train
#     score_test[, cls]  <- prob_test
#   }
#
#   # Aggregation by maximization across OvA scores
#   y_pred_train <- factor(apply(score_train, 1, function(row) classes[which.max(row)]), levels = classes)
#   y_pred_test  <- factor(apply(score_test,  1, function(row) classes[which.max(row)]), levels = classes)
#
#   # Evaluation (macro-averaged via caret)
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
#
#
#
# # ======================================================
# # SVM OvO + VOTING WITH TIE-BREAKING
# # ======================================================
#
# # --- Libraries ---
# library(e1071)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # for combn()
#
# # --- Data ---
# y <- as.factor(y)
# X <- t(X)  # transpose if samples are in columns
# classes <- levels(y)
#
# # --- Preprocessing ---
# zero_var_features <- nearZeroVar(X, saveMetrics = TRUE)
# X <- X[, !zero_var_features$zeroVar, drop = FALSE]
#
# preProc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preProc, X)
#
# # --- Tie-breaking function (random among tied classes) ---
# resolve_tie <- function(votes_row, classes) {
#   max_vote  <- max(votes_row)
#   tied_idx  <- which(votes_row == max_vote)
#   if (length(tied_idx) == 1) classes[tied_idx] else sample(classes[tied_idx], 1)
# }
#
# # --- Stratified cross-validation ---
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # --- Results + Models ---
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
# fold_models <- list()  # store OvO SVM models per fold
#
# # --- OvO cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   # All class pairs for OvO
#   class_pairs <- combn(classes, 2, simplify = FALSE)
#   ovo_fold    <- list()
#
#   # Vote matrices (rows = samples, cols = classes)
#   votes_train <- matrix(0, nrow = nrow(X_train), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   # Train a binary SVM for each pair and cast votes
#   for (pair in class_pairs) {
#     class1 <- pair[1]; class2 <- pair[2]
#
#     # Select samples belonging to the current pair (train side)
#     idx_pair_train <- which(y_train %in% c(class1, class2))
#     pair_X_train   <- X_train[idx_pair_train, , drop = FALSE]
#     pair_y_train   <- droplevels(y_train[idx_pair_train])
#
#     model <- svm(
#       x = pair_X_train,
#       y = pair_y_train,
#       kernel = "linear",
#       type = "C-classification"
#     )
#     ovo_fold[[paste(class1, class2, sep = "_vs_")]] <- model
#
#     # Predictions on full train/test (each model votes for its predicted class)
#     pred_train <- predict(model, X_train)
#     pred_test  <- predict(model, X_test)
#
#     # Accumulate votes (train)
#     for (i in seq_along(pred_train)) {
#       pc <- as.character(pred_train[i])
#       if (pc %in% classes) votes_train[i, pc] <- votes_train[i, pc] + 1
#     }
#     # Accumulate votes (test)
#     for (i in seq_along(pred_test)) {
#       pc <- as.character(pred_test[i])
#       if (pc %in% classes) votes_test[i, pc] <- votes_test[i, pc] + 1
#     }
#   }
#
#   # Aggregation with tie-breaking
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test,  1, resolve_tie, classes = classes), levels = classes)
#
#   # Evaluation (macro-averaged via caret)
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train  = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train    = mean(cm_train$byClass[, "Recall"],    na.rm = TRUE),
#     F1.train        = mean(cm_train$byClass[, "F1"],        na.rm = TRUE),
#     Accuracy.test   = cm_test$overall["Accuracy"],
#     Precision.test  = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test     = mean(cm_test$byClass[, "Recall"],    na.rm = TRUE),
#     F1.test         = mean(cm_test$byClass[, "F1"],        na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ovo_fold
# }
#
#
#
# # ======================================================
# # Logistic Regression OvA + MAXIMIZATION
# # ======================================================
#
# # --- Libraries ---
# library(glmnet)   # Regularized logistic regression
# library(caret)    # Cross-validation and metrics
# library(dplyr)    # Data manipulation
# library(tibble)   # Structured results
#
# # --- Data ---
# y <- as.factor(y)
# X <- t(X)  # transpose if samples are in columns
# classes <- levels(y)
#
# # --- Handle missing values ---
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # --- Normalization ---
# preProc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preProc, X)
#
# # --- Stratified cross-validation folds ---
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # --- Results container ---
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
# fold_models <- list()  # store OvA models per fold
#
# # --- OvA cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   ova_fold    <- list()
#   score_train <- matrix(NA, nrow = nrow(X_train), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   # Train one-vs-all logistic regressions
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#     model <- cv.glmnet(
#       x = as.matrix(X_train),
#       y = y_bin_train,
#       family = "binomial",
#       alpha = 1,
#       type.measure = "class"
#     )
#     ova_fold[[cls]] <- model
#
#     prob_train <- predict(model, newx = as.matrix(X_train), s = "lambda.min", type = "response")
#     prob_test  <- predict(model, newx = as.matrix(X_test),  s = "lambda.min", type = "response")
#
#     score_train[, cls] <- prob_train
#     score_test[, cls]  <- prob_test
#   }
#
#   # Aggregation by maximization across OvA scores
#   y_pred_train <- factor(apply(score_train, 1, function(row) classes[which.max(row)]), levels = classes)
#   y_pred_test  <- factor(apply(score_test,  1, function(row) classes[which.max(row)]), levels = classes)
#
#   # Evaluation (macro-averaged metrics)
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
#
#
# # ======================================================
# # LOGISTIC REGRESSION OvO + VOTING WITH TIE-BREAKING
# # ======================================================
#
# # --- Libraries ---
# library(glmnet)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # for combn()
#
# # --- Data ---
# y <- as.factor(y)
# X <- t(X)  # transpose if samples are in columns
# classes <- levels(y)
#
# # --- Handle missing values (column-mean imputation) ---
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # --- Normalization ---
# preProc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preProc, X)
#
# # --- Stratified folds ---
# set.seed(123)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # --- Tie-breaking (random among tied classes) ---
# resolve_tie <- function(votes_row, classes) {
#   max_vote   <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) classes[candidates] else sample(classes[candidates], 1)
# }
#
# # --- Results + model storage ---
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
# # --- OvO cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   ovo_fold <- list()
#   votes_train <- matrix(0, nrow = nrow(X_train), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   # Train pairwise models and collect votes
#   for (pair in class_pairs) {
#     class1 <- pair[1]; class2 <- pair[2]
#
#     idx_train_pair <- which(y_train %in% c(class1, class2))
#     X_pair <- X_train[idx_train_pair, , drop = FALSE]
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
#     for (i in seq_along(pred_test)) {
#       votes_test[i, as.character(pred_test[i])] <- votes_test[i, as.character(pred_test[i])] + 1
#     }
#   }
#
#   # Aggregation with tie-breaking
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test,  1, resolve_tie, classes = classes), levels = classes)
#
#   # Evaluation (macro-averaged via caret)
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train  = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train    = mean(cm_train$byClass[, "Recall"],    na.rm = TRUE),
#     F1.train        = mean(cm_train$byClass[, "F1"],        na.rm = TRUE),
#     Accuracy.test   = cm_test$overall["Accuracy"],
#     Precision.test  = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test     = mean(cm_test$byClass[, "Recall"],    na.rm = TRUE),
#     F1.test         = mean(cm_test$byClass[, "F1"],        na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ovo_fold
# }
#
#
#
# # ======================================================
# # DECISION TREE OvA + MAXIMIZATION
# # ======================================================
#
# library(rpart)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # --- Data ---
# y <- as.factor(y)
# X <- t(X)
# colnames(X) <- make.names(colnames(X))  # ensure valid column names
#
# # --- Normalization ---
# preProc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preProc, X)
#
# # --- Stratified cross-validation ---
# set.seed(123)
# k <- 10
# folds   <- createFolds(y, k = k, list = TRUE)
# classes <- levels(y)
#
# # --- Model storage per fold ---
# fold_models <- list()
#
# # --- Results container ---
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
# # --- OvA cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   ova_models  <- list()
#   score_train <- matrix(NA, nrow = nrow(X_train), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = nrow(X_test),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   # Train one-vs-all decision trees and collect probabilities for class "Yes"
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#     data_train  <- data.frame(X_train, y_bin_train = y_bin_train)
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
#   # Aggregation by maximization across OvA scores
#   y_pred_train <- factor(apply(score_train, 1, function(row) classes[which.max(row)]), levels = classes)
#   y_pred_test  <- factor(apply(score_test,  1, function(row) classes[which.max(row)]),  levels = classes)
#
#   # Evaluation (macro-averaged via caret)
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train  = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train    = mean(cm_train$byClass[, "Recall"],    na.rm = TRUE),
#     F1.train        = mean(cm_train$byClass[, "F1"],        na.rm = TRUE),
#     Accuracy.test   = cm_test$overall["Accuracy"],
#     Precision.test  = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test     = mean(cm_test$byClass[, "Recall"],    na.rm = TRUE),
#     F1.test         = mean(cm_test$byClass[, "F1"],        na.rm = TRUE)
#   )
#
#   fold_models[[fold_idx]] <- ova_models
# }
#
#
# # ======================================================
# # DECISION TREE OvO + VOTING WITH TIE-BREAKING
# # ======================================================
#
# library(rpart)
# library(caret)
# library(dplyr)
# library(tibble)
# library(combinat)  # for combn()
#
# # --- Data ---
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# classes <- levels(y)
# k <- 10
# folds <- createFolds(y, k = k, list = TRUE)
#
# # --- Results container ---
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
# # --- Tie-breaking function ---
# resolve_tie <- function(votes_row, classes) {
#   max_vote   <- max(votes_row)
#   candidates <- which(votes_row == max_vote)
#   if (length(candidates) == 1) classes[candidates] else sample(classes[candidates], 1)
# }
#
# # --- OvO cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X)), test_idx)
#
#   X_train <- as.data.frame(X[train_idx, ])
#   y_train <- y[train_idx]
#   X_test  <- as.data.frame(X[test_idx, ])
#   y_test  <- y[test_idx]
#
#   # Vote matrices
#   votes_train <- matrix(0, nrow = length(train_idx), ncol = length(classes))
#   votes_test  <- matrix(0, nrow = length(test_idx),  ncol = length(classes))
#   colnames(votes_train) <- colnames(votes_test) <- classes
#
#   # All class pairs (OvO scheme)
#   class_pairs <- combn(classes, 2, simplify = FALSE)
#
#   for (pair in class_pairs) {
#     class1 <- pair[1]; class2 <- pair[2]
#
#     idx_pair_train <- which(y_train %in% pair)
#     idx_pair_test  <- which(y_test %in% pair)
#
#     X_pair_train <- X_train[idx_pair_train, , drop = FALSE]
#     y_pair_train <- droplevels(y_train[idx_pair_train])
#
#     # Train decision tree for this class pair
#     model <- rpart(y_pair_train ~ ., data = cbind(y_pair_train = y_pair_train, X_pair_train), method = "class")
#
#     # Predictions on full train/test sets
#     pred_train <- predict(model, X_train, type = "class")
#     pred_test  <- predict(model, X_test,  type = "class")
#
#     # Accumulate votes (train)
#     for (i in seq_along(pred_train)) {
#       if (pred_train[i] %in% pair) {
#         votes_train[i, as.character(pred_train[i])] <- votes_train[i, as.character(pred_train[i])] + 1
#       }
#     }
#     # Accumulate votes (test)
#     for (i in seq_along(pred_test)) {
#       if (pred_test[i] %in% pair) {
#         votes_test[i, as.character(pred_test[i])] <- votes_test[i, as.character(pred_test[i])] + 1
#       }
#     }
#   }
#
#   # Aggregation by majority voting with tie-breaking
#   y_pred_train <- factor(apply(votes_train, 1, resolve_tie, classes = classes), levels = classes)
#   y_pred_test  <- factor(apply(votes_test,  1, resolve_tie, classes = classes), levels = classes)
#
#   # Evaluation (macro-averaged via caret)
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   ovo_dt_results <- ovo_dt_results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train  = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train    = mean(cm_train$byClass[, "Recall"],    na.rm = TRUE),
#     F1.train        = mean(cm_train$byClass[, "F1"],        na.rm = TRUE),
#     Accuracy.test   = cm_test$overall["Accuracy"],
#     Precision.test  = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test     = mean(cm_test$byClass[, "Recall"],    na.rm = TRUE),
#     F1.test         = mean(cm_test$byClass[, "F1"],        na.rm = TRUE)
#   )
# }
#
#
#
#
# # ======================================================
# # KNN OvA + MAXIMIZATION
# # ======================================================
#
# library(class)
# library(caret)
# library(dplyr)
# library(tibble)
#
# # --- Data preparation ---
# y <- as.factor(y)
# X <- t(X)
# set.seed(123)
#
# # Remove constant (zero-variance) columns
# X <- X[, apply(X, 2, var, na.rm = TRUE) != 0, drop = FALSE]
#
# # Simple mean imputation (if needed)
# if (any(is.na(X))) {
#   for (j in seq_len(ncol(X))) {
#     if (anyNA(X[, j])) {
#       X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)
#     }
#   }
# }
#
# # Normalization
# preproc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preproc, X)
#
# # Stratified folds
# folds   <- createFolds(y, k = 10, list = TRUE)
# classes <- levels(y)
#
# # Results container
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
# # --- OvA cross-validation loop ---
# for (fold_idx in seq_along(folds)) {
#   test_idx  <- folds[[fold_idx]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   # Score matrices (probability that sample belongs to each class 'Yes')
#   score_train <- matrix(NA, nrow = length(train_idx), ncol = length(classes))
#   score_test  <- matrix(NA, nrow = length(test_idx),  ncol = length(classes))
#   colnames(score_train) <- colnames(score_test) <- classes
#
#   # Train OvA by re-labeling target for each class and collect probabilities
#   for (cls in classes) {
#     y_bin_train <- factor(ifelse(y_train == cls, "Yes", "No"))
#
#     # class::knn with prob=TRUE returns the posterior of the winning class
#     pred_train <- knn(train = X_train, test = X_train, cl = y_bin_train, k = 3, prob = TRUE)
#     pred_test  <- knn(train = X_train, test = X_test,  cl = y_bin_train, k = 3, prob = TRUE)
#
#     # Convert returned posteriors to P(class == "Yes")
#     p_train <- attr(pred_train, "prob")
#     p_train <- ifelse(pred_train == "Yes", p_train, 1 - p_train)
#
#     p_test  <- attr(pred_test, "prob")
#     p_test  <- ifelse(pred_test  == "Yes", p_test,  1 - p_test)
#
#     score_train[, cls] <- p_train
#     score_test[, cls]  <- p_test
#   }
#
#   # Aggregation by maximization over class scores
#   y_pred_train <- factor(apply(score_train, 1, function(row) classes[which.max(row)]), levels = classes)
#   y_pred_test  <- factor(apply(score_test,  1, function(row) classes[which.max(row)]),  levels = classes)
#
#   # Evaluation (macro-averaged via caret)
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   results <- results %>% add_row(
#     Fold = fold_idx,
#     Accuracy.train  = cm_train$overall["Accuracy"],
#     Precision.train = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Recall.train    = mean(cm_train$byClass[, "Recall"],    na.rm = TRUE),
#     F1.train        = mean(cm_train$byClass[, "F1"],        na.rm = TRUE),
#     Accuracy.test   = cm_test$overall["Accuracy"],
#     Precision.test  = mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.test     = mean(cm_test$byClass[, "Recall"],    na.rm = TRUE),
#     F1.test         = mean(cm_test$byClass[, "F1"],        na.rm = TRUE)
#   )
# }
#
#
# # ======================================================
# # KNN OvO + VOTING WITH TIE-BREAKING
# # ======================================================
#
# library(class)
# library(caret)
# library(dplyr)
# library(tibble)
# library(gtools)  # for combinations()
#
# # --- Data preparation ---
# y <- as.factor(y)
# X <- t(X)
#
# # Remove constant features
# X <- X[, apply(X, 2, var, na.rm = TRUE) != 0, drop = FALSE]
#
# # Simple mean imputation if NA values exist
# if (anyNA(X)) {
#   X[is.na(X)] <- colMeans(X, na.rm = TRUE)
# }
#
# # Normalization
# preProc   <- preProcess(X, method = c("center", "scale"))
# X_scaled  <- predict(preProc, X)
#
# # --- Cross-validation parameters ---
# set.seed(123)
# k <- 10
# folds   <- createFolds(y, k = k, list = TRUE)
# classes <- levels(y)
# pairs   <- combinations(length(classes), 2, classes)
#
# # --- Results container ---
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
# # --- OvO cross-validation loop ---
# for (i in seq_along(folds)) {
#   test_idx  <- folds[[i]]
#   train_idx <- setdiff(seq_len(nrow(X_scaled)), test_idx)
#
#   X_train <- X_scaled[train_idx, , drop = FALSE]
#   y_train <- y[train_idx]
#   X_test  <- X_scaled[test_idx,  , drop = FALSE]
#   y_test  <- y[test_idx]
#
#   # Voting matrices
#   vote_train <- matrix(0, nrow = length(train_idx), ncol = length(classes), dimnames = list(NULL, classes))
#   vote_test  <- matrix(0, nrow = length(test_idx),  ncol = length(classes), dimnames = list(NULL, classes))
#
#   # Train OvO classifiers for each pair of classes
#   for (p in 1:nrow(pairs)) {
#     cls1 <- pairs[p, 1]
#     cls2 <- pairs[p, 2]
#
#     idx_pair <- y_train %in% c(cls1, cls2)
#     X_pair   <- X_train[idx_pair, , drop = FALSE]
#     y_pair   <- y_train[idx_pair]
#
#     # Predictions (train/test) restricted to the current pair
#     pred_train <- knn(train = X_pair, test = X_train, cl = y_pair, k = 3)
#     pred_test  <- knn(train = X_pair, test = X_test,  cl = y_pair, k = 3)
#
#     # Accumulate votes
#     for (s in seq_along(pred_train)) {
#       vote_train[s, as.character(pred_train[s])] <- vote_train[s, as.character(pred_train[s])] + 1
#     }
#     for (s in seq_along(pred_test)) {
#       vote_test[s, as.character(pred_test[s])] <- vote_test[s, as.character(pred_test[s])] + 1
#     }
#   }
#
#   # --- Final prediction with tie-breaking ---
#   predict_vote <- function(votes) {
#     apply(votes, 1, function(v) {
#       winners <- names(which(v == max(v)))
#       if (length(winners) > 1) winners[1] else winners  # tie-break arbitrarily
#     })
#   }
#
#   y_pred_train <- factor(predict_vote(vote_train), levels = classes)
#   y_pred_test  <- factor(predict_vote(vote_test),  levels = classes)
#
#   # Evaluation (macro-averaged metrics)
#   cm_train <- confusionMatrix(y_pred_train, y_train)
#   cm_test  <- confusionMatrix(y_pred_test,  y_test)
#
#   knn_ovo_results <- knn_ovo_results %>% add_row(
#     Fold = paste0("Fold", i),
#     Accuracy.empirique      = cm_train$overall["Accuracy"],
#     Accuracy.generalization = cm_test$overall["Accuracy"],
#     Precision.empirique     = mean(cm_train$byClass[, "Precision"], na.rm = TRUE),
#     Precision.generalization= mean(cm_test$byClass[, "Precision"], na.rm = TRUE),
#     Recall.empirique        = mean(cm_train$byClass[, "Recall"],    na.rm = TRUE),
#     Recall.generalization   = mean(cm_test$byClass[, "Recall"],    na.rm = TRUE),
#     F1.empirique            = mean(cm_train$byClass[, "F1"],       na.rm = TRUE),
#     F1.generalization       = mean(cm_test$byClass[, "F1"],        na.rm = TRUE),
#     Methods = "KNN_OvO",
#     Features = ncol(X_scaled)
#   )
# }
#
