# =========================================================
# FUNCTION: build_experiment_metrics
# =========================================================
#
# Purpose:
# This function builds a structured dataframe containing
# performance metrics obtained from cross-validation
# experiments for both training and test sets.
#
# The function is generic and can be reused for:
# - Predomics
# - SVM
# - Random Forest
# - Logistic Regression
# - Any other classification model
#
# It also stores experiment metadata such as:
# dataset name, approach, constraint level,
# binarisation strategy, sparsity level, etc.
#
# =========================================================

build_experiment_metrics <- function(

  # Result object containing all CV metrics
  # Example:
  # res_clf_mc_balance_2026
  res_obj,

  # Best sparsity level selected during optimization
  # Example:
  # "k_9"
  #
  # Here:
  # k_9 means the best model using 9 variables
  # (or best sparsity configuration)
  # that achieved the best performance.
  k_name,

  # Total number of training instances
  # used during cross-validation
  #
  # Example:
  # 486
  total_instances_train,

  # Total number of test instances
  # used during cross-validation
  #
  # Example:
  # 54
  total_instances_test,

  # Aggregation or classification method used
  #
  # Examples:
  # "Voting_with_Tie_Breaking"
  # "Random_Forest"
  # "SVM"
  method_name,

  # Total number of variables/features
  # in the original dataset
  #
  # Example:
  # "3385"
  #
  # Here:
  # the dataset initially contains
  # 3385 variables/features.
  k_value,

  # Constraint level used during variable selection
  #
  # Examples:
  # "Semi_Constrained"
  # "Fully_Constrained"
  # "Unconstrained"
  constraint_factor,

  # Dataset name
  #
  # Examples:
  # "Balanced Enterotype"
  # "CRC"
  # "T2D"
  dataset_name,

  # Classification approach name
  #
  # Examples:
  # "Terbeam Predomics"
  # "SVM"
  # "Random Forest"
  approch_name,

  # Binarisation strategy used
  #
  # Examples:
  # "OVO" = One-vs-One
  # "OVA" = One-vs-All
  binarisation,

  # Number of cross-validation folds
  #
  # Default:
  # 10-fold cross-validation
  num_folds = 10

) {

  # =======================================================
  # INTERNAL FUNCTION:
  # Build train/test dataframe
  # =======================================================

  build_metrics_df <- function(score_list,
                               total_instances,
                               set_name) {

    # Extract metrics

    acc <- as.numeric(score_list$acc)
    rec <- as.numeric(score_list$rec)
    prc <- as.numeric(score_list$prc)
    f1s <- as.numeric(score_list$f1s)

    # =====================================================
    # Security check:
    # Verify that all vectors contain the expected
    # number of folds
    # =====================================================

    if (length(acc) != num_folds ||
        length(rec) != num_folds ||
        length(prc) != num_folds ||
        length(f1s) != num_folds) {

      stop(
        paste0(
          "Length error in ", set_name, ": ",
          "acc=", length(acc), ", ",
          "rec=", length(rec), ", ",
          "prc=", length(prc), ", ",
          "f1s=", length(f1s)
        )
      )
    }

    # =====================================================
    # Approximate confusion matrix statistics
    # =====================================================

    TP <- round(rec * total_instances)

    FN <- round((1 - rec) * total_instances)

    FP <- round((TP / prc) - TP)

    # =====================================================
    # Error rate (%)
    # =====================================================

    ErrorRate <- round((1 - acc) * 100, 2)

    # =====================================================
    # Final dataframe
    # =====================================================

    data.frame(

      Fold = paste0("fold_", 1:num_folds),

      Accuracy = acc,

      Recall = rec,

      Precision = prc,

      F1 = f1s,

      TP = TP,

      FP = FP,

      FN = FN,

      ErrorRate = ErrorRate,

      Methods = method_name,

      K = rep(k_value, num_folds),

      Constraint_factor = constraint_factor,

      Dataset = dataset_name,

      Approach = approch_name,

      Set = set_name,

      Binarisation = binarisation
    )
  }

  # =======================================================
  # TRAINING SCORES
  # =======================================================

  train_scores <- list(

    acc =
      res_obj$crossVal$scores$empirical.acc[k_name, ],

    rec =
      res_obj$crossVal$scores$empirical.rec[k_name, ],

    prc =
      res_obj$crossVal$scores$empirical.prc[k_name, ],

    f1s =
      res_obj$crossVal$scores$empirical.f1s[k_name, ]
  )

  # =======================================================
  # TEST SCORES
  # =======================================================

  test_scores <- list(

    acc =
      res_obj$crossVal$scores$generalization.acc[k_name, ],

    rec =
      res_obj$crossVal$scores$generalization.rec[k_name, ],

    prc =
      res_obj$crossVal$scores$generalization.prc[k_name, ],

    f1s =
      res_obj$crossVal$scores$generalization.f1s[k_name, ]
  )

  # =======================================================
  # BUILD DATAFRAMES
  # =======================================================

  train_df <- build_metrics_df(
    train_scores,
    total_instances_train,
    "Train"
  )

  test_df <- build_metrics_df(
    test_scores,
    total_instances_test,
    "Test"
  )

  # =======================================================
  # FINAL DATAFRAME
  # =======================================================

  final_df <- rbind(train_df, test_df)

  return(final_df)
}








