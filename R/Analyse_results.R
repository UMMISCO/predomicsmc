# # #Colorectal cancer
# # #T2D
# # #Balanced Enterotype
# # #Imbalanced Enterotype
# #
# # #Selective_Confidence
# # #Voting_with_Tie_Breaking
# # #Logistic Regression,SVM,Decision Tree, Artificial Neural Network, KNN,Logistic Regression, Random Forest
# #
# # #Full_Constrained
# # #Unconstrained
# # #Semi_Constrained
# #
# # #Terbeam Predomics
# # #Train
# # #Test
# # #"Multi_K"
# #
# #Balance : 54 = 486
# # # UnBalance : 72 = 646
# # CRC : 16 : 137
# # # TD2 : 14 : 128
# # #3385, 2049
# # # Nombre de folds et instances
#  num_folds <- 10
#  total_instances_train <- 137
#  total_instances_test <- 16
# #
# # # Fonction pour construire un data.frame a partir des resultats d'un ensemble (train/test)
#  build_metrics_df <- function(score_list, total_instances, set_name) {
# #   # Extraire les scores
#    acc <- as.numeric(score_list$acc["k_10", ])
#    acc <- as.numeric(score_list$acc)
#    rec <- as.numeric(score_list$rec)
#    prc <- as.numeric(score_list$prc)
#    f1s <- as.numeric(score_list$f1s)
# #
# #   # Calcul TP, FP, FN
#    TP <- round(rec * total_instances)
#    FN <- round((1 - rec) * total_instances)
#    FP <- round((TP / prc) - TP)
#    ErrorRate <- round((1 - acc) * 100, 2)
# #
# #   # Construction du data.frame
#    df <- data.frame(
#      Fold = paste0("fold_", 1:num_folds),
#      Accuracy = acc,
#      Recall = rec,
#      Precision = prc,
#      F1 = f1s,
#      TP = TP,
#      FP = FP,
#      FN = FN,
#      ErrorRate = ErrorRate,
#      Methods = "Voting_with_Tie_Breaking",
#      K = rep("2049", num_folds),
#      Constraint_factor = "Semi_Constrained",
#      Dataset = "Colorectal cancer",
#      Approach = "Terbeam Predomics",
#      Set = set_name,
#      Binarisation = "OVO"
#    )
#    return(df)
#   }
# #
# # # Recuperation des scores
#  train_scores <- list(
#     #acc = res_clf_TD2_voting_tie_semi$crossVal$scores$empirical.acc,
#     acc = Exa$crossVal$scores$empirical.acc["k_8", ],
#     rec = Exa$crossVal$scores$empirical.rec["k_8", ],
#     prc = Exa$crossVal$scores$empirical.prc["k_8", ],
#     f1s = Exa$crossVal$scores$empirical.f1s["k_8", ]
#  )
# #
#  test_scores <- list(
#    acc = Exa$crossVal$scores$generalization.acc["k_8", ],
#    rec = Exa$crossVal$scores$generalization.rec["k_8", ],
#    prc = Exa$crossVal$scores$generalization.prc["k_8", ],
#    f1s = Exa$crossVal$scores$generalization.f1s["k_8", ]
#  )
# #
# # # Generer les deux tableaux
#  train_df <- build_metrics_df(train_scores, total_instances_train, "Train")
#  test_df  <- build_metrics_df(test_scores, total_instances_test, "Test")
# #
# # # Fusion des resultats
#  predo_crc <- rbind(train_df, test_df)
# #
# # # Affichage final
#  print(predo_crc)
#
#
#
# #
# # # ##Df_Predomics <- rbind(A_Terbeam,A_Terga1)
# #save(Analysis_Dataset_Complet_Sota_Predo, file = "Analysis_Dataset_Complet_Sota_Predo.rda")
#
