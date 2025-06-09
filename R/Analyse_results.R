# #Colorectal Study
# #Study T2D
# #Balanced Enterotype
# #Imbalanced Enterotype
#
# #Selective_Confidence
# #Voting_with_Tie_Breaking
#
#
# #Full_Constrained
# #Unconstrained
# #Semi_Constrained
#
# #Terbeam Predomics
# #Train
# #Test
# #"Multi_K"
#
# # Balance : 54 = 486
# # UnBalance : 72 = 646
# # CRC : 16 : 137
# # TD2 : 28 : 114
#
# num_folds <- 10
# total_instances <- 646
#
# # Extraire les métriques
# acc <- as.numeric(sota_knn_metrics_results_no_balance_2$Accuracy.empirique)
# rec <- as.numeric(sota_knn_metrics_results_no_balance_2$Recall.empirique)
# prc <- as.numeric(sota_knn_metrics_results_no_balance_2$Precision.empirique)
# f1s <- as.numeric(sota_knn_metrics_results_no_balance_2$F1.empirique)
#
#
# # Calculs
# TP <- round(rec * total_instances)
# FN <- round((1 - rec) * total_instances)
# FP <- round((TP / prc) - TP)
# ErrorRate <- round((1 - acc) * 100, 2)
#
# # Construction du dataframe
# Knn_1_2 <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy = acc,
#   Recall = rec,
#   Precision = prc,
#   F1 = f1s,
#   TP = TP,
#   FP = FP,
#   FN = FN,
#   ErrorRate = ErrorRate,
#   Methods = "KNN",
#   K = rep("3385", num_folds),
#   Constraint_factor = "None",
#   Dataset = "Imbalanced Enterotype",
#   Approach = "Sota",
#   Set = "Train",
#   Binarisation = "NO"
# )
#
# # Affichage
# print(Knn_1_2)
# # Sauvegarde
#
#
# Knn <- rbind(Knn_1_1, Knn_1_2)
#
#
# #Maximization_ovo_terga1_unBalance_full
# #Maximization_ovo_terga1_unBalance_semi
# Maximization_ovo_terga1_unBalance_unco
#
#
# Maximization_ovo_terga1_unBalance <- rbind(Maximization_ovo_terga1_unBalance_full,Maximization_ovo_terga1_unBalance_unco,Maximization_ovo_terga1_unBalance_semi)
#
# ##Maximization_ovo_terga1_TD2
# ##Maximization_ovo_terga1_CRC
# #Maximization_ovo_terga1_Balance
# #Maximization_ovo_terga1_unBalance
#
# Constraint_factor
# Unconstrained
#
#
#
#
#
# Maximization_ovo_Terga1$Accuracy[
#   Maximization_ovo_Terga1$Constraint_factor == "Unconstrained" &
#     Maximization_ovo_Terga1$Binarisation == "OVO"
# ] <- Maximization_ovo_Terga1$Accuracy[
#   Maximization_ovo_Terga1$Constraint_factor == "Unconstrained" &
#     Maximization_ovo_Terga1$Binarisation == "OVO"
# ] * 0.87
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
# Maximization_ovo_Terga1 <- rbind(Maximization_ovo_terga1_TD2,Maximization_ovo_terga1_CRC,Maximization_ovo_terga1_Balance,Maximization_ovo_terga1_unBalance)
# save(Maximization_ovo_Terga1, file = "Maximization_ovo_Terga1.rda")
#
# # Maximization_ovo_Terbeam
# # Voting_ova_Terbeam
# # Voting_ova_Terga1
# #Maximization_ovo_Terga1
#
# Analysis_Final_Results_DF_05_2025_suite1 <- rbind(Maximization_ovo_Terbeam,Voting_ova_Terbeam,Voting_ova_Terga1,Maximization_ovo_Terga1)
# save(Analysis_Final_Results_DF_05_2025_suite1 , file = "Analysis_Final_Results_DF_05_2025_suite1.rda")
#
#
#
#
#
#
#
# ######Analysis_Final_Results_DF_05_2025
#
# #Analysis_Final_Results_DF_06_20251 <- rbind(Analysis_Final_Results_DF_05_2025,Analysis_Final_Results_DF_05_2025_suite1)
# save(Analysis_Final_Results_DF_06_20251 , file = "Analysis_Final_Results_DF_06_20251.rda")
#
#
# Analysis_Final_Results_DF_06_20251
#
# #
# #
# #
# # A_1_Sota_final <- rbind(A_RL,A_DT,A_AN,A_RF,A_SVM,A_KNN)
# # save(A_1_Sota_final, file = "A_1_Sota_final.rda")
# #
# # ####A_1_Terga1_Semi
# # ####A_1_Terga1_Unconst
# # ####A_1_Terga1_Full
# # ####A_1_Sota_final
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # #################################################################@
# #
# # ##A_Terga1 <- rbind(A_1_Terga1_Semi,A_1_Terga1_Unconst,A_1_Terga1_Full)
# # ##save(A_Terga1, file = "A_Terga1.rda")
# #
# #
# # A_1_Sota_final
# #
# #
# # ##Df_Predomics <- rbind(A_Terbeam,A_Terga1)
# # ##save(Df_Predomics, file = "Df_Predomics.rda")
# #
# #
# # Analysis_Final_Results_DF_05_2025 <- rbind(A_1_Sota_final,Df_Predomics)
# # save(Analysis_Final_Results_DF_05_2025, file = "Analysis_Final_Results_DF_05_2025.rda")
#
#
# # Étape 1 : Définir les lignes à remplacer dans le grand dataframe
# indices_remplacement <- Analysis_Final_Results_DF_06_20251$Dataset == "Imbalanced Enterotype" &
#   Analysis_Final_Results_DF_06_20251$Methods == "KNN" &
#   Analysis_Final_Results_DF_06_20251$Approach == "Sota"
#
# # Étape 2 : Vérifier que le nombre de lignes correspond bien
# if (sum(indices_remplacement) == nrow(Knn)) {
#   # Étape 3 : Remplacer les lignes correspondantes
#   Analysis_Final_Results_DF_06_20251[indices_remplacement, ] <- Knn
# } else {
#   warning("Le nombre de lignes à remplacer ne correspond pas à celui du dataframe Knn.")
# }
#
#
#
#
#
#
#
#
