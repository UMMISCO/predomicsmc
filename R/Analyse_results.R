#
# ###### Analysis  Rapports
#
# ## 1. Terbeam Maximization constrained
# num_folds <- 5
#
# # Initialisation du data frame
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
#   Methods = "_terbeam_maximization_constrained_no_balance",
#   Features = rep(7, num_folds)
#
# )
#
# # Vérification de la structure de 'terbeam_maximization_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("maximization_terbeam_constrained_3") &&
#     !is.null(maximization_terbeam_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_maximization_constrained_df$Accuracy.empirique <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$Accuracy.generalization <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$Precision.empirique <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$Precision.generalization <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$Recall.empirique <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$Recall.generalization <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$F1.empirique <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terbeam_maximization_constrained_df$F1.generalization <- as.numeric(
#     maximization_terbeam_constrained_3$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terbeam_maximization_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_maximization_constrained_df)
# terbeam_maximization_constrained_df_no_balance_3 = terbeam_maximization_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_maximization_constrained_df_no_balance_3, file = "terbeam_maximization_constrained_df_no_balance_3.rda")
#
#
# ## 2 Terbeam Maximization unconstrained
#
# num_folds <- 5
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
#   Methods = "terbeam_maximization_unconstrained_no_balance",
#   Features = rep(4, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_maximization_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("maximization_terbeam_unconstrained_3") &&
#     !is.null(maximization_terbeam_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_maximization_unconstrained_df$Accuracy.empirique <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Accuracy.generalization <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Precision.empirique <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Precision.generalization <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Recall.empirique <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$Recall.generalization <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$F1.empirique <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_maximization_unconstrained_df$F1.generalization <- as.numeric(
#     maximization_terbeam_unconstrained_3$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("L'objet 'terbeam_maximization_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_maximization_unconstrained_df)
# terbeam_maximization_unconstrained_df_no_balance_3 = terbeam_maximization_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_maximization_unconstrained_df_no_balance_3, file = "terbeam_maximization_unconstrained_df_no_balance_3.rda")
#
#
# ## 3. Terbeam ranking constrained
#
# num_folds <- 5
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
#   Methods = "_terbeam_ranking_constrained_no_balance",
#   Features = rep(7, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_ranking_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("ranking_terbeam_constrained_3") &&
#     !is.null(ranking_terbeam_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_ranking_constrained_df$Accuracy.empirique <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$Accuracy.generalization <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$Precision.empirique <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$Precision.generalization <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$Recall.empirique <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$Recall.generalization <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$F1.empirique <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terbeam_ranking_constrained_df$F1.generalization <- as.numeric(
#     ranking_terbeam_constrained_3$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terbeam_ranking_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_ranking_constrained_df)
# terbeam_ranking_constrained_df_no_balance_3 = terbeam_ranking_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_ranking_constrained_df_no_balance_3, file = "terbeam_ranking_constrained_df_no_balance_3.rda")
#
#
#
# #### 4
#
# num_folds <- 5
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
#   Methods = "terbeam_ranking_unconstrained_no_balance",
#   Features = rep(4, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_ranking_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("ranking_terbeam_unconstrained_3") &&
#     !is.null(ranking_terbeam_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_ranking_unconstrained_df$Accuracy.empirique <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$empirical.acc["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Accuracy.generalization <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$generalization.acc["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Precision.empirique <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$empirical.rec["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Precision.generalization <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$generalization.rec["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Recall.empirique <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$empirical.prc["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$Recall.generalization <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$generalization.prc["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$F1.empirique <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$empirical.f1s["k_4", ]
#   )
#
#   terbeam_ranking_unconstrained_df$F1.generalization <- as.numeric(
#     ranking_terbeam_unconstrained_3$crossVal$scores$generalization.f1s["k_4", ]
#   )
# } else {
#   stop("L'objet 'terbeam_ranking_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_ranking_unconstrained_df)
# terbeam_ranking_unconstrained_df_no_balance_3 = terbeam_ranking_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_ranking_unconstrained_df_no_balance_3, file = "terbeam_ranking_unconstrained_df_no_balance_3.rda")
#
#
# #### 5
#Colorectal Study
#Study on Type 2 Diabetes
#Balanced Enterotype
#Imbalanced Enterotype

#Terbeam_Selective_Confidence_df_4
#Terbeam_Voting_with_Tie_Breaking_df_1

# num_folds <- 10
#
# # Initialisation du data frame pour le modèle predomics_aggregation_ova_constrained
# Fabien_Voting_Terbeam_1 <- data.frame(
#   Fold = paste0("fold_", 1:num_folds),
#   Accuracy = numeric(num_folds),
#   Methods = "Voting_with_Tie_Breaking",
#   K = rep("6", num_folds),
#   Constraint_factor = ("Unconstrained"),
#   Dataset = "Balanced Enterotype",
#   Approach = "Terga1 Predomics",
#   Binarisation = "OVO"
#
# )
#
#
# Fabien_Voting_Terbeam_1$Accuracy <- as.numeric(
#   res_clf_voting_terbeam_balance$crossVal$scores$generalization.acc["k_6", ],
#   res_clf_voting_terbeam_balance$crossVal$scores$generalization.rec["k_6", ],
#   res_clf_voting_terbeam_balance$crossVal$scores$generalization.prc["k_6", ],
#   res_clf_voting_terbeam_balance$crossVal$scores$generalization.f1s["k_6", ]
#   )
#
# print(Fabien_Voting_Terbeam_1)
#
# save(Fabien_Voting_Terbeam_1 , file = "Fabien_Voting_Terbeam_1.rda")









# #### 6
#
# num_folds <- 5
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
#   Methods = "terbeam_predomics_aggregation_ova_unconstrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ova_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ova_terbeam_unconstrained_3") &&
#     !is.null(Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ova_unconstrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ova_unconstrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ova_terbeam_unconstrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ova_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ova_unconstrained_df)
# terbeam_predomics_aggregation_ova_unconstrained_df_no_balance_3 = terbeam_predomics_aggregation_ova_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ova_unconstrained_df_no_balance_3, file = "terbeam_predomics_aggregation_ova_unconstrained_df_no_balance_3.rda")
#
# #### 7
#
# num_folds <- 5
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
#   Methods = "_terbeam_predomics_aggregation_ovo_constrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ovo_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ovo_terbeam_constrained_3") &&
#     !is.null(Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ovo_constrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_constrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_constrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ovo_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ovo_constrained_df)
# terbeam_predomics_aggregation_ovo_constrained_df_no_balance_3 = terbeam_predomics_aggregation_ovo_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ovo_constrained_df_no_balance_3, file = "terbeam_predomics_aggregation_ovo_constrained_df_no_balance_3.rda")
#
# #### 8
#
# num_folds <- 5
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
#   Methods = "terbeam_predomics_aggregation_ovo_unconstrained_no_balance",
#   Features = rep(7, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_predomics_aggregation_ovo_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ovo_terbeam_unconstrained_3") &&
#     !is.null(Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terbeam_predomics_aggregation_ovo_unconstrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terbeam_unconstrained_3$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terbeam_predomics_aggregation_ovo_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ovo_unconstrained_df)
# terbeam_predomics_aggregation_ovo_unconstrained_df_no_balance_3 = terbeam_predomics_aggregation_ovo_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ovo_unconstrained_df_no_balance_3, file = "terbeam_predomics_aggregation_ovo_unconstrained_df_no_balance_3.rda")
#
#
# #### 9
#
# num_folds <- 5
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
#   Methods = "_terbeam_voting_constrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_voting_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("voting_terbeam_constrained_3") &&
#     !is.null(voting_terbeam_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_voting_constrained_df$Accuracy.empirique <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$Accuracy.generalization <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$Precision.empirique <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$Precision.generalization <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$Recall.empirique <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$Recall.generalization <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$F1.empirique <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terbeam_voting_constrained_df$F1.generalization <- as.numeric(
#     voting_terbeam_constrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("terbeam_voting_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_voting_constrained_df)
# terbeam_voting_constrained_df_no_balance_3 = terbeam_voting_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_voting_constrained_df_no_balance_3, file = "terbeam_voting_constrained_df_no_balance_3.rda")
#
# #### 10
#
# num_folds <- 5
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
#   Methods = "terbeam_voting_unconstrained_no_balance",
#   Features = rep(9, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_voting_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("voting_terbeam_unconstrained_3") &&
#     !is.null(voting_terbeam_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_voting_unconstrained_df$Accuracy.empirique <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Accuracy.generalization <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Precision.empirique <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Precision.generalization <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Recall.empirique <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Recall.generalization <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$F1.empirique <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$F1.generalization <- as.numeric(
#     voting_terbeam_unconstrained_3$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terbeam_voting_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_voting_unconstrained_df)
# terbeam_voting_unconstrained_df_no_balance_3 = terbeam_voting_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_voting_unconstrained_df_no_balance_3 , file = "terbeam_voting_unconstrained_df_no_balance_3.rda")
#
# #### 11
#
# num_folds <- 5
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
#   Methods = "_terbeam_weighted_constrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_weighted_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("weighted_terbeam_constrained_3") &&
#     !is.null(weighted_terbeam_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_weighted_constrained_df$Accuracy.empirique <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$Accuracy.generalization <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$Precision.empirique <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$Precision.generalization <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$Recall.empirique <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$Recall.generalization <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$F1.empirique <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terbeam_weighted_constrained_df$F1.generalization <- as.numeric(
#     weighted_terbeam_constrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terbeam_weighted_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_weighted_constrained_df)
# terbeam_weighted_constrained_df_no_balance_3 = terbeam_weighted_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_weighted_constrained_df_no_balance_3, file = "terbeam_weighted_constrained_df_no_balance_3.rda")
#
# #### 12
#
# num_folds <- 5
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
#   Methods = "terbeam_weighted_unconstrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terbeam_weighted_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("weighted_terbeam_unconstrained_3") &&
#     !is.null(weighted_terbeam_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_weighted_unconstrained_df$Accuracy.empirique <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Accuracy.generalization <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Precision.empirique <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Precision.generalization <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Recall.empirique <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$Recall.generalization <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$F1.empirique <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terbeam_weighted_unconstrained_df$F1.generalization <- as.numeric(
#     weighted_terbeam_unconstrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terbeam_weighted_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_weighted_unconstrained_df)
# terbeam_weighted_unconstrained_df_no_balance_3 = terbeam_weighted_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_weighted_unconstrained_df_no_balance_3, file = "terbeam_weighted_unconstrained_df_no_balance_3.rda")
#
# #### Terga 1
#
# ### 1
#
# num_folds <- 5
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
#   Methods = "_terga1_maximization_constrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terga1_maximization_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("maximization_terga1_constrained_3") &&
#     !is.null(maximization_terga1_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_maximization_constrained_df$Accuracy.empirique <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$Accuracy.generalization <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$Precision.empirique <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$Precision.generalization <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$Recall.empirique <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$Recall.generalization <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$F1.empirique <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terga1_maximization_constrained_df$F1.generalization <- as.numeric(
#     maximization_terga1_constrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terga1_maximization_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_maximization_constrained_df)
# terga1_maximization_constrained_df_no_balance_3 = terga1_maximization_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_maximization_constrained_df_no_balance_3, file = "terga1_maximization_constrained_df_no_balance_3.rda")
#
# ### 2
#
# num_folds <- 5
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
#   Methods = "terga1_maximization_unconstrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terga1_maximization_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("maximization_terga1_unconstrained_3") &&
#     !is.null(maximization_terga1_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_maximization_unconstrained_df$Accuracy.empirique <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$Accuracy.generalization <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$Precision.empirique <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$Precision.generalization <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$Recall.empirique <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$Recall.generalization <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$F1.empirique <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terga1_maximization_unconstrained_df$F1.generalization <- as.numeric(
#     maximization_terga1_unconstrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terga1_maximization_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_maximization_unconstrained_df)
# terga1_maximization_unconstrained_df_no_balance_3 = terga1_maximization_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_maximization_unconstrained_df_no_balance_3, file = "terga1_maximization_unconstrained_df_no_balance_3.rda")
#
# ### 3
#
# num_folds <- 5
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
#   Methods = "_terga1_ranking_constrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terga1_ranking_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("ranking_terga1_constrained_3") &&
#     !is.null(ranking_terga1_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_ranking_constrained_df$Accuracy.empirique <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$Accuracy.generalization <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$Precision.empirique <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$Precision.generalization <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$Recall.empirique <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$Recall.generalization <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$F1.empirique <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terga1_ranking_constrained_df$F1.generalization <- as.numeric(
#     ranking_terga1_constrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terga1_ranking_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_ranking_constrained_df)
# terga1_ranking_constrained_df_no_balance_3 = terga1_ranking_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_ranking_constrained_df_no_balance_3, file = "terga1_ranking_constrained_df_no_balance_3.rda")
#
# ### 4
#
# num_folds <- 5
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
#   Methods = "terga1_ranking_unconstrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terga1_ranking_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("ranking_terga1_unconstrained_3") &&
#     !is.null(ranking_terga1_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_ranking_unconstrained_df$Accuracy.empirique <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$Accuracy.generalization <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$Precision.empirique <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$Precision.generalization <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$Recall.empirique <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$Recall.generalization <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$F1.empirique <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terga1_ranking_unconstrained_df$F1.generalization <- as.numeric(
#     ranking_terga1_unconstrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terga1_ranking_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_ranking_unconstrained_df)
# terga1_ranking_unconstrained_df_no_balance_3 = terga1_ranking_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_ranking_unconstrained_df_no_balance_3, file = "terga1_ranking_unconstrained_df_no_balance_3.rda")
#
# ### 5
#
# num_folds <- 5
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
#   Methods = "_terga1_Predomics_aggregation_ova_constrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ova_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ova_terga1_constrained_3") &&
#     !is.null(Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ova_constrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_constrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_constrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ova_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ova_constrained_df)
# terga1_Predomics_aggregation_ova_constrained_df_no_balance_3 = terga1_Predomics_aggregation_ova_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ova_constrained_df_no_balance_3, file = "terga1_Predomics_aggregation_ova_constrained_df_no_balance_3.rda")
#
# ### 6
# num_folds <- 5
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
#   Methods = "terga1_Predomics_aggregation_ova_unconstrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ova_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ova_terga1_unconstrained_3") &&
#     !is.null(Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ova_unconstrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ova_terga1_unconstrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ova_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ova_unconstrained_df)
# terga1_Predomics_aggregation_ova_unconstrained_df_no_balance_3 = terga1_Predomics_aggregation_ova_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ova_unconstrained_df_no_balance_3 , file = "terga1_Predomics_aggregation_ova_unconstrained_df_no_balance_3.rda")
#
# ### 7
#
# num_folds <- 5
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
#   Methods = "_terga1_Predomics_aggregation_ovo_constrained_no_balance",
#   Features = rep(10, num_folds)
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ovo_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ovo_terga1_constrained_3") &&
#     !is.null(Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ovo_constrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$empirical.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$generalization.acc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$empirical.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$generalization.rec["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$empirical.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$generalization.prc["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$empirical.f1s["k_10", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_constrained_3$crossVal$scores$generalization.f1s["k_10", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ovo_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ovo_constrained_df)
# terga1_Predomics_aggregation_ovo_constrained_df_no_balance_3 = terga1_Predomics_aggregation_ovo_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ovo_constrained_df_no_balance_3, file = "terga1_Predomics_aggregation_ovo_constrained_df_no_balance_3.rda")
#
# ### 8
# num_folds <- 5
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
#   Methods = "terga1_Predomics_aggregation_ovo_unconstrained_no_balance",
#   Features = rep(5, num_folds)
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ovo_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("Predomics_aggregation_ovo_terga1_unconstrained_3") &&
#     !is.null(Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Accuracy.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$empirical.acc["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Accuracy.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$generalization.acc["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Precision.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$empirical.rec["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Precision.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$generalization.rec["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Recall.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$empirical.prc["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$Recall.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$generalization.prc["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$F1.empirique <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$empirical.f1s["k_5", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_unconstrained_df$F1.generalization <- as.numeric(
#     Predomics_aggregation_ovo_terga1_unconstrained_3$crossVal$scores$generalization.f1s["k_5", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ovo_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ovo_unconstrained_df)
# terga1_Predomics_aggregation_ovo_unconstrained_df_no_balance_3 = terga1_Predomics_aggregation_ovo_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ovo_unconstrained_df_no_balance_3, file = "terga1_Predomics_aggregation_ovo_unconstrained_df_no_balance_3.rda")
#
# ### 9
# num_folds <- 5
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
#   Methods = "_terga1_voting_constrained_no_balance",
#   Features = rep(9, num_folds)
# )
#
# # Vérification de la structure de 'terga1_voting_constrained' et 'crossVal'
# if (exists("voting_terga1_constrained_3") &&
#     !is.null(voting_terga1_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_voting_constrained_df$Accuracy.empirique <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terga1_voting_constrained_df$Accuracy.generalization <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terga1_voting_constrained_df$Precision.empirique <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terga1_voting_constrained_df$Precision.generalization <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terga1_voting_constrained_df$Recall.empirique <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terga1_voting_constrained_df$Recall.generalization <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terga1_voting_constrained_df$F1.empirique <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terga1_voting_constrained_df$F1.generalization <- as.numeric(
#     voting_terga1_constrained_3$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terga1_voting_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_voting_constrained_df)
# terga1_voting_constrained_df_no_balance_3 = terga1_voting_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_voting_constrained_df_no_balance_3, file = "terga1_voting_constrained_df_no_balance_3.rda")
#
# ### 10
#
# num_folds <- 5
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
#   Methods = "terga1_voting_unconstrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terga1_voting_unconstrained' et 'crossVal'
# if (exists("voting_terga1_unconstrained_3") &&
#     !is.null(voting_terga1_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_voting_unconstrained_df$Accuracy.empirique <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$Accuracy.generalization <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$Precision.empirique <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$Precision.generalization <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$Recall.empirique <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$Recall.generalization <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$F1.empirique <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terga1_voting_unconstrained_df$F1.generalization <- as.numeric(
#     voting_terga1_unconstrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terga1_voting_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_voting_unconstrained_df)
# terga1_voting_unconstrained_df_no_balance_3 = terga1_voting_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_voting_unconstrained_df_no_balance_3, file = "terga1_voting_unconstrained_df_no_balance_3.rda")
#
# ### 11
#
# num_folds <- 5
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
#   Methods = "_terga1_weighted_constrained_no_balance",
#   Features = rep(9, num_folds)
# )
#
# # Vérification de la structure de 'terga1_weighted_constrained' et 'crossVal'
# if (exists("weighted_terga1_constrained_3") &&
#     !is.null(weighted_terga1_constrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_weighted_constrained_df$Accuracy.empirique <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$Accuracy.generalization <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$Precision.empirique <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$Precision.generalization <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$Recall.empirique <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$Recall.generalization <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$F1.empirique <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terga1_weighted_constrained_df$F1.generalization <- as.numeric(
#     weighted_terga1_constrained_3$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terga1_weighted_constrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_weighted_constrained_df)
# terga1_weighted_constrained_df_no_balance_3 = terga1_weighted_constrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_weighted_constrained_df_no_balance_3, file = "terga1_weighted_constrained_df_no_balance_3.rda")
#
# ### 12
#
# num_folds <- 5
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
#   Methods = "terga1_weighted_unconstrained_no_balance",
#   Features = rep(6, num_folds)
# )
#
# # Vérification de la structure de 'terga1_weighted_unconstrained' et 'crossVal'
# if (exists("weighted_terga1_unconstrained_3") &&
#     !is.null(weighted_terga1_unconstrained_3$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_weighted_unconstrained_df$Accuracy.empirique <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$empirical.acc["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$Accuracy.generalization <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$generalization.acc["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$Precision.empirique <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$empirical.rec["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$Precision.generalization <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$generalization.rec["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$Recall.empirique <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$empirical.prc["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$Recall.generalization <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$generalization.prc["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$F1.empirique <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$empirical.f1s["k_6", ]
#   )
#
#   terga1_weighted_unconstrained_df$F1.generalization <- as.numeric(
#     weighted_terga1_unconstrained_3$crossVal$scores$generalization.f1s["k_6", ]
#   )
# } else {
#   stop("L'objet 'terga1_weighted_unconstrained_no_balance' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_weighted_unconstrained_df)
# terga1_weighted_unconstrained_df_no_balance_3 = terga1_weighted_unconstrained_df
# # Sauvegarder le data frame en fichier RDA
# save(terga1_weighted_unconstrained_df_no_balance_3, file = "terga1_weighted_unconstrained_df_no_balance_3.rda")
#
# ##################################################################################################################
# ## Fusion dataframe
#
# analysis_table_final2 <- bind_rows(
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
#   sota_dt_metrics_results_no_balance,
#   sota_forest_metrics_results_no_balance,
#   sota_gbm_metrics_results_no_balance,
#   sota_knn_metrics_results_no_balance,
#   sota_svm_metrics_results_no_balance,
#   sota_logreg_metrics_results_no_balance,
#   sota_ann_metrics_results_no_balance
# )
# # Check the combined data frame
# print(head(analysis_table_final2))
# analysis_table_final_complet_no_balance = analysis_table_final2
# # Save the combined data frame to an RDA file
# save(analysis_table_final_complet_no_balance, file = "analysis_table_final_complet_no_balance.rda")
#
#
#
# # Vérifier les types de colonnes avant de pivoter
# #str(analysis_table_final)  # Vérifiez la structure de votre data frame
#
# library(dplyr)
# library(tidyr)
# library(stringr)
#
#
#
#
# # Imputation des valeurs manquantes (si nécessaire)
# analysis_table_final <- analysis_table_final %>%
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = -c(Fold, Methods),
#     names_to = c("Metric", "Partition"),
#     names_sep = "\\.",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Partition = recode(Partition,
#                        'empirique' = 'train',
#                        'generalization' = 'test'
#     )
#   ) %>%
#   mutate(
#     Partition = factor(Partition, levels = c("train", "test")),
#     Value = as.numeric(Value),
#     Group = case_when(
#       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
#       TRUE ~ "SOTA"  # Pour toutes les autres méthodes
#     )
#   )
#
# # Agréger les données pour le résumé
# df_summary <- df %>%
#   group_by(Methods, Partition, Metric) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df %>% select(Methods, Group) %>% distinct(), by = "Methods")  # Assurez-vous que Group est présent
#
# # Vérification des valeurs de Group
# print(unique(df_summary$Group))  # Vérifiez les groupes présents
#
# # Tracez le graphique
# df_summary %>%
#   #filter(Metric != "Precision") %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +  # Taille des points
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Facettage par Metric et Group
#   ylab("Value") +
#   xlab("Methods") +  # Réglage des labels
#   theme_bw() +
#   scale_colour_manual(values = c("seagreen", "firebrick3")) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
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
# # Charger la bibliothèque
# library(dplyr)
#
# # Combiner les tableaux de données
# analysis_table_final2 <- bind_rows(
#   # Terga1 - Unconstrained
#   terga1_Predomics_aggregation_ova_unconstrained_df,
#   terga1_Predomics_aggregation_ovo_unconstrained_df,
#   terga1_maximization_unconstrained_df,
#   terga1_ranking_unconstrained_df,
#   terga1_voting_unconstrained_df,
#   terga1_weighted_unconstrained_df,
#
#   # Terga1 - Constrained
#   terga1_Predomics_aggregation_ova_constrained_df,
#   terga1_Predomics_aggregation_ovo_constrained_df,
#   terga1_maximization_constrained_df,
#   terga1_ranking_constrained_df,
#   terga1_voting_constrained_df,
#   terga1_weighted_constrained_df,
#
#   # Terbeam - Unconstrained
#   terbeam_predomics_aggregation_ova_unconstrained_df,
#   terbeam_predomics_aggregation_ovo_unconstrained_df,
#   terbeam_maximization_unconstrained_df,
#   terbeam_ranking_unconstrained_df,
#   terbeam_voting_unconstrained_df,
#   terbeam_weighted_unconstrained_df,
#
#   # Terbeam - Constrained
#   terbeam_predomics_aggregation_ova_constrained_df,
#   terbeam_predomics_aggregation_ovo_constrained_df,
#   terbeam_maximization_constrained_df,
#   terbeam_ranking_constrained_df,
#   terbeam_voting_constrained_df,
#   terbeam_weighted_constrained_df,
#
#   # SOTA Metrics Results - No Balance
#   #sota_dt_metrics_results_no_balance,
#   dt_metrics_results,
#   sota_forest_metrics_results_no_balance,
#   sota_gbm_metrics_results_no_balance,
#   sota_knn_metrics_results_no_balance,
#   sota_svm_metrics_results_no_balance,
#   sota_logreg_metrics_results_no_balance,
#   sota_ann_metrics_results_no_balance
# )
#
# filtered_dataframe <- analysis_table_final2[analysis_table_final2$Methods == "Decision Tree", ]
#
# # Afficher le DataFrame filtré
# print(filtered_dataframe)
#
#
# colnames(analysis_table_final2)
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
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes (si nécessaire)
# analysis_table_final <- analysis_table_final_complet_no_balance_2 %>%
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = -c(Fold, Methods, k),  # Ajout de `k` comme colonne conservée
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
#       TRUE ~ "SOTA"  # Pour toutes les autres méthodes
#     )
#   )
#
# # Agréger les données pour le résumé
# df_summary <- df %>%
#   group_by(Methods, Partition, Metric) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df %>% select(Methods, Group, k) %>% distinct(), by = "Methods")  # Ajout de `k`
#
# # Modifier l'affichage des méthodes :
# df_summary <- df_summary %>%
#   mutate(
#     # Insérer un retour à la ligne après certains mots pour limiter la longueur
#     Methods = str_replace_all(Methods, "(?<=\\bterbeam) ", "\n"),  # Ajoute \n après "terbeam"
#     Methods = str_replace_all(Methods, "(?<=\\bterga1) ", "\n"),  # Ajoute \n après "terga1"
#     Methods = str_replace_all(Methods, "(?<=\\bSOTA) ", "\n"),  # Ajoute \n après "SOTA"
#
#     # Ajouter `k` après le nom de la méthode
#     Methods = paste0(Methods, " - k: ", k)
#   )
#
# # Vérification des valeurs de Group et Methods modifiées
# print(unique(df_summary$Methods))
#
# # Tracez le graphique
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +  # Taille des points
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Facettage par Metric et Group
#   ylab("Value") +
#   xlab("Methods") +  # Réglage des labels
#   theme_bw() +
#   scale_colour_manual(values = c("seagreen", "firebrick3")) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotation des labels
#     legend.position = "bottom"
#   )
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
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes (si nécessaire)
# analysis_table_final <- analysis_table_final_complet_no_balance_2 %>%
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = -c(Fold, Methods, k),  # Ajout de `k` comme colonne conservée
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
#       TRUE ~ "SOTA"  # Pour toutes les autres méthodes
#     )
#   )
#
# # Agréger les données pour le résumé
# df_summary <- df %>%
#   group_by(Methods, Partition, Metric) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df %>% select(Methods, Group, k) %>% distinct(), by = "Methods")  # Ajout de `k`
#
# # Modifier l'affichage des méthodes :
# df_summary <- df_summary %>%
#   mutate(
#     # Insérer un retour à la ligne après des mots-clés pour limiter la longueur
#     Methods = str_replace_all(Methods, "_", " "),  # Remplace les underscores par des espaces
#     Methods = str_replace_all(Methods, "(Predomics|aggregation|unconstrained|balance)", "\\1\n"),  # Ajoute \n après certains mots
#     Methods = paste0(Methods, " - k: ", k)  # Ajouter `k` après le nom de la méthode
#   )
#
# # Vérification des valeurs de Methods modifiées
# print(unique(df_summary$Methods))
#
# # Tracez le graphique
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +  # Taille des points
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Facettage par Metric et Group
#   ylab("Value") +
#   xlab("Methods") +  # Réglage des labels
#   theme_bw() +
#   scale_colour_manual(values = c("seagreen", "firebrick3")) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotation des labels
#     legend.position = "bottom"
#   )
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
# analysis_table_final <- analysis_table_final_complet_no_balance_2 %>%
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
#
#
#
#
#
#
#
#
#
# ############################
#
#
# #bon code
#
#
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = -c(Fold, Methods, k),  # Exclure `k` (anciennement Features)
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
#   group_by(Methods, Partition, Metric, k) %>%  # Utilisation de `k` au lieu de `Features`
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
#
#
#
# ####################################################################################
#
#
#
#
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
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
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df_accuracy %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
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
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Faceting par Metric et Group
#   ylab("Accuracy") +  # Changer "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = c("seagreen", "firebrick3")) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
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
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
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
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df_accuracy %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
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
# # Définir une palette de couleurs personnalisée
# custom_colors <- c("Predomics Terbeam unconstrained" = "green",
#                    "Predomics Terbeam constrained" = "blue",
#                    "Predomics Terga1 unconstrained" = "purple",
#                    "Predomics Terga1 constrained" = "orange",
#                    "SOTA" = "red")
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c("Predomics Terbeam unconstrained" = 16,  # Cercle
#                    "Predomics Terbeam constrained" = 4,   # Carré
#                    "Predomics Terga1 unconstrained" = 17, # Triangle
#                    "Predomics Terga1 constrained" = 15,   # Croix
#                    "SOTA" = 19)  # Rond plein
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Group, shape = Group)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Faceting par Metric et Group
#   ylab("DT2, Adedona, Control") +  # Changer "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
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
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
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
# # Regrouper "Predomics Terbeam constrained" et "Predomics Terga1 constrained"
# df$Group <- recode(df$Group,
#                    "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                    "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained"
# )
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df_accuracy %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
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
# # Définir une palette de couleurs personnalisée
# custom_colors <- c("Predomics Terbeam unconstrained" = "green",
#                    "Predomics Terbeam & Terga1 constrained" = "blue",  # Regroupé
#                    "Predomics Terga1 unconstrained" = "purple",
#                    "SOTA" = "red")
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c("Predomics Terbeam unconstrained" = 16,  # Cercle
#                    "Predomics Terbeam & Terga1 constrained" = 4,   # Carré, pour le groupe combiné
#                    "Predomics Terga1 unconstrained" = 17, # Triangle
#                    "SOTA" = 19)  # Rond plein
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Group, shape = Group)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Faceting par Metric et Group
#   ylab("DT2, Adedona, Control") +  # Changer "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
#
#
#
#
#
#
#
#
#
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_balance %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
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
# # Regrouper "Predomics Terbeam unconstrained" et "Predomics Terga1 unconstrained"
# df$Group <- recode(df$Group,
#                    "Predomics Terbeam unconstrained" = "Predomics Terbeam & Terga1 unconstrained",  # Fusion de Terbeam et Terga1 unconstrained
#                    "Predomics Terga1 unconstrained" = "Predomics Terbeam & Terga1 unconstrained",  # Fusion de Terga1 et Terbeam unconstrained
#                    "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                    "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained"
# )
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df_accuracy %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
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
# # Définir une palette de couleurs personnalisée
# custom_colors <- c("Predomics Terbeam & Terga1 unconstrained" = "green",  # Regroupé
#                    "Predomics Terbeam & Terga1 constrained" = "blue",  # Regroupé
#                    "SOTA" = "red")  # SOTA reste séparé
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c("Predomics Terbeam & Terga1 unconstrained" = 16,  # Cercle
#                    "Predomics Terbeam & Terga1 constrained" = 4,   # Carré, pour le groupe combiné
#                    "SOTA" = 19)  # Rond plein
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Group, shape = Group)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Faceting par Metric et Group
#   ylab("DT2, Adedona, Control") +  # Changer "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
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
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
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
#     ),
#     Subgroup = case_when(
#       str_starts(Methods, "terbeam") ~ "Terbeam",
#       str_starts(Methods, "terga1") ~ "Terga1",
#       TRUE ~ "SOTA"
#     )
#   )
#
# # Regrouper "Predomics Terbeam unconstrained" et "Predomics Terga1 unconstrained"
# df$Group <- recode(df$Group,
#                    "Predomics Terbeam unconstrained" = "Predomics Terbeam & Terga1 unconstrained",  # Fusion de Terbeam et Terga1 unconstrained
#                    "Predomics Terga1 unconstrained" = "Predomics Terbeam & Terga1 unconstrained",  # Fusion de Terga1 et Terbeam unconstrained
#                    "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                    "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained"
# )
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k, Subgroup) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df_accuracy %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
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
# # Définir une palette de couleurs personnalisée avec distinction entre Terbeam et Terga1
# custom_colors <- c("Predomics Terbeam & Terga1 unconstrained" = "green",  # Regroupé
#                    "Predomics Terbeam & Terga1 constrained" = "blue",  # Regroupé
#                    "SOTA" = "red",
#                    "Terbeam" = "darkgreen",  # Différencier Terbeam
#                    "Terga1" = "darkblue")  # Différencier Terga1
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c("Predomics Terbeam & Terga1 unconstrained" = 16,  # Cercle
#                    "Predomics Terbeam & Terga1 constrained" = 4,   # Carré, pour le groupe combiné
#                    "SOTA" = 19,  # Rond plein
#                    "Terbeam" = 17,  # Forme spécifique pour Terbeam
#                    "Terga1" = 18)  # Forme spécifique pour Terga1
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Subgroup, shape = Subgroup)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Faceting par Metric et Group
#   ylab("DT2, Adedona, Control") +  # Changer "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
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
# # Chargement des librairies
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Imputation des valeurs manquantes uniquement sur les colonnes numériques
# analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# analysis_table_final <- analysis_table_final %>%
#   rename(k = Features)
#
# # Convertir en format long
# df <- analysis_table_final %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
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
#     ),
#     Subgroup = case_when(
#       str_starts(Methods, "terbeam") ~ "Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Terga1 constrained",
#       TRUE ~ "SOTA"
#     )
#   )
#
# # Regrouper "Predomics Terbeam unconstrained" et "Predomics Terga1 unconstrained"
# df$Group <- recode(df$Group,
#                    "Predomics Terbeam unconstrained" = "Predomics Terbeam & Terga1 unconstrained",  # Fusion de Terbeam et Terga1 unconstrained
#                    "Predomics Terga1 unconstrained" = "Predomics Terbeam & Terga1 unconstrained",  # Fusion de Terga1 et Terbeam unconstrained
#                    "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                    "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained"
# )
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k, Subgroup) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(df_accuracy %>% select(Methods, Group) %>% distinct(), by = "Methods")
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
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
# # Définir une palette de couleurs personnalisée avec distinction entre Terbeam, Terga1 et les types constrained/unconstrained
# custom_colors <- c("Predomics Terbeam & Terga1 unconstrained" = "green",  # Regroupé
#                    "Predomics Terbeam & Terga1 constrained" = "blue",  # Regroupé
#                    "SOTA" = "red",
#                    "Terbeam unconstrained" = "darkgreen",  # Différencier Terbeam unconstrained
#                    "Terga1 unconstrained" = "darkblue",  # Différencier Terga1 unconstrained
#                    "Terbeam constrained" = "lightgreen",  # Différencier Terbeam constrained
#                    "Terga1 constrained" = "lightblue")  # Différencier Terga1 constrained
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c("Predomics Terbeam & Terga1 unconstrained" = 16,  # Cercle
#                    "Predomics Terbeam & Terga1 constrained" = 4,   # Carré, pour le groupe combiné
#                    "SOTA" = 19,  # Rond plein
#                    "Terbeam unconstrained" = 17,  # Forme spécifique pour Terbeam unconstrained
#                    "Terga1 unconstrained" = 18,  # Forme spécifique pour Terga1 unconstrained
#                    "Terbeam constrained" = 15,  # Forme étoile pour Terbeam constrained (changer ici)
#                    "Terga1 constrained" = 3)  # Forme spécifique pour Terga1 constrained
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Subgroup, shape = Subgroup)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Metric ~ Group, scales = "free") +  # Faceting par Metric et Group
#   ylab("DT2, Adedona, Control") +  # Changer "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
#
#
#
#
#
#
# ################# fusion
#
#
# # Chargement des librairies nécessaires
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Fusionner les deux dataframes en ajoutant une colonne pour différencier les datasets
# analysis_table_no_balance <- analysis_table_final_complet_no_balance %>%
#   mutate(Dataset = "no_balance")
#
# analysis_table_balance <- analysis_table_final_complet_balance %>%
#   mutate(Dataset = "balance")
#
# # Combiner les deux tables
# df_combined <- bind_rows(analysis_table_no_balance, analysis_table_balance)
#
# # Imputation des valeurs manquantes pour les colonnes numériques
# df_combined <- df_combined %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# df_combined <- df_combined %>%
#   rename(k = Features)
#
# # Convertir en format long
# df_long <- df_combined %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
#     names_to = c("Metric", "Partition"),
#     names_sep = "\\.",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Partition = recode(Partition, 'empirique' = 'train', 'generalization' = 'test'),
#     Partition = factor(Partition, levels = c("train", "test")),
#     Value = as.numeric(Value),
#     Group = case_when(
#       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
#       TRUE ~ "SOTA"
#     ),
#     Subgroup = case_when(
#       str_starts(Methods, "terbeam") ~ "Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Terga1 constrained",
#       TRUE ~ "SOTA"
#     )
#   )
#
# # Regrouper "Predomics Terbeam unconstrained" et "Predomics Terga1 unconstrained"
# df_long$Group <- recode(df_long$Group,
#                         "Predomics Terbeam unconstrained" = "Predomics Terbeam & Terga1 unconstrained",
#                         "Predomics Terga1 unconstrained" = "Predomics Terbeam & Terga1 unconstrained",
#                         "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                         "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained")
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df_long %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k, Subgroup, Group, Dataset) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
#
# # Vérification des résultats
# print(unique(df_summary$Group))
#
# # Définir une palette de couleurs personnalisée avec distinction entre Terbeam, Terga1 et les types constrained/unconstrained
# custom_colors <- c(
#   "Predomics Terbeam & Terga1 unconstrained" = "green",
#   "Predomics Terbeam & Terga1 constrained" = "blue",
#   "SOTA" = "red",
#   "Terbeam unconstrained" = "darkgreen",
#   "Terga1 unconstrained" = "darkblue",
#   "Terbeam constrained" = "lightgreen",
#   "Terga1 constrained" = "lightblue"
# )
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c(
#   "Predomics Terbeam & Terga1 unconstrained" = 16,
#   "Predomics Terbeam & Terga1 constrained" = 4,
#   "SOTA" = 19,
#   "Terbeam unconstrained" = 17,
#   "Terga1 unconstrained" = 18,
#   "Terbeam constrained" = 15,
#   "Terga1 constrained" = 3
# )
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Subgroup, shape = Subgroup)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Dataset ~ Group, scales = "free") +  # Faceting par Dataset et Group
#   ylab("Accuracy") +  # Changez "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
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
# ############################################################################################
#
#
# # Chargement des librairies nécessaires
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Fusionner les quatre dataframes en ajoutant une colonne pour différencier les datasets
# analysis_table_no_balance <- analysis_table_final_complet_no_balance %>%
#   mutate(Dataset = "no_balance")
#
# analysis_table_balance <- analysis_table_final_complet_balance %>%
#   mutate(Dataset = "balance")
#
# analysis_table_no_balance_2 <- analysis_table_final_complet_no_balance_2 %>%
#   mutate(Dataset = "no_balance_2")
#
# analysis_table_no_balance_3 <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(Dataset = "no_balance_3")
#
# # Combiner les quatre tables
# df_combined <- bind_rows(analysis_table_no_balance,
#                          analysis_table_balance,
#                          analysis_table_no_balance_2,
#                          analysis_table_no_balance_3)
#
# # Imputation des valeurs manquantes pour les colonnes numériques
# df_combined <- df_combined %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# df_combined <- df_combined %>%
#   rename(k = Features)
#
# # Convertir en format long
# df_long <- df_combined %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
#     names_to = c("Metric", "Partition"),
#     names_sep = "\\.",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Partition = recode(Partition, 'empirique' = 'train', 'generalization' = 'test'),
#     Partition = factor(Partition, levels = c("train", "test")),
#     Value = as.numeric(Value),
#     Group = case_when(
#       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
#       TRUE ~ "SOTA"
#     ),
#     Subgroup = case_when(
#       str_starts(Methods, "terbeam") ~ "Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Terga1 constrained",
#       TRUE ~ "SOTA"
#     )
#   )
#
# # Regrouper "Predomics Terbeam unconstrained" et "Predomics Terga1 unconstrained"
# df_long$Group <- recode(df_long$Group,
#                         "Predomics Terbeam unconstrained" = "Predomics Terbeam & Terga1 unconstrained",
#                         "Predomics Terga1 unconstrained" = "Predomics Terbeam & Terga1 unconstrained",
#                         "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                         "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained")
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df_long %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k, Subgroup, Group, Dataset) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
#
# # Vérification des résultats
# print(unique(df_summary$Group))
#
# # Définir une palette de couleurs personnalisée avec distinction entre Terbeam, Terga1 et les types constrained/unconstrained
# custom_colors <- c(
#   "Predomics Terbeam & Terga1 unconstrained" = "green",
#   "Predomics Terbeam & Terga1 constrained" = "blue",
#   "SOTA" = "red",
#   "Terbeam unconstrained" = "darkgreen",
#   "Terga1 unconstrained" = "darkblue",
#   "Terbeam constrained" = "lightgreen",
#   "Terga1 constrained" = "lightblue"
# )
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c(
#   "Predomics Terbeam & Terga1 unconstrained" = 16,
#   "Predomics Terbeam & Terga1 constrained" = 4,
#   "SOTA" = 19,
#   "Terbeam unconstrained" = 17,
#   "Terga1 unconstrained" = 18,
#   "Terbeam constrained" = 15,
#   "Terga1 constrained" = 3
# )
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Subgroup, shape = Subgroup)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Dataset ~ Group, scales = "free") +  # Faceting par Dataset et Group
#   ylab("Accuracy") +  # Changez "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
#
#
# ##############
#
#
#
# # Chargement des librairies nécessaires
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
#
# # Fusionner les quatre dataframes en ajoutant une colonne pour différencier les datasets
# analysis_table_no_balance <- analysis_table_final_complet_no_balance %>%
#   mutate(Dataset = "MetaCardis enterotype non-balanced")
#
# analysis_table_balance <- analysis_table_final_complet_balance %>%
#   mutate(Dataset = "MetaCardis enterotype balanced")
#
# analysis_table_no_balance_2 <- analysis_table_final_complet_no_balance_2 %>%
#   mutate(Dataset = "CRC, Adenoma and Control")
#
# analysis_table_no_balance_3 <- analysis_table_final_complet_no_balance_3 %>%
#   mutate(Dataset = "T2D,IGT and Control ")
#
# # Combiner les quatre tables
# df_combined <- bind_rows(analysis_table_no_balance,
#                          analysis_table_balance,
#                          analysis_table_no_balance_2,
#                          analysis_table_no_balance_3)
#
# # Imputation des valeurs manquantes pour les colonnes numériques
# df_combined <- df_combined %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#
# # Renommer "Features" en "k"
# df_combined <- df_combined %>%
#   rename(k = Features)
#
# # Convertir en format long
# df_long <- df_combined %>%
#   pivot_longer(
#     cols = c("Accuracy.empirique", "Accuracy.generalization"),  # Se limiter aux colonnes de précision
#     names_to = c("Metric", "Partition"),
#     names_sep = "\\.",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Partition = recode(Partition, 'empirique' = 'train', 'generalization' = 'test'),
#     Partition = factor(Partition, levels = c("train", "test")),
#     Value = as.numeric(Value),
#     Group = case_when(
#       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
#       TRUE ~ "SOTA"
#     ),
#     Subgroup = case_when(
#       str_starts(Methods, "terbeam") ~ "Terbeam unconstrained",
#       str_starts(Methods, "_terbeam") ~ "Terbeam constrained",
#       str_starts(Methods, "terga1") ~ "Terga1 unconstrained",
#       str_starts(Methods, "_terga1") ~ "Terga1 constrained",
#       TRUE ~ "SOTA"
#     )
#   )
#
# # Regrouper "Predomics Terbeam unconstrained" et "Predomics Terga1 unconstrained"
# df_long$Group <- recode(df_long$Group,
#                         "Predomics Terbeam unconstrained" = "Predomics Terbeam & Terga1 unconstrained",
#                         "Predomics Terga1 unconstrained" = "Predomics Terbeam & Terga1 unconstrained",
#                         "Predomics Terbeam constrained" = "Predomics Terbeam & Terga1 constrained",
#                         "Predomics Terga1 constrained" = "Predomics Terbeam & Terga1 constrained")
#
# # Filtrer pour ne garder que la partition "test" et la métrique "Accuracy.generalization"
# df_accuracy <- df_long %>%
#   filter(Metric == "Accuracy" & Partition == "test")
#
# # Agréger les données pour le résumé
# df_summary <- df_accuracy %>%
#   group_by(Methods, Partition, Metric, k, Subgroup, Group, Dataset) %>%
#   summarize(
#     mean_value = mean(Value, na.rm = TRUE),
#     sd_value = sd(Value, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Supprimer les lignes avec des valeurs manquantes ou vides dans `Metric` ou `Group`
# df_summary <- df_summary %>%
#   filter(!is.na(Metric) & !is.na(Group) & Metric != "" & Group != "")
#
# # Suppression des préfixes "terbeam", "_terbeam", "terga1", "_terga1" dans l'affichage des méthodes
# df_summary <- df_summary %>%
#   mutate(Methods = str_remove(Methods, "^_?terbeam_?|^_?terga1_?"))
#
# # Vérification des résultats
# print(unique(df_summary$Group))
#
# # Définir une palette de couleurs personnalisée
# custom_colors <- c(
#   "Predomics Terbeam & Terga1 unconstrained" = "green",
#   "Predomics Terbeam & Terga1 constrained" = "blue",
#   "SOTA" = "red",
#   "Terbeam unconstrained" = "darkgreen",
#   "Terga1 unconstrained" = "darkblue",
#   "Terbeam constrained" = "lightgreen",
#   "Terga1 constrained" = "lightblue"
# )
#
# # Définir une palette de formes personnalisée
# custom_shapes <- c(
#   "Predomics Terbeam & Terga1 unconstrained" = 16,
#   "Predomics Terbeam & Terga1 constrained" = 4,
#   "SOTA" = 19,
#   "Terbeam unconstrained" = 17,
#   "Terga1 unconstrained" = 18,
#   "Terbeam constrained" = 15,
#   "Terga1 constrained" = 8
# )
#
# # Tracez le graphique avec faceting pour "Accuracy.generalization" uniquement
# df_summary %>%
#   ggplot(aes(x = Methods, y = mean_value, color = Subgroup, shape = Subgroup)) +
#   geom_point(position = position_dodge(width = 0.5), size = 4) +
#   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
#                 position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(Dataset ~ Group, scales = "free") +  # Faceting par Dataset et Group
#   ylab("Accuracy") +  # Changez "Value" en "Accuracy" pour plus de clarté
#   xlab("Methods") +
#   theme_bw() +
#   scale_colour_manual(values = custom_colors) +  # Appliquer les couleurs personnalisées
#   scale_shape_manual(values = custom_shapes) +   # Appliquer les formes personnalisées
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#     legend.position = "bottom"
#   )
#
#
#
#
# #analysis_table_final_complet_balance_ = analysis_table_final_complet_balance
#
# #save(analysis_table_final_complet_balance_, file = "analysis_table_final_complet_balance_.rda")
#
#
#
#
































































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
#
#
#
# num_folds <- 10
# total_instances <- 70
#
# # Extraire les métriques
# acc <- as.numeric(sota_svm_metrics_results_no_balance$Accuracy.generalization)
# rec <- as.numeric(sota_svm_metrics_results_no_balance$Recall.generalization)
# prc <- as.numeric(sota_svm_metrics_results_no_balance$Precision.generalization)
# f1s <- as.numeric(sota_svm_metrics_results_no_balance$F1.generalization)
#
# # Calculs
# TP <- round(rec * total_instances)
# FN <- round((1 - rec) * total_instances)
# FP <- round((TP / prc) - TP)
# ErrorRate <- round((1 - acc) * 100, 2)
#
# # Construction du dataframe
# KNN_unBalance_1_1 <- data.frame(
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
#   Set = "Test",
#   Binarisation = "NO"
# )
#
# # Affichage
# print(KNN_unBalance_1_1)
# # Sauvegarde
#
#
# KNN_unBalance <- rbind(KNN_unBalance_1_1,KNN_unBalance_1_2)
#
#
#
# ##KNN_T2D
# ##KNN_CRC
# ##KNN_Balance
# ##KNN_unBalance
#
#
#
# A_KNN <- rbind(KNN_T2D,KNN_CRC,KNN_Balance,KNN_unBalance)
# save(A_KNN, file = "A_KNN.rda")
#
# ###A_RL
# ###A_DT
# ###A_AN
# ###A_RF
# ###A_SVM
# ##A_KNN
#
#
#
#
#
#
# A_1_Sota_final <- rbind(A_RL,A_DT,A_AN,A_RF,A_SVM,A_KNN)
# save(A_1_Sota_final, file = "A_1_Sota_final.rda")
#
# ####A_1_Terga1_Semi
# ####A_1_Terga1_Unconst
# ####A_1_Terga1_Full
# ####A_1_Sota_final
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
# #################################################################@
#
# ##A_Terga1 <- rbind(A_1_Terga1_Semi,A_1_Terga1_Unconst,A_1_Terga1_Full)
# ##save(A_Terga1, file = "A_Terga1.rda")
#
#
# A_1_Sota_final
#
#
# ##Df_Predomics <- rbind(A_Terbeam,A_Terga1)
# ##save(Df_Predomics, file = "Df_Predomics.rda")
#
#
# Analysis_Final_Results_DF_05_2025 <- rbind(A_1_Sota_final,Df_Predomics)
# save(Analysis_Final_Results_DF_05_2025, file = "Analysis_Final_Results_DF_05_2025.rda")

