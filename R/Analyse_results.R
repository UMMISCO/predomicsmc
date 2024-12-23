#
# ###### Analysis  Rapports
#
# ## 1. Terbeam Maximization constrained
# num_folds <- 10
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
#   Methods = "_terbeam_maximization_constrained"  # Méthode spécifiée
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
# } else {
#   stop("L'objet 'terbeam_maximization_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_maximization_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_maximization_constrained_df, file = "terbeam_maximization_constrained_df.rda")
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
#   Methods = "terbeam_maximization_unconstrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_maximization_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_maximization_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_maximization_unconstrained_df, file = "terbeam_maximization_unconstrained_df.rda")
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
#   Methods = "_terbeam_ranking_constrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_ranking_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_ranking_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_ranking_constrained_df, file = "terbeam_ranking_constrained_df.rda")
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
#   Methods = "terbeam_ranking_unconstrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_ranking_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_ranking_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_ranking_unconstrained_df, file = "terbeam_ranking_unconstrained_df.rda")
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
#   Methods = "_terbeam_predomics_aggregation_ova_constrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_predomics_aggregation_ova_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ova_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ova_constrained_df, file = "terbeam_predomics_aggregation_ova_constrained_df.rda")
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
#   Methods = "terbeam_predomics_aggregation_ova_unconstrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_predomics_aggregation_ova_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ova_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ova_unconstrained_df, file = "terbeam_predomics_aggregation_ova_unconstrained_df.rda")
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
#   Methods = "_terbeam_predomics_aggregation_ovo_constrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_predomics_aggregation_ovo_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ovo_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ovo_constrained_df, file = "terbeam_predomics_aggregation_ovo_constrained_df.rda")
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
#   Methods = "terbeam_predomics_aggregation_ovo_unconstrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_predomics_aggregation_ovo_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_predomics_aggregation_ovo_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_predomics_aggregation_ovo_unconstrained_df, file = "terbeam_predomics_aggregation_ovo_unconstrained_df.rda")
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
#   Methods = "_terbeam_voting_constrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_voting_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_voting_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_voting_constrained_df, file = "terbeam_voting_constrained_df.rda")
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
#   Methods = "terbeam_voting_unconstrained"  # Méthode spécifiée
# )
#
# # Vérification de la structure de 'terbeam_voting_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terbeam_voting_unconstrained") &&
#     !is.null(terbeam_voting_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terbeam_voting_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Precision.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Precision.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Recall.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$Recall.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$F1.empirique <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terbeam_voting_unconstrained_df$F1.generalization <- as.numeric(
#     terbeam_voting_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terbeam_voting_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_voting_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_voting_unconstrained_df, file = "terbeam_voting_unconstrained_df.rda")
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
#   Methods = "_terbeam_weighted_constrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_weighted_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_weighted_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_weighted_constrained_df, file = "terbeam_weighted_constrained_df.rda")
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
#   Methods = "terbeam_weighted_unconstrained"  # Méthode spécifiée
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
#   stop("L'objet 'terbeam_weighted_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terbeam_weighted_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terbeam_weighted_unconstrained_df, file = "terbeam_weighted_unconstrained_df.rda")
#
# #### Terga 1
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
#   Methods = "_terga1_maximization_constrained"  # Méthode spécifiée
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
#   stop("L'objet 'terga1_maximization_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_maximization_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_maximization_constrained_df, file = "terga1_maximization_constrained_df.rda")
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
#   Methods = "terga1_maximization_unconstrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_maximization_unconstrained_df, file = "terga1_maximization_unconstrained_df.rda")
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
#   Methods = "_terga1_ranking_constrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_ranking_constrained_df, file = "terga1_ranking_constrained_df.rda")
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
#   Methods = "terga1_ranking_unconstrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_ranking_unconstrained_df, file = "terga1_ranking_unconstrained_df.rda")
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
#   Methods = "_terga1_Predomics_aggregation_ova_constrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ova_constrained_df, file = "terga1_Predomics_aggregation_ova_constrained_df.rda")
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
#   Methods = "terga1_Predomics_aggregation_ova_unconstrained"  # Méthode spécifiée
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ova_unconstrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_Predomics_aggregation_ova_unconstrained") &&
#     !is.null(terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ova_unconstrained_df$Accuracy.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.acc["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Accuracy.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.acc["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Precision.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.rec["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Precision.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.rec["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Recall.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.prc["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$Recall.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.prc["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$F1.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$empirical.f1s["k_9", ]
#   )
#
#   terga1_Predomics_aggregation_ova_unconstrained_df$F1.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ova_unconstrained$crossVal$scores$generalization.f1s["k_9", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ova_unconstrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ova_unconstrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ova_unconstrained_df, file = "terga1_Predomics_aggregation_ova_unconstrained_df.rda")
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
#   Methods = "_terga1_Predomics_aggregation_ovo_constrained"  # Méthode spécifiée
# )
#
# # Vérification de la structure de 'terga1_Predomics_aggregation_ovo_constrained' et 'crossVal'
# # Assurez-vous que ces objets contiennent bien les éléments souhaités
# if (exists("terga1_Predomics_aggregation_ovo_constrained") &&
#     !is.null(terga1_Predomics_aggregation_ovo_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_Predomics_aggregation_ovo_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Precision.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Precision.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Recall.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$Recall.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$F1.empirique <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terga1_Predomics_aggregation_ovo_constrained_df$F1.generalization <- as.numeric(
#     terga1_Predomics_aggregation_ovo_constrained$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terga1_Predomics_aggregation_ovo_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_Predomics_aggregation_ovo_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ovo_constrained_df, file = "terga1_Predomics_aggregation_ovo_constrained_df.rda")
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
#   Methods = "terga1_Predomics_aggregation_ovo_unconstrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_Predomics_aggregation_ovo_unconstrained_df, file = "terga1_Predomics_aggregation_ovo_unconstrained_df.rda")
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
#   Methods = "_terga1_voting_constrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_voting_constrained_df, file = "terga1_voting_constrained_df.rda")
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
#   Methods = "terga1_voting_unconstrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_voting_unconstrained_df, file = "terga1_voting_unconstrained_df.rda")
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
#   Methods = "_terga1_weighted_constrained"  # Méthode spécifiée
# )
#
# # Vérification de la structure de 'terga1_weighted_constrained' et 'crossVal'
# if (exists("terga1_weighted_constrained") &&
#     !is.null(terga1_weighted_constrained$crossVal$scores)) {
#
#   # Remplissage des valeurs pour chaque métrique
#   terga1_weighted_constrained_df$Accuracy.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.acc["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$Accuracy.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.acc["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$Precision.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.rec["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$Precision.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.rec["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$Recall.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.prc["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$Recall.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.prc["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$F1.empirique <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$empirical.f1s["k_7", ]
#   )
#
#   terga1_weighted_constrained_df$F1.generalization <- as.numeric(
#     terga1_weighted_constrained$crossVal$scores$generalization.f1s["k_7", ]
#   )
# } else {
#   stop("L'objet 'terga1_weighted_constrained' ou 'crossVal$scores' n'existe pas.")
# }
#
# # Vérifier les résultats
# print(terga1_weighted_constrained_df)
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_weighted_constrained_df, file = "terga1_weighted_constrained_df.rda")
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
#   Methods = "terga1_weighted_unconstrained"  # Méthode spécifiée
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
#
# # Sauvegarder le data frame en fichier RDA
# save(terga1_weighted_unconstrained_df, file = "terga1_weighted_unconstrained_df.rda")
#
# ##################################################################################################################
# ## Fusion dataframe
#
# analysis_table_final <- bind_rows(
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
#   sota_dt_metrics_results,
#   sota_forest_metrics_results,
#   sota_gbm_metrics_results,
#   sota_knn_metrics_results,
#   sota_svm_metrics_results,
#   sota_logreg_metrics_results,
#   sota_ann_metrics_results
# )
# # Check the combined data frame
# print(head(analysis_table_final))
# # Save the combined data frame to an RDA file
# save(analysis_table_final, file = "analysis_table_final.rda")
#
#
#
# # Vérifier les types de colonnes avant de pivoter
# #str(analysis_table_final)  # Vérifiez la structure de votre data frame
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
