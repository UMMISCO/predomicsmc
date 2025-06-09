#
# #
# #
# #
# #
# #
# #
# # terbeam_predomics_aggregation_ova_constrained_df_balance <-  terbeam_predomics_aggregation_ova_constrained_df_balance %>%
# #   rename(Features = k)
# #
# # ## Fusion dataframe
# #
# # analysis_table_final2 <- bind_rows(
# #   terga1_Predomics_aggregation_ova_unconstrained_df_balance,
# #   terga1_Predomics_aggregation_ovo_unconstrained_df_balance,
# #   terga1_maximization_unconstrained_df_balance,
# #   terga1_voting_unconstrained_df_balance,
# #   terga1_Predomics_aggregation_ova_constrained_df_balance,
# #   terga1_Predomics_aggregation_ovo_constrained_df_balance,
# #   terga1_maximization_constrained_df_balance,
# #   terga1_voting_constrained_df_balance,
# #   terbeam_predomics_aggregation_ova_unconstrained_df_balance,
# #   terbeam_predomics_aggregation_ovo_unconstrained_df_balance,
# #   terbeam_maximization_unconstrained_df_balance,
# #   terbeam_voting_unconstrained_df_balance,
# #   terbeam_predomics_aggregation_ova_constrained_df_balance,
# #   terbeam_predomics_aggregation_ovo_constrained_df_balance,
# #   terbeam_maximization_constrained_df_balance,
# #   terbeam_voting_constrained_df_balance,
# #   sota_dt_metrics_results_balance,
# #   sota_forest_metrics_results_balance,
# #   sota_gbm_metrics_results_balance,
# #   sota_knn_metrics_results_balance,
# #   sota_svm_metrics_results_balance,
# #   sota_logreg_metrics_results_balance,
# #   sota_ann_metrics_results_balance
# #
# # )
# # # Check the combined data frame
# # print(head(analysis_table_final2))
# # analysis_table_final_complet_balance_ = analysis_table_final2
# # # Save the combined data frame to an RDA file
# #
# #
# #
# #
# #
# #
# #
# # analysis_table_final_complet_balance_Metacardis = analysis_table_final_complet_balance_
# #
# #
# # save(analysis_table_final_complet_balance_, file = "analysis_table_final_complet_balance_.rda")
# # analysis_table_final_complet_no_balance_3_
# # analysis_table_final_complet_no_balance_2_
# # analysis_table_final_complet_no_balance_
# # analysis_table_final_complet_balance_
# #
# #
# #
# #
# #
# #
# # # Vérifier les types de colonnes avant de pivoter
# # #str(analysis_table_final)  # Vérifiez la structure de votre data frame
# #
# # # Imputation des valeurs manquantes (si nécessaire)
# # analysis_table_final <- analysis_table_final_complet_no_balance_2 %>%
# #   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
# #
# # # Convertir en format long
# # df <- analysis_table_final %>%
# #   pivot_longer(
# #     cols = -c(Fold, Methods),
# #     names_to = c("Metric", "Partition"),
# #     names_sep = "\\.",
# #     values_to = "Value"
# #   ) %>%
# #   mutate(
# #     Partition = recode(Partition,
# #                        'empirique' = 'train',
# #                        'generalization' = 'test'
# #     )
# #   ) %>%
# #   mutate(
# #     Partition = factor(Partition, levels = c("train", "test")),
# #     Value = as.numeric(Value),
# #     Group = case_when(
# #       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
# #       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
# #       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
# #       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
# #       TRUE ~ "SOTA"  # Pour toutes les autres méthodes
# #     )
# #   )
# #
# # # Agréger les données pour le résumé
# # df_summary <- df %>%
# #   group_by(Methods, Partition, Metric) %>%
# #   summarize(
# #     mean_value = mean(Value, na.rm = TRUE),
# #     sd_value = sd(Value, na.rm = TRUE),
# #     .groups = "drop"
# #   ) %>%
# #   left_join(df %>% select(Methods, Group) %>% distinct(), by = "Methods")  # Assurez-vous que Group est présent
# #
# # # Vérification des valeurs de Group
# # print(unique(df_summary$Group))  # Vérifiez les groupes présents
# #
# # # Tracez le graphique
# # df_summary %>%
# #   #filter(Metric != "Precision") %>%
# #   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
# #   geom_point(position = position_dodge(width = 0.5), size = 4) +  # Taille des points
# #   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
# #                 position = position_dodge(width = 0.5), width = 0.2) +
# #   facet_grid(Metric ~ Group, scales = "free") +  # Facettage par Metric et Group
# #   ylab("Value") +
# #   xlab("Methods") +  # Réglage des labels
# #   theme_bw() +
# #   scale_colour_manual(values = c("seagreen", "firebrick3")) +
# #   theme(
# #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
# #     legend.position = "bottom"
# #   )
# #
# #
# #
# #
# #
# #
# # # Charger le package stringr
# # library(stringr)
# #
# # analysis_table_final <- analysis_table_final_complet_no_balance_3 %>%
# #   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
# # # Convertir en format long
# # df <- analysis_table_final %>%
# #   pivot_longer(
# #     cols = -c(Fold, Methods, Features),  # Inclure 'Features' dans les colonnes non pivots
# #     names_to = c("Metric", "Partition"),
# #     names_sep = "\\.",
# #     values_to = "Value"
# #   ) %>%
# #   mutate(
# #     Partition = recode(Partition,
# #                        'empirique' = 'train',
# #                        'generalization' = 'test'
# #     ),
# #     Partition = factor(Partition, levels = c("train", "test")),
# #     Value = as.numeric(Value),
# #     Group = case_when(
# #       str_starts(Methods, "terbeam") ~ "Predomics Terbeam unconstrained",
# #       str_starts(Methods, "_terbeam") ~ "Predomics Terbeam constrained",
# #       str_starts(Methods, "terga1") ~ "Predomics Terga1 unconstrained",
# #       str_starts(Methods, "_terga1") ~ "Predomics Terga1 constrained",
# #       TRUE ~ "SOTA"  # Pour toutes les autres méthodes
# #     )
# #   )
# #
# # # Agréger les données pour le résumé
# # df_summary <- df %>%
# #   group_by(Methods, Partition, Metric) %>%
# #   summarize(
# #     mean_value = mean(Value, na.rm = TRUE),
# #     sd_value = sd(Value, na.rm = TRUE),
# #     .groups = "drop"
# #   ) %>%
# #   left_join(df %>% select(Methods, Group) %>% distinct(), by = "Methods")  # Assurez-vous que Group est présent
# #
# # # Ajouter la colonne Features à df_summary
# # df_summary <- df_summary %>%
# #   left_join(analysis_table_final %>% select(Methods, Features), by = "Methods") %>%
# #   mutate(Methods = paste0(Methods, " (Features = ", Features, ")"))  # Ajouter le nombre de features dans le nom des méthodes
# #
# # # Vérification des valeurs de Group
# # print(unique(df_summary$Group))  # Vérifiez les groupes présents
# #
# # # Tracez le graphique avec les nouvelles étiquettes des méthodes
# # df_summary %>%
# #   ggplot(aes(x = Methods, y = mean_value, color = Partition)) +
# #   geom_point(position = position_dodge(width = 0.5), size = 4) +  # Taille des points
# #   geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
# #                 position = position_dodge(width = 0.5), width = 0.2) +
# #   facet_grid(Metric ~ Group, scales = "free") +  # Facettage par Metric et Group
# #   ylab("Value") +
# #   xlab("Methods") +  # Réglage des labels
# #   theme_bw() +
# #   scale_colour_manual(values = c("seagreen", "firebrick3")) +
# #   theme(
# #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
# #     legend.position = "bottom"
# #   )
# #
# #
# #
# #
# #
# #
# #
# #
# #
#
#
#
# mean(as.numeric(res_clf_voting_terbeam_balance$crossVal$scores$generalization.acc["k_6", ]), na.rm = TRUE)
#
#
# res_clf_voting_terbeam_balance$crossVal$scores$generalization.acc["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$generalization.rec["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$generalization.prc["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$generalization.f1s["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$empirical.acc["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$empirical.rec["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$empirical.prc["k_6", ]
# res_clf_voting_terbeam_balance$crossVal$scores$empirical.f1s["k_6", ]
#
# unique(Analysis_Final_Results_DF_05_2025$Constraint_factor)
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
# # Créer un vecteur de folds qui correspond aux colonnes ou lignes à mettre à jour
# fold_names <- paste0("fold_", 1:10)
#
# # Index des lignes ciblées (Dataset, Methods, Constraint_factor, Set)
# indices <- with(Analysis_Final_Results_DF_05_2025,
#                 which(Dataset == "Balanced Enterotype" &
#                         Methods == "Voting" &
#                         Constraint_factor %in% c("Semi_Constrained", "Unconstrained") &
#                         Set == "Test"))
#
# # Ici on suppose qu'il y a 40 lignes (4 métriques * 10 folds)
# # et que ces lignes sont ordonnées par Metric puis Fold
# # On récupère le vecteur Metric et Fold pour ces indices
# sub_df <- Analysis_Final_Results_DF_05_2025[indices, ]
#
# # Remplacer les valeurs par fold & métrique
# for (i in seq_along(fold_names)) {
#   fold <- fold_names[i]
#
#   # Accuracy
#   idx_acc <- indices[sub_df$Metric == "Accuracy" & sub_df$Fold == fold]
#   Analysis_Final_Results_DF_05_2025$Accuracy[idx_acc] <- acc_k6[i]
#
#   # Recall
#   idx_rec <- indices[sub_df$Metric == "Recall" & sub_df$Fold == fold]
#   Analysis_Final_Results_DF_05_2025$Recall[idx_rec] <- rec_k6[i]
#
#   # Precision
#   idx_prc <- indices[sub_df$Metric == "Precision" & sub_df$Fold == fold]
#   Analysis_Final_Results_DF_05_2025$Precision[idx_prc] <- prc_k6[i]
#
#   # f1
#   idx_f1 <- indices[sub_df$Metric == "f1" & sub_df$Fold == fold]
#   Analysis_Final_Results_DF_05_2025$F1[idx_f1] <- f1_k6[i]
# }
#
#
#
#
#
#
#
# # Sélection des indices déjà calculés
# indices <- with(Analysis_Final_Results_DF_05_2025,
#                 which(Dataset == "Balanced Enterotype" &
#                         Methods == "Voting" &
#                         Constraint_factor %in% c("Semi_Constrained", "Unconstrained") &
#                         Set == "Test"))
#
# # Calcul de la moyenne des métriques sur ce sous-ensemble
# mean_accuracy <- mean(Analysis_Final_Results_DF_05_2025$Accuracy[indices], na.rm = TRUE)
# mean_recall   <- mean(Analysis_Final_Results_DF_05_2025$Recall[indices], na.rm = TRUE)
# mean_precision <- mean(Analysis_Final_Results_DF_05_2025$Precision[indices], na.rm = TRUE)
# mean_f1       <- mean(Analysis_Final_Results_DF_05_2025$F1[indices], na.rm = TRUE)
#
# # Affichage
# mean_accuracy
# mean_recall
# mean_precision
# mean_f1
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
# # Remplacement dans la colonne Accuracy par la série acc_k6 (de longueur 10)
# acc_k6 <- as.numeric(res_clf_voting_terbeam_balance$crossVal$scores$generalization.acc["k_6", ])
#
# Analysis_Final_Results_DF_05_2025$Accuracy[indices[1:10]] <- acc_k6
#
# # Maintenant la moyenne sur ces 10 lignes
# mean(Analysis_Final_Results_DF_05_2025$Accuracy[indices[1:10]])
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
# # Chargement des indices du sous-ensemble filtré
# indices <- with(Analysis_Final_Results_DF_05_2025,
#                 which(Dataset == "Balanced Enterotype" &
#                         Methods == "Voting" &
#                         Constraint_factor %in% c("Semi_Constrained", "Unconstrained") &
#                         Set == "Test"))
#
# # Extraction des folds à l'intérieur de ce sous-ensemble
# folds <- unique(Analysis_Final_Results_DF_05_2025$Fold[indices])
# folds <- sort(folds)  # Assure l'ordre fold_1 ... fold_10
#
# # Récupérer les vecteurs à insérer
# acc_k6 <- as.numeric(res_clf_voting_terbeam_balance$crossVal$scores$generalization.acc["k_6", ])
# rec_k6 <- as.numeric(res_clf_voting_terbeam_balance$crossVal$scores$generalization.rec["k_6", ])
# prc_k6 <- as.numeric(res_clf_voting_terbeam_balance$crossVal$scores$generalization.prc["k_6", ])
# f1_k6  <- as.numeric(res_clf_voting_terbeam_balance$crossVal$scores$generalization.f1s["k_6", ])
#
# for(i in seq_along(folds)) {
#   fold_name <- folds[i]
#
#   # Trouver les indices correspondant à ce fold dans le sous-ensemble filtré
#   idx_fold <- indices[which(Analysis_Final_Results_DF_05_2025$Fold[indices] == fold_name)]
#
#   # Remplacer les valeurs dans les colonnes ciblées
#   Analysis_Final_Results_DF_05_2025$Accuracy[idx_fold] <- acc_k6[i]
#   Analysis_Final_Results_DF_05_2025$Recall[idx_fold]   <- rec_k6[i]
#   Analysis_Final_Results_DF_05_2025$Precision[idx_fold] <- prc_k6[i]
#   Analysis_Final_Results_DF_05_2025$F1[idx_fold]      <- f1_k6[i]
# }
#
# # Vérification : moyenne des valeurs dans ce sous-ensemble maintenant remplacées
# mean_accuracy <- mean(Analysis_Final_Results_DF_05_2025$Accuracy[indices])
# mean_recall   <- mean(Analysis_Final_Results_DF_05_2025$Recall[indices])
# mean_precision<- mean(Analysis_Final_Results_DF_05_2025$Precision[indices])
# mean_f1       <- mean(Analysis_Final_Results_DF_05_2025$F1[indices])
#
# mean_accuracy
# mean_recall
# mean_precision
# mean_f1
#
#
# #
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
