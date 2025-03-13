# #############
# # 18/02/2025: Get metagenomics datasets multiclass for Predomics multiclass testing
# #############
#
# ###load experimentHub metadata from DeepIntegromics
# df.eh <- read.table("/data/projects/deepintegromics/analyses/2.experimentHub_tabPFN/experimentHubMetadata.txt", header = TRUE, sep = "\t", comment.char = "", quote = "")
# length(unique(df.eh$study_name))
# ##get groups x study
# df.eh.table <- table(df.eh$study_name, df.eh$study_condition)
# ##Get number of groups x study
# df.eh.table.vec <- rowSums(df.eh.table>0)
# ##get studies with >2 groups
# length(df.eh.table.vec[df.eh.table.vec>2])
# ## metacardis one
# df.eh.table.mc <- df.eh.table["MetaCardis_2020_a",]
# df.eh.table.mc[df.eh.table.mc>0]
#
# ##Get target studies
# df.eh.table.vec.tstudies <- df.eh.table.vec[df.eh.table.vec>2]
# tlist <- list()
# for(i in names(df.eh.table.vec.tstudies))
# {
#   print(i)
#   #subset main to study
#   idf <- df.eh[df.eh$study_name %in% i,]
#   #do the table + save
#   tlist[[i]] <- table(idf$study_condition)
# }
#
#
# ## get the abundance tables
# load("/data/projects/deepintegromics/analyses/2.experimentHub_tabPFN/experimentHubRelAbn_93studies_22005samples_Robjects.Rda")
# ##Iterate over target studies to get abundance table + sample groups
# predomics.inputs <- list()
# for(i in names(tlist))
# {
#   ##Get the samples from sampleMetadata
#   isamples <- sampleMetadata$sample_id_edit[sampleMetadata$study_name %in% i]
#   ##Get the abundance table + re-transform to relative abundances
#   iabndf <- studies.merged.df[,intersect(isamples, colnames(studies.merged.df))]
#   iabndf <- as.data.frame(apply(iabndf, 2, function(x){x/sum(x)}))
#   ##Save abndf
#   predomics.inputs[[i]][["X"]] <- iabndf
#   ##Save sample groups
#   idf <- sampleMetadata[sampleMetadata$sample_id_edit %in% colnames(iabndf),c("sample_id_edit","study_condition")]
#   predomics.inputs[[i]][["y.df"]] <- idf
# }
#
# ##Save
# save(predomics.inputs, file = "/data/projects/predomics/btrmc/analyses/1.datasetsExperimentHub/predomics.inputs.ExperimentHubMulticlass.Rda")
# ##Get the summary of the colsums of abundance tables (all 1)
# lapply(predomics.inputs, function(x){summary(colSums(x[["X"]]))})
#
# ##CRC studies
# tlist.sub <- sapply(tlist, function(x){match("CRC", names(x))})
# tlist.sub.ids <- names(tlist.sub[!is.na(tlist.sub)])
# tlist[tlist.sub.ids] ##5 studies around CRC; 4/5 with 3 groups
# # $FengQ_2015
# #
# # CRC adenoma control
# # 46      47      61
# #
# # $HanniganGD_2017
# #
# # CRC adenoma control
# # 27      26      28
# #
# # $ThomasAM_2018a
# #
# # CRC adenoma control
# # 29      27      24
# #
# # $YachidaS_2019
# #
# # CRC                   adenoma carcinoma_surgery_history                   control
# # 258                        67                        40                       251
# #
# # $ZellerG_2014
# #
# # CRC adenoma control
# # 53      42      61
# #
#
# ##Get the summary of the colsums of abundance tables (all 1)
# lapply(predomics.inputs, function(x){summary(colSums(x[["X"]]))})
#
#
#
#
#
# ##########################################################################################
#
# # Extraire X et y de chaque dataset
# data1 = FengQ_2015
# data2 = HanniganGD_2017
# data3 = ThomasAM_2018a
# data4 = ZellerG_2014
#
# X1 <- data1$X
# X2 <- data2$X
# X3 <- data3$X
# X4 <- data4$X
#
# y1 <- data1$y.df$study_condition
# y2 <- data2$y.df$study_condition
# y3 <- data3$y.df$study_condition
# y4 <- data4$y.df$study_condition
#
# # Vérifier que toutes les variables sont les mêmes dans chaque dataset
# common_features <- Reduce(intersect, list(colnames(X1), colnames(X2), colnames(X3), colnames(X4)))
#
# # Filtrer pour ne garder que les colonnes communes
# X1 <- X1[, common_features]
# X2 <- X2[, common_features]
# X3 <- X3[, common_features]
# X4 <- X4[, common_features]
#
# # Fusionner X et y
# X_merged <- rbind(X1, X2, X3, X4)
# y_merged <- c(y1, y2, y3, y4)
#
# # Conserver la structure
# merged_data <- list(
#   X = X_merged,
#   y.df = data.frame(study_condition = y_merged)
# )
#
# # Vérifier la fusion
# dim(merged_data$X)  # Doit contenir toutes les observations fusionnées
# table(merged_data$y.df$study_condition)  # Vérifier la répartition des classes
#
