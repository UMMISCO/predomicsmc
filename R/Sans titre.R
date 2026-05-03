# library(ggplot2)
# library(reshape2)
# library(patchwork)
# load("~/Documents/multiclasse_predomics/mcpredomics/vignettes/mda_complete.annot_full_constraint_T2D.rda")
# mda_complete.annot_full_constraint_T2D$source <- "full-constraint"
# load("~/Documents/multiclasse_predomics/mcpredomics/vignettes/mda_complete.annot_semi_constraint_T2D.rda")
# mda_complete.annot_semi_constraint_T2D$source <- "semi-constraint"
# load("~/Documents/multiclasse_predomics/mcpredomics/vignettes/mda_complete.annot_unconstraint_T2D.rda")
# mda_complete.annot_unconstraint_T2D$source <- "unconstraint"
#
# alldf <- rbind(mda_complete.annot_full_constraint_T2D, mda_complete.annot_semi_constraint_T2D, mda_complete.annot_unconstraint_T2D)
# alldf$species <- sapply(strsplit(alldf$feature, split = "\\|"), function(x){x[7]})
# alldf$phylum <- sapply(strsplit(alldf$feature, split = "\\|"), function(x){x[2]})
#
# alldf2 <- reshape2::dcast(data = alldf, formula = species+phylum+model~source, value.var = "importance")
# alldf2$`full-constraint` <- ifelse(is.na(alldf2$`full-constraint`),0, alldf2$`full-constraint`)
# alldf2$`semi-constraint` <- ifelse(is.na(alldf2$`semi-constraint`),0, alldf2$`semi-constraint`)
# alldf2$unconstraint <- ifelse(is.na(alldf2$unconstraint),0,alldf2$unconstraint)
# alldf2 <- reshape2::melt(alldf2)
# colnames(alldf2) <- gsub("variable","source", colnames(alldf2))
# colnames(alldf2) <- gsub("value","importance", colnames(alldf2))
#
# alldf3 <- reshape2::dcast(data = alldf, formula = species+phylum+model~source, value.var = "coefficient")
# alldf3 <- reshape2::melt(alldf3)
# colnames(alldf3) <- gsub("variable","source", colnames(alldf3))
# colnames(alldf3) <- gsub("value","coefficient", colnames(alldf3))
# alldf3$coefficient <- ifelse(is.na(alldf3$coefficient),0,alldf3$coefficient)
# alldf3$coefficient <- factor(alldf3$coefficient, levels=c(-1,1,0))
#
# alldf23 <- cbind(alldf2, alldf3[,"coefficient",drop=FALSE])
# alldf23$source <- factor(alldf23$source,
#                          levels=c("full-constraint","semi-constraint","unconstraint"),
#                          labels=c("full\nconstraint","semi\nconstraint","unconstraint"))
#
# p_top <- ggplot(alldf23, aes(x = model, y = species)) +
#   geom_tile(aes(fill = importance), color = "grey90", linewidth = 0.3) +
#   geom_point(
#     aes(color = coefficient),
#     size = 3.8, stroke = 0, shape = 16,
#     na.rm = TRUE
#   ) +
#   scale_fill_gradient(low = "white", high = "steelblue", name = "MDA | CV") +
#   scale_color_manual(
#     name   = "Sign",
#     values = c("firebrick1", "deepskyblue1","white"),
#     breaks = c(-1,1,0),
#     labels = c("-1","1",""),
#     drop   = TRUE
#   ) +
#   labs(title = "", x = NULL, y = NULL) +
#   facet_grid(
#     phylum ~ source,
#     scales = "free_y",   # free only in y
#     space  = "free_y"    # height of each phylum according to number of rows
#   ) +
#   # scale_y_discrete(labels = feature_labeller) +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid   = element_blank(),
#     axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
#     axis.text.y  = element_text(size = 16),
#     plot.title   = element_text(face = "bold", size = 22),
#     plot.margin  = ggplot2::margin(t = 10, r = 25, b = 10, l = 30),
#     strip.text.y = element_text(angle = 0, size = 18, face = "bold"),
#     legend.position = "right",
#     legend.text     = element_text(size = 16),
#     legend.title    = element_text(size = 18, face = "bold")
#   )
#
# print(p_top)
#
# ##plot presence/absence
# alldf.table <- data.frame(table(alldf$feature, alldf$source))
# alldf.table$species <- sapply(strsplit(as.character(alldf.table$Var1), split = "\\|"), function(x){x[7]})
# alldf.table$phylum <- sapply(strsplit(as.character(alldf.table$Var1), split = "\\|"), function(x){x[2]})
# alldf.table$Freq.cat <- factor(alldf.table$Freq, levels=c(0,3), labels=c("absent","present"))
# alldf.table$facet_var <- "feature\nsharing"
# plot2 <- ggplot(alldf.table, aes(x=Var2, y=species, fill=Freq.cat)) +
#   geom_tile(color = "grey90", linewidth = 0.3) +
#   facet_grid(phylum~facet_var, scales = "free", space = "free") +
#   scale_fill_manual(values = c("white","black")) +
#   xlab("source") +
#   theme_minimal(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         strip.text.y = element_blank(),
#         strip.background.y = element_rect(fill = NA, linetype = 0),
#         legend.position = "bottom",
#         legend.title = element_blank())
#
# hlay <- "
# ABBB
# "
# library(patchwork)
# pdf(file="~/Documents/multiclasse_predomics/mcpredomics/vignettes/figure.pdf", height = 10, w=15)
# plot2 + p_top + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + plot_layout(design = hlay)
# dev.off()
#
#
#
#
#
#
#
#
# library(ggplot2)
# library(reshape2)
# library(patchwork)
#
# #load("~/Documents/multiclasse_predomics/mcpredomics/vignettes/mda_complete.annot_full_constraint_T2D.rda")
# data_contraint$source <- "full-constraint"
# #load("~/Documents/multiclasse_predomics/mcpredomics/vignettes/mda_complete.annot_semi_constraint_T2D.rda")
# data_semicontraint$source <- "semi-constraint"
# #load("~/Documents/multiclasse_predomics/mcpredomics/vignettes/mda_complete.annot_unconstraint_T2D.rda")
# data_uncontraint$source <- "unconstraint"
#
# alldf <- rbind(data_contraint,
#                data_semicontraint,
#                data_uncontraint)
# alldf$species <- sapply(strsplit(alldf$feature, split = "\\|"), function(x){x[7]})
# alldf$phylum  <- sapply(strsplit(alldf$feature, split = "\\|"), function(x){x[2]})
#
# alldf2 <- reshape2::dcast(data = alldf, formula = species+phylum+model~source, value.var = "importance")
# alldf2$`full-constraint` <- ifelse(is.na(alldf2$`full-constraint`),0, alldf2$`full-constraint`)
# alldf2$`semi-constraint` <- ifelse(is.na(alldf2$`semi-constraint`),0, alldf2$`semi-constraint`)
# alldf2$unconstraint <- ifelse(is.na(alldf2$unconstraint),0,alldf2$unconstraint)
# alldf2 <- reshape2::melt(alldf2)
# colnames(alldf2) <- gsub("variable","source", colnames(alldf2))
# colnames(alldf2) <- gsub("value","importance", colnames(alldf2))
#
# alldf3 <- reshape2::dcast(data = alldf, formula = species+phylum+model~source, value.var = "coefficient")
# alldf3 <- reshape2::melt(alldf3)
# colnames(alldf3) <- gsub("variable","source", colnames(alldf3))
# colnames(alldf3) <- gsub("value","coefficient", colnames(alldf3))
# alldf3$coefficient <- ifelse(is.na(alldf3$coefficient),0,alldf3$coefficient)
# alldf3$coefficient <- factor(alldf3$coefficient, levels=c(-1,1,0))
#
# alldf23 <- cbind(alldf2, alldf3[,"coefficient",drop=FALSE])
# alldf23$source <- factor(alldf23$source,
#                          levels=c("full-constraint","semi-constraint","unconstraint"),
#                          labels=c("full\nconstraint","semi\nconstraint","unconstraint"))
#
# p_top <- ggplot(alldf23, aes(x = model, y = species)) +
#   geom_tile(aes(fill = importance), color = "grey90", linewidth = 0.3) +
#   geom_point(
#     aes(color = coefficient),
#     size = 3.8, stroke = 0, shape = 16,
#     na.rm = TRUE
#   ) +
#   scale_fill_gradient(low = "white", high = "steelblue", name = "MDA | CV") +
#   scale_color_manual(
#     name   = "Sign",
#     values = c("firebrick1", "deepskyblue1","white"),
#     breaks = c(-1,1,0),
#     labels = c("first class","second class",""),
#     drop   = TRUE
#   ) +
#   labs(title = "", x = NULL, y = NULL) +
#   facet_grid(
#     phylum ~ source,
#     scales = "free_y",
#     space  = "free_y"
#   ) +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid   = element_blank(),
#     axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
#     axis.text.y  = element_text(size = 16),
#     plot.title   = element_text(face = "bold", size = 22),
#     plot.margin  = ggplot2::margin(t = 10, r = 25, b = 10, l = 30),
#     strip.text.y = element_text(angle = 0, size = 18, face = "bold"),
#     legend.position = "right",
#     legend.text     = element_text(size = 16),
#     legend.title    = element_text(size = 18, face = "bold")
#   )
#
# print(p_top)
#
# ## plot presence/absence
# alldf.table <- data.frame(table(alldf$feature, alldf$source))
# alldf.table$species <- sapply(strsplit(as.character(alldf.table$Var1), split = "\\|"), function(x){x[7]})
# alldf.table$phylum <- sapply(strsplit(as.character(alldf.table$Var1), split = "\\|"), function(x){x[2]})
# alldf.table$Freq.cat <- factor(alldf.table$Freq, levels=c(0,3), labels=c("absent","present"))
# alldf.table$facet_var <- "feature\nsharing"
#
# plot2 <- ggplot(alldf.table, aes(x=Var2, y=species, fill=Freq.cat)) +
#   geom_tile(color = "grey90", linewidth = 0.3) +
#   facet_grid(phylum~facet_var, scales = "free", space = "free") +
#   scale_fill_manual(values = c("white","black")) +
#   xlab("source") +
#   theme_minimal(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         strip.text.y = element_blank(),
#         strip.background.y = element_rect(fill = NA, linetype = 0),
#         legend.position = "bottom",
#         legend.title = element_blank())
#
# hlay <- "
# ABBB
# "
#
# pdf(file="~/Documents/multiclasse_predomics/mcpredomics/vignettes/figure4.pdf",
#     height = 10, width = 15)
# plot2 + p_top + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
#   plot_layout(design = hlay)
# dev.off()
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
# library(ggplot2)
# library(reshape2)
# library(patchwork)
#
# ## =========================
# ## 0) Add source
# ## =========================
# data_contraint1$source     <- "full-constraint"
# data_semicontraint3$source <- "semi-constraint"
# data_uncontraint2$source   <- "unconstraint"
#
# ## =========================
# ## 1) Bind all
# ## =========================
# alldf <- rbind(
#   data_contraint1,
#   data_semicontraint3,
#   data_uncontraint2
# )
#
# ## =========================
# ## 2) Robust taxonomy extraction
# ## =========================
# spl <- strsplit(as.character(alldf$feature), "\\|")
#
# alldf$species <- vapply(seq_along(spl), function(i) {
#   x <- spl[[i]]
#   if (length(x) >= 7 && !is.na(x[7]) && nzchar(x[7])) x[7] else as.character(alldf$feature[i])
# }, character(1))
#
# alldf$phylum <- vapply(seq_along(spl), function(i) {
#   x <- spl[[i]]
#   if (length(x) >= 2 && !is.na(x[2]) && nzchar(x[2])) x[2] else "p__Unknown"
# }, character(1))
#
# ## =========================
# ## 3) Importance
# ## =========================
# alldf2 <- reshape2::dcast(
#   data = alldf,
#   formula = species + phylum + model ~ source,
#   value.var = "importance",
#   fun.aggregate = mean,
#   fill = 0
# )
#
# alldf2 <- reshape2::melt(alldf2)
# colnames(alldf2) <- gsub("variable","source", colnames(alldf2))
# colnames(alldf2) <- gsub("value","importance", colnames(alldf2))
#
# ## =========================
# ## 4) Coefficient
# ## =========================
# alldf3 <- reshape2::dcast(
#   data = alldf,
#   formula = species + phylum + model ~ source,
#   value.var = "coefficient",
#   fun.aggregate = function(x) x[1]
# )
#
# alldf3 <- reshape2::melt(alldf3)
# colnames(alldf3) <- gsub("variable","source", colnames(alldf3))
# colnames(alldf3) <- gsub("value","coefficient", colnames(alldf3))
#
# alldf3$coefficient <- ifelse(is.na(alldf3$coefficient), 0, alldf3$coefficient)
# alldf3$coefficient <- factor(alldf3$coefficient, levels = c(-1, 1, 0))
#
# ## =========================
# ## 5) Combine (safe)
# ## =========================
# alldf23 <- merge(
#   alldf2, alldf3,
#   by = c("species", "phylum", "model", "source"),
#   all = TRUE
# )
#
# alldf23$source <- factor(
#   alldf23$source,
#   levels = c("full-constraint","semi-constraint","unconstraint"),
#   labels = c("full\nconstraint","semi\nconstraint","unconstraint")
# )
#
# ## =========================
# ## 6) Main plot
# ## =========================
# p_top <- ggplot(alldf23, aes(x = model, y = species)) +
#   geom_tile(aes(fill = importance), color = "grey90", linewidth = 0.3) +
#   geom_point(
#     aes(color = coefficient),
#     size = 3.8, stroke = 0, shape = 16,
#     na.rm = TRUE
#   ) +
#   scale_fill_gradient(low = "white", high = "steelblue", name = "MDA | CV") +
#   scale_color_manual(
#     name   = "Sign",
#     values = c("firebrick1", "deepskyblue1","white"),
#     breaks = c(-1,1,0),
#     labels = c("first class","second class",""),
#     drop   = TRUE
#   ) +
#   labs(title = "", x = NULL, y = NULL) +
#   facet_grid(
#     phylum ~ source,
#     scales = "free_y",
#     space  = "free_y"
#   ) +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid   = element_blank(),
#     axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
#     axis.text.y  = element_text(size = 16),
#     plot.title   = element_text(face = "bold", size = 22),
#     plot.margin  = ggplot2::margin(t = 10, r = 25, b = 10, l = 30),
#     strip.text.y = element_text(angle = 0, size = 18, face = "bold"),
#     legend.position = "right",
#     legend.text     = element_text(size = 16),
#     legend.title    = element_text(size = 18, face = "bold")
#   )
#
# print(p_top)
#
# ## =========================
# ## 7) Presence/absence (robust fix)
# ## =========================
# alldf.table <- as.data.frame(table(alldf$feature, alldf$source), stringsAsFactors = FALSE)
# colnames(alldf.table) <- c("feature", "source", "Freq")
#
# spl2 <- strsplit(as.character(alldf.table$feature), "\\|")
#
# alldf.table$species <- vapply(seq_along(spl2), function(i) {
#   x <- spl2[[i]]
#   if (length(x) >= 7 && !is.na(x[7]) && nzchar(x[7])) x[7] else as.character(alldf.table$feature[i])
# }, character(1))
#
# alldf.table$phylum <- vapply(seq_along(spl2), function(i) {
#   x <- spl2[[i]]
#   if (length(x) >= 2 && !is.na(x[2]) && nzchar(x[2])) x[2] else "p__Unknown"
# }, character(1))
#
# alldf.table$Freq.cat <- factor(
#   ifelse(alldf.table$Freq > 0, "present", "absent"),
#   levels = c("absent", "present")
# )
#
# alldf.table$facet_var <- "feature\nsharing"
#
# plot2 <- ggplot(alldf.table, aes(x = source, y = species, fill = Freq.cat)) +
#   geom_tile(color = "grey90", linewidth = 0.3) +
#   facet_grid(phylum ~ facet_var, scales = "free", space = "free") +
#   scale_fill_manual(values = c("white","black")) +
#   xlab("source") +
#   theme_minimal(base_size = 20) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     strip.text.y = element_blank(),
#     strip.background.y = element_rect(fill = NA, linetype = 0),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   )
#
# ## =========================
# ## 8) Combine & export
# ## =========================
# hlay <- "
# ABBB
# "
#
# pdf(
#   file="~/Documents/multiclasse_predomics/mcpredomics/vignettes/figure5.pdf",
#   height = 14, width = 25
# )
#
# plot2 + p_top +
#   theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
#   plot_layout(design = hlay)
#
# dev.off()
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
# ##########################################################
#
#
# ## =========================================================
# ## Heatmap MDA | CV + Sign + Feature sharing
# ## Version adaptée aux nouveaux dataframes (features = CAG)
# ## Tous les fichiers supposés dans: vignettes/
# ## =========================================================
#
# ## =========================================================
# ## MDA heatmap + coefficient sign + feature sharing
# ## New dataframes with CAG-only features
# ## All files assumed in: vignettes/
# ## No vapply() anywhere
# ## =========================================================
#
# library(ggplot2)
# library(reshape2)
# library(patchwork)
# library(dplyr)
#
# ## -------------------------
# ## 0) Paths
# ## -------------------------
# base_dir <- "vignettes"
# taxo_name <- "taxo_hs_9_9_igc_1436_CAG_sup_500_gtdb_214_long.tsv"
# taxo_file <- file.path(base_dir, taxo_name)
#
# ## -------------------------
# ## 1) Add source
# ## -------------------------
# data_contraint1$source     <- "full-constraint"
# data_semicontraint3$source <- "semi-constraint"
# data_uncontraint2$source   <- "unconstraint"
#
# ## -------------------------
# ## 2) Bind all
# ## -------------------------
# alldf <- rbind(
#   data_contraint1,
#   data_semicontraint3,
#   data_uncontraint2
# )
#
# ## Ensure character types
# alldf$feature <- as.character(alldf$feature)
# alldf$model   <- as.character(alldf$model)
# alldf$source  <- as.character(alldf$source)
#
# ## -------------------------
# ## 3) Load taxonomy (from vignettes/)
# ## -------------------------
# df.annot <- NULL
# if (file.exists(taxo_file)) {
#   df.annot <- read.table(
#     taxo_file,
#     sep = "\t", header = TRUE, quote = "", comment.char = "",
#     stringsAsFactors = FALSE
#   )
# }
#
# ## -------------------------
# ## 4) Add phylum/species
# ##     New feature = "CAGxxxxx" => cannot use strsplit("|")
# ## -------------------------
# if (!is.null(df.annot) && "cag_name" %in% colnames(df.annot)) {
#   alldf <- alldf %>%
#     left_join(df.annot, by = c("feature" = "cag_name"))
# }
#
# ## If columns absent, create them
# if (!"phylum" %in% colnames(alldf))  alldf$phylum  <- NA_character_
# if (!"species" %in% colnames(alldf)) alldf$species <- NA_character_
#
# ## Clean fallbacks
# alldf$phylum <- ifelse(is.na(alldf$phylum) | alldf$phylum == "",
#                        "p__Unknown", as.character(alldf$phylum))
#
# ## If species missing, fallback to CAG id
# alldf$species <- ifelse(is.na(alldf$species) | alldf$species == "" | alldf$species == "unclassified",
#                         alldf$feature, as.character(alldf$species))
#
# ## -------------------------
# ## 5) Importance
# ## -------------------------
# alldf2_w <- reshape2::dcast(
#   data = alldf,
#   formula = species + phylum + model ~ source,
#   value.var = "importance",
#   fun.aggregate = mean,
#   fill = 0
# )
#
# alldf2 <- reshape2::melt(
#   alldf2_w,
#   id.vars = c("species", "phylum", "model"),
#   variable.name = "source",
#   value.name = "importance"
# )
#
# ## -------------------------
# ## 6) Coefficient
# ##     Take first non-NA value if duplicates
# ## -------------------------
# first_non_na <- function(x) {
#   x <- x[!is.na(x)]
#   if (length(x) == 0) return(NA_real_)
#   x[1]
# }
#
# alldf3_w <- reshape2::dcast(
#   data = alldf,
#   formula = species + phylum + model ~ source,
#   value.var = "coefficient",
#   fun.aggregate = first_non_na,
#   fill = NA
# )
#
# alldf3 <- reshape2::melt(
#   alldf3_w,
#   id.vars = c("species", "phylum", "model"),
#   variable.name = "source",
#   value.name = "coefficient"
# )
#
# alldf3$coefficient <- ifelse(is.na(alldf3$coefficient), 0, alldf3$coefficient)
# alldf3$coefficient <- factor(alldf3$coefficient, levels = c(-1, 1, 0))
#
# ## -------------------------
# ## 7) Combine safely
# ## -------------------------
# alldf23 <- merge(
#   alldf2, alldf3,
#   by = c("species", "phylum", "model", "source"),
#   all = TRUE
# )
#
# alldf23$source <- factor(
#   alldf23$source,
#   levels = c("full-constraint", "semi-constraint", "unconstraint"),
#   labels = c("full\nconstraint", "semi\nconstraint", "unconstraint")
# )
#
# ## Order phylum for cleaner facets
# phylum_order <- sort(unique(as.character(alldf23$phylum)))
# alldf23$phylum <- factor(alldf23$phylum, levels = phylum_order)
#
# ## -------------------------
# ## 8) Main heatmap
# ##     Rouge = second class (-1)
# ##     Bleu  = first class  (+1)
# ## -------------------------
# p_top <- ggplot(alldf23, aes(x = model, y = species)) +
#   geom_tile(aes(fill = importance), color = "grey90", linewidth = 0.3) +
#   geom_point(
#     aes(color = coefficient),
#     size = 3.8, stroke = 0, shape = 16,
#     na.rm = TRUE
#   ) +
#   scale_fill_gradient(low = "white", high = "steelblue", name = "MDA | CV") +
#   scale_color_manual(
#     name   = "Sign",
#     values = c("firebrick1", "deepskyblue1", "white"),
#     breaks = c(-1, 1, 0),
#     labels = c("second class", "first class", ""),
#     drop   = TRUE
#   ) +
#   labs(title = "", x = NULL, y = NULL) +
#   facet_grid(
#     phylum ~ source,
#     scales = "free_y",
#     space  = "free_y"
#   ) +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid   = element_blank(),
#     axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
#     axis.text.y  = element_text(size = 16),
#     plot.title   = element_text(face = "bold", size = 22),
#     plot.margin  = ggplot2::margin(t = 10, r = 25, b = 10, l = 30),
#     strip.text.y = element_text(angle = 0, size = 18, face = "bold"),
#     legend.position = "right",
#     legend.text     = element_text(size = 16),
#     legend.title    = element_text(size = 18, face = "bold")
#   )
#
# print(p_top)
#
# ## -------------------------
# ## 9) Presence/absence
# ## -------------------------
# alldf.table <- as.data.frame(table(alldf$feature, alldf$source), stringsAsFactors = FALSE)
# colnames(alldf.table) <- c("feature", "source", "Freq")
# alldf.table$feature <- as.character(alldf.table$feature)
#
# ## Add taxonomy to presence table
# if (!is.null(df.annot) && "cag_name" %in% colnames(df.annot)) {
#   alldf.table <- alldf.table %>%
#     left_join(df.annot, by = c("feature" = "cag_name"))
# }
#
# if (!"phylum" %in% colnames(alldf.table))  alldf.table$phylum  <- NA_character_
# if (!"species" %in% colnames(alldf.table)) alldf.table$species <- NA_character_
#
# alldf.table$phylum <- ifelse(is.na(alldf.table$phylum) | alldf.table$phylum == "",
#                              "p__Unknown", as.character(alldf.table$phylum))
#
# alldf.table$species <- ifelse(is.na(alldf.table$species) | alldf.table$species == "" | alldf.table$species == "unclassified",
#                               alldf.table$feature, as.character(alldf.table$species))
#
# alldf.table$Freq.cat <- factor(
#   ifelse(alldf.table$Freq > 0, "present", "absent"),
#   levels = c("absent", "present")
# )
#
# alldf.table$facet_var <- "feature\nsharing"
# alldf.table$phylum <- factor(alldf.table$phylum, levels = phylum_order)
#
# plot2 <- ggplot(alldf.table, aes(x = source, y = species, fill = Freq.cat)) +
#   geom_tile(color = "grey90", linewidth = 0.3) +
#   facet_grid(phylum ~ facet_var, scales = "free", space = "free") +
#   scale_fill_manual(values = c("white", "black")) +
#   xlab("source") +
#   theme_minimal(base_size = 20) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     strip.text.y = element_blank(),
#     strip.background.y = element_rect(fill = NA, linetype = 0),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   )
#
# print(plot2)
#
# ## -------------------------
# ## 10) Combine & export
# ## -------------------------
# hlay <- "
# ABBB
# "
#
# out_pdf <- file.path(base_dir, "figure_new_classes.pdf")
#
# pdf(out_pdf, height = 10, width = 15)
#
# plot2 + p_top +
#   theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
#   plot_layout(design = hlay)
#
# dev.off()
#
# message("Saved: ", out_pdf)
