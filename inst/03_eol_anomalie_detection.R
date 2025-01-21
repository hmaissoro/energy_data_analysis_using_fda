# Dectection d'anomalies ----

# Cette Fonction calcule la fonction de répartition empirique d'un échantillon
# prend en argument un vecteur
repartition_function <- function(sample_vecteur){
  # --- Taille de l'échantillon
  len <- length(sample_vecteur)
  
  # -- Calcul de la fonction de répartition
  fr.x <- rank(x = sample_vecteur, ties.method = "max")/len
  
  return(fr.x)
}


# Fonction profondeur de Tukey
# Prend en argument un vecteur c(X(t_1), X(t_2), ...., X(t_N))
tukey_depth <- function(sample_vector){
  
  #--- Fonction de répartition
  fr.x <- repartition_function(sample_vector)
  
  #--- Fonction depth
  depth <- 1/2 - abs(1/2 - fr.x)
  
  return(depth)
}

# Calcul la profondeur de Tukey d'un échantillon de fonction
# Prend en argument une matrice N x Sampling time 
# Renvoie un vecteur de valeurs de la fonction profondeur
functional_tukey_depth <- function(fda_matrix){
  # On calcule la profondeur instant par instant
  fda_depth <- apply(X = fda_matrix, MARGIN = 2, FUN = tukey_depth)
  
  # On normalise
  fda_depth <- rowSums(fda_depth)/ncol(fda_depth)
  
  # On renvoie le résultat
  return(fda_depth)
}

## Format the data
dt_fts_mat <- dcast(data = dt_to_use, formula = year_week ~ t, value.var = "CdC")
fts_mat <- as.matrix(dt_fts_mat[, .SD, .SDcols = ! "year_week"])

## Compute the depth
depth_tukey <- functional_tukey_depth(fts_mat)
dt_fts_depth <- dt_fts_mat[, .( "id_curves" = as.character(year_week))]
dt_fts_depth[, depth := depth_tukey]

## Plot color curve by depth gradient
dt_graph_depth <- merge(x = dt_to_use[, .("id_curves" = as.character(year_week), CdC, t)],
                        y = dt_fts_depth, by = "id_curves")

grph_depth <- ggplot(dt_graph_depth, aes(x = t, y = CdC, group = id_curves, color = depth)) +
  geom_line() +
  labs(x = "t", y = "Puissance (KW)") +
  theme(axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = , r = 10, b = 0, l = 0)),
        axis.text.x =  element_text(size = 14),
        axis.text.y =  element_text(size = 14)) +
  guides(color = guide_legend(override.aes = list(size = 2)))
grph_depth
ggsave(plot = grph_depth, filename = file.path("./clusering_pdc/real_data_analysis/CdC_OYONN-63_all_depth.png"),
       width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(grph_depth) ; gc()

## Statistics by vacation date
dt_graph_depth_vacations <- merge(x = dt_vacances, y = dt_graph_depth[, .(id_curves, depth)], by = "id_curves")
dt_graph_depth_vacations[, day := wday(as.Date(id_curves))]
dt_graph_depth_vacations[, weekend := 1]
dt_graph_depth_vacations[day %in% c(1, 7), weekend := 2]
dt_graph_depth_vacations[, vacances_weekend := weekend * vacances]

dt_graph_depth_vacations[, .("q0.025" = quantile(depth, 0.025),
                             "q0.25" = quantile(depth, 0.25),
                             "median" = quantile(depth, 0.5),
                             "mean" = mean(depth), 
                             "q0.75" = quantile(depth, 0.75),
                             "q0.975" = quantile(depth, 0.975)),
                         by = "vacances"]