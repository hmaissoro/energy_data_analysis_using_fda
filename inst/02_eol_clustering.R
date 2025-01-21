library("fda.usc")
library("fda")
library("data.table")
library("magrittr")
library("ggplot2")
theme_set(theme_minimal())

# ---- import data extract ---- #
## Extract 2021 data
data_eole <- readRDS("/home/projets/2020-A258-ENEDIS_Prev/Eolien/data_final_2020_2021.RDS")
dt_raw <- data_eole$data
rm(data_eole) ; gc()

## Number of prod
dt_raw[, id_producteur := bit64::as.character.integer64(id_producteur)]
id_prod <- dt_raw[, unique(id_producteur)]
length(id_prod)

## get dates
dt_raw[, date := as.Date(horodate)]

## 2021 data
dt_raw <- dt_raw[year(horodate) == 2021]

## Aggregate daily
dt_raw[, date := as.Date(horodate)]
dt_raw[, x := mean(FC, na.rm = TRUE), by = c("date", "id_producteur")]
dt_raw <- unique(dt_raw[, .(id_producteur, date, x)])
dt_raw[, .N, by = id_producteur][, summary(N)]
id_prod_to_remove <- dt_raw[, .N, by = id_producteur][N < 365, id_producteur]
dt_raw <- dt_raw[ ! id_producteur %in% id_prod_to_remove]
dt_raw[, t := (1:365) / 365, by = id_producteur]
id_prod <- dt_raw[, unique(id_producteur)]
## N curves = 905

## Reshape for adaptive fpca
dt_list <- lapply(id_prod, function(id){
  list(
    "t" = dt_raw[id_producteur == id, t],
    "x" = dt_raw[id_producteur == id, x]
  )
})

names(dt_list) <- id_prod
saveRDS(object = dt_list, file = "./data/eolien/data_eol_iid_for_afpca.RDS")

## add ref data
data_ref <- data.table::fread("/home/projets/2020-A258-ENEDIS_Prev/Eolien/ref_producteur_2022.csv")
data_ref[, id_producteur := bit64::as.character.integer64(id_producteur)]
data_ref <- data_ref[filiere == "Eolien"]

data_fdc <- merge(x = dt_raw, 
                  y = data_ref[, .(id_producteur, lon, lat, code_postal_centrale)],
                  by = "id_producteur")

saveRDS(object = data_fdc, file = "./data/eolien/data_eolien_2021.RDS")

data_fdc_dcast <- dcast(data = data_fdc, formula = t ~ id_prod, value.var = "x")
dygraphs::dygraph(data_fdc_dcast) %>% 
  dygraphs::dyLegend(show = "never") %>% 
  dygraphs::dyOptions(colors = "#218BB9")

############################# #

# ---- import function ---- #
lapply(list.files('./R/FDAdapt_R/FDAdapt_R/',
                  recursive = T, full.names = T, pattern = ".R$|.r$"), function(X) {
                    source(X, encoding = 'UTF-8')
                  }) %>% invisible
# ---- import data ---- #
# Import and prepare the data
dt <- readRDS("./clusering_pdc/real_data_analysis/data/PDC_102721_OYONN-063_EA_FACT.RDS")

dt[, date := as.Date(Time)]
date_to_exclude <- dt[, .N, by  = "date"][N < 144, date]
dt <- dt[! date %in% date_to_exclude]
dt[, hour := as.ITime(Time)]
dt <- dt[order(date, hour)]

## Mean every 2h
# dt_aggregate <- dt[, .("CdC" = aggregate(x = as.ts(CdC), ndeltat = 12, FUN = max)), by = "date"]
dt[, .N, by = "date"]

# Weekly curves
dt[, "year_month" := paste0(lubridate::year(date), "_", lubridate::month(date))]
dt[, tind := 1:.N, by = "year_month"]
dt[, t := tind / max(tind)]

ggplot2::ggplot(data = dt, mapping = aes(x = t, y = CdC, group = year_month)) +
  geom_line()

dt_to_use <- copy(dt)

# Plot curves ?
# dt_to_use <- dt[year(date) == 2023]
grph <- ggplot(dt_to_use, aes(x = t, y = CdC, group = year_month)) +
  geom_line(colour = "#154360") +
  labs(x = "t", y = "Puissance (KW)") +
  theme(axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = , r = 10, b = 0, l = 0)),
        axis.text.x =  element_text(size = 14),
        axis.text.y =  element_text(size = 14)) +
  guides(color = guide_legend(override.aes = list(size = 2)))
grph

# Save and clean
ggsave(plot = grph, filename = file.path("./clusering_pdc/real_data_analysis/CdC_OYONN-63_all.png"),
       width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(grph) ; gc()

# Format the data
id_ts <- dt_to_use[, unique(year_month)]
dt_list_ts <- lapply(id_ts, function(id){
  list(
    "t" = dt_to_use[year_month == id, t],
    "x" = dt_to_use[year_month == id, CdC]
  )
})

names(dt_list_ts) <- id_ts
saveRDS(object = dt_list_ts, file = "./clusering_pdc/real_data_analysis/data/data_ts_for_fpca.RDS")

# ---- adaptive fpca ---- #
## For ts case
ts_est_params <- estimate_holder_quantities(
  curves = dt_list_ts, grid_param = seq(0.1, 0.9, len = 20), 
  weighted = TRUE, cv = FALSE, n_sample = 20, separate_curves = TRUE, inflate_bw = FALSE
)
ts_afpca_values <- evalues_adaptive(
  curves = dt_list_ts, grid_bandwidth = lseq(from = 1/48, to = 0.15, length.out = 51),
  grid_smooth = seq(0, 1, len = 100), k0 = 1, nvalues = 5, params = ts_est_params
)

ts_afpca_functions <- efunctions_adaptive(
  curves = dt_list_ts, grid_bandwidth = lseq(from = 1/48, to = 0.15, length.out = 51),
  grid_smooth = seq(0, 1, len = 100), k0 = 1, nfunctions = 5, params = ts_est_params
)

## Compute scores
id_ts_smooth <- names(ts_afpca_functions$centered_curves)
dt_ts_score_afpca <- data.table::rbindlist(lapply(id_ts_smooth, function(id, efunctions){
  mat <- efunctions$eigenfunctions * efunctions$centered_curves[[id]]
  score_data <- apply(X = mat, MARGIN = 2, FUN = function(x){
    pracma::trapz(x = seq(0, 1, len = 100), y = x)
  })
  score_mat <- matrix(data = score_data, nrow = 1)
  score_mat <- as.data.table(score_mat)
  names(score_mat) <- paste0("comp_", seq_len(dim(score_mat)[2]))
  score_mat <- data.table("date" = id, score_mat)
  return(score_mat)
}, efunctions = ts_afpca_functions))

# ---- clustering ---- #

## Direct application of the clustering
source("./clusering_pdc/real_data_analysis/curves_FPCAclustering.R")

nclust <- 10
kmeans_cluster_adapt <- scores_clustering(
  scores = dt_ts_score_afpca[, .SD, .SDcols = ! "date"], 
  id_curves = dt_ts_score_afpca[, date], 
  clustering_method = "kmeans", ncluster = nclust
)
kmeans_cluster_adapt$dt_cluster

## Plot color curve by clustering index
dt_graph_clust <- merge(x = dt_to_use[, .("id_curves" = as.character(year_week), CdC, t)],
                        y = kmeans_cluster_adapt$dt_cluster, by = "id_curves")
dt_graph_clust[, cluster_index := as.factor(cluster_index)]

grph_cluster <- ggplot(dt_graph_clust, aes(x = t, y = CdC, group = id_curves, color = cluster_index)) +
  geom_line() +
  labs(x = "t", y = "Puissance (KW)") +
  theme(axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = , r = 10, b = 0, l = 0)),
        axis.text.x =  element_text(size = 14),
        axis.text.y =  element_text(size = 14)) +
  guides(color = guide_legend(override.aes = list(size = 2)))
grph_cluster
ggsave(plot = grph_cluster, filename = file.path(paste0("./clusering_pdc/real_data_analysis/CdC_OYONN-63_all_nclasses=", nclust, ".png")),
       width = 7, height = 5, units = "in", dpi = 300, bg = "white")
rm(grph_cluster) ; gc()

## Caractérisation de clusters à venir

## Import and merge with vacances scolaires
dt_vacances <- data.table::fread("./clusering_pdc/real_data_analysis/data/weekly_vacations_zoneA_2011_2024.csv")
dt_vacances[, id_curves := paste0(year, "_", week)]
setnames(dt_vacances, old = "indicator", new = "vacances")
dt_vacances[, id_curves := as.character(id_curves)]

dt_clusters <- data.table::merge.data.table(
  x = kmeans_cluster_adapt$dt_cluster,
  y = dt_vacances, 
  by = "id_curves"
)

table(dt_clusters$vacances, dt_clusters$cluster_index)

## Mois
dt_clusters[, mois := month(as.Date(id_curves))]
table(dt_clusters$mois, dt_clusters$cluster_index)

## jour de la semaine
dt_clusters[, day := lubridate::wday(as.Date(id_curves))]
table(dt_clusters$day, dt_clusters$cluster_index)


dt_clusters[]

## Plot des courbes dans les clusters







