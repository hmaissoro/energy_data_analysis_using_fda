library(data.table)
library(magrittr)
library(ggplot2)
library(latex2exp)

# Import data ----
path_data <- "/home/projets/2020-A258-ENEDIS_Prev/Eolien/"
files <- c(
  "data_final_2016_2017.RDS",
  "data_final_2017_2018.RDS",
  "data_final_2019_2020_new.RDS",
  "data_final_2020_2021.RDS")

data_eole <- readRDS(paste0(path_data, files[4]))
dt_raw <- data_eole$data
rm(data_eole) ; gc()

# Number of prod
dt_raw[, id_producteur := bit64::as.character.integer64(id_producteur)]
id_prod <- dt_raw[, unique(id_producteur)]
length(id_prod)

# Focus on four productor
dt_raw[, date := as.Date(horodate)]

## We consider the id_prods with the lower NA values for ease of application.
dt_num_na <- dt_raw[, .("number_na" = sum(is.na(valeur))), by = c("id_producteur", "date")]
dt_num_na <- dt_num_na[number_na == 0, .N, by = "id_producteur"][N == max(N)]
id_prod_kept <- dt_num_na[, id_producteur]
dt_raw <- dt_raw[id_producteur %in% id_prod_kept]

# Plot and save some curves ----
id_prod_graph <- sample(id_prod_kept, 1)

## Independent case: 1 id_producteur = 1 curve
grph <- ggplot(data = dt_raw[id_producteur %in% id_prod_graph],
               mapping = aes(x = horodate, y = FC, group = id_producteur, color = id_producteur)) +
  geom_line(colour = "#154360") +
  xlab("Timestamp") +
  ylab("Load Factor") +
  ylim(0, 1.1) +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = , r = 10, b = 0, l = 0)),
        axis.text.x =  element_text(size = 14),
        axis.text.y =  element_text(size = 14)) +
  guides(color = guide_legend(override.aes = list(size = 2)))
grph

# Save and clean
ggsave(plot = grph, filename = file.path(paste0("./PrevProd_Eol/graphs/eol_iid_one_prod=", id_prod_graph, ".png")),
       width = 7 * 1.5, height = 5, units = "in", dpi = 300, bg = "white")
rm(grph) ; gc()


## FTS case 
## The date is the curve identifier
## Scale the intraday observation time to [0,1].
## Take the curve value FC
dt_one_prod <- dt_raw[id_producteur == id_prod_graph]
n_obs <- dt_one_prod[, .N, by = "date"][, max(N)]
dt_one_prod[, N := .N, by = "date"]
dt_one_prod <- dt_one_prod[N == n_obs]

## Extract data from April to August 2021
dt_one_prod <- dt_one_prod[(year(date)== 2020 & month(date) %in% c(11, 12)) |
                             (year(date)== 2021 & month(date) %in% c(1, 2, 3))]
dt_one_prod[, tobs := (1:n_obs) / n_obs, by = "date"]

## Format and save
dt_id_curve <- data.table::data.table(
  "date" = dt_one_prod[, sort(unique(date))],
  "id_curve" = 1:dt_one_prod[, length(unique(date))]
)
dt_fts <- data.table::merge.data.table(x = dt_one_prod, y = dt_id_curve, by = "date")
dt_fts <- dt_fts[, .(id_curve, tobs, "X" = FC)]

saveRDS(object = dt_fts, file = paste0("./PrevProd_Eol/data/dt_fts_eol_id_prod=", id_prod_graph, ".RDS"))

## Plot FTS
grph_fts <- ggplot(data = dt_fts[, .(id_curve, tobs, X)],
                   mapping = aes(x = tobs, y = X, group = id_curve)) +
  geom_line(colour = "#154360") +
  xlab("t") +
  ylab("Load Factor") +
  ylim(0, 1.1) +
  theme_minimal() + 
  theme(legend.position = "right",
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = , r = 10, b = 0, l = 0)),
        axis.text.x =  element_text(size = 14),
        axis.text.y =  element_text(size = 14)) +
  guides(color = guide_legend(override.aes = list(size = 2)))
grph_fts

# Save and clean
ggsave(plot = grph_fts, filename = file.path(paste0("./PrevProd_Eol/graphs/eol_fts_one_prod=", id_prod_graph, ".png")),
       width = 7 * 1.5, height = 5, units = "in", dpi = 300, bg = "white")
rm(grph_fts) ; gc()

# The eolien position map ----
library(raster)
library(sf)

data_ref <- data.table::fread("/home/projets/2020-A258-ENEDIS_Prev/Eolien/ref_producteur_2022.csv")
data_ref[, id_producteur := bit64::as.character.integer64(id_producteur)]
data_ref <- data_ref[filiere == "Eolien"]

adm_fr <- readRDS("./data/gadm36_FRA_1_sp.rds")

png(filename = "./graphs/eolien/maps_eolien_location.png",
    res = 300,
    width = 7 * 1.5, height = 5, units = "in", bg = "white")
par(mar = c(0, 0, 0, 0), bg = "white")
plot(adm_fr)
points(data_ref[, lon], data_ref[, lat], 
       col = "#1B4F72", pch = 5, cex = 0.6)
dev.off()
