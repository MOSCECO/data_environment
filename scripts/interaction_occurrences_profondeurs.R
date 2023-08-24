# Interaction données d'occurrences variables environnementales ----
# environmental variable
database <- "copernicus"
that_var <- "so"

# occurrence
superfamily <- "Muricoidea"
species     <- "Claremontiella_nodulosa"
O <- occ[[superfamily]][[species]]

# profondeurs
gebco <- rast(
  here(
    "data", "raw", "gebco",
    "GEBCO_17_Apr_2023_9ec486961797",
    "gebco_2022_n28.87_s-23.08_w-112.033_e-40.6667.tif"
  )
)

# suppression des valeurs d'altitude ----
# gebco_bathy_df <- as.data.frame(gebco, xy = T) %>%
#   as.data.table()
# names(gebco_bathy_df)[3] <- "depth"
# gebco_bathy_df$depth[gebco_bathy_df$depth > 0] <- NA
# gebco_bathy <- rast(gebco_bathy_df)
# writeRaster(
#   gebco_bathy,
#   here("data", "raw", "gebco", "gebco_bathymetry.tif")
# )
# rm(gebco_bathy_df)
# x11() ; plot(gebco_bathy)
gebco_bathy <- rast(here("data", "raw", "gebco", "gebco_bathymetry.tif"))

# extraction des profondeurs des occurrences ----
O$depth <- terra::extract(gebco, O, ID = F)[[1]]
# on regarde les occurrences sur des altitudes (et pas des profondeurs)
# x11() ; hist(O$depth[O$depth >= 0], breaks = 100)
# on fixe une incertitude de 10m
bathy_uncertainty <- 10
O <- O %>% filter(depth <= bathy_uncertainty)
dim(O) # 362  23
# mise à zéro des altitudes dans le jeu de données restant
O$depth[O$depth > 0] <- 0
# on détermine la profondeur du quantile 95% qui servira de filtre
# 1) aux occurrences
# 2) à l'intervalle bathymétrique retenu pour la génération de climatologies
bathy_threshold <- quantile(-na.omit(O$depth), 0.95) # 576

# sélection de l'intervalle bathymétrique dans le stars
debco <- as.data.frame(gebco, xy = T)
names(debco)[3] <- "depth"

# filtre pour ne garder que des valeurs côtières
debcoast <- debco %>%
  filter(depth > -bathy_threshold & depth <= bathy_uncertainty)
debcoast$depth[debcoast$depth > 0] <- 0
gebcoast <- rast(debcoast)
# x11() ; plot(gebcoast)

# filtre selon les profondeurs pour les climatologies de salinité
seuils <- list(
  c(bathy_uncertainty, -9.573),
  c(-9.573, -25.2114),
  c(-25.2114, -77.8539),
  c(-77.8539, -130.666),
  c(-130.666, -bathy_threshold)
)
names(seuils) <- paste0("lvl", 0:4)

bathy_slices <- lapply(
  seuils,
  \(bnd) {
    tbout <- debco %>%
      filter(depth <= bnd[[1]] & depth > bnd[[2]])
    tbout$depth[tbout$depth > 0] <- 0
    rout <- rast(tbout)
    return(rout)
  }
)
names(bathy_slices) <- paste0("lvl", 0:4)
# bathy_slices$lvl0 %>% lapply(\(x) {x11() ; plot(x)})

O$depth2 <- terra::extract(gebcoast, O, ID = F)[[1]]
table(is.na(O$depth2)) #   342    20

# sauvegarde
saveRDS(
  gebcoast,
  here(
    "data",
    "tidy",
    paste(
      "bathymetry", "gebco", "raster",
      bathy_threshold %>% paste0("m"),
      tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".rds")
  )
)
saveRDS(
  bathy_slices,
  here(
    "data",
    "tidy",
    paste(
      "bathymetry", "gebco", "raster", "slices",
      bathy_threshold %>% paste0("m"),
      tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".rds")
  )
)
writeRaster(
  gebcoast,
  here(
    "data",
    "tidy",
    paste(
      "bathymetry", "gebco", "raster",
      bathy_threshold %>% paste0("m"),
      tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".tif")
  )
)
lapply(
  names(bathy_slices),
  \(nr) {
    writeRaster(
      bathy_slices[[nr]],
      here(
        "data",
        "tidy",
        paste(
          "bathymetry", "gebco", "raster",
          paste(seuils[[nr]], collapse = "_") %>% paste0("m"),
          tolower(superfamily), tolower(species),
          sep = "_") %>%
          paste0(".tif")
      )
    )
  }
)
saveRDS(
  O,
  here(
    "data", "tidy",
    paste(
      "occurrences", "with", "depths",
      bathy_threshold %>% paste0("m"),
      tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".rds")
  )
)
