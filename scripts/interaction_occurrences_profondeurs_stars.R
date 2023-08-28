# Interaction données d'occurrences variables environnementales ----
# environmental variable
database <- "copernicus"
that_var <- "so"

# aggrégation de toutes les occurrences
O <- do.call(rbind, lapply(occ, \(l) do.call(rbind, l)))
bb <- c(xmin = -125, ymin = -60, ymax = 60, xmax = 65) %>% st_bbox()
st_crs(bb) <- st_crs(O)
O2 <- sf::st_crop(O, bb)
O <- O[-which(O$occurrenceID %in% O2$occurrenceID), ]


# profondeurs
gebc0 <- read_stars(
  here("data", "raw", "gebco", "gebco_2023_n60.0_s-60.0_w-140.0_e-20.0.tif")
)
gebc1 <- read_stars(
  here("data", "raw", "gebco", "gebco_2023_n60.0_s-60.0_w-20.0_e65.0.tif")
)
gebco <- stars::st_mosaic(gebc0, gebc1)
gebco_s2 <- st_as_stars(gebco)
names(gebco_s2) <- "depth"

# suppression des valeurs d'altitude ----
# gebco_bathy <- gebco_s2
# gebco_bathy[gebco_bathy["depth", , ] > 0] <- 0
# saveRDS(
#   gebco_bathy,
#   here::here("data", "raw", "gebco", "gebco_bathymetry.rds")
# )
gebco_bathy <- readRDS(here("data", "raw", "gebco", "gebco_bathymetry.rds"))

# extraction des profondeurs des occurrences ----
O$depth <- st_extract(gebco_bathy, O) %>%
  st_drop_geometry()
# Études des profondeurs pour toutes les occurrences
quantile(O[["depth"]] %>% unlist(use.names = F), 0.1)
# et selon les espèces
Os <- split(O, f = O$scientificName)
res <- lapply(
  Os,
  \(tb) {
    u <- tb %>%
      st_drop_geometry() %>%
      select(depth) %>%
      unlist(use.names = F)
    quantile(u, 0.1)
  }
)
sort(unlist(res))
Op <- Os[which(unlist(res, use.names = F) < -500)]
Op %>% lapply(dim)
Op %>% lapply(\(tb) {
  u <- tb %>%
    st_drop_geometry() %>%
    select(depth) %>%
    unlist(use.names = F)
  summary(u)
})
# Toutes les moyennes des profondeurs pour les espèces qui ont les quantiles 10%
# les plus profonds sont aux alentours de quelques centaines de m
# Coralliophila aedonia est particulièrement

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
bathy_threshold <- quantile(-unlist(na.omit(O$depth)), 0.95)
# bathy_threshold <- 576 # Claremontiella nodulosa
bathy_threshold <- 615 # toutes les espèces côtières

# sélection de l'intervalle bathymétrique dans le stars
# rm(gebco_bathy)
# gebco_s2[
#   gebco_s2["depth", , ] < -bathy_threshold | gebco_s2["depth", , ] > 0
# ] <- NA
#
# write_stars(
#   gebco_s2,
#   paste(
#     "bathymetry", "gebco", "raster",
#     bathy_threshold %>% paste0("m"),
#     sep = "_"
#   ) %>%
#     paste0(".tif")
# )

gebco_rast <- here(
  "data", "tidy", paste(
    "bathymetry", "gebco", "raster",
    bathy_threshold %>% paste0("m"),
    sep = "_"
  ) %>%
    paste0(".tif")
) %>% rast()

# filtre selon les profondeurs pour les climatologies de salinité
seuils <- list(
  c(0, -9.573),
  c(-9.573, -25.2114),
  c(-25.2114, -77.8539),
  c(-77.8539, -130.666),
  c(-130.666, -bathy_threshold)
)
names(seuils) <- paste0("lvl", 0:4)

bathy_slices <- lapply(
  seuils,
  \(bnd) {
    # bnd <- seuils$lvl0
    rout <- ifel(
      gebco_rast <= bnd[[1]] & gebco_rast > bnd[[2]], 1, NA
    )
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
      # tolower(superfamily), tolower(species),
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
          # tolower(superfamily), tolower(species),
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
      # tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".rds")
  )
)
