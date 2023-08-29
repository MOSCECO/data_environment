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
bathy_threshold <- lapply(
  Os,
  \(tb) {
    u <- tb %>%
      st_drop_geometry() %>%
      select(depth) %>%
      unlist(use.names = F)
    quantile(-u, 0.75)  # Seuil arbitraire pour être assez stringent pour ne pas
    # aller échantillonner des pseudo-absences complètement en dehors de la
    # niche de l'espèce
  }
)
# Bathymetric threshold
BT <- ceiling(summary(sort(unlist(bathy_threshold)))[5]/10)*10

bathy_threshold_rounded <- lapply(bathy_threshold, \(x) ceiling(x/10)*10)

# Espèces "profondes" (à mettre en lien avec les listes d'espèces "côtières"
# et "profondes" de data_biologic)
Op <- Os[which(unlist(bathy_threshold, use.names = F) > 500)]
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

# Écriture d'un seul raster de profondeurs pour les climatologies
# à partir du troisième quartile des profondeurs à 75% de toutes les espèces
# Dossier de sortie
# pb <- here("data", "tidy", "bathymetrie_gebco_raster")
# makeMyDir(pb)
#
# file_name <- paste(
#   "bathymetry", "gebco", "raster", BT %>% paste0("m"), sep = "_"
# ) %>%
#   paste0(".tif")
# bool <- !file.exists(here(pb, file_name))
#
# if (bool) {
#
#   # Nettoyage de la mémoire vive
#   gc()
#
#   # sélection de l'intervalle bathymétrique
#   gebco_s2[
#     gebco_s2["depth", , ] < -BT | gebco_s2["depth", , ] > 0
#   ] <- NA
#
#   # Sauvegarde
#   write_stars(gebco_s2, here(pb, file_name))
#
# }

# Nouvelle importation
gebco_rast <- here(
  "data", "tidy", "bathymetrie_gebco_raster",
  "bathymetry_gebco_raster_200m.tif"
) %>% rast()

# filtre selon les profondeurs pour les climatologies de salinité
seuils <- list(
  c(0, -9.573),
  c(-9.573, -25.2114),
  c(-25.2114, -77.8539),
  c(-77.8539, -130.666),
  c(-130.666, -BT)
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

# sauvegarde
saveRDS(
  bathy_slices,
  here(
    "data",
    "tidy",
    "bathymetrie_gebco_raster",
    paste(
      "bathymetry", "gebco", "raster", "slices",
      BT %>% paste0("m"),
      # tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".rds")
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
        "bathymetrie_gebco_raster",
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
