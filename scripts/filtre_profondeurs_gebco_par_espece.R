# occurrence
superfamily <- "Majoidea"
species     <- "Mithraculus_forceps"
O <- occ[[superfamily]][[species]]

# Création d'un spatial raster par espèce pour les climatologies
# Dossier de sortie
pb <- here("data", "tidy", "bathymetrie_gebco_raster")
makeMyDir(pb)

# Nettoyage mémoire vive
rm(gebco_bathy)
# Boucle
lapply(
  sort(unlist(bathy_threshold_rounded), decreasing = T)[
    sort(unlist(bathy_threshold_rounded), decreasing = T) > 0
  ],
  \(btr) {

    file_name <- paste(
        "bathymetry", "gebco", "raster", btr %>% paste0("m"), sep = "_"
      ) %>%
        paste0(".tif")
    bool <- !file.exists(here(pb, file_name))

    if (bool) {

      # Nettoyage de la mémoire vive
      gc()

      # sélection de l'intervalle bathymétrique
      gebco_s2[
        gebco_s2["depth", , ] < -btr | gebco_s2["depth", , ] > 0
      ] <- NA

      # Sauvegarde
      write_stars(gebco_s2, here(pb, file_name))

    }

  }
)

# gebco_rast <- here(
#   "data", "tidy", paste(
#     "bathymetry", "gebco", "raster",
#     bathy_threshold %>% paste0("m"),
#     sep = "_"
#   ) %>%
#     paste0(".tif")
# ) %>% rast()
#
# # filtre selon les profondeurs pour les climatologies de salinité
# seuils <- list(
#   c(0, -9.573),
#   c(-9.573, -25.2114),
#   c(-25.2114, -77.8539),
#   c(-77.8539, -130.666),
#   c(-130.666, -bathy_threshold)
# )
# names(seuils) <- paste0("lvl", 0:4)
#
# bathy_slices <- lapply(
#   seuils,
#   \(bnd) {
#     # bnd <- seuils$lvl0
#     rout <- ifel(
#       gebco_rast <= bnd[[1]] & gebco_rast > bnd[[2]], 1, NA
#     )
#     return(rout)
#   }
# )
# names(bathy_slices) <- paste0("lvl", 0:4)
# # bathy_slices$lvl0 %>% lapply(\(x) {x11() ; plot(x)})
#
# O$depth2 <- terra::extract(gebcoast, O, ID = F)[[1]]
# table(is.na(O$depth2)) #   342    20
#
# # sauvegarde
# saveRDS(
#   gebcoast,
#   here(
#     "data",
#     "tidy",
#     paste(
#       "bathymetry", "gebco", "raster",
#       bathy_threshold %>% paste0("m"),
#       tolower(superfamily), tolower(species),
#       sep = "_") %>%
#       paste0(".rds")
#   )
# )
# saveRDS(
#   bathy_slices,
#   here(
#     "data",
#     "tidy",
#     paste(
#       "bathymetry", "gebco", "raster", "slices",
#       bathy_threshold %>% paste0("m"),
#       # tolower(superfamily), tolower(species),
#       sep = "_") %>%
#       paste0(".rds")
#   )
# )
# writeRaster(
#   gebcoast,
#   here(
#     "data",
#     "tidy",
#     paste(
#       "bathymetry", "gebco", "raster",
#       bathy_threshold %>% paste0("m"),
#       tolower(superfamily), tolower(species),
#       sep = "_") %>%
#       paste0(".tif")
#   )
# )
# lapply(
#   names(bathy_slices),
#   \(nr) {
#     writeRaster(
#       bathy_slices[[nr]],
#       here(
#         "data",
#         "tidy",
#         paste(
#           "bathymetry", "gebco", "raster",
#           paste(seuils[[nr]], collapse = "_") %>% paste0("m"),
#           # tolower(superfamily), tolower(species),
#           sep = "_") %>%
#           paste0(".tif")
#       )
#     )
#   }
# )
# saveRDS(
#   O,
#   here(
#     "data", "tidy",
#     paste(
#       "occurrences", "with", "depths",
#       bathy_threshold %>% paste0("m"),
#       # tolower(superfamily), tolower(species),
#       sep = "_") %>%
#       paste0(".rds")
#   )
# )
