# Raster hybride des climatologies de salinité
# tranches de profondeurs redimensionnées
bathy_slices_redim <- lapply(
  list.files(
    here("data", "tidy", "bathymetrie_gebco_raster", "slices_redim"),
    full.names = T
  ),
  read_stars
)
names(bathy_slices_redim) <- paste0("lvl", 0:4)

# chemin de sauvegarde
path_climso <- here("data", "analysis", "climatologies_global", "so")
makeMyDir(path_climso)

# climatologies de salinité
clim_sos <- list.files(
  path_climso, full.names = T, pattern = ".tif"
)[c(1, 5, 3, 4, 2)] %>%
  lapply(read_stars)
names(clim_sos)   <- paste0("lvl", 0:4)

clim_sos_spatrast <- lapply(clim_sos, rast)
lapply(clim_sos_spatrast, \(x) {x11(); plot(x[["mean.so_lyr.1"]])})
clim_so_spatrast  <- terra::mosaic(
  clim_sos_spatrast$lvl0,
  clim_sos_spatrast$lvl1,
  clim_sos_spatrast$lvl2,
  clim_sos_spatrast$lvl3,
  clim_sos_spatrast$lvl4
)
# x11() ; plot(clim_so_spatrast)
table(is.na(values(clim_so_spatrast$mean.so_lyr.1)))
table(is.na(values(clim_bottomt$mean.bottomt_lyr.1)))
writeRaster(
  clim_so_spatrast,
  here(path_climso, paste("climatology", "so", "hybride", sep = "_") %>% paste0(".tif"))
)
