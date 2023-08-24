# Raster hybride des climatologies
# importation première climatologie
s2_bottomt <- list.files(
  here("data", "raw", "env", "copernicus", "bottomT"), full.names = T
) %>% 
  read_stars(along = 3)

# tranches de profondeurs
bathy_slices <- list.files(
  here("data", "tidy"), 
  pattern = "bathymetry_gebco_raster_*.*tif", 
  full.names = T
) %>% lapply(rast)
bathy_slices <- bathy_slices[c(5, 4, 2, 3, 1)]
names(bathy_slices) <- paste0("lvl", 0:4)
# redimensionnement profondeurs selon les dimensions spatiales de copernicus
# zones côtières
redimStars <- function(myRaster, myClim) {
  crs(myRaster) <- "epsg:4326"
  s2            <- st_as_stars(myRaster)
  raster_clim   <- myClim
  raster_clim   <- raster_clim[, , , 1]
  raster_clim   <- split(raster_clim, 3)
  s2_redim      <- st_warp(s2, raster_clim)
  return(s2_redim)
}
s2_gebcoast_redim <- redimStars(gebcoast, s2_bottomt)
# tranches
bathy_slices_redim <- lapply(bathy_slices, redimStars, myClim = s2_bottomt)

# économie de mémoire
rm(s2_bottomt)

# masques pour "filtrer par attribut" les climatotologies stars
table(is.na(s2_gebcoast_redim["depth", ,]))
names(bathy_slices_redim$lvl0) <- "depth"
names(bathy_slices_redim$lvl1) <- "depth"
names(bathy_slices_redim$lvl2) <- "depth"
names(bathy_slices_redim$lvl3) <- "depth"
names(bathy_slices_redim$lvl4) <- "depth"
table(is.na(bathy_slices_redim$lvl0["depth", ,]))[1] %>% as.numeric() +
table(is.na(bathy_slices_redim$lvl1["depth", ,]))[1] %>% as.numeric() +
table(is.na(bathy_slices_redim$lvl2["depth", ,]))[1] %>% as.numeric() +
table(is.na(bathy_slices_redim$lvl3["depth", ,]))[1] %>% as.numeric() +
table(is.na(bathy_slices_redim$lvl4["depth", ,]))[1] %>% as.numeric()

# chemin de sauvegarde
path_clim <- here("data", "tidy", "climatologies_stars")
makeMyDir(path_clim)
path_climso <- here(path_clim, "so")
makeMyDir(path_climso)

# climatologies de salinité
clim_sos <- list.files(path_climso, full.names = T, pattern = ".rds")[c(1, 5, 3, 4, 2)] %>% 
  lapply(readRDS)
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