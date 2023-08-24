redimStars <- function(myRaster, myClim) {
  crs(myRaster) <- "epsg:4326"
  s2            <- st_as_stars(myRaster)
  raster_clim   <- myClim
  raster_clim   <- raster_clim[, , , 1]
  raster_clim   <- split(raster_clim, 3)
  s2_redim      <- st_warp(s2, raster_clim)
  return(s2_redim)
}