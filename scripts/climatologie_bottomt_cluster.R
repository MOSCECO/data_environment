# Découpage des netcdf et réalisation d'une climatologie moyenne
# Tentative sur le cluster
t0 <- Sys.time()

options("rgdal_show_exportToProj4_warnings"="none")

source(here::here("scripts", "boot_cluster.R"))

print("Chargement des librairies réussie")

# profondeurs
gebcoast <- here(
  "data", "raw", "bathymetrie_gebco_raster", "bathymetry_gebco_raster_150m.tif"
) %>%
  rast()

# Import des données environnementales
s2_bottomt <- list.files(
  here("data", "raw", "env", "copernicus", "bottomT"), full.names = T
)

s2_bottomt <- s2_bottomt %>%
  lapply(read_ncdf, proxy = FALSE)

# redimensionnement profondeurs selon les dimensions spatiales de copernicus
crs(gebcoast) <- "epsg:4326"
s2_gebcoast <- st_as_stars(gebcoast)
raster_bottomt <- s2_bottomt[[1]]
raster_bottomt <- raster_bottomt[, , , 1]
raster_bottomt <- st_as_stars(raster_bottomt)
raster_bottomt <- split(raster_bottomt, 3)
s2_gebcoast_redim <- st_warp(s2_gebcoast, raster_bottomt)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
names(s2_gebcoast_redim) <- "depth"
table(is.na(s2_gebcoast_redim["depth", ,]))

# traduction en stars
s2_bottomt <- do.call(c, s2_bottomt)

# Découpage selon les bathymétries d'intérêt
gc(verbose = F)
s2_bottomt_mask <- s2_bottomt
print("Conversion en stars")
s2_bottomt_mask <- st_as_stars(s2_bottomt_mask)
print("Conversion en stars réussie")
print(s2_bottomt_mask)
print("Découpe des profondeurs")
s2_bottomt_mask[is.na(s2_gebcoast_redim)] <- NA
print("Découpe des profondeurs réussie")

print(s2_bottomt_mask)

# climatologies ----
# moyenne
print("Climatologie moyenne")
s2_bottomt_mask_mean <- st_apply(
  s2_bottomt_mask, c("longitude", "latitude"), \(x) mean(x, na.rm = T)
)
s2_bottomt_mask_mean <- st_set_crs(s2_bottomt_mask_mean, "EPSG:4326")
s2_bottomt_mask_mean <- setNames(s2_bottomt_mask_mean, "mean.bottomt")
print("Climatologie moyenne réussie")

# sauvegarde
write_stars(
  s2_bottomt_mask_mean,
  dsn = here("data", "tidy", "climatology_global", "bottomt_mean.tif")
)

tf <- Sys.time()
dt <- (tf - t0)
print(dt)
