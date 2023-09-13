# warping gebcoast

# Choix du paramètre
my_var <- "bottomT"

# profondeurs
gebcoast <- here(
  "data", "tidy", "bathymetrie_gebco_raster", "bathymetry_gebco_raster_150m.tif"
) %>%
  rast()

# données environnementales
s2 <- list.files(
  here("data", "raw", "env", "copernicus", my_var), full.names = T
)[1] %>%
  read_stars()

# redimensionnement profondeurs selon les dimensions spatiales de copernicus
crs(gebcoast) <- "epsg:4326"
s2_gebcoast <- st_as_stars(gebcoast)
r <- s2
r <- r[, , , 1]
r <- st_as_stars(r)
s2_gebcoast_redim <- st_warp(s2_gebcoast, r)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
names(s2_gebcoast_redim) <- "depth"
table(is.na(s2_gebcoast_redim["depth", ,]))

# sauvegarde
write_stars(
  s2_gebcoast_redim,
  here(
    "data",
    "tidy",
    "bathymetrie_gebco_raster",
    "bathymetry_gebco_raster_150m_0.083x0.083.tif"
  )
)

# salinités et bathy_slices
# Choix du paramètre
my_var <- "so"

# données environnementales
s2 <- list.files(
  here("data", "raw", "env", "copernicus", my_var), full.names = T
)[1] %>%
  read_stars()

# tranches de profondeurs
bathy_slices <- list.files(
  here("data", "tidy", "bathymetrie_gebco_raster"),
  pattern = "bathymetry_gebco_raster_.+_-[0-9]+.*tif",
  full.names = T
) %>%
  lapply(rast)
vec_order <- c(6, 5, 3, 4, 1)
bathy_slices <- bathy_slices[vec_order]
names(bathy_slices) <-paste0("lvl", 0:4)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
names(s2_gebcoast_redim) <- "depth"
table(is.na(s2_gebcoast_redim["depth", ,]))

# redimensionnement profondeurs selon les dimensions spatiales de copernicus
# zones côtières
s2_gebcoast_redim <- redimStars(gebcoast, s2)
names(s2_gebcoast_redim) <- "depth"
# tranches
bathy_slices_redim <- lapply(bathy_slices, redimStars, myClim = s2)
bathy_slices_redim2 <- lapply(bathy_slices_redim, \(s) {
  names(s) <- "depth"
  return(s)
})
# masques pour "filtrer par attribut" les climatotologies stars
table(is.na(s2_gebcoast_redim["depth", ,]))
table(is.na(bathy_slices_redim2$lvl0["depth", ,]))[1] %>% as.numeric() +
  table(is.na(bathy_slices_redim2$lvl1["depth", ,]))[1] %>% as.numeric() +
  table(is.na(bathy_slices_redim2$lvl2["depth", ,]))[1] %>% as.numeric() +
  table(is.na(bathy_slices_redim2$lvl3["depth", ,]))[1] %>% as.numeric() +
  table(is.na(bathy_slices_redim2$lvl4["depth", ,]))[1] %>% as.numeric()

# sauvegarde
path_bathy_slices_redim <-  here(
  "data",
  "tidy",
  "bathymetrie_gebco_raster",
  "slices_redim"
)
makeMyDir(path_bathy_slices_redim)
lapply(
  names(bathy_slices_redim2),
  \(n) {
    write_stars(
      bathy_slices_redim2[[n]],
      here(
        path_bathy_slices_redim,
        paste("bathymetry", "redim", n, "0.083x0.083") %>% paste0(".tif")
      )
    )
  }
)
