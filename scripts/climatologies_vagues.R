# climatologies vagues

# importation première climatologie
s2_bottomt <- list.files(
  here("data", "raw", "env", "copernicus", "bottomT"), full.names = T
) %>% 
  read_stars(along = 3)

# importations stars
s2_waves <- list.files(
  here("data", "raw", "env", "copernicus", "waves"),
  full.names = T
)[1:2] %>% read_stars(proxy = FALSE)

# réduction aux valeurs côtière pour l'espèce considérée
# redimensionnement profondeurs selon les dimensions spatiales de copernicus
s2_gebcoast_redim <- redimStars(gebcoast, s2_bottomt)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
table(is.na(s2_gebcoast_redim["depth", ,]))
s2_waves_mask <- s2_waves
s2_waves_mask[is.na(s2_gebcoast_redim)] <- NA
x11() ; plot(s2_waves_mask[, , , 1])

# climatologies ----
# moyenne
s2_waves_mask_mean <- st_apply(
  s2_waves_mask, c("x", "y"), \(x) mean(x, na.rm = T)
)
s2_waves_mask_mean <- st_set_crs(s2_waves_mask_mean, "EPSG:4326")
s2_waves_mask_mean <- setNames(
  s2_waves_mask_mean, 
  paste0("mean.", c("VHM0_WW", "VHM0_SW1", "VHM0"))
)

# écart-type
s2_waves_mask_stdv <- st_apply(
  s2_waves_mask, c("x", "y"), \(x) sd(x, na.rm = T)
)
s2_waves_mask_stdv <- st_set_crs(s2_waves_mask_stdv, "EPSG:4326")
s2_waves_mask_stdv <- setNames(
  s2_waves_mask_stdv, 
  paste0("stdv.", c("VHM0_WW", "VHM0_SW1", "VHM0"))
)

# maximum
s2_waves_mask_maxi <- st_apply(s2_waves_mask, c("x", "y"), max)
s2_waves_mask_maxi <- st_set_crs(s2_waves_mask_maxi, "EPSG:4326")
s2_waves_mask_maxi <- setNames(
  s2_waves_mask_maxi, 
  paste0("maxi.", c("VHM0_WW", "VHM0_SW1", "VHM0"))
)

# minimum
s2_waves_mask_mini <- st_apply(s2_waves_mask, c("x", "y"), min)
s2_waves_mask_mini <- st_set_crs(s2_waves_mask_mini, "EPSG:4326")
s2_waves_mask_mini <- setNames(
  s2_waves_mask_maxi, 
  paste0("mini.", c("VHM0_WW", "VHM0_SW1", "VHM0"))
)

# aggrégation des climatologies

s2_waves_mask_clim <- c(
  s2_waves_mask_mean, 
  s2_waves_mask_stdv, 
  s2_waves_mask_maxi, 
  s2_waves_mask_mini
)

# chemin de sauvegarde
path_clim <- here("data", "tidy", "climatologies_stars")
makeMyDir(path_clim)
path_climwa <- here(path_clim, "waves")
makeMyDir(path_climwa)

saveRDS(s2_waves_mask_clim, here(path_climwa, "waves_clims.rds"))
saveRDS(s2_waves_mask_clim, here("data", "tidy", "clim", "waves_clims.rds"))