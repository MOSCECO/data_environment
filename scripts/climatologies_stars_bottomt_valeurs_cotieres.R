# environmental variable
database <- "copernicus"
that_var <- "so"

# occurrence
superfamily <- "Muricoidea"
species     <- "Claremontiella_nodulosa" 
O <- occ[[superfamily]][[species]]

# profondeurs
seuils <- c(9.573, 25.2114, 77.8539, 130.666)
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

O$depth2 <- terra::extract(gebcoast, O, ID = F)[[1]]
table(is.na(O$depth2)) #   342    20 

# Import des données environnementales en stars (bottomt)
s2_bottomt <- list.files(
  here("data", "raw", "env", "copernicus", "bottomT"), full.names = T
)[c(1:2)] %>% 
  read_stars(along = 3)

s2_waves <- list.files(
  here("data", "raw", "env", "copernicus", "waves"), full.names = T
)[c(2:3)] %>% # problème avec le premier fichier
  read_stars(along = 3, proxy = F)

s2_so_000 <- list.files(
  here("data", "raw", "env", "copernicus", "so"), 
  pattern = "0.494025", 
  full.names = T
) %>% 
  read_stars(along = 4)

s2_so_009 <- list.files(
  here("data", "raw", "env", "copernicus", "so"), 
  pattern = "9.573",
  full.names = T
) %>% 
  read_stars(along = 4)

s2_so_025 <- list.files(
  here("data", "raw", "env", "copernicus", "so"), 
  pattern = "25.2114",
  full.names = T
) %>% 
  read_stars(along = 4)

s2_so_077 <- list.files(
  here("data", "raw", "env", "copernicus", "so"), 
  pattern = "77.8539",
  full.names = T
) %>% 
  read_stars(along = 4)

s2_so_130 <- list.files(
  here("data", "raw", "env", "copernicus", "so"), 
  pattern = "130.666",
  full.names = T
) %>% 
  read_stars(along = 4)

# redimensionnement profondeurs selon les dimensions spatiales de copernicus
crs(gebcoast) <- "epsg:4326"
s2_gebcoast <- st_as_stars(gebcoast)
raster_bottomt <- s2_bottomt
raster_bottomt <- raster_bottomt[, , , 1]
raster_bottomt <- split(raster_bottomt, 3)
s2_gebcoast_redim <- st_warp(s2_gebcoast, raster_bottomt)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
table(is.na(s2_gebcoast_redim["depth", ,]))
s2_bottomt_mask <- s2_bottomt
s2_bottomt_mask[is.na(s2_gebcoast_redim)] <- NA
x11() ; plot(s2_bottomt_mask[, , , 1])

# climatologies ----
# moyenne
s2_bottomt_mask_mean <- st_apply(
  s2_bottomt_mask, c("x", "y"), \(x) mean(x, na.rm = T)
)
s2_bottomt_mask_mean <- st_set_crs(s2_bottomt_mask_mean, "EPSG:4326")
s2_bottomt_mask_mean <- setNames(s2_bottomt_mask_mean, "mean.bottomt")

# écart-type
s2_bottomt_mask_stdv <- st_apply(
  s2_bottomt_mask, c("x", "y"), \(x) sd(x, na.rm = T)
)
s2_bottomt_mask_stdv <- st_set_crs(s2_bottomt_mask_stdv, "EPSG:4326")
s2_bottomt_mask_stdv <- setNames(s2_bottomt_mask_stdv, "stdv.bottomt")

# maximum
s2_bottomt_mask_maxi <- st_apply(s2_bottomt_mask, c("x", "y"), max)
s2_bottomt_mask_maxi <- st_set_crs(s2_bottomt_mask_maxi, "EPSG:4326")
s2_bottomt_mask_maxi <- setNames(s2_bottomt_mask_maxi, "maxi.bottomt")

# S2 <- s2_bottomt_mask[, , ,]
# ggplot() + geom_stars(data = S2)
# S2_clim <- st_apply(S2, c("x", "y"), range)
# S2_climin <- st_apply(S2, c("x", "y"), min)
# S2_climin2 <- st_apply(s2_bottomt_mask, c("x", "y"), min, na.rm = T)
# S2_climax <- st_apply(S2, c("x", "y"), max)
# ggplot() + geom_stars(data = S2_clim)

# minimum
s2_bottomt_mask_mini <- st_apply(s2_bottomt_mask, c("x", "y"), min)
s2_bottomt_mask_mini <- st_set_crs(s2_bottomt_mask_mini, "EPSG:4326")
s2_bottomt_mask_mini <- setNames(s2_bottomt_mask_mini, "mini.bottomt")

# aggrégation des climatologies
s2_bottomt_mask_clim <- c(
  s2_bottomt_mask_mean, 
  s2_bottomt_mask_stdv, 
  s2_bottomt_mask_maxi, 
  s2_bottomt_mask_mini
)

# extraction
O_bottomt <- st_extract(s2_bottomt_mask_clim, O)
apply(O_bottomt, 2, \(x) table(is.na(x)))
# tampon et conversion en raster pour les points sans valeurs
my_buffer <- 5000
O_proj <- st_transform(
  O[is.na(O_bottomt %>% st_drop_geometry() %>% select(1)), ], "EPSG:31970"
)
O_buffer <- st_buffer(O_proj, dist = my_buffer)
O_buffer_4326 <- st_transform(O_buffer, "EPSG:4326")
# ggplot() +
#   geom_stars(data = s2_bottomt_mask_mean %>%
#                st_crop(st_bbox(O_buffer_4326[1, ]))) +
#   geom_sf(data = O_buffer_4326[1, ])

# Nombre de cellules du raster intersectant le point avec tampon
table(
  st_intersects(O_buffer_4326, s2_bottomt_mask_clim %>% st_as_sf()) %>% 
    lengths() == 0
)
# traduction de l'intersection en numéro de cellule
z <- st_intersects(O_buffer_4326, s2_bottomt_mask_clim %>% st_as_sf())
z.chara <- z %>% 
  as.character() %>% 
  ifelse(. == "integer(0)", NA, .)
z1 <- z.chara[grepl(":", z.chara)] %>% 
  str_split(":") %>%
  lapply(\(x) as.numeric(x[1]):as.numeric(x[2]))
z2 <- gsub("[^0-9,-]", "", z.chara[grepl(",", z.chara)]) %>% 
  str_split(., ",") %>% 
  lapply(as.numeric)
zf <- z.chara %>% as.list()
zf[grepl(":", z.chara)] <- z1
zf[grepl(",", z.chara)] <- z2
zf <- zf %>% lapply(as.numeric)

# on prend la moyenne de toutes les cellules intersectées pour l'attribuer
# au point
sf <- s2_bottomt_mask_clim %>% st_as_sf()
values_extract_buffer <- do.call(
  rbind, 
  lapply(
    zf, 
    \(xy) {
      sf[xy, ] %>% st_drop_geometry() %>% apply(2, mean)
    }
  )
) %>% as_tibble()

# attribution des valeurs aux occurrences sans valeur
O_bottomt_final <- do.call(
  cbind, 
  lapply(
    names(O_bottomt %>% st_drop_geometry()),
    \(n) {
      O_bottomt[[n]][is.na(O_bottomt[[n]])] <- values_extract_buffer[[n]]
      return(O_bottomt[[n]])
    }
  )
) %>% st_sf(geometry = O_bottomt$geometry)
# d <- cbind(O_bottomt$mean.bottomt, a$`1`, (O_bottomt$mean.bottomt == a$`1`) %>% 
#              as.character())
path_data_clim <- here("data", "tidy", "clim")
makeMyDir(path_data_clim)
saveRDS(O_bottomt_final, here(path_data_clim, "dataset_bottomt_clim.rds"))
saveRDS(s2_bottomt_mask_clim, here(path_data_clim, "bottomt_clims.rds"))
