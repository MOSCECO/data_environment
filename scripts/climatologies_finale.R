# Regroupement de toutes les climatologies

clim_bottomt <- readRDS(here("data", "tidy", "clim", "bottomt_clims.rds")) %>% 
  rast()
clim_so <- here(
  "data", "tidy", "climatologies_stars", "so", "climatology_so_hybride.tif"
) %>% rast()
clim_waves <- readRDS(here("data", "tidy", "clim", "waves_clims.rds")) %>% 
  rast()

# aggrégation
clim_join <- c(clim_bottomt, clim_so, clim_waves)
names(clim_join) <- gsub(
  "_lyr.1", "", names(clim_join %>% st_drop_geometry())
)
clim_valu <- as.data.frame(clim_join, xy = T, na.rm = F)
clim_star <- st_as_stars(clim_valu)
st_crs(clim_star) <- st_crs("EPSG:4326")
# x11(); plot(clim_star["mean.VHM0_SW1_lyr.1", , ])

# conversion sf + interpolation
clim_sf <- st_as_sf(clim_star)
my_observer <- clim_sf %>% st_transform(crs = "EPSG:31970")
# cl <- doMPI::startMPIcluster()
# doMPI::registerDoMPI(cl)
# idw_climatologies <- lapply(
#   names(clim_sf %>% st_drop_geometry()),
#   \(var_idw) {
#     GVI::sf_to_rast(
#       observer = my_observer %>% select(all_of(var_idw)) %>% na.omit(),
#       v        = var_idw,
#       aoi      = my_observer[is.na(my_observer[[var_idw]]), ],
#       # raster_res = 25000, = plus rapide quand on augmente la résolution
#       raster_res = 30500,
#       cores = 2,
#       progress = T
#     )
#   }
# )
# names(idw_climatologies) <- names(clim_sf %>% st_drop_geometry())

# sauvegarde
path_data_clim <- here("data", "tidy", "clim")
makeMyDir(path_data_clim)
path_data_climna <- here("data", "tidy", "clim", "idw_for_na")
makeMyDir(path_data_climna)

# lapply(
#   names(idw_climatologies),
#   \(n) {
#     writeRaster(
#       idw_climatologies[[n]],
#       here(
#         path_data_climna,
#         paste("missing", "values", "clim", "idw", n, sep = "_") %>%
#           paste0(".tif")
#       ),
#       overwrite = T
#     )
#   }
# )
# idw_climatologies <- lapply(list.files(path_data_climna, full.names = T), rast)
names(idw_climatologies) <- names(clim_join)

# complétion des valeurs NA
clim_sf_idw <- do.call(
  cbind, 
  lapply(
    names(idw_climatologies), 
    \(n) {
      tt <- terra::extract(idw_climatologies[[n]], st_centroid(
        my_observer[is.na(my_observer[[n]]), ])
      )
      my_col <- clim_sf[[n]] 
      my_col[is.na(my_col)] <- tt[, 2]
      
      return(my_col)
    }
  )
) %>% as.data.frame() %>% 
  st_as_sf(geometry = st_geometry(clim_sf))
names(clim_sf_idw)[1:20] <- names(clim_sf)[1:20]
apply(clim_sf_idw, 2, \(x) table(is.na(x)))

# attribution tables clim_valu valeurs des climatologies
clim_valu_na <- is.na(clim_valu)
rows_some_na <- which(rowSums(clim_valu_na) != 20)
clim_list <- lapply(
  names(clim_valu %>% select(-c(x, y))), 
  \(varenv) {
    # varenv  <- names(clim_valu %>% select(-c(x, y)))[5]
    rows_na <- which(is.na(clim_valu[, varenv]))
    rows_na <- rows_na[rows_na %in% rows_some_na]
    sf <- clim_valu[rows_na, c("x", "y", varenv)] %>% 
      st_as_sf(coords = c("x", "y"), remove = F)
    st_crs(sf) <- st_crs("EPSG:4326")
    res     <- terra::extract(
      idw_climatologies[[varenv]] %>% project("epsg:4326"), 
      sf
    )
    out <- clim_valu[, varenv]
    out[rows_na] <- res$lyr.1
    return(out)
  }
)
lapply(clim_list, \(x) table(is.na(x)))
names(clim_list) <- names(clim_join)

clim_valu2 <- cbind(
  clim_valu[, c("x", "y")], 
  do.call(cbind, clim_list)
)

# conversion en stars
clim_star2 <- st_as_stars(clim_valu2)
st_crs(clim_star2) <- st_crs("EPSG:4326")
# x11(); plot(clim_star2["mean.VHM0_SW1", , ])

# extraction initiale
# s2_mask_clim <- clim_star2[
#   c("mean.bottomt", "mean.so", "mean.VHM0"), ,
#   # c("mean.VHM0_lyr.1"), , 
# ]
s2_mask_clim <- clim_star2
O_env <- s2_mask_clim %>% st_extract(O)

NNA <- ifelse(is.na(O_env$mean.bottomt_lyr.1), 1, 0) +
  ifelse(is.na(O_env$mean.so_lyr.1), 1, 0) +
  ifelse(is.na(O_env$mean.VHM0_lyr.1), 1, 0)
table(NNA == 1|NNA == 2)
# 239 -> 0!
dim(na.omit(O_env))
# 290 !!

# extraction tampon
# tampon et conversion en raster pour les points sans valeurs
my_buffer <- 5000
all_na_position <- Reduce(
  union, 
  apply(O_env %>% st_drop_geometry(), 2, \(x) which(is.na(x)))
) %>% sort()
O_proj <- st_transform(O[all_na_position, ], "EPSG:31970")
O_buffer <- st_buffer(O_proj, dist = my_buffer)
O_buffer_4326 <- st_transform(O_buffer, "EPSG:4326")
# ggplot() +
#   geom_stars(data = s2_bottomt_mask_mean %>%
#                st_crop(st_bbox(O_buffer_4326[1, ]))) +
#   geom_sf(data = O_buffer_4326[1, ])

# Nombre de cellules du raster intersectant le point avec tampon
table(
  st_intersects(O_buffer_4326, s2_mask_clim %>% st_as_sf()) %>% 
    lengths() == 0
)

# traduction de l'intersection en numéro de cellule
z <- st_intersects(O_buffer_4326, s2_mask_clim %>% st_as_sf())
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
sf <- s2_mask_clim %>% st_as_sf()
values_extract_buffer <- do.call(
  rbind, 
  lapply(
    zf, 
    \(xy) {
      sf[xy, ] %>% st_drop_geometry() %>% apply(2, mean)
    }
  )
) %>% as_tibble()
apply(values_extract_buffer, 2, \(x) table(is.na(x)))

# attribution des valeurs aux occurrences sans valeur
O_env_final <- do.call(
  cbind, 
  lapply(
    names(O_env %>% st_drop_geometry()),
    \(n) {
      O_env[[n]][which(is.na(O_env[[n]]))] <- 
        values_extract_buffer[[n]][which(is.na(O_env[[n]]))]
      return(O_env[[n]])
    }
  )
) %>% st_sf(geometry = O_env$geometry)
names(O_env_final) <- names(O_env)
NNA_final <- ifelse(is.na(O_env_final$mean.bottomt_lyr.1), 1, 0) +
  ifelse(is.na(O_env_final$mean.so_lyr.1), 1, 0) +
  ifelse(is.na(O_env_final$mean.VHM0_lyr.1), 1, 0)
table(NNA_final == 1|NNA_final == 2)
dim(na.omit(O_env_final))
# d <- cbind(O_env$mean.bottomt, a$`1`, (O_env$mean.bottomt == a$`1`) %>% 
#              as.character())

# sauvegarde
path_data_clim <- here("data", "tidy", "clim")
makeMyDir(path_data_clim)
saveRDS(O_env_final, here(path_data_clim, "dataset_occ_clims.rds"))
saveRDS(s2_mask_clim, here(path_data_clim, "clims.rds"))