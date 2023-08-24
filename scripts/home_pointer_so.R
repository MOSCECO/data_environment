# environmental variable
database <- "copernicus"
that_var <- "so"

# occurrence
superfamily <- "Majoidea"
species     <- "Mithraculus_forceps"  
O <- occ[[superfamily]][[species]]

# profondeurs
seuils <- c(9.573, 25.2114, 77.8539, 130.666)

# raster des profondeurs et altitudes (amériques)
gebco <- rast(
  here(
    "data", "raw", "gebco", 
    "gebco_2023_n60.0_s-60.0_w-140.0_e-20.0.tif"
  )
)

# suppression des valeurs d'altitude
# gebco_bathy_df <- as.data.frame(gebco, xy = T) %>%
#   as.data.table()
# names(gebco_bathy_df)[3] <- "depth"
# gebco_bathy_df$depth[gebco_bathy_df$depth > 0] <- NA
# gebco_bathy <- rast(gebco_bathy_df)
# gebco_bathy <- ifel(gebco > 0, NA, gebco)
# writeRaster(
#   gebco_bathy,
#   here("data", "raw", "gebco", "gebco_bathymetry.tif"),
#   overwrite = T
# )
# rm(gebco_bathy_df)
# x11() ; plot(gebco_bathy)

# raster des profondeurs uniquement (amériques)
gebco_bathy <- rast(here("data", "raw", "gebco", "gebco_bathymetry.tif"))

# extraction des données de profondeurs pour chaque occurrence de 
# l'espèce considérée

# O$depth <- terra::extract(gebco_bathy, O, ID = F)[[1]] # Première extraction
# my_buffer <- 100 # initialisation de la taille du buffer
# O$depth_buffer <- O$depth # séparation d'un descripteur profondeur et d'un 
# # descripteur profondeur issue du buffer
# O$size_buffer  <- ifelse(is.na(O$depth_buffer), NA, 0) # toutes les occurrences
# à laquelle une une valeur de profondeur a été attribuée lors de la première
# extraction n'ont pas de taille de buffer. 

# Tant qu'une occurrence n'a pas de profondeur associée, la boucle 
# while(length(table(is.na(O$depth_buffer))) > 1) {
#   # affiche les compteurs
#   print(paste("buffer", "=", my_buffer))
#   print(table(is.na(O$depth_buffer)))
#   
#   # crée un tampon autour des occurrences
#   O_buffer <- st_buffer(st_transform(O, "EPSG:32620"), my_buffer) %>% 
#     st_transform(., "EPSG:4326")
#   
#   # ajoute la taille du tampon à la table des occurrences pour répérer les 
#   # occurrences dont la valeur de profondeur est très éloignée de sa position
#   O$size_buffer[is.na(O$depth_buffer)] <- my_buffer
#   
#   # remplace uniquement les valeurs de profondeurs des points sans données
#   O$depth_buffer[is.na(O$depth_buffer)] <- lapply(
#     which(is.na(O$depth_buffer)),
#     \(i) {
#       terra::extract(gebco_bathy, O_buffer[i, ], ID = F)[[1]] %>% 
#         mean(na.rm = T) %>% 
#         round(0)
#     }
#   ) %>% unlist(use.names = F)
#   
#   # incrémente le compteur
#   my_buffer <- my_buffer + 100
# }
# Sortie : table O modifiée
# saveRDS(O, here("data", "raw", "O.rds"))
O <- readRDS(here("data", "raw", "O.rds"))

# détermination des intervalles de profondeurs
# breaks <- c(
#   Inf, 
#   -(seuils[1]+seuils[2])/2, 
#   -(seuils[2]+seuils[3])/2,
#   -(seuils[3]+seuils[4])/2,
#   -Inf
# )
# chaque occurrence sera associé à un intervalle (qui correspond aux salinités
# disponibles) en fonction de sa profondeur
# v <- cut(O$depth_buffer, breaks, include.lowest = TRUE)
# v <- factor(v)
# levels(v) <- c("so130", "so77", "so25", "so9")
# Nouvelle table dissociée en liste dont chaque élément est une table avec
# les occurrences d'un intervalle de profondeur donnée et qui pourra être itérée
# Os <- O
# Os$cat_so <- v
# Os <- Os %>% split(f = Os$cat_so)

# saveRDS(Os, here("data", "raw", "Os.rds"))
Os <- readRDS(here("data", "raw", "Os.rds"))

# ggplot() +
#   geom_sf(data = m %>% st_crop(st_bbox(O)), fill = NA) + 
#   geom_tile(
#     data = gebco_df, aes(x, y, fill = depth) 
#   ) + 
#   geom_sf(data = O, aes(col = depth2)) +
#   scale_color_gradient(low = "darkgreen", high = "lightgreen", na.value = "red")

# fichier de sauvegarde (à cause des crashs)
path_env_points <- here("data", "raw", "env_points")
makeMyDir(path_env_points)

path_env_points_db <- here(path_env_points, database)
makeMyDir(path_env_points_db)

path_env_points_db_var <- here(path_env_points_db, that_var)
makeMyDir(path_env_points_db_var)

path_epdv_supfam <- here(path_env_points_db_var, superfamily)
makeMyDir(path_epdv_supfam)

path_epdv_supfam_sp <- here(path_epdv_supfam, species)
makeMyDir(path_epdv_supfam_sp)

path_epdv_supfam_sp_depth <- here(path_epdv_supfam_sp, paste0("depth", seuils))
lapply(path_epdv_supfam_sp_depth, makeMyDir)

# Modification des coordonnées pour les points qui n'interceptent pas exactement
# la grille de données utilisée
# Os2_sf <- mapply(
#   \(so_depth, so_lvl) {
#     
#     print(so_lvl)
#     print(so_depth)
#     
#     # sélection d'une table d'occurrences pour une profondeur donnée
#     tb <- Os[[so_lvl]]
#     
#     # importation de valeurs environnementales en stars et conversion en sf
#     s2 <- list.files(
#       here("data", "raw", "env", "copernicus", that_var), 
#       pattern = so_depth %>% as.character(), 
#       full.names = T
#     )[[1]] %>% read_stars()
#     s2s2 <- s2[, , , 1] %>% st_as_stars()
#     sf <- st_as_sf(s2s2)
#     st_crs(sf) <- st_crs(tb)
#     # x11() ; ggplot() + geom_sf(data = sf)
#     
#     # identification des polygones à partir desquels on va extraire les données
#     id_plg_values <- st_nearest_feature(tb, sf)
#     ct_plg_values <- sf[id_plg_values, ] %>%
#       st_centroid() %>% 
#       st_geometry()
#     
#     # remplacement des coordonées points qui ne tombent 
#     # pas parfaitement dans la mer
#     positions_wrong_pts <- which(st_intersects(tb, sf) %>% lengths() == 0)
#     tb2 <- tb %>% st_drop_geometry()
#     tb2[positions_wrong_pts, c("decimalLongitude", "decimalLatitude")] <-
#       (ct_plg_values %>% st_coordinates())[positions_wrong_pts, ]
#     tb2_sf <- st_as_sf(
#       tb2,
#       coords = c("decimalLongitude", "decimalLatitude"),
#       crs = st_crs(tb),
#       remove = F
#     )
#     # vérification que tous les points intersectent bien les polygones
#     print(table(st_intersects(tb2_sf, sf) %>% lengths() == 0))
#     
#     return(tb2_sf)
#   },
#   (rev(seuils) %>% as.character()),
#   names(Os),
#   SIMPLIFY  = F, 
#   USE.NAMES = T
# )
# names(Os2_sf) <- names(Os)

# saveRDS(Os2_sf, here("data", "raw", "Os2_sf.rds"))
Os2_sf <- readRDS(here("data", "raw", "Os2_sf.rds"))

# visualisation du déplacement des polygones : 
# on peut voir clairement la résolution du raster qui ne correspond pas 
# aux environnements où ont été collectées les occurrences (hypercôtiers) 
# ggplot() + 
#   geom_sf(data = m %>% st_crop(st_bbox(tb2_sf)), fill = NA) + 
#   geom_sf(data = Os[[so_lvl]], col = "red", alpha = 0.5) + 
#   geom_sf(data = tb2_sf, col = "green", alpha = 0.5) + 
#   xlim(-85, -80) + ylim(19, 30) + 
#   geom_sf(
#     data = sf %>% st_crop(c(xmin = -85, ymin = 19, xmax = -80, ymax = 30)), 
#     fill = NA
#   )

# data_env_pointers_ngb <- mapply(
#   \(so_depth, so_lvl) {
#     
#     print(paste(so_lvl, paste0("(", so_depth, ")")))
#     # so_lvl <- "so9"
#     # so_depth <- "9.573"
#     # so_lvl <- "so77"
#     # so_depth <- "77.8539"
#     # so_lvl <- "so130"
#     # so_depth <- "130.666"
#     O2_sf <- Os2_sf[[so_lvl]]
#     
#     lapply(
#       1:nrow(O2_sf),
#       \(i) {
#         # i <- 1
#         out <- if (
#           list.files(
#             here(path_epdv_supfam_sp, paste0("depth", so_depth)), 
#             pattern = sprintf("%03d", i)
#           ) %>% length() == 0
#         ) {
#           tryCatch(
#             expr = {
#               # i <- 1
#               print(paste(i, nrow(O2_sf), sep = "/"))
#               o <- O2_sf[i, ]
#               oproj <- st_transform(o, crs = "EPSG:32620")
#               
#               # emprise rectangulaire
#               bproj <- bSquare(oproj, 100000^2)
#               b <- st_transform(bproj, crs = "EPSG:4326")
#               (bbox <- st_bbox(b))
#               
#               # slice du netcdf
#               vs <- lapply(
#                 list.files(
#                   here("data", "raw", "env", "copernicus", that_var), 
#                   pattern = so_depth %>% as.character(), 
#                   full.names = T
#                 ),
#                 \(path) {
#                   # path <- list.files(
#                   #   here("data", "raw", "env", "copernicus", that_var),
#                   #   pattern = so_depth %>% as.character(),
#                   #   full.names = T
#                   # )[[2]]
#                   # print(path)
#                   nc <- tidync(path)
#                   nc_crop <- nc %>% 
#                     hyper_filter(
#                       longitude = longitude > bbox["xmin"] & longitude < bbox["xmax"],
#                       latitude  = latitude  > bbox["ymin"] & latitude  < bbox["ymax"]
#                     )
#                   tb <- nc_crop %>% hyper_tibble()
#                   
#                   # transformation en stars
#                   s2 <- st_as_stars(
#                     tb, 
#                     dims = c("longitude", "latitude", "time"),
#                     xy   = c("longitude", "latitude"),
#                     y_decreasing = F,
#                     crs = st_crs(o)
#                   )
#                   s2 <- st_set_crs(s2, value = st_crs(o))
#                   # extraction de la donnée
#                   v <- stars::st_extract(s2, o)
#                   
#                   return(v)
#                 })
#               names(vs) <- here("data", "raw", "env", "copernicus", that_var) %>%
#                 list.files(pattern = so_depth)  %>% 
#                 substr(1, 40)
#               saveRDS(
#                 vs,
#                 here(
#                   path_epdv_supfam_sp, 
#                   paste0("depth", so_depth),
#                   paste0("env_data_point", sprintf("%03d", i), ".rds")
#                 )
#               )
#               return(vs)
#             },
#             error = \(e) {
#               saveRDS(
#                 NA,
#                 here(
#                   path_epdv_supfam_sp, 
#                   paste0("depth", so_depth),
#                   paste0("env_data_point_err", sprintf("%03d", i), ".rds")
#                 )
#               )
#               return(NA)
#             }
#           )
#         }
#         return(out)
#       } )
#   },
#   rev(seuils) %>% as.character(), 
#   names(Os2_sf),
#   USE.NAMES = T, 
#   SIMPLIFY = F
# )
# 
# # importation des données de points
# data_env_pointers_ngb <- (path_epdv_supfam_sp %>% 
#                             list.files(full.names = T)) %>% 
#   lapply(
#     \(path) {
#       list.files(path, full.names = T) %>% 
#         lapply(readRDS)
#     }
#   )
# names(data_env_pointers_ngb) <- c("so130", "so25", "so77", "so9")
# data_env_pointers_ngb <- data_env_pointers_ngb[c(1, 3, 2, 4)]

# saveRDS(
#   data_env_pointers_ngb,
#   here(path_epdv_supfam_sp, paste0("data_env_pointers_ngb_", that_var, ".rds"))
# )

data_env_pointers_ngb <- here(
  path_epdv_supfam_sp, paste0("data_env_pointers_ngb_", that_var, ".rds")
) %>% 
  readRDS()

data_env_pointers_ngb2 <- Reduce(c, data_env_pointers_ngb)

# occurrences qui renvoient une erreur
vecna <- which(is.na(data_env_pointers_ngb2))
# Pourquoi ?
# i = 350
# carte du monde
# m <- st_read(here("data", "raw", "shp", "mappemonde", "mappemonde.shp"))
# ggplot() +
#   geom_sf(data = m, fill = NA) +
#   geom_sf(data = b, col = "red", fill = NA) +
#   xlim(-120, -110) +
#   ylim(20, 30) +
#   geom_sf(data = o, col = "blue")
# En attendant :on supprime juste les occurrences qui posent problème. 
# occurrences qui continuent de tomber sur la terre... ça doit être dû
# aux séparations entre polygones, fair eune union avant de filtrer les 
# occurrences dans data_occ_prep

O2_sf <- do.call(rbind, Os2_sf)

# réorganisation
data_per_occ_per_var <- lapply(
  1:nrow(O2_sf), 
  \(i) {
    # environnemental data linked to occurrence
    # i <- 10
    tv <- if (!is.na(data_env_pointers_ngb2[i])) {
      de_occ <- data_env_pointers_ngb2[[i]]
      vartime <- lapply(
        de_occ, 
        # temporal slice of environnemental data for each occurrence 
        \(de_slice) {
          # extract its time stamps from the stars
          # de_slice <- de_occ$`copernicus_waves_2022-10-01_2022-11-01`
          ttime <- (stars::st_get_dimension_values(de_slice, "time")*3600) %>% 
            as.POSIXct(origin = "1950-01-01")
          # create one tibble per variable stored into the stars object
          tbs_out <- lapply(
            names(de_slice),
            \(that_var) {
              vvalu <- de_slice[[that_var]]
              out <- tibble(
                x     = O[i, "decimalLongitude"] %>% 
                  st_drop_geometry() %>% unlist(),
                y     = O[i, "decimalLatitude"]  %>% 
                  st_drop_geometry() %>% unlist(),
                time  = ttime, 
                value = t(vvalu)[, 1]
              )
              return(out)
            })
          names(tbs_out) <- names(de_slice) 
          return(tbs_out)
        })
      timevar <- vartime %>% transpose()
      
      lapply(timevar, \(x) {
        y <- do.call(rbind, x)
        return(y[!duplicated(y), ])
      })
    } else { 
      
      tibble(
        x     = O[i, "decimalLongitude"] %>% st_drop_geometry() %>% unlist(),
        y     = O[i, "decimalLatitude"]  %>% st_drop_geometry() %>% unlist(),
        time  = NA, 
        value = NA
      ) }
    
    
    return(tv)
    
  })

# Vérification 
vnan <- which(
  data_per_occ_per_var %>% 
    lapply(pluck, that_var) %>% 
    lapply(\(tb) unique(tb$value)) %>% 
    is.na()
)
table(vnan) # integer(0) ok

# ggplot() +
#   geom_sf(data = m, fill = NA) +
#   geom_sf(data = O[-vnan,], col = "green") +
#   geom_sf(data = O[vnan, ], col = "red") +
#   xlim(-120, -40) +
#   ylim(-30, 30)

# climatologies MOYENNES et ÉCARTS-TYPES
d <- lapply(
  1:length(data_per_occ_per_var), 
  \(i) {
    # i <- 1
    print(paste0(i, "/", length(data_per_occ_per_var)))
    o <- data_per_occ_per_var[[i]]
    mean_values <- if(class(o)[1] == "list") {
      mean_values <- lapply(
        o, 
        \(tb) {
          # tb <- o$VHM0_WW
          tb_out <- tb %>%
            group_by(x, y) %>% 
            summarise(value_mean = mean(value), value_stdv = sd(value))
        }
      )
      names(mean_values) <- names(o)
      mean_values
    } else {
      list(
        so  = o %>% select(-time)
      )
    }
    
    return(mean_values)
  }
)

dt <- transpose(d)

d2 <- lapply(dt, \(x) do.call(rbind, x))
d2$so <- d2$so %>% select(-value)
lapply(d2, dim)
lapply(d2, \(tb) table(duplicated(tb)))

d3 <- lapply(d2, \(x) x[!duplicated(x),] %>% na.omit())
lapply(d3, dim)

# fichier de sauvegarde
path_clim_global <- here("data", "tidy", "clim_global")
makeMyDir(path_clim_global)

path_supfam <- here(path_clim_global, superfamily)
makeMyDir(path_supfam)

path_supfam_sp <- here(path_clim_global, superfamily, species)
makeMyDir(path_supfam_sp)

# sauvegarde
lapply(
  names(d2), \(n) write.csv(
    d2[[n]],
    here(path_supfam_sp, paste0(tolower(n), ".csv")), 
    row.names = F
  )
)