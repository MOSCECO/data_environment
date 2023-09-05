# environmental variable
database <- "copernicus"
that_var <- "bottomT"

# occurrence
superfamily <- "Muricoidea"
species     <- "Stramonita_rustica"
O <- occ[[superfamily]][[species]]

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

# importation d'un objet stars et conversion en sf
s2 <- list.files(
  here("data", "raw", "env", "copernicus", "bottomT"), full.names = T
)[[2]] %>% read_stars()
s2s2 <- s2[, , , 1] %>% st_as_stars()
sf <- st_as_sf(s2s2)
st_crs(sf) <- st_crs(O)

# identification des polygones à partir desquels on va extraire les données
id_plg_values <- st_nearest_feature(O, sf)

saveRDS(
  id_plg_values,
  here(path_epdv_supfam_sp, paste0("polygone_extraction_", that_var, ".rds"))
)

id_plg_values <- here(
  path_epdv_supfam_sp, paste0("polygone_extraction_", that_var, ".rds")
) %>%
  readRDS()

ct_plg_values <- sf[id_plg_values, ] %>%
  st_centroid() %>%
  st_geometry()

# remplacement des coordonées points qui ne tombent pas parfaitement dans la mer
positions_wrong_pts <- which(st_intersects(O, sf) %>% lengths() == 0)
O2 <- O %>% st_drop_geometry()
O2[positions_wrong_pts, c("decimalLongitude", "decimalLatitude")] <-
  (ct_plg_values %>% st_coordinates())[positions_wrong_pts, ]
O2_sf <- st_as_sf(
  O2, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(O)
)
# vérification
table(st_intersects(O2_sf, sf) %>% lengths() == 0)
# FALSE
# 473 ok

saveRDS(
  O2_sf,
  here(path_epdv_supfam_sp, paste0("occ_coords_corrigees_", that_var, ".rds"))
)

O2_sf <- here(
  path_epdv_supfam_sp, paste0("occ_coords_corrigees_", that_var, ".rds")
) %>%
  readRDS()

data_env_pointers_ngb <- lapply(
  1:nrow(O2_sf),
  \(i) {

    out <- if (
      list.files(here(path_epdv_supfam_sp), pattern = sprintf("%03d", i)) %>%
      length() == 0
    ) {
      tryCatch(
        expr = {
          # i <- 1
          print(paste(i, nrow(O2_sf), sep = "/"))
          o <- O2_sf[i, ]
          oproj <- st_transform(o, crs = "EPSG:32620")

          # emprise rectangulaire
          bproj <- bSquare(oproj, 100000^2)
          b <- st_transform(bproj, crs = "EPSG:4326")
          (bbox <- st_bbox(b))

          # slice du netcdf
          vs <- lapply(
            list.files(
              here("data", "raw", "env", "copernicus", that_var), full.names = T
            ),
            \(path) {
              # path <- list.files(
              #   here("data", "raw", "env", "copernicus", "waves"), full.names = T
              # )[[2]]
              nc <- tidync(path)
              nc_crop <- nc %>%
                hyper_filter(
                  longitude = longitude > bbox["xmin"] & longitude < bbox["xmax"],
                  latitude  = latitude  > bbox["ymin"] & latitude  < bbox["ymax"]
                )
              tb <- nc_crop %>% hyper_tibble()

              # transformation en stars
              s2 <- st_as_stars(
                tb,
                dims = c("longitude", "latitude", "time"),
                xy   = c("longitude", "latitude"),
                y_decreasing = F,
                crs = st_crs(o)
              )
              s2 <- st_set_crs(s2, value = st_crs(o))
              # extraction de la donnée
              v <- stars::st_extract(s2, o)

              return(v)
            })
          names(vs) <- here("data", "raw", "env", "copernicus", that_var) %>%
            list.files() %>%
            substr(1, 38)
          saveRDS(
            vs,
            here(
              path_epdv_supfam_sp,
              paste0("env_data_point", sprintf("%03d", i), ".rds")
            )
          )
          return(vs)
        },
        error = \(e) {
          saveRDS(
            NA,
            here(
              path_epdv_supfam_sp,
              paste0("env_data_point", sprintf("%03d", i), ".rds")
            )
          )
          return(NA)
        }
      )
    }

    return(out)
  })

table(is.na(data_env_pointers_ngb))

saveRDS(
  data_env_pointers_ngb,
  here(path_epdv_supfam_sp, paste0("data_env_pointers_ngb_", that_var, ".rds"))
)

# data_env_pointers_ngb <- here(
#   path_epdv_supfam_sp, paste0("data_env_pointers_ngb_", that_var, ".rds")
# ) %>%
#   readRDS()

data_env_pointers_ngb <- here(path_epdv_supfam_sp) %>%
  list.files(pattern = "env_data_point", full.names = T) %>%
  lapply(readRDS)

# occurrences qui renvoient une erreur
vecna <- which(is.na(data_env_pointers_ngb))
# Pourquoi ?
# i = 350
# carte du monde
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

# réorganisation
data_per_occ_per_var <- lapply(
  1:nrow(O2_sf),
  \(i) {
    # environnemental data linked to occurrence
    # i <- 10
    tv <- if (!is.na(data_env_pointers_ngb[i])) {
      de_occ <- data_env_pointers_ngb[[i]]
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
        bottomT  = o %>% select(-time)
      )
    }

    return(mean_values)
  }
)

dt <- transpose(d)

d2 <- lapply(dt, \(x) do.call(rbind, x))
# d2$bottomT <- d2$bottomT %>% select(-value)
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
