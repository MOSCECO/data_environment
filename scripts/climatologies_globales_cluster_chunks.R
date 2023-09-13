# séparation d'un objet stars en différentes parties
t0 <- Sys.time()

options("rgdal_show_exportToProj4_warnings"="none")

source(here::here("scripts", "boot_cluster.R"))

print("Chargement des librairies réussie")

# Choix du paramètre
my_var <- "bottomT"

# Choix des fonctions
v_funs <- c("mean", "sd", "mini", "maxi", "quantile")
v_fnms <- c("mean", "stdv", "mini", "maxi", "qt01", "qt05", "qt95", "qt99")

# profondeurs
gebcoast <- here(
  "data",
  "raw",
  "bathymetrie_gebco_raster",
  "bathymetry_gebco_raster_150m_0.083x0.083.tif"
) %>%
  rast()

# Import des données environnementales
s2 <- list.files(
  here("data", "raw", "env", "copernicus", my_var), full.names = T
) %>%
  read_stars(proxy = T)

m <- st_get_dimension_values(s2, "x") %>% length()
s <- split(1:m, ceiling(seq_along(1:m)/107))

# parallélisation avec parallel
cl <- makeCluster(parallel::detectCores() - 1)
# cl <- makePSOCKcluster(1, manual=TRUE, outfile='log.txt')
print(cl)

clusterExport(cl, "maxi")
clusterExport(cl, "mini")
clusterExport(cl, "s2")
clusterExport(cl, "s")
clusterExport(cl, "gebcoast")
clusterExport(cl, "v_funs")
clusterExport(cl, "v_fnms")
clusterExport(cl, "my_var")

clusterEvalQ(cl = cl, {library("stars")})

CLIMS <- parLapply(
  cl = cl,
  X = seq_along(s),
  fun = \(i) {

    s2c   <- st_as_stars(s2[, s[[i]], , ])
    bathy <- (gebcoast %>% st_as_stars())[, s[[i]], ]

    # Découpage selon les bathymétries d'intérêt
    gc(verbose = F)
    s2_mask <- s2c
    print("Découpe des profondeurs")
    s2_mask[is.na(bathy)] <- NA
    print("Découpe des profondeurs réussie")

    # Climatologies ----
    CLIM <- lapply(
      v_funs,
      \(FUN) {
        s2_clim_out <- if (FUN != "quantile") {

          # FUN <- "mean"
          print(FUN)

          name_clim <- switch(
            FUN, mean = "mean", sd = "stdv", mini = "mini", maxi = "maxi"
          )
          name_clim <- name_clim %>% paste(tolower(my_var), sep = ".")

          s2_mask_clim <- st_apply(
            X        = s2_mask,
            MARGIN   = c("x", "y"),
            FUN      = \(x) get(FUN)(x, na.rm = T),
            CLUSTER  = cl
          )
          s2_mask_clim <- st_set_crs(s2_mask_clim, "EPSG:4326")
          s2_mask_clim <- setNames(s2_mask_clim, name_clim)

          write_stars(
            s2_mask_clim,
            dsn = here(
              "data", "tidy", "climatology_global",
              paste(
                "climatologies", "globales",
                name_clim,
                min(s[[i]]),
                max(s[[i]]),
                sep = "_"
              ) %>%
                paste0(".tif")
            )
          )

          return(s2_mask_clim)
        } else {
          Lout <- lapply(
            c(0.01, 0.05, 0.95, 0.99),
            \(prob) {

              print(paste(FUN, prob))

              name_clim <- paste(
                paste0("qt", sprintf("%02d", prob*100)), tolower(my_var), sep = "."
              )

              s2_mask_clim <- st_apply(
                X        = s2_mask,
                MARGIN   = c("x", "y"),
                FUN      = \(x) get(FUN)(x, na.rm = T, probs = prob),
                CLUSTER  = cl
              )
              s2_mask_clim <- st_set_crs(s2_mask_clim, "EPSG:4326")
              s2_mask_clim <- setNames(s2_mask_clim, name_clim)

              write_stars(
                s2_mask_clim,
                dsn = here(
                  "data", "tidy", "climatology_global",
                  paste("climatologies", "globales", name_clim, sep = "_") %>%
                    paste0(".tif")
                )
              )

              return(s2_mask_clim)
            }
          )
          return(Lout)
        }
      }
    )

    CLIM <- CLIM[1:4] %>% append(CLIM[[5]])
    names(CLIM) <- v_fnms

    return(CLIM)

  }
)

# Arrêt du cluster
stopCluster(cl)

# Regroupement des données
CLIM_mosaic <- do.call(
  c,
  sapply(
    v_fnms,
    \(nm) {
      A <- CLIMS %>% lapply(pluck, nm)
      do.call(st_mosaic, A)
    },
    simplify = F,
    USE.NAMES = T
  )
)

# sauvegarde
path_clim_analysis <- here("data", "analysis", "climatologies_global") %>%
  makeMyDir()

write_stars(
  CLIM_mosaic,
  dsn = here(
    path_clim_analysis,
    paste("climatologies", "globales", tolower(my_var), sep = "_") %>%
      paste0(".tif")
  )
)

saveRDS(
  CLIM_mosaic,
  here(
    path_clim_analysis,
    paste("climatologies", "globales", tolower(my_var), sep = "_") %>%
      paste0(".rds")
  )
)

tf <- Sys.time()
dt <- (tf - t0)
print(dt)
