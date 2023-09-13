# Découpage des netcdf et réalisation d'une climatologie moyenne
# Tentative sur le cluster
t0 <- Sys.time()

options("rgdal_show_exportToProj4_warnings"="none")

source(here::here("scripts", "boot_cluster.R"))

print("Chargement des librairies réussie")

# Choix du paramètre
my_var <- "bottomT"

# profondeurs
gebcoast <- here(
  "data", "raw", "bathymetrie_gebco_raster", "bathymetry_gebco_raster_150m.tif"
) %>%
  rast()

# Import des données environnementales
s2 <- list.files(
  here("data", "raw", "env", "copernicus", my_var), full.names = T
) %>%
  read_stars()

print(s2)

# redimensionnement profondeurs selon les dimensions spatiales de copernicus
crs(gebcoast) <- "epsg:4326"
s2_gebcoast <- st_as_stars(gebcoast)
r <- s2
r <- r[, , , 1]
print(r)
r <- st_as_stars(r)
print(r)
# r <- split(r, 3)
# print(r)
s2_gebcoast_redim <- st_warp(s2_gebcoast, r)
print(s2_gebcoast_redim)
# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
names(s2_gebcoast_redim) <- "depth"
table(is.na(s2_gebcoast_redim["depth", ,]))

# Découpage selon les bathymétries d'intérêt
gc(verbose = F)
s2_mask <- s2
print("Conversion en stars")
s2_mask <- st_as_stars(s2_mask)
print("Conversion en stars réussie")
print(s2_mask)
print("Découpe des profondeurs")
s2_mask[is.na(s2_gebcoast_redim)] <- NA
print("Découpe des profondeurs réussie")

# parallélisation avec parallel
cl <- makeCluster(parallel::detectCores() - 1)
print(cl)
# Climatologies ----
CLIM <- lapply(
  c("mean", "sd", "min", "max", "quantile"),
  \(FUN) {
    s2_clim_out <- if (FUN != "quantile") {

      print(FUN)

      name_clim <- switch(
        FUN, mean = "mean", sd = "stdv", min = "mini", max = "maxi"
      )
      name_clim <- FUN %>% paste(tolower(my_var), sep = ".")

      s2_mask_clim <- st_apply(
        X        = s2_mask,
        MARGIN   = c("x", "y"),
        FUN      = get(FUN),
        ...      = list(na.rm = T),
        # CLUSTER  = cl,
        PROGRESS = FALSE
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
            MARGIN   = c("longitude", "latitude"),
            FUN      = get(FUN),
            ...      = list(na.rm = T, probs = prob),
            # CLUSTER  = cl,
            PROGRESS = FALSE
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
      return(unlist(Lout))
    }
  }
)
stopCluster(cl)

# aggrégation des climatologies
s2_mask_clim <- do.call(c, CLIM)

# sauvegarde
write_stars(
  s2_mask_clim,
  dsn = here(
    "data", "tidy", "climatology_global",
    paste("climatologies", "globales", tolower(my_var), sep = "_") %>%
      paste0(".tif")
  )
)

tf <- Sys.time()
dt <- (tf - t0)
print(dt)
