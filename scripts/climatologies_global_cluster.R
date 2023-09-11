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
)

s2 <- s2 %>%
  lapply(read_ncdf, proxy = FALSE)

# redimensionnement profondeurs selon les dimensions spatiales de copernicus
crs(gebcoast) <- "epsg:4326"
s2_gebcoast <- st_as_stars(gebcoast)
r <- s2[[1]]
r <- r[, , , 1]
r <- st_as_stars(r)
r <- split(r, 3)
s2_gebcoast_redim <- st_warp(s2_gebcoast, r)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
names(s2_gebcoast_redim) <- "depth"
table(is.na(s2_gebcoast_redim["depth", ,]))

# traduction en stars
s2 <- do.call(c, s2)

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

print(s2_mask)

# parallélisation avec parallel
cl <- makeCluster(parallel::detectCores() - 1)
lapply(
  c("mean", "sd", "min", "max", "quantile"),
  \(FUN) {
    s2_clim_out <- if (FUN != "quantile") {

      print(FUN)

      name_clim <- switch(
        FUN, mean = "mean", sd = "stdv", min = "mini", max = "maxi"
      )
      name_clim <- FUN %>% paste(tolower(my_var), sep = ".")

      st_apply(
        X        = s2_mask,
        MARGIN   = c("longitude", "latitude"),
        FUN      = get(FUN),
        ...      = list(na.rm = T),
        CLUSTER  = cl,
        PROGRESS = TRUE
      )
      s2_mask_clim <- st_set_crs(s2_mask_clim, "EPSG:4326")
      s2_mask_clim <- setNames(s2_mask_clim, name_clim)
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
            CLUSTER  = cl,
            PROGRESS = TRUE
          )
          s2_mask_clim <- st_set_crs(s2_mask_clim, "EPSG:4326")
          s2_mask_clim <- setNames(s2_mask_clim, name_clim)
          return(s2_mask_clim)
        }
      )
      return(unlist(Lout))
    }
  }
)

# écart-type
print("Climatologie écart-type")
s2_mask_stdv <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) sd(x, na.rm = T)
)
s2_mask_stdv <- st_set_crs(s2_mask_stdv, "EPSG:4326")
s2_mask_stdv <- setNames(s2_mask_stdv, "stdv.bottomt")
print("Climatologie écart-type réussie")

# maximum
print("Climatologie maximum")
s2_mask_maxi <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) max(x, na.rm = T)
)
s2_mask_maxi <- st_set_crs(s2_mask_maxi, "EPSG:4326")
s2_mask_maxi <- setNames(s2_mask_maxi, "maxi.bottomt")
print("Climatologie maximum réussie")

# qt99mum
print("Climatologie qt99mum")
s2_mask_qt99 <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) min(x, na.rm = T)
)
s2_mask_qt99 <- st_set_crs(s2_mask_qt99, "EPSG:4326")
s2_mask_qt99 <- setNames(s2_mask_qt99, "qt99.bottomt")
print("Climatologie qt99mum réussie")

# quantile 1%
print("Climatologie quantile 1%")
s2_mask_qt01 <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) quantile(x, 0.01, na.rm = T)
)
s2_mask_qt01 <- st_set_crs(s2_mask_qt01, "EPSG:4326")
s2_mask_qt01 <- setNames(s2_mask_qt01, "qt01.bottomt")
print("Climatologie quantile 1% réussie")

# quantile 5%
print("Climatologie quantile 5%")
s2_mask_qt05 <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) quantile(x, 0.05, na.rm = T)
)
s2_mask_qt05 <- st_set_crs(s2_mask_qt05, "EPSG:4326")
s2_mask_qt05 <- setNames(s2_mask_qt05, "qt05.bottomt")
print("Climatologie quantile 5% réussie")

# quantile 95%
print("Climatologie quantile 95%")
s2_mask_qt95 <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) quantile(x, 0.95, na.rm = T)
)
s2_mask_qt95 <- st_set_crs(s2_mask_qt095, "EPSG:4326")
s2_mask_qt95 <- setNames(s2_mask_qt095, "qt95.bottomt")
print("Climatologie quantile 95% réussie")

# quantile 99%
print("Climatologie quantile 99%")
s2_mask_qt99 <- st_apply(
  s2_mask, c("longitude", "latitude"), \(x) quantile(x, 0.99, na.rm = T)
)
s2_mask_qt99 <- st_set_crs(s2_mask_qt99, "EPSG:4326")
s2_mask_qt99 <- setNames(s2_mask_qt99, "qt99.bottomt")
print("Climatologie quantile 99% réussie")

# aggrégation des climatologies
s2_mask_clim <- c(
  s2_mask_mean,
  s2_mask_stdv,
  s2_mask_mini,
  s2_mask_maxi,
  s2_mask_qt01,
  s2_mask_qt05,
  s2_mask_qt95,
  s2_mask_qt99
)

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
