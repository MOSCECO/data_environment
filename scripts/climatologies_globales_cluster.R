# Découpage des netcdf et réalisation d'une climatologie moyenne
# Tentative sur le cluster
t0 <- Sys.time()

options("rgdal_show_exportToProj4_warnings"="none")

source(here::here("scripts", "boot_cluster.R"))

print("Chargement des librairies réussie")

# Choix du paramètre
my_var <- "bottomT"

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
