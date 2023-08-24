# Raster hybride des climatologies
# importation première climatologie
s2_bottomt <- list.files(
  here("data", "raw", "env", "copernicus", "bottomT"), full.names = T
) %>% 
  read_stars(along = 3)

# tranches de profondeurs
bathy_slices <- list.files(
  here("data", "tidy"), 
  pattern = "bathymetry_gebco_raster_*.*tif", 
  full.names = T
) %>% lapply(rast)
bathy_slices <- bathy_slices[c(5, 4, 2, 3, 1)]
names(bathy_slices) <- paste0("lvl", 0:4)
# redimensionnement profondeurs selon les dimensions spatiales de copernicus
# zones côtières
s2_gebcoast_redim <- redimStars(gebcoast, s2_bottomt)
# tranches
bathy_slices_redim <- lapply(bathy_slices, redimStars, myClim = s2_bottomt)
print(bathy_slices_redim)
# économie de mémoire
rm(s2_bottomt)

# masques pour "filtrer par attribut" les climatotologies stars
table(is.na(s2_gebcoast_redim["depth", ,]))
names(bathy_slices_redim$lvl0) <- "depth"
table(is.na(bathy_slices_redim$lvl0["depth", ,]))

# chemin de sauvegarde
path_clim <- here("data", "tidy", "climatologies_stars")
makeMyDir(path_clim)
path_climso <- here(path_clim, "so")
makeMyDir(path_climso)

# boucle pour générer les climatologies
bathy_slices_clim <- mapply(
  \(dpth, bathy) {
    
    s2_mask_clim <- if(length(list.files(here(path_climso), pattern = dpth)) == 0) {
      
      cat(paste0(dpth, " "))
      
      # importation
      cat("importation ")
      s2 <- list.files(
        here("data", "raw", "env", "copernicus", "so"), 
        pattern = dpth, 
        full.names = T
      ) %>% 
        read_stars(along = 4)
      cat("ok\n")
      
      # masque de valeurs nulles
      cat(
        paste0(rep(" ", nchar(dpth)) %>% paste(collapse = ""), " masque ") 
      )
      s2_mask <- s2
      s2_mask[is.na(bathy)] <- NA
      cat("ok\n")
      
      # climatologies ----
      # moyenne
      cat(
        paste0(rep(" ", nchar(dpth)) %>% paste(collapse = ""), " moyenne ") 
      )
      
      s2_mask_mean <- st_apply(
        s2_mask, c("x", "y"), \(x) mean(x, na.rm = T)
      )
      s2_mask_mean <- st_set_crs(s2_mask_mean, "EPSG:4326")
      s2_mask_mean <- setNames(s2_mask_mean, "mean.so")
      cat("ok\n")
      
      # écart-type
      cat(
        paste0(rep(" ", nchar(dpth)) %>% paste(collapse = ""), " écart-type ") 
      )
      s2_mask_stdv <- st_apply(
        s2_mask, c("x", "y"), \(x) sd(x, na.rm = T)
      )
      s2_mask_stdv <- st_set_crs(s2_mask_stdv, "EPSG:4326")
      s2_mask_stdv <- setNames(s2_mask_stdv, "stdv.so")
      cat("ok\n")
      
      # maximum
      cat(
        paste0(rep(" ", nchar(dpth)) %>% paste(collapse = ""), " maximum ") 
      )
      s2_mask_maxi <- st_apply(s2_mask, c("x", "y"), max)
      s2_mask_maxi <- st_set_crs(s2_mask_maxi, "EPSG:4326")
      s2_mask_maxi <- setNames(s2_mask_maxi, "maxi.so")
      cat("ok\n")
      
      # S2 <- s2_mask[, , ,]
      # ggplot() + geom_stars(data = S2)
      # S2_clim <- st_apply(S2, c("x", "y"), range)
      # S2_climin <- st_apply(S2, c("x", "y"), min)
      # S2_climin2 <- st_apply(s2_mask, c("x", "y"), min, na.rm = T)
      # S2_climax <- st_apply(S2, c("x", "y"), max)
      # ggplot() + geom_stars(data = S2_clim)
      
      # minimum
      cat(
        paste0(rep(" ", nchar(dpth)) %>% paste(collapse = ""), " minimum ") 
      )
      s2_mask_mini <- st_apply(s2_mask, c("x", "y"), min)
      s2_mask_mini <- st_set_crs(s2_mask_mini, "EPSG:4326")
      s2_mask_mini <- setNames(s2_mask_mini, "mini.so")
      cat("ok\n")
      
      # aggrégation des climatologies
      
      s2_mask_clim <- c(
        s2_mask_mean, 
        s2_mask_stdv, 
        s2_mask_maxi, 
        s2_mask_mini
      )
      
      # sauvegarde
      cat(
        paste0(rep(" ", nchar(dpth)) %>% paste(collapse = ""), " minimum ") 
      )
      s2_mask_clim %>% saveRDS(
        here(
          path_climso,
          paste("climatologies", "masque", "so", dpth, sep = "_") %>%
            paste0(".rds")
        )
      )
      cat("ok\n")
      
      # économie de la mémoire
      rm(s2)
      
      s2_mask_clim
    }
    return(s2_mask_clim)
  },
  c("0.494025", "9.573", "25.2114", "77.8539", "130.666"),
  bathy_slices_redim, 
  SIMPLIFY = F, 
  USE.NAMES = T
)