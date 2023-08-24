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
    "gebco_2023_n60.0_s-60.0_w-140.0_e-20.0.tif"
  )
)

# suppression des valeurs d'altitude
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

# extraction
O$depth <- terra::extract(gebco, O, ID = F)[[1]]
# on regarde les occurrences sur des altitudes (et pas des profondeurs)
x11() ; hist(O$depth[O$depth >= 0], breaks = 100)
# incertitude de 10m
O <- O %>% filter(depth <= 10)
dim(O)


summary(as.POSIXct(Otime$eventDate))
ggplot() + 
  geom_tile(data = debco_min, aes(x, y, fill = depth)) + 
  geom_sf(data = m, fill = NA) + 
  geom_sf(data = O, aes(col = depth)) + 
  scale_color_viridis_c(na.value = "red") + 
  xlim(-45, -40) +  
  ylim(-23.7, -22)

O$depth[O$depth > 0] <- 0
quantile(-na.omit(O$depth), 0.95)
debco <- as.data.frame(gebco, xy = T)
names(debco)[3] <- "depth"
debco_frame <- debco %>% 
  filter(x > -45 & x < -37) %>% 
  filter(y > -33 & y < -21)
debcoast <- debco %>% 
  filter(depth > -500 & depth <= 0)
gebcoast <- rast(debcoast)
x11() ; plot(gebcoast)
O$depth2 <- terra::extract(gebcoast, O, ID = F)[[1]]
table(is.na(O$depth2))
ggplot() + 
  geom_tile(data = debco_min, aes(x, y, fill = depth)) + 
  geom_sf(data = m, fill = NA) + 
  geom_sf(data = O, aes(col = depth)) + 
  scale_color_viridis_c(na.value = "red") + 
  xlim(-45, -40) +  
  ylim(-23.7, -22)