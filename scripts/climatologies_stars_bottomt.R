# redimensionnement profondeurs selon les dimensions spatiales de copernicus
crs(gebcoast) <- "epsg:4326"
s2_gebcoast <- st_as_stars(gebcoast)
s2_bottomt <- bottomt_s2
s2_bottomt <- s2_bottomt[, , , 1]
s2_bottomt <- split(s2_bottomt, 3)
s2_gebcoast_redim <- st_warp(s2_gebcoast, s2_bottomt)

# sélection des cellules des variables de copernicus à partir des cellules
# non-nulles de profondeur
table(is.na(s2_gebcoast_redim["depth", ,]))
s2_bottomt_mask <- bottomt_s2
s2_bottomt_mask[is.na(s2_gebcoast_redim)] <- NA
x11() ; plot(s2_bottomt_mask[, , , 1])

t0 <- Sys.time()
s2_bottomt_mask_mean <- st_apply(
  s2_bottomt_mask, c("x", "y"), \(x) mean(x, na.rm = T)
)
s2_bottomt_mask_mean <- st_set_crs(s2_bottomt_mask_mean, "EPSG:4326")
(deltat <- Sys.time() - t0)
s2_bottomt_mask_mean <- setNames(s2_bottomt_mask_mean, "mean.bottomt")
x11(); ggplot() + 
  geom_stars(data = s2_bottomt_mask_mean) + 
  scale_fill_viridis_c()

# extraction
O_bottomt_mean <- st_extract(s2_bottomt_mask_mean, O)
table(is.na(O_bottomt_mean$mean.bottomt))

x11(); ggplot() + 
  geom_stars(data = s2_bottomt_mask_mean) + 
  geom_sf(data = O_bottomt_mean, aes(col = is.na(mean.bottomt))) +
  scale_fill_viridis_c()

t0 <- Sys.time()
s2_bottomt_mask_stdv <- st_apply(
  s2_bottomt_mask, c("x", "y"), \(x) sd(x, na.rm = T)
)
(deltat <- Sys.time() - t0)
s2_bottomt_mask_stdv <- setNames(s2_bottomt_mask_stdv, "stdv.bottomt")
x11(); ggplot() + 
  geom_stars(data = s2_bottomt_mask_stdv) + 
  scale_fill_viridis_c(option = "A")

t0 <- Sys.time()
s2_bottomt_mask_maxi <- st_apply(
  s2_bottomt_mask, c("x", "y"), \(x) max(x, na.rm = T)
)
(deltat <- Sys.time() - t0)
s2_bottomt_mask_maxi <- setNames(s2_bottomt_mask_maxi, "maxi.bottomt")
x11(); ggplot() + 
  geom_stars(data = s2_bottomt_mask_maxi) + 
  scale_fill_viridis_c(option = "B")

t0 <- Sys.time()
s2_bottomt_mask_mini <- st_apply(
  s2_bottomt_mask, c("x", "y"), \(x) min(x, na.rm = T)
)
(deltat <- Sys.time() - t0)
s2_bottomt_mask_mini <- setNames(s2_bottomt_mask_mini, "mini.bottomt")
x11(); ggplot() + 
  geom_stars(data = s2_bottomt_mask_mini) + 
  scale_fill_viridis_c(option = "C")