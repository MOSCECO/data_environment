# agrégation de toutes les climatologies globals de copernicus
p <- here("data", "analysis", "climatologies_global")
cgc <- do.call(
  c,
  lapply(
    list.files(p)[-2],
    \(nm) {
      # nm <- "bottomt"
      here(p, nm) %>%
        list.files(pattern = paste0("_", nm, ".+rds"), full.names = T) %>%
        readRDS()
    })
) %>%
  rast()

names(cgc) <- names(cgc) %>%
  str_split("_") %>%
  lapply(pluck, 1) %>%
  unlist(use.names = F)

writeRaster(
  cgc, here("data", "analysis", "climatologies_globales_copernicus.tif")
)
# cgc <- here("data", "analysis", "climatologies_globales_copernicus.tif") %>%
#   rast()
