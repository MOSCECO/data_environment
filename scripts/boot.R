# libraries
libs_to_call <- list(

  "data.table",
  "doParallel",
  "foreach",
  "gstat",
  "ggplot2",
  "here",
  "hydroGOF",
  "ncdf4",
  "raster",
  "RCMEMS",
  "remotes",
  "rgdal",
  "reshape2",
  "sf",
  "stars",
  "stringr",
  "terra",
  "tidync",
  "tidyverse",
  "units"

)

lapply(libs_to_call, function(i) {

  bool <- is.element(i, .packages(all.available = TRUE))

  if (!bool) {
    install.packages(i)
  }

  library(i, character.only = TRUE)

}
)

# remote libraries (github)
Sys.getenv("GITHUB_PAT")
Sys.unsetenv("GITHUB_PAT")
Sys.getenv("GITHUB_PAT")

remote_libs_to_call <- list(
  "RCMEMS",
  "GVI"
)

github_accounts <- list(
  "markpayneatwork",
  "STBrinkmann"
)

mapply(
  function(pckg, usr) {

    bool <- is.element(pckg, .packages(all.available = TRUE))

    if (!bool) {
      devtools::install_github(paste0(usr, "/", pckg))
    }

    library(pckg, character.only = TRUE)

  },
  remote_libs_to_call,
  github_accounts,
  SIMPLIFY = FALSE
)

# functions
lapply(
  list.files(
    here("scripts", "FUN"),
    full.names = T
  ),
  source
)

# paths
input     <- here("data", "raw",  "env")
output    <- here("data", "tidy", "env")
superfamilies <- c("Majoidea", "Muricoidea")
names(superfamilies) <- superfamilies
makeMyDir(input)
makeMyDir(output)

# countries and folder creation
countries <- list("GLP", "MTQ")
m <- st_read(here("data", "raw", "shp", "mappemonde", "mappemonde.shp"))

lapply(
  countries,
  function(cnt) {
    folders_countries_to_create <- here(output, cnt)
    if (!dir.exists(folders_countries_to_create)) {
      dir.create(folders_countries_to_create)
    }
  }
)

# mappemonde shp
wrld_sf <- st_read(here("data", "raw", "shp", "mappemonde", "mappemonde.shp"))

# EPSG
wgs <- 4326

# Données d'occurrences
# INVMAR
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/data_occurrences/data/tidy/occ_threshold/list_occ_thresh_nearest.rds /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/data_environment/data/raw/"
system(cmd)
pa <- readRDS(
  here("data", "raw", "list_occ_thresh_nearest.rds")
)

# dossier copié de data_occ_prep > data > tidy
occ <- lapply(
  superfamilies,
  \(supfam) {
    o <- list.files(
      here("data", "raw", "global_occ_filtered", supfam),
      pattern = "rds$",
      full.names = T
    ) %>%
      lapply(readRDS)
    names(o) <- list.files(
      here("data", "raw", "global_occ_filtered", supfam),
      pattern = "rds$"
    ) %>% substr(nchar("global_occ_filtered") + 2, nchar(.) - 4)
    return(o)
  }
)

# occ_pointer <- lapply(
#   superfamilies,
#   \(supfam) {
#     o <- list.files(
#       here("data", "raw", "global_occ_filtered", supfam),
#       pattern = "csv",
#       full.names = T
#     ) %>%
#       lapply(read.csv)
#     names(o) <- list.files(
#       here("data", "raw", "global_occ_filtered", supfam),
#       pattern = "csv"
#     ) %>%
#       substr(nchar("global_occ_filtered") + 2, nchar(.) - 4)
#     return(o)
#   }
# )

# génération des climatologies et extraction des valeurs associées aux
# occurrences avec l'aide du paquet stars.

# rasters de profondeurs et profondeurs avec occurrences (par espèce)
# source(here("scripts", "interaction_occurrences_profondeurs.R"))
O <- list.files(
  here("data", "tidy"), pattern = "occurrences", full.names = T
) %>% readRDS()
gebcoast <- list.files(
  here("data", "tidy"),
  pattern = "bathymetry_gebco_raster_5.*rds",
  full.names = T
) %>% readRDS()
crs(gebcoast) <- "epsg:4326"

# vecteur tamponné de toutes les occurrences d'espèces dont il faut modéliser
# la distribution
# am <- st_read(here("data", "raw", "shp", "buffer_climatology_species.shp"))
# gebcoast_stars <- st_as_stars(gebcoast)
# am_stars  <- st_rasterize(am) %>%
#   st_warp(gebcoast_stars)
# am_raster <- rast(am_stars)
# par(mfrow = c(1, 2)); plot(gebcoast); plot(am_raster)
