# Interaction données d'occurrences variables environnementales ----
# environmental variable
database <- "copernicus"
that_var <- "so"

# aggrégation de toutes les occurrences
O <- do.call(rbind, lapply(occ, \(l) do.call(rbind, l)))
bb <- c(xmin = -125, ymin = -60, ymax = 60, xmax = 65) %>% st_bbox()
st_crs(bb) <- st_crs(O)
O2 <- sf::st_crop(O, bb)
O <- O[-which(O$occurrenceID %in% O2$occurrenceID), ]


# profondeurs
gebc0 <- read_stars(
  here("data", "raw", "gebco", "gebco_2023_n60.0_s-60.0_w-140.0_e-20.0.tif")
)
gebc1 <- read_stars(
  here("data", "raw", "gebco", "gebco_2023_n60.0_s-60.0_w-20.0_e65.0.tif")
)
gebco <- stars::st_mosaic(gebc0, gebc1)
gebco_s2 <- st_as_stars(gebco)
names(gebco_s2) <- "depth"

# suppression des valeurs d'altitude ----
# gebco_bathy <- gebco_s2
# gebco_bathy[gebco_bathy["depth", , ] > 0] <- 0
# saveRDS(
#   gebco_bathy,
#   here::here("data", "raw", "gebco", "gebco_bathymetry.rds")
# )
gebco_bathy <- readRDS(here("data", "raw", "gebco", "gebco_bathymetry.rds"))

# extraction des profondeurs des occurrences ----
O$depth <- st_extract(gebco_bathy, O) %>%
  st_drop_geometry()

# Études des profondeurs pour toutes les occurrences
quantile(O[["depth"]] %>% unlist(use.names = F), 0.1)
# et selon les espèces
Os <- split(O, f = O$scientificName)
bathy_threshold <- lapply(
  Os,
  \(tb) {
    u <- tb %>%
      st_drop_geometry() %>%
      select(depth) %>%
      unlist(use.names = F)
    quantile(-u, 0.75)  # Seuil arbitraire pour être assez stringent pour ne pas
    # aller échantillonner des pseudo-absences complètement en dehors de la
    # niche de l'espèce
  }
)
# Bathymetric threshold
BT <- ceiling(summary(sort(unlist(bathy_threshold)))[5]/10)*10

bathy_threshold_rounded <- lapply(bathy_threshold, \(x) ceiling(x/10)*10)

# Relation avec la liste des espèces "cotières" et "profondes" des explorations
# du MNHN.
Os2 <- Os[names(pa)]
# concaténation toutes occcurrences
# names(pa$`Amphithrax hemphilli`)[
#   which(
#     !names(pa$`Amphithrax hemphilli`) %in% names(Os2$`Amphithrax hemphilli`)
#   )
# ]
pa2 <- sapply(
  pa,
  \(l) {
    # l <- pa$`Amphithrax hemphilli`
    l <- l %>% filter(individualCount > 0)
    l <- l %>% select(-ISL)
    names(l)[which(names(l) == "depth_MNHN")] <- "depth"
    l$depth <- -l$depth
    l_sf <- st_as_sf(
      l, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326
    )
    return(l_sf)
  },
  simplify = F,
  USE.NAMES = T
)

Os2 <- mapply(
  \(g, l) {

    # g <- Os2$`Amphithrax hemphilli`
    # l <- pa$`Amphithrax hemphilli`

    g$family <- rep(unique(l$family), nrow(g))
    g$TAX    <- rep(unique(l$TAX), nrow(g))

    g <- g %>% select(all_of(names(l)))

    return(g)

  },
  Os2,
  pa2,
  SIMPLIFY = F,
  USE.NAMES = T
)

OO <- mapply(rbind, Os2, pa2, SIMPLIFY = F, USE.NAMES = T)

# Étude des différences de profondeurs entre bases de données
summary(OO$`Amphithrax hemphilli`$depth)
tb <- count(OO$`Amphithrax hemphilli` %>% st_drop_geometry(), depth)
tb <- apply(tb, 2, unlist) %>% as_tibble()
ggplot(data = tb, aes(x = depth, y = n)) +
  geom_col(width = 100, fill = "red", alpha = 0.5)
OO <- OO %>% lapply(\(tb) {
  tb$depth <- unlist(tb$depth)
  return(tb)
})

names(OO) %>%
  lapply(
    \(bn) {
      tb <- OO[[bn]] %>% st_drop_geometry()
      print(paste(bn, ":", "Total"))
      print(summary(tb$depth))
      print(paste(bn, ":", "MNHN"))
      print(summary(tb %>% filter(database == "INVMAR") %>% select(depth) %>% unlist()))
      print(paste(bn, ":", "GBIF"))
      print(summary(tb %>% filter(database != "INVMAR") %>% select(depth) %>% unlist()))
      cat("\n")
      return(NULL)
    }
  )

oo <- do.call(rbind, OO)
oo2 <- oo %>% filter(depth > -1000)
ggplot(data = oo2, aes(y = scientificName, x = depth)) +
  geom_boxplot()

# Occurrences extrêmement profondes pour les occurrences du GBIF :
# décision de réaliser une troncature de la distribution des profondeurs
# des occurrences du GBIF au troisième quantile
OO_filter_depth <- sapply(
  OO,
  \(tb) {
    sm <- summary(
      tb %>% filter(database != "INVMAR") %>% select(depth) %>% unlist()
    )
    tb %>%
      filter(!(database != "INVMAR" & depth < -ceiling(-sm[2]/10)*10))
  },
  simplify = F,
  USE.NAMES = T
)

names(OO_filter_depth) %>%
  lapply(
    \(bn) {
      tb <- OO_filter_depth[[bn]] %>% st_drop_geometry()
      print(paste(bn, ":", "Total"))
      print(summary(tb$depth))
      print(paste(bn, ":", "MNHN"))
      print(summary(tb %>% filter(database == "INVMAR") %>% select(depth) %>% unlist()))
      print(paste(bn, ":", "GBIF"))
      print(summary(tb %>% filter(database != "INVMAR") %>% select(depth) %>% unlist()))
      cat("\n")
      return(NULL)
    }
  )
# L'écart s'est bien réduit, on regarde maintenant la distribution des
# profondeurs espèce par espèce
oo_filter_depth <- do.call(rbind, OO_filter_depth)
ggplot(data = oo_filter_depth, aes(y = scientificName, x = depth)) +
  geom_boxplot()

# Valeurs aberrantes pour Claremontiella nodulosa, et peut-être
# Siratus consuelae, difficile à dire, mais on est à chaque fois en présence
# d'un tracé de dragues profondes.
# Pour ce qui est de Stenorhynchus seticornis, on a des nasses qui ont été
# posées à 130, 140 et 150 m de fond, ce qui est plus clair en terme de
# profondeur, mais pose un problème d'appât (est-ce que l'espèce a été attirée
# hors de sa zone habituelle ?)
# Finalement pour 130 m on a Coralliophila salebrosa, qui a été récupérée en
# drague côtière (moins de variation de profondeurs j'imagine).
# profondeur minimale :
# BT_threshold30 <- -ceiling(-min(oo_filter_depth$depth)/100)*100
BT_threshold30 <- -150


# Espèces "profondes" (à mettre en lien avec les listes d'espèces "côtières"
# et "profondes" de data_biologic)
Op <- Os[which(unlist(bathy_threshold, use.names = F) > 500)]
Op %>% lapply(dim)
Op %>% lapply(\(tb) {
  u <- tb %>%
    st_drop_geometry() %>%
    select(depth) %>%
    unlist(use.names = F)
  summary(u)
})
# Toutes les moyennes des profondeurs pour les espèces qui ont les quantiles 10%
# les plus profonds sont aux alentours de quelques centaines de m
# Coralliophila aedonia est particulièrement

# Écriture d'un seul raster de profondeurs pour les climatologies
# à partir du troisième quartile des profondeurs à 75% de toutes les espèces
# Dossier de sortie
# pb <- here("data", "tidy", "bathymetrie_gebco_raster")
# makeMyDir(pb)
#
# file_name <- paste(
#   # "bathymetry", "gebco", "raster", BT %>% paste0("m"), sep = "_"
#   "bathymetry", "gebco", "raster", 150 %>% paste0("m"), sep = "_"
# ) %>%
#   paste0(".tif")
# bool <- !file.exists(here(pb, file_name))
#
# if (bool) {
#
#   # Nettoyage de la mémoire vive
#   gc()
#
#   # sélection de l'intervalle bathymétrique
#   gebco_s2[
#     # gebco_s2["depth", , ] < -BT | gebco_s2["depth", , ] > 0
#     gebco_s2["depth", , ] < -150 | gebco_s2["depth", , ] > 0
#   ] <- NA
#
#   # Sauvegarde
#   write_stars(gebco_s2, here(pb, file_name))
#
# }

# Nouvelle importation
gebco_rast <- here(
  "data", "tidy", "bathymetrie_gebco_raster",
  "bathymetry_gebco_raster_150m.tif"
) %>% rast()

# filtre selon les profondeurs pour les climatologies de salinité
seuils <- list(
  c(0, -9.573),
  c(-9.573, -25.2114),
  c(-25.2114, -77.8539),
  c(-77.8539, -130.666),
  # c(-130.666, -BT)
  c(-130.666, -150)
)
names(seuils) <- paste0("lvl", 0:4)

# bathy_slices <- lapply(
#   seuils,
#   \(bnd) {
#     # bnd <- seuils$lvl0
#     rout <- ifel(
#       gebco_rast <= bnd[[1]] & gebco_rast > bnd[[2]], 1, NA
#     )
#     return(rout)
#   }
# )
# names(bathy_slices) <- paste0("lvl", 0:4)
# bathy_slices$lvl0 %>% lapply(\(x) {x11() ; plot(x)})

# sauvegarde
# saveRDS(
#   bathy_slices,
#   here(
#     "data",
#     "tidy",
#     "bathymetrie_gebco_raster",
#     paste(
#       "bathymetry", "gebco", "raster", "slices",
#       # BT %>% paste0("m"),
#       150 %>% paste0("m"),
#       # tolower(superfamily), tolower(species),
#       sep = "_") %>%
#       paste0(".rds")
#   )
# )
# lapply(
#   names(bathy_slices)[5],
#   \(nr) {
#     writeRaster(
#       bathy_slices[[nr]],
#       here(
#         "data",
#         "tidy",
#         "bathymetrie_gebco_raster",
#         paste(
#           "bathymetry", "gebco", "raster",
#           paste(seuils[[nr]], collapse = "_") %>% paste0("m"),
#           # tolower(superfamily), tolower(species),
#           sep = "_") %>%
#           paste0(".tif")
#       )
#     )
#   }
# )
saveRDS(
  # O,
  OO_filter_depth,
  here(
    "data", "tidy",
    paste(
      "occurrences", "with", "depths",
      # bathy_threshold %>% paste0("m"),
      150 %>% paste0("m"),
      # tolower(superfamily), tolower(species),
      sep = "_") %>%
      paste0(".rds")
  )
)
