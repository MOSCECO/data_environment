# téléchargement des données de vagues

# chemin vers le motuclient
path_motuclient <- paste0(
  # "C:/Users/gmaniel04/AppData/Local/Programs/Python/",
  # "Python310/Lib/site-packages/motuclient/motuclient.py"
  "/home/borea/.local/lib/python3.10/site-packages/motuclient.py"
)

# accréditations sur le CMEMS
user_name <- "gmaniel"
password  <- "BahaMut900:Copernicus"

# nom du produit et des variables à télécharger
database   <- "copernicus"
lien_motu  <- "https://nrt.cmems-du.eu/motu-web/Motu"
service_id <- "GLOBAL_ANALYSISFORECAST_WAV_001_027-TDS"
product_id <- "cmems_mod_glo_wav_anfc_0.083deg_PT3H-i"
that_var <- "waves"
variables <- c("VHM0", "VHM0_SW1", "VHM0_WW")

# dates de téléchargement
date_min <- "2020-12-01" %>% as.Date()
date_max <- "2023-01-01" %>% as.Date()
date_seq <- seq(date_min, date_max, by = "weeks")
date_cmb <- date_seq[-length(date_seq)] %>%
  as.character() %>%
  cbind(date_seq[-1] %>% as.character()) %>%
  as_tibble()
names(date_cmb) <- c("date_str", "date_end")
# création dossier de destination
penv <- here(input, database)
makeMyDir(penv)
penvvar <- here(input, database, that_var)
makeMyDir(penvvar)

# emprise maximale
# bboxes <- do.call(
#   rbind,
#   lapply(
#     names(occ),
#     \(supfam) {
#       bbxs <- lapply(
#         occ[[supfam]],
#         \(sf) {
#           st_bbox(sf)
#         }
#       )
#       do.call(rbind, bbxs)
#     }
#   )
# ) %>% as_tibble()
#
# global_bbox <- st_bbox(
#   c(
#     xmin = min(bboxes$xmin),
#     ymin = min(bboxes$ymin),
#     xmax = max(bboxes$xmax),
#     ymax = max(bboxes$ymax)
#   )
# )
# global_bbox_chara <- global_bbox %>% as.character()
global_bbox_chara <- c(-140, -60, 65, 60) %>% as.character()

# ggplot() +
#   geom_sf(data = wrld_sf %>% st_crop(global_bbox))

apply(
  date_cmb,
  1,
  \(tup) {

    paste("date min", date_min <- tup[[1]])
    paste("date_max", date_max <- tup[[2]])

    # nom du fichier de destination
    file_name <- paste(
      database, that_var, date_min, date_max,
      paste0(global_bbox_chara, collapse = "_"),
      sep = "_"
    ) %>% paste0(".nc")

    if(
      !file.exists(here(penvvar, file_name))
    ) {
      configuration <-
        CMEMS.config(
          script        = path_motuclient,
          user          = user_name,
          pwd           = password,
          motu          = lien_motu,
          service.id    = service_id,
          product.id    = product_id,
          variable      = variables,
          date.min      = date_min,
          date.max      = date_max,
          longitude.min = global_bbox_chara[1],
          latitude.min  = global_bbox_chara[2],
          longitude.max = global_bbox_chara[3],
          latitude.max  = global_bbox_chara[4],
          out.dir       = penvvar,
          out.name      = file_name
        )

      CMEMS.download(configuration)
    }
  }
)
