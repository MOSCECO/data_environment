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
lien_motu  <- "https://my.cmems-du.eu/motu-web/Motu"
service_id <- "GLOBAL_MULTIYEAR_PHY_001_030-TDS"
product_id <- "cmems_mod_glo_phy_my_0.083_P1D-m"
that_var   <- "so"
variables  <- c("so")

# depths
# depths <- c(9.5730, 25.2114, 77.8539, 130.6660) %>% as.character()
depths <- c(0.494025) %>% as.character()

# dates de téléchargement
date_min <- "2012-01-01" %>% as.Date()
date_max <- "2017-01-01" %>% as.Date()
date_seq <- seq(date_min, date_max, by = "quarter")
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
bboxes <- do.call(
  rbind, 
  lapply(
    names(occ), 
    \(supfam) {
      bbxs <- lapply(
        occ[[supfam]], 
        \(sf) {
          st_bbox(sf)
        }
      )
      do.call(rbind, bbxs)
    }
  )
) %>% as_tibble()

global_bbox <- st_bbox(
  c(
    xmin = min(bboxes$xmin),
    ymin = min(bboxes$ymin),
    xmax = max(bboxes$xmax),
    ymax = max(bboxes$ymax)
  )
)
global_bbox_chara <- global_bbox %>% as.character()

# ggplot() + 
#   geom_sf(data = wrld_sf %>% st_crop(global_bbox))

lapply(
  depths, 
  \(dpth) {
    apply(
      date_cmb, 
      1, 
      \(tup) {
        paste("date min", date_min <- tup[[1]])
        paste("date_max", date_max <- tup[[2]])
        
        # nom du fichier de destination
        file_name <- paste(
          database, paste0(that_var, dpth), date_min, date_max, 
          paste0(global_bbox_chara, collapse = "_"),
          sep = "_"
        ) %>% paste0(".nc")
        
        print(file_name)
        
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
              depth.min     = dpth, 
              depth.max     = dpth, 
              out.dir       = penvvar,
              out.name      = file_name
            )
          
          tryCatch(
            {
              CMEMS.download(configuration)
            },
            error = \(e) e
          )
        }
      }
    )
  }
)