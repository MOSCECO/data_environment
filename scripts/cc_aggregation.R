# après problème d'aggrégation des climatologies sur le cluster, code pour
# le regrouper

p <- here("data", "analysis", "climatologies_global")

# tout sauf salinité ----
cg <- sapply(
  # c("mean", "stdv", "mini", "maxi", "qt01", "qt05", "qt95", "qt99"),
  c("mean", "stdv", "mini", "maxi"),
  \(nm) {
    # nm <- "mean"
    sapply(
      list.files(p)[-2],
      \(v) {
        # v <- "bottomt"
        do.call(
          st_mosaic,
          here(p, v) %>%
            list.files(full.names = T, pattern = nm) %>%
            lapply(read_stars)
        )
      },
      simplify = F, USE.NAMES = T)
  },
  simplify = F, USE.NAMES = T)

CG <- sapply(
  list.files(here(p))[-2],
  \(v) {
    out <- cg %>% lapply(pluck, v)
    names(out) <- names(out) %>% paste(v, sep = ".")
    return(out)
  },
  simplify = F,
  USE.NAMES = T
)

CG <- lapply(CG, \(l) do.call(c, l))

lapply(
  names(CG),
  \(v) {
    CLIM_mosaic <- CG[[v]]
    # sauvegarde
    write_stars(
      CLIM_mosaic,
      dsn = here(
        p, v,
        paste("climatologies", "globales", v, sep = "_") %>%
          paste0(".tif")
      )
    )

    saveRDS(
      CLIM_mosaic,
      here(
        p, v,
        paste("climatologies", "globales", v, sep = "_") %>%
          paste0(".rds")
      )
    )

  })

# salinité ----
cg <- sapply(
  # c("mean", "stdv", "mini", "maxi", "qt01", "qt05", "qt95", "qt99"),
  c("mean", "stdv", "mini", "maxi"),
  \(nm) {
    # nm <- "mean"
    sapply(
      list.files(here(p, "so"))[c(1, 5, 3, 4, 2)],
      \(v) {
        # v <- "so0.49"
        do.call(
          st_mosaic,
          here(p, "so", v) %>%
            list.files(full.names = T, pattern = nm) %>%
            lapply(read_stars)
        )
      },
      simplify = F, USE.NAMES = T)
  },
  simplify = F, USE.NAMES = T)

CG <- sapply(
  list.files(here(p, "so"))[c(1, 5, 3, 4, 2)],
  \(v) {
    out <- cg %>% lapply(pluck, v)
    names(out) <- names(out) %>% paste(v, sep = ".")
    return(out)
  },
  simplify = F,
  USE.NAMES = T
)

CG <- lapply(CG, \(l) do.call(c, l))

lapply(
  names(CG),
  \(v) {
    CLIM_mosaic <- CG[[v]]
    # sauvegarde
    write_stars(
      CLIM_mosaic,
      dsn = here(
        p, "so",
        paste("climatologies", "globales", v, sep = "_") %>%
          paste0(".tif")
      )
    )

    saveRDS(
      CLIM_mosaic,
      here(
        p, "so",
        paste("climatologies", "globales", v, sep = "_") %>%
          paste0(".rds")
      )
    )

  })
