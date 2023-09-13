# libraries
libs_to_call <- list(

  "doParallel",
  "foreach",
  "ggplot2",
  "here",
  "reshape2",
  "RNetCDF",
  "sf",
  "stars",
  "terra",
  NULL

)

lapply(
  libs_to_call,

  function(i) {

    if (!is.null(i)) {

      bool <- is.element(i, .packages(all.available = TRUE))

      if (!bool) {
        install.packages(i)
      }

      library(i, character.only = TRUE)
    }

  }
)
