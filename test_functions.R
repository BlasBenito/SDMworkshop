# 1 testing importASC ---------------------------
library(raster)
x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = "+init=epsg:4326",
  to.memory = FALSE
  )

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = NULL,
  to.memory = FALSE
)

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = "unknown",
  to.memory = FALSE
)

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = NA,
  to.memory = FALSE
)

x <- importASC(
  folder = "/home/blas/Dropbox/TEACHING/CURSOS_MDE_GBIF/NicheModellingGBIF_2019/taller1/prepara_presencias_y_variables/1_variables",
  crs = "+init=epsg:4326",
  to.memory = TRUE
)

rm(x)


# 2 testing plotRaster ----------------------------------------------------
library(SDMworkshop)
data("europe2000")
plotRaster(
  x = europe2000[["bio1"]],
  option = "B",
  opacity = 0.7
  )


# 3 testing makeVirtualSpecies --------------------------------------------
library(SDMworkshop)
data("europe2000")

niche.parameters <- list(
  bio12 = c(500, 250),
  bio5 = c(240, 50),
  bio6 = c(10, 30),
  human_footprint = c(0, 30),
  topo_slope = c(0, 2),
  landcover_veg_herb = c(100, 35)
)

vs <- makeVirtualSpecies(
  variables = europe2000,
  niche.parameters = NULL,
  n = 200
  )

