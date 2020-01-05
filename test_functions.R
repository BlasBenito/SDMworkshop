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
  variable = europe2000[["bio1"]],
  option = "B",
  opacity = 0.7
  )

#plotting points
data(virtualSpecies)
plotRaster(
  x = europe2000[["bio1"]],
  option = "B",
  opacity = 0.7,
  points.x = virtualSpecies$observed.presence$x,
  points.y = virtualSpecies$observed.presence$y,
  points.size = 5
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
  max.n = 200,
  species.type = "multiplicative",
  seed = NULL
  )


# 4 testing autoVIF ----------------------------------------------------
library(SDMworkshop)
library(raster)
data("europe2000")
df <- as.data.frame(europe2000)
vif.output <- autoVIF(x = df)
vif.output <- autoVIF(x = df, try.to.keep = c("bio5", "bio6", "bio12", "ndvi_minimum", "topo_diversity", "biolog"))
vif.output <- autoVIF(x = df, try.to.keep = c("bio5", "bio6", "bio12", "ndvi_minimum", "topo_diversity", "biolog", "artro"))
vif.output <- autoVIF(x = df, try.to.keep = c("bio5", "bio6", "bio12", "bio1", "ndvi_minimum", "topo_diversity"), verbose = TRUE)

#example
data("europe2000")
df <- raster::as.data.frame(europe2000[[c("bio1", "bio5", "bio6", "bio11")]])
selected.vars <- autoVIF(
  x = df,
  try.to.keep = NULL,
  verbose = TRUE
  )
selected.vars

selected.vars <- autoVIF(
  x = df,
  try.to.keep = c("bio5", "bio6"),
  verbose = TRUE
)
selected.vars

# 5 testing corPB -------------------------------------------------------
data(virtualSpeciesPB)
cPB <- corPB(
x = virtualSpeciesPB,
presence.column = "presence",
variables = c("bio1", "bio5", "bio6")
)
cPB

#autoVIF can also take the output of corPB
#as try.to.keep argument, as follows:
data(virtualSpeciesPB)
data("europe2000")
df <- raster::as.data.frame(europe2000[[c("bio1", "bio5", "bio6", "bio11", "bio12")]])

cPB <- SDMworkshop::corPB(
x = virtualSpeciesPB,
presence.column = "presence",
variables = c("bio1", "bio5", "bio6", "bio11", "bio12")
)

#note that cPB$df$variable is ordered from
#higher to lower biserial correlation
#higher biserial correlation is linked
#to higher predictive importance
selected.vars <- SDMworkshop::autoVIF(
 x = df,
 try.to.keep = cPB$df$variable,
 verbose = TRUE
)
selected.vars


# 5 testing weightPB -------------------------------------------------------
data("virtualSpeciesPB")
weights <- weightPB(x = virtualSpeciesPB$presence)
table(weights)

weights <- weightPB(x = c(0, 1, 0, 1))

# 5 testing autocor -------------------------------------------------------
data("virtualSpecies")
data(europe2000)
sp.cor <- testSpatialCorrelation(
  xy = virtualSpecies$observed.presence,
  variables = europe2000
  )
sp.cor

# 5 testing autocor -------------------------------------------------------
data("virtualSpecies")
data(europe2000)
sp.cor <- reduceSpatialCorrelation(
  xy = virtualSpecies$observed.presence,
  variables = europe2000,
  minimum.distance = 3,
  random.start = TRUE,
  seed = NULL,
  verbose = TRUE
)
sp.cor
nrow(virtualSpecies$observed.presence)
nrow(sp.cor)

plotRaster(
  variable = europe2000[["bio1"]],
  points.x = virtualSpecies$observed.presence$x,
  points.y = virtualSpecies$observed.presence$y
)

plotRaster(
  variable = europe2000[["bio1"]],
  points.x = sp.cor$x,
  points.y = sp.cor$y
)
