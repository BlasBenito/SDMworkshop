
#variables present
load("data_old/presencia_y_variables.RData")
variables <- europe2000
projection(variables)
names(variables)
res(variables)
europe2000 <- variables
save(europe2000, file = "data/europe2000.RData")

#variables past
europe21kBP <- brick(stack(list.files(path="data_old/4_proyeccion_temporal/europe21kBP",pattern='*.asc', full.names=TRUE)))
save(europe21kBP, file = "data/europe21kBP.RData")

#presence
virtualSpecies$xy #keep this
virtualSpecies$nicho.dimensiones #keep this
virtualSpecies$nicho.parametros #keep this, but turn into dataframe
virtualSpecies$nicho.mapa$probability.of.occurrence #keep this or the previous one
virtualSpecies$nicho.mapa$nicho.plot #keep this

nicho.parametros <- t(as.data.frame(virtualSpecies$nicho.parametros))
colnames(nicho.parametros) <- c("mean", "sd")

#virtual species
virtualSpecies <- list()
virtualSpecies$niche.dimensions <- virtualSpecies$nicho.dimensiones
virtualSpecies$niche.parameters <- nicho.parametros
virtualSpecies$niche.plot <- virtualSpecies$nicho.mapa$nicho.plot
virtualSpecies$suitability <- virtualSpecies$nicho.mapa$suitab.raster
virtualSpecies$observed.presence <- virtualSpecies$xy
save(virtualSpecies, file = "data/virtualSpecies.RData")


#presence table
#################
source("funciones.R")
data(virtualSpecies)
data(europe2000)

#thinning presence data
xy <- thinning(
  xy = virtualSpecies$observed.presence,
  brick = europe2000,
  separacion = 2
)

#valores de las variables para virtualSpecies$xy
xy <- data.frame(
  xy,
  raster::extract(
    x = europe2000,
    y = xy,
    df = TRUE,
    cellnumbers = FALSE
  )
)
xy$ID <- NULL

#añadimos columna de presencia (presencia = 1)
xy$presence <- 1

#generating background
background <- data.frame(
  dismo::randomPoints(
    mask = europe2000,
    n = nrow(na.omit(as.data.frame(europe2000))) / 5
    )
  )


#keeping min-max
for(variable in names( nrow(na.omit(as.data.frame(europe2000))))){

  #buscamos las coordenadas de la celda con el menor valor
  xy.min <- raster::xyFromCell(object = europe2000,
                               cell = raster::which.min(europe2000[[variable]])[1]
  )
  #buscamos las coordenadas de la celda con el mayor valor
  xy.max <- raster::xyFromCell(object = europe2000,
                               cell = raster::which.max(europe2000[[variable]])[1]
  )

  #las unimos al background
  background <- rbind(background, xy.min, xy.max)
}

#eliminamos duplicados
background <- background[!duplicated(background), ]

#vemos el background
plotPresencia(
  brick = europe2000,
  variable = "human_footprint",
  lon = background$x,
  lat = background$y
)

#le añadimos los valores de las variables
background <- data.frame(
  background,
  raster::extract(
    x = europe2000,
    y = background,
    df = TRUE,
    cellnumbers = FALSE
  )
)
background$ID <- NULL

#añadimos columna de presencia (presencia = 1)
background$presence <- 0

presenceBackground <- rbind(xy, background)

save(presenceBackground, file = "data/presenceBackground.RData")
