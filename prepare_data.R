
#variables present
load("data_old/presencia_y_variables.RData")
variables <- variables$brick
projection(variables)
names(variables)
res(variables)
europe2000 <- variables
save(europe2000, file = "data/europe2000.RData")

#variables past
europe21kBP <- brick(stack(list.files(path="data_old/4_proyeccion_temporal/europe21kBP",pattern='*.asc', full.names=TRUE)))
save(europe21kBP, file = "data/europe21kBP.RData")

#presence
sp$xy #keep this
sp$nicho.dimensiones #keep this
sp$nicho.parametros #keep this, but turn into dataframe
sp$nicho.mapa$probability.of.occurrence #keep this or the previous one
sp$nicho.mapa$nicho.plot #keep this

nicho.parametros <- t(as.data.frame(sp$nicho.parametros))
colnames(nicho.parametros) <- c("mean", "sd")


virtualSpecies <- list()
virtualSpecies$niche.dimensions <- sp$nicho.dimensiones
virtualSpecies$niche.parameters <- nicho.parametros
virtualSpecies$niche.plot <- sp$nicho.mapa$nicho.plot
virtualSpecies$suitability <- sp$nicho.mapa$suitab.raster
virtualSpecies$observed.presence <- sp$xy
save(virtualSpecies, file = "data/virtualSpecies.RData")
