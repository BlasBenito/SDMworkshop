makeVirtualSpecies <- function(variables, niche.parameters = NULL, n = 100){

  #carga librerías
  require(raster)
  require(virtualspecies)
  require(viridis)
  require(ggplot2)
  require(cowplot)
  require(tidyr)
  require(dplyr)
  require(HH)
  ggplot2::theme_set(cowplot::theme_cowplot())

  #variables brick to dataframe
  variables.df <-
    variables %>%
    raster::as.data.frame() %>%
    na.omit()

  #if niche.parameters is null, creates a random definition of niche parameters
  if(is.null(niche.parameters) == TRUE){

    #gets random niche dimensions
    variables.names <- names(variables)
    niche.dimensions <- sample(x = variables.names, sample(x = 2:floor(length(variables.names)/2)))

    #applies automatic vif to niche dimensions to keep uncorrelated ones
    repeat {
      vif.result <- data.frame(
        HH::vif(variables.df[, niche.dimensions]),
        stringsAsFactors = FALSE
        )
      vif.result$variable <- rownames(vif.result)
      colnames(vif.result)[1] <- "vif"
      vif.result <- vif.result[order(vif.result$vif, decreasing=TRUE), ]
      if (vif.result[1, "vif"] > 5){
        niche.dimensions <- niche.dimensions[niche.dimensions != vif.result[1, "variable"]]
        if(length(niche.dimensions) < 2){
          break
        }
      } else {
        break
      }
    }


    #list to store niche parameters
    niche.parameters <- list()

    #iterates through niche dimensions
    for(i in niche.dimensions){

      #computes random mean and random sd for the niche definition
      niche.parameters[[i]] <-
        c(
          sample(x = seq(quantile(variables.df[,i], 0.1), quantile(variables.df[,i], 0.9), length.out = 1000), size = 1),
          (max(variables.df[,i]) - min(variables.df[,i])) / sample(x = seq(2, 20, by = 0.1), size = 1)
        )
    }

  }

  #getting niche dimensions
  niche.dimensions <- names(niche.parameters)

  #keeping niche dimensions only
  variables.df <- variables.df[, niche.dimensions]

  #applies niche parameters to the values of the niche dimensions
  #to generate response curves
  variables.df.nrow <- nrow(variables.df)
  response.curves <- list()
  for(i in niche.dimensions){
    response.curves[[i]] <- rnorm(
      variables.df.nrow,
      mean = niche.parameters[[i]][1],
      sd = niche.parameters[[i]][2]
    )
  }

  #from list to data frame
  response.curves <- data.frame(do.call("cbind", response.curves)) %>%
    tidyr::pivot_longer(
      cols = niche.dimensions,
      names_to = "variable",
      values_to = "value"
    ) %>%
    data.frame()

  #variables.df to long
  variables.df.long <-
    variables.df %>%
    tidyr::pivot_longer(
      cols = niche.dimensions,
      names_to = "variable",
      values_to = "value"
    ) %>%
    data.frame()

  #plots density of the variables
  plot.use.availability <- ggplot(
    data = variables.df.long,
    aes(
      x = value,
      group = variable
    )
  ) +
    geom_density(
      fill = viridis::viridis(1, begin = 0.99),
      alpha = 1,
      size = 0
    ) +
    facet_wrap("variable", scales = "free") +
    geom_density(
      data = response.curves,
      aes(
        x = value,
        group = variable
      ),
      fill = viridis::viridis(1, begin = 0.3),
      alpha = 0.5,
      size = 0
    )

  #wrapper for virtualspecies::formatFunctions
  args <- list()
  for(i in 1:length(niche.parameters)){
    args[[i]] <- c(
      fun = "dnorm",
      mean = niche.parameters[[i]][1],
      sd = niche.parameters[[i]][2]
      )
  }
  names(args) <- names(niche.parameters)

  #generating the details file for virtualspecies::generateSpFromFun
  details <- list()
  for (i in names(args)){
    details[[i]]$fun <- args[[i]]["fun"]
    args[[i]] <- args[[i]][-(names(args[[i]]) %in% "fun")]
    details[[i]]$args <- as.list(args[[i]])
    details[[i]]$args <- sapply(details[[i]]$args, as.numeric)
  }

  #generating niche map from details
  virtual.species.temp <- virtualspecies::generateSpFromFun(
    raster.stack = variables[[niche.dimensions]],
    parameters = details,
    rescale = TRUE
  )

  #raster to dataframe for ggplotting
  niche.map.df <-
    as.data.frame(virtual.species.temp$suitab.raster, xy = TRUE) %>%
    na.omit()

  #computes initial prevalence
  prevalence <- sum(niche.map.df$layer) / nrow(niche.map.df)

  #initial presence cells
  presence.cells <- 1

  #loop to adjust prevalence
  while(presence.cells < (n * 2)){

    #adds presence-absence to the virtual species
    virtual.species.temp <- virtualspecies::convertToPA(
      PA.method = "probability",
      prob.method = "linear",
      x = virtual.species.temp,
      species.prevalence = prevalence,
      plot = FALSE
    )

    #checking that there are enough presences
    presence.cells <-
      as.data.frame(virtual.species.temp$pa.raster, xy = TRUE) %>%
      na.omit() %>%
      filter(layer == TRUE) %>%
      nrow()

    #increases prevalence
    prevalence <- prevalence + 0.01

  }

  #adjusting n
  if(n > presence.cells){n <- presence.cells}

  #sampling occurrences
  xy <- sampleOccurrences(
    virtual.species.temp,
    n = n,
    type = "presence only",
    correct.by.suitability = TRUE,
    plot = FALSE
  )$sample.points[c("x", "y")]

  #output object
  virtual.species <- list()
  virtual.species$niche.dimensions <- names(niche.parameters)
  virtual.species$niche.parameters <- niche.parameters
  virtual.species$niche.plot <- plot.multipanel
  virtual.species$suitability <- virtual.species.temp$suitab.raster
  virtual.species$observed.presence <- xy

  #plot niche map
  plot.niche.map <- ggplot() +
    geom_tile(
      data = niche.map.df,
      aes(
        x = x,
        y = y,
        fill = layer
        )
      ) +
    viridis::scale_fill_viridis(direction = -1) +
    labs(fill = "Suitability") +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(
      data = xy,
      aes(
        x = x,
        y = y
        ),
      shape = 1
      )

  #multipanel plot
  plot.multipanel <- cowplot::plot_grid(
    plot.use.availability,
    plot.niche.map,
    ncol = 1,
    axis = "l",
    align = "hv")
  print(plot.multipanel)

  return(virtual.species)

}
