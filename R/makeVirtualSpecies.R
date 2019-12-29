makeVirtualSpecies <- function(variables, niche.parameters = NULL, seed = NULL, species.type = c("additive", "multiplicative"), max.n = 200){

  #carga librerías
  require(raster)
  require(virtualspecies)
  require(viridis)
  require(cowplot)
  require(tidyverse, warn.conflicts = FALSE)
  ggplot2::theme_set(cowplot::theme_cowplot())

  #setting random seed
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  #variables brick to dataframe
  variables.df <-
    variables %>%
    raster::as.data.frame() %>%
    na.omit()

  #if niche.parameters is null, creates a random definition of niche parameters
  if(is.null(niche.parameters) == TRUE){

    #gets random niche dimensions
    variable.names <- variables@data@names
    if(length(variable.names) > 2){
      niche.dimensions <- sample(
        x = variable.names,
        sample(x = 2:floor(length(variable.names)/2))
        )
    } else {
      niche.dimensions <- variable.names
    }

    #applies automatic vif to niche dimensions to keep uncorrelated ones
    niche.dimensions <- autoVIF(x = variables.df[, niche.dimensions])

    #list to store niche parameters
    niche.parameters <- list()

    #iterates through niche dimensions
    for(i in niche.dimensions){

      #computes random mean and sd for the niche function
      #mean is selected between quantiles 0.1 and 0.9 of the range of each niche dimension
      #sd takes a value between the 0.5 and 0.05 of the range of the variable.
      niche.parameters[[i]] <-
        c(
          sample(x = seq(quantile(variables.df[,i], 0.1), quantile(variables.df[,i], 0.9), length.out = 1000), size = 1),
          (max(variables.df[,i]) - min(variables.df[,i])) / sample(x = seq(2, 20, by = 0.1), size = 1)
        )
    }

  }#end of random species

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

  #generates the functions definitions for virtualspecies::generateSpFromFun
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
    rescale = TRUE,
    species.type = species.type
  )

  #raster to dataframe for ggplotting
  niche.map.df <-
    as.data.frame(virtual.species.temp$suitab.raster, xy = TRUE) %>%
    na.omit()

  #computes initial prevalence
  # prevalence <- sum(niche.map.df$layer) / nrow(niche.map.df)

  #initial presence cells
  presence.cells <- 1

  #loop to adjust prevalence
  # while(presence.cells < (n * 2)){

    #adds presence-absence to the virtual species
    # virtual.species.temp <- virtualspecies::convertToPA(
    #   x = virtual.species.temp,
    #   PA.method = "probability",
    #   prob.method = "logistic",
    #   alpha = -0.01,
    #   species.prevalence = prevalence,
    #   plot = FALSE
    # )

    virtual.species.temp <- virtualspecies::convertToPA(
      x = virtual.species.temp,
      PA.method = "probability",
      prob.method = "linear",
      a = 1,
      b = 0,
      plot = FALSE
    )

    #checking that there are enough presences
    presence.cells <-
      as.data.frame(virtual.species.temp$pa.raster, xy = TRUE) %>%
      na.omit() %>%
      filter(layer == TRUE) %>%
      nrow()

  #   #increases prevalence
  #   prevalence <- prevalence + 0.01
  #
  # }

  #adjusting n
  if(max.n > presence.cells){max.n <- presence.cells}

  #sampling occurrences
  xy <- sampleOccurrences(
    virtual.species.temp,
    n = max.n,
    type = "presence only",
    correct.by.suitability = TRUE,
    plot = FALSE
  )$sample.points[c("x", "y")]

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

  #output object
  virtual.species <- list()
  virtual.species$niche.dimensions <- names(niche.parameters)
  virtual.species$niche.parameters <- niche.parameters
  virtual.species$niche.plot <- plot.multipanel
  virtual.species$suitability <- virtual.species.temp$suitab.raster
  virtual.species$observed.presence <- xy

  return(virtual.species)

}
