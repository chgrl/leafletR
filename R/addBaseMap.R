addBaseMap <- function(name, title, url, options) {
  # get existing base maps
  baseMaps <-  getOption("leafletBaseMaps")
  
  # create base map
  if(missing(title)) title <- name
  newBaseMap <- list(title=title, url=url)
  if(missing(options)) options <- NULL
  newBaseMap$options <- options
  
  # add base map
  baseMaps[[name]] <- newBaseMap
  options(leafletBaseMaps=baseMaps)
}
