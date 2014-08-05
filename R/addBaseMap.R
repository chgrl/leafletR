addBaseMap <- function(shortname, name, url, options) {
  baseMaps <-  getOption("leafletBaseMaps")
  
  newBaseMap <- list(name=name, url=url)
  if (!missing(options)) {
    newBaseMap$options = options
  }
  baseMaps[[shortname]] <- newBaseMap
  options(leafletBaseMaps=baseMaps)
}