leafletInt <-
function(dat, path, title, size, base.map, center, zoom, style, popup, incl.data) {
  
  
  basemaps <- getOption("leafletBaseMaps")
  # Popups
  if (! is.list(popup)) {
    popup <- list(popup)
  }  
  brew(system.file("templates/main.brew", package = "leafletR"), path) 
}
