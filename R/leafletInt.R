leafletInt <-
function(dat, path, title, size, base.map, center, zoom, style, popup, incl.data) {
  # Basemap
  basemaps <- list(
    osm = list(
      name = "OpenStreetMap",
      url = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
      options = list(
        attribution='&copy; <a href="http://openstreetmap.org/copyright", target="_blank">OpenStreetMap contributors</a>'
      )
    ),
    tls = list(
      name= "Thunderforest Landscape",
      url = "http://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png",
      options = list(
        attribution='Tiles &copy; <a href="http://thunderforest.com", target="_blank">Thunderforest</a>, Map data &copy; <a href="http://openstreetmap.org/copyright", target="_blan\">OpenStreetMap contributors</a>'
      )
    ),
    mqosm = list(
      name = "MapQuest OSM",
      url = "http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png",
      options = list(
        subdomains= '1234',
        type= 'osm',
        attribution='Tiles &copy; <a href=\"http://www.mapquest.com\", target=\"_blank\">MapQuest</a>, Map data &copy; <a href=\"http://openstreetmap.org/copyright\", target=\"_blank\">OpenStreetMap contributors</a>'
      )
    ),
    mqsat = list(
      name = "MapQuest Open Aerial",
      url = "http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png",
      options = list(
        subdomains= '1234',
        type= 'sat',
        maxZoom= 11,
        attribution='Tiles &copy; <a href=\"http://www.mapquest.com\", target=\"_blank\">MapQuest</a>, Imagery &copy; NASA/JPL-Caltech and USDA Farm Service Agency'
      )
    ),
    water = list(
      name = "Stamen Watercolor",
      url = "http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png",
      options = list(
        attribution='Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>'
      )
    ),
    toner = list(
      name = "Stamen Toner",
      url = "http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png",
      options = list(
        attribution='Tiles &copy; <a href=\"http://stamen.com\", target=\"_blank\">Stamen Design</a> (<a href=\"http://creativecommons.org/licenses/by/3.0\", target=\"_blank\">CC BY 3.0</a>), Map data &copy; <a href=\"http://openstreetmap.org\", target=\"_blank\">OpenStreetMap</a> (<a href=\"http://creativecommons.org/licenses/by-sa/3.0\", target=\"_blank\">CC BY SA</a>)'
      )
    )
  )
  
  # Popups
  if (! is.list(popup)) {
    popup <- list(popup)
  }
  
  #include data
  
  
  
  brew(system.file("files/template.brew", package = "leafletR"), path) 
}
