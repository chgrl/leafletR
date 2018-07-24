<img src="leafletR_logo.png" alt="leafletR" />

An R package to create interactive web-maps based on the Leaflet JavaScript library

[![Build Status](https://api.travis-ci.org/chgrl/leafletR.png)](https://travis-ci.org/chgrl/leafletR)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/leafletR)

Display your spatial data on interactive web-maps using the open-source JavaScript library https://github.com/Leaflet/Leaflet. The package provides basic web-mapping functionality to combine vector data files and online map tiles from different sources.

Official release on CRAN: http://cran.r-project.org/package=leafletR

#### Install from GitHub
```
devtools::install_github("chgrl/leafletR")
```

#### Make a map
```
library(leafletR)

# load example data (Fiji Earthquakes)
data(quakes)

# store data in GeoJSON file (just a subset here)
q.dat <- toGeoJSON(data=quakes[1:99,], dest=tempdir(), name="quakes")

# make style based on quake magnitude
q.style <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), 
  style.val=rev(heat.colors(5)), leg="Richter Magnitude", 
  fill.alpha=0.7, rad=8)

# create map
q.map <- leaflet(data=q.dat, dest=tempdir(), title="Fiji Earthquakes", 
  base.map="osm", style=q.style, popup="mag")

# view map in browser
q.map
```


