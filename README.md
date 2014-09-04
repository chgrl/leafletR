<img src="leafletR_logo.png" alt="leafletR" />

An R package to create interactive web-maps based on the Leaflet JavaScript library

[![Build Status](https://api.travis-ci.org/chgrl/leafletR.png)](https://travis-ci.org/chgrl/leafletR)

Display your spatial data on interactive web-maps using the open-source JavaScript library https://github.com/Leaflet/Leaflet. The package provides basic web-mapping functionality to combine vector data files and online map tiles from different sources.

Official release on CRAN: http://cran.r-project.org/package=leafletR

Feel free to flattr, if you like leafletR: <a href="https://flattr.com/submit/auto?user_id=chgrl&amp;url=https%3A%2F%2Fgithub.com/chgrl/leafletR" target="_blank"><img src="http://api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0" /></a>

#### Install from GitHub
```
if(!require(devtools)) install.packages('devtools')
devtools::install_github('leafletR', 'chgrl')
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
  base.map="mqsat", style=q.style, popup="mag")

# view map in browser
browseURL(q.map)
```


