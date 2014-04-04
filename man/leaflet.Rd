\encoding{UTF-8}
\name{leaflet}
\alias{leaflet}
\alias{leaf}
\title{Create a Leaflet web-map}
\description{Creates a web-map of users' spatial data over open-source base maps. Output consists of a ready to use HTML file and a GeoJSON data file.}
\usage{
leaflet(data, dest, title, size, base.map="osm", center, 
  zoom, style, popup, incl.data=FALSE, overwrite=TRUE)
leaf(data, dest, title, size, base.map="osm", center, 
  zoom, style, popup, incl.data=FALSE, overwrite=TRUE)
}
\arguments{
  \item{data}{Name(s) of data file(s) (GeoJSON format), as string or a list of strings. Plotting order follows the file sequence.}
  \item{dest}{Path to the \code{data} file, as string. Optional -- if missing, the current working directory is used.}
  \item{title}{Map title, as string. Default is \code{"map"}.}
  \item{size}{Size of the map on the website in pixels, as numeric vector -- \code{c(width, height)}. Optional -- if missing, a fullscreen (browser window) map is generated.}
  \item{base.map}{Base map(s) in the background of the data, as string. One or a list of \code{"osm"} (OpenStreetMap standard map), \code{"tls"} (Thunderforest Landscape), \code{"cm"} (CloudMade), \code{"mqosm"} (MapQuest OSM), \code{"mqsat"} (MapQuest Open Aerial), \code{"water"} (Stamen Watercolor) or \code{"toner"} (Stamen Toner). Default is \code{"osm"}. If \code{base.map} is a list, the last item is used as default base map and a layer control button is added to the map.}
  \item{center}{Map center coordinates in decimal degrees, as vector of two numeric values: \code{c(latitude, longitude)}. Optional -- if missing, the data layer is centered automatically.}
  \item{zoom}{Map zoom level, as integer value. Usually a value between \code{0} (global small scale) and \code{18} (detailed large scale). The MapQuest Open Aerial map (\code{base.map="mqsat"}) provides only 12 zoom levels [0-11]. Optional -- if missing, the zoom level is calculated for the bounding box of the data layer.}
  \item{style}{Style(s) of the data layer(s). One or a list of style object(s), created by \code{\link{styleSingle}}, \code{\link{styleGrad}} or \code{\link{styleCat}}. Optional -- if missing, a default style is applied.}
  \item{popup}{Properties (attributes) of the data to be shown in a popup when a map object is clicked. String or a vector of strings. \code{"*"} adds all available properties to the popup (\href{http://cran.r-project.org/package=RJSONIO}{RJSONIO} package required). A \code{list} of (vectors of) strings specifies properties for multiple \code{data} layers. Per default no popups are shown.}
  \item{incl.data}{If \code{TRUE}, \code{data} is included in the HTML file itself. Per default (\code{incl.data=FALSE}) the data is saved in a separate file. Including data in the HTML file allows for viewing the map locally on Chrome and Opera browsers.}
  \item{overwrite}{\code{TRUE} (which is the default) overwrites existing files with the same name.}
}
\note{
Please note: \code{data} only accepts GeoJSON files with one geometry type and geographical coordinates (longlat, WGS84). 
}
\references{
Base map tiles are provided by 
\tabular{lll}{
 \tab OpenStreetMap standard map \tab \url{http://www.openstreetmap.org} \cr
 \tab Thunderforest Landscape \tab \url{http://www.thunderforest.com} \cr
 \tab CloudMade \tab \url{http://cloudmade.com} \cr
 \tab MapQuest OSM \tab \url{http://www.mapquest.com} \cr
 \tab MapQuest Open Aerial \tab \url{http://www.mapquest.com} \cr
 \tab Stamen Watercolor \tab \url{http://stamen.com} \cr
 \tab Stamen Toner \tab \url{http://stamen.com}
}
}
\value{HTML file path, as string.}
\author{Christian Graul}
\seealso{\code{\link{styleSingle}}, \code{\link{styleGrad}}, \code{\link{styleCat}}}
\examples{
\dontrun{
# prepare data
data(quakes)
dat <- toGeoJSON(data=quakes, dest=tempdir())

# create map quick and simple
map <- leaflet(dat)
browseURL(map)

# set output directory and map title
map <- leaflet(data=dat, dest=tempdir(), title="Fiji Earthquakes")
browseURL(map)

# set map size, center and zoom level
map <- leaflet(data=dat, dest=tempdir(), 
  size=c(800,600), center=c(-18.35, 179.75), zoom=6)
browseURL(map)

# set base map and popup
# magnitude is used as popup (type names(quakes) for available properties)
map <- leaflet(data=dat, dest=tempdir(), 
  base.map="mqsat", popup="mag")
browseURL(map)

# include data in HTML file
map <- leaflet(dat, incl.data=TRUE)
browseURL(map)

# preserve existing files from overwriting
map <- leaflet(dat, overwrite=FALSE)

# more than one base map
map <- leaflet(data=dat, dest=tempdir(), 
  base.map=list("osm", "mqsat", "tls"))
browseURL(map)

# multiple properties in the popup
map <- leaflet(data=dat, dest=tempdir(), 
  popup=c("mag", "depth"))
browseURL(map)

# all available properties in the popup
map <- leaflet(data=dat, dest=tempdir(), 
  popup="*")
browseURL(map)

# change style
sty <- styleSingle(col="red", fill=NA)
map <- leaflet(data=dat, dest=tempdir(), base.map="mqsat", style=sty)
browseURL(map)

# more than one data set
park <- toGeoJSON(data=system.file(package="leafletR", "files", 
  "park_sk.zip"), dest=tempdir())
peak <- toGeoJSON(system.file(package="leafletR", "files", "peak_sk.kml"), 
  dest=tempdir())
sty.1 <- styleSingle(col="green", fill="green")
sty.2 <- styleSingle(col="brown", fill="brown", rad=3)
map <- leaflet(data=list(park, peak), dest=tempdir(), 
  style=list(sty.1, sty.2), popup=list("*", "Name"))
browseURL(map)
}
}
\keyword{methods}
