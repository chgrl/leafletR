\encoding{UTF-8}
\name{leaflet}
\alias{leaflet}
\alias{leaf}
\title{Create a Leaflet web-map}
\description{Creates a web-map of users' spatial data over open base maps. Output consists of a ready to use HTML file (and a GeoJSON/TopoJSON data file).}
\usage{
leaflet(data, dest, title, size, base.map="osm", center, zoom, 
  style, popup, label, controls="all", incl.data=FALSE, overwrite=TRUE)
leaf(data, dest, title, size, base.map="osm", center, zoom, 
  style, popup, label, controls="all", incl.data=FALSE, overwrite=TRUE)
}
\arguments{
  \item{data}{Name(s) of data file(s) (GeoJSON/TopoJSON format), as string or a list of strings. Plotting order follows the file sequence.}
  \item{dest}{Path to the \code{data} file, as string. Optional -- if missing, the current working directory is used.}
  \item{title}{Map title, as string. Default is \code{"map"}.}
  \item{size}{Size of the map on the website in pixels, as numeric vector -- \code{c(width, height)}. Optional -- if missing, a fullscreen (browser window) map is generated.}
  \item{base.map}{Base map(s) in the background of the data, as string. One or a list of \code{"osm"} (OpenStreetMap standard map), \code{"tls"} (Thunderforest Landscape), \code{"mqosm"} (MapQuest OSM), \code{"mqsat"} (MapQuest Open Aerial), \code{"water"} (Stamen Watercolor), \code{"toner"} (Stamen Toner), \code{"tonerbg"} (Stamen Toner background), \code{"tonerlite"} (Stamen Toner lite), \code{"positron"} (CartoDB Positron) or \code{"darkmatter"} (CartoDB Dark matter). Default is \code{"osm"}. If \code{base.map} is a list, the last item is used as default base map and a layer control button is added to the map.}
  \item{center}{Map center coordinates in decimal degrees, as vector of two numeric values: \code{c(latitude, longitude)}. Optional -- if missing, the data layer is centered automatically. \code{code} has to be specified to use \code{center}.}
  \item{zoom}{Map zoom level, as integer value. Usually a value between \code{0} (global small scale) and \code{18} (detailed large scale). The MapQuest Open Aerial map (\code{base.map="mqsat"}) provides only 12 zoom levels [0-11]. Optional -- if missing, the zoom level is calculated for the bounding box of the data layer. \code{center} has to be specified to use \code{zoom}.}
  \item{style}{Style(s) of the data layer(s). One or a list of style object(s), created by \code{\link{styleSingle}}, \code{\link{styleGrad}} or \code{\link{styleCat}}. Optional -- if missing, a default style is applied.}
  \item{popup}{Properties (attributes) of the data to be shown in a popup when a map object is clicked. String or a vector of strings. \code{"*"} adds all available properties to the popup. A \code{list} of (vectors of) strings specifies properties for multiple \code{data} layers. Per default no popups are shown.}
  \item{label}{Property (attribute) of the data to be shown in a dynamic label, as string. A \code{list} of strings specifies properties for multiple \code{data} layers. Per default no labels are shown. Only point data is supported and markers do not work very well.}
  \item{controls}{List of controls to be added to the map. Available controls are \code{"zoom"}, \code{"scale"}, \code{"layer"} and \code{"legend"}. \code{"all"} (the default) adds all controls. Controls are only added if necessary, e.g. in case of one data layer there is no legend. \code{NA} omits all controls. Note: data layer controls only appear if \code{incl.data} is set to \code{TRUE}.}
  \item{incl.data}{If \code{TRUE}, \code{data} is included in the HTML file itself. Per default (\code{incl.data=FALSE}) the data is saved in a separate file. Including data in the HTML file allows for viewing the map locally on some browsers (e.g. Chrome and Opera).}
  \item{overwrite}{\code{TRUE} (which is the default) overwrites existing files with the same name.}
}
\note{
\code{data} only accepts GeoJSON/TopoJSON files with one geometry type and geographical coordinates (longlat, WGS84).

Due to the security restrictions of some browsers (namely Chrome and Opera) it might be necessary to save the data inside the html file using \code{incl.data = TRUE}.
}
\references{
Base map tiles are provided by 
\tabular{lll}{
 \tab OpenStreetMap standard map \tab \url{http://www.openstreetmap.org} \cr
 \tab Thunderforest Landscape \tab \url{http://www.thunderforest.com} \cr
 \tab MapQuest OSM \tab \url{http://www.mapquest.com} \cr
 \tab MapQuest Open Aerial \tab \url{http://www.mapquest.com} \cr
 \tab Stamen Watercolor \tab \url{http://stamen.com} \cr
 \tab Stamen Toner \tab \url{http://stamen.com} \cr
 \tab Stamen Toner background \tab \url{http://stamen.com} \cr
 \tab Stamen Toner lite \tab \url{http://stamen.com} \cr
 \tab CartoDB Positron \tab \url{http://cartodb.com} \cr
 \tab CartoDB Dark matter \tab \url{http://cartodb.com}
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

# create and view simple map
map <- leaflet(dat, dest=tempdir())
map  # redirects to browseURL(map)

# set output directory and map title
map <- leaflet(data=dat, dest=tempdir(), title="Fiji Earthquakes")

# set map size, center and zoom level
map <- leaflet(data=dat, dest=tempdir(), 
  size=c(800,600), center=c(-18.35, 179.75), zoom=6)

# set base map and popup/label
# magnitude is used as popup (type names(quakes) for available properties)
map <- leaflet(data=dat, dest=tempdir(), 
  base.map="mqsat", popup="stations", label="mag")
  
# minimalist? - no base map
map <- leaflet(data=dat, dest=tempdir(), 
  base.map=NA, popup="mag")

# include data in HTML file
map <- leaflet(dat, dest=tempdir(), incl.data=TRUE)

# preserve existing files from overwriting
map <- leaflet(dat, dest=tempdir(), overwrite=FALSE)

# more than one base map
map <- leaflet(data=dat, dest=tempdir(), 
  base.map=list("osm", "mqsat", "tls"))

# multiple properties in the popup
map <- leaflet(data=dat, dest=tempdir(), 
  popup=c("mag", "depth"))

# all available properties in the popup
map <- leaflet(data=dat, dest=tempdir(), 
  popup="*")

# change style
sty <- styleSingle(col="red", fill=NA)
map <- leaflet(data=dat, dest=tempdir(), base.map="mqsat", style=sty)

# controls
map <- leaflet(data=dat, dest=tempdir(), controls=NA)  # no controls
map <- leaflet(data=dat, dest=tempdir(), controls="scale")  # scale only
map <- leaflet(data=dat, dest=tempdir(), controls=c("zoom", "scale"))

# more than one data set
park <- system.file(package="leafletR", "files", "park_sk.geojson")
peak <- toGeoJSON(system.file(package="leafletR", "files", "peak_sk.kmz"), 
  dest=tempdir())  # httr package required
sty.1 <- styleSingle(col="green", fill="green")
sty.2 <- styleSingle(col="brown", fill="brown", rad=3)
map <- leaflet(data=list(park, peak), dest=tempdir(), 
  style=list(sty.1, sty.2), popup=list("*", "Name"))

# names in legend
# note: "_" and "." are replaced with blanks in the legend 
map <- leaflet(data=list(National_Parks=park, Peaks.above.600.m)=peak), 
  dest=tempdir(), style=list(sty.1, sty.2), popup=list("*", "Name"))
}
}
\keyword{methods}
