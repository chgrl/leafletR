\encoding{UTF-8}
\name{leafletR-package}
\alias{leafletR-package}
\alias{leafletR}
\docType{package}
\title{Interactive web-maps based on the Leaflet JavaScript library}
\description{Display your spatial data on interactive web-maps using the open-source JavaScript library Leaflet. The package provides basic web-mapping functionality to combine vector data and online map tiles from different sources.
}
\details{
\code{leafletR} lets you display spatial data on interactive maps in web browsers (a.k.a. slippy maps). It takes advantage of the open-source JavaScript library Leaflet.js, developed by Vladimir Agafonkin. Focusing simplicity, the package provides basic web-mapping functionality and styling options only. For map display an internet connection is required to load the Leaflet library, stylesheets and base map tiles. The ready to use html file output can be viewed locally or uploaded to a web-server.

\code{leafletR} supports GeoJSON files directly. Additionally it contains conversion tools for sp spatial objects, several popular spatial vector data formats and R data frames containing point coordinates.
	
\code{leafletR} uses open base map tiles only. Map data is provided by the \href{http://www.openstreetmap.org}{OpenStreetMap} project and satellite images are provided by courtesy of NASA/ JPL-Caltech and U.S. Department of Agriculture, Farm Service Agency.

Try the example below to check if \code{leafletR} has been correctly installed. Any question and feedback is welcome via email to <christian.graul@gmail.com> or on \href{https://github.com/chgrl/leafletR}{GitHub}.

Example output:

\figure{quakes.png}{options: width=350px}
}
\author{
Christian Graul, with contributions from Francois Guillem

Maintainer: Christian Graul <christian.graul@gmail.com>
}
\references{
\url{http://leafletjs.com}
}
\keyword{package}
\examples{
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
#q.map
}
