\encoding{UTF-8}
\name{addBaseMap}
\alias{addBaseMap}
\title{Add custom base maps}
\description{Add a custom base map to the list of maps available in the function \code{\link{leaflet}}.}
\usage{
addBaseMap(shortname, name, url, options)
}
\arguments{
  \item{shortname}{Short name of the base map. This is the name that will be used in \code{\link{leaflet}}.}
  \item{name}{Name of the base map.}
  \item{url}{URL for the base map. See \url{http://leafletjs.com/reference.html#tilelayer} for more information.}
  \item{options}{(Optional list of additional options. See \url{http://leafletjs.com/reference.html#tilelayer} for for a list of valid options.}
}
\author{François Guillem}
\seealso{\code{\link{leaflet}}}
\examples{
# duplicates osm base map
addBaseMap(
  "myosm", 
  "Duplicated OpenStreetMap", 
  url="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  options=list(
    attribution='&copy; <a href="http://openstreetmap.org/copyright", target="_blank">OpenStreetMap contributors</a>'
  )
)

browseURL(leaflet(base.map="myosm"))
}
\keyword{methods}
