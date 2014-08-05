\encoding{UTF-8}
\name{addBaseMap}
\alias{addBaseMap}
\title{Add custom basemaps}
\description{Add a custom base map to the list of maps available in the function code{/link{leaflet}}}.
\usage{
addBaseMap(shortname, name, url, options)
}
\arguments{
  \item{shortname}{short name of the basemap. This is the the name that will be used in \code{\link{leaflet}}}
  \item{name}{Name of the basemap.}
  \item{url}{URL for the basemap. See \url{http://leafletjs.com/reference.html#tilelayer} for more information.}
  \item{options}{(optional) ist containing aditional options. See \url{http://leafletjs.com/reference.html#tilelayer} for for a list of valid options.}
}
\note{

}
\author{François Guillem}
\seealso{\code{\link{styleSingle}}, \code{\link{styleGrad}}, \code{\link{styleCat}}}
\examples{
# Duplicates osm base map
addBaseMap(
  "myosm", 
  "Duplicated OpenStreetMap", 
  url = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  options = list(
    attribution='&copy; <a href="http://openstreetmap.org/copyright", target="_blank">OpenStreetMap contributors</a>'
  )
)

browseURL(leaflet(base.map = "myosm"))
}
\keyword{methods}
