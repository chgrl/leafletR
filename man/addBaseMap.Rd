\encoding{UTF-8}
\name{addBaseMap}
\alias{addBaseMap}
\alias{base}
\title{Add custom base maps}
\description{Add a custom base map to the list of maps available in the function \code{\link{leaflet}}.}
\usage{
addBaseMap(name, title, url, options)
}
\arguments{
  \item{name}{Name of the base map.}
  \item{title}{Title of the base map, used in the layer control of the resulting map. Optional -- if missing, \code{name} is used.}
  \item{url}{URL for the base map. See \url{http://leafletjs.com/reference.html#tilelayer} for more information.}
  \item{options}{(Optional list of additional options. See \url{http://leafletjs.com/reference.html#tilelayer} for for a list of valid options.}
}
\author{François Guillem}
\seealso{\code{\link{leaflet}}}
\examples{
\dontrun{
# duplicates osm base map
addBaseMap(
  name="myosm", 
  title="Duplicated OpenStreetMap", 
  url="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  options=list(
    attribution='&copy; <a href="http://openstreetmap.org/copyright", target=
      "_blank">OpenStreetMap contributors</a>'
  )
)

browseURL(leaflet(base.map="myosm"))
}
}
\keyword{methods}
