\encoding{UTF-8}
\name{getProperties}
\alias{getProperties}
\alias{prop}
\title{Print property names of a GeoJSON file}
\description{Prints the names of all available properties of a GeoJSON file.}
\usage{
getProperties(data, print=TRUE)
prop(data, print=TRUE)
}
\arguments{
  \item{data}{Path to data file as string or GeoJSON/TopoJSON object.}
  \item{print}{If \code{TRUE} (default), the property names are printed.}
}
\value{Property names as string vector.}
\author{Christian Graul}
\seealso{\code{\link{styleCat}}, \code{\link{styleGrad}}}
\examples{
\dontrun{
data(quakes)
qks <- toGeoJSON(data=quakes, dest=tempdir())
getProperties(data=qks)
}}
\keyword{methods}