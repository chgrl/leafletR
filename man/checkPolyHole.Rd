\encoding{UTF-8}
\name{checkPolyHole}
\alias{checkPolyHole}
\title{Internal function of spToGeoJSON}
\description{Checks if hole is part of polygon or not. Used by \code{\link{spToGeoJSON}}.}
\usage{checkPolyHole(polyCoord, holeCoord)}
\arguments{
  \item{polyCoord}{Polygon coordinates as data.frame.}
  \item{holeCoord}{Hole coordinates as data.frame.}
}
\value{Boolean}
\author{Christian Graul}
\seealso{\code{\link{spToGeoJSON}}}
\keyword{methods}
\keyword{internal}
