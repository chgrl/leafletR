\encoding{UTF-8}
\name{dfToGeoJSON}
\alias{dfToGeoJSON}
\title{Internal function of toGeoJSON}
\description{Create GeoJSON file from data frame}
\usage{dfToGeoJSON(data, name, dest, lat.lon, overwrite)}
\arguments{
  \item{data}{\code{\link{data.frame}}.}
  \item{name}{Name of the resulting GeoJSON file, as string.}
  \item{dest}{Directory the file shall be saved to, as string.}
  \item{lat.lon}{Names or indices of the columns in \code{data} containing the coordinates, as vector of two: \code{c(latitude, longitude)}.}
  \item{overwrite}{\code{TRUE} overwrites existing files with the same name.}
}
\value{GeoJSON file path, as string.}
\author{Christian Graul}
\seealso{\code{\link{toGeoJSON}}}
\keyword{methods}
\keyword{internal}