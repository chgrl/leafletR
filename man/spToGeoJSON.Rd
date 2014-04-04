\encoding{UTF-8}
\name{spToGeoJSON}
\alias{spToGeoJSON}
\title{Internal function of toGeoJSON}
\description{Create GeoJSON file from Spatial object}
\usage{spToGeoJSON(data, class, name, dest, overwrite)}
\arguments{
  \item{data}{\code{Spatial} object.}
  \item{class}{Class of sp object.}
  \item{name}{Name of the resulting GeoJSON file, as string.}
  \item{dest}{Directory the file shall be saved to, as string.}
  \item{overwrite}{\code{TRUE} overwrites existing files with the same name.}
}
\value{GeoJSON file path, as string.}
\author{Christian Graul}
\seealso{\code{\link{toGeoJSON}}}
\keyword{methods}
\keyword{internal}