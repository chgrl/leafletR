\encoding{UTF-8}
\name{fileToGeoJSON}
\alias{fileToGeoJSON}
\title{Internal function of toGeoJSON}
\description{Create GeoJSON file from external spatial data file}
\usage{fileToGeoJSON(data, name, dest, overwrite)}
\arguments{
  \item{data}{Path to external spatial data file, as string.}
  \item{name}{Name of the resulting GeoJSON file, as string.}
  \item{dest}{Directory the file shall be saved to, as string.}
  \item{overwrite}{\code{TRUE} overwrites existing files with the same name.}
}
\value{GeoJSON file path, as string.}
\author{Christian Graul}
\seealso{\code{\link{toGeoJSON}}}
\keyword{methods}
\keyword{internal}