\encoding{UTF-8}
\name{leafletInt}
\alias{leafletInt}
\title{Internal function leaflet}
\description{Internal function for \code{\link{leaflet}}}
\usage{
leafletInt(dat, path, title, size, base.map, center, 
  zoom, style, popup, incl.data)}
\arguments{
  \item{dat}{Name(s) of data file(s).}
  \item{path}{Path to the \code{data} file.}
  \item{title}{Map title.}
  \item{size}{Size of the map.}
  \item{base.map}{Base map in the background of the data.}
  \item{center}{Map center coordinates.}
  \item{zoom}{Map zoom level.}
  \item{style}{Style(s) of the data layer(s).}
  \item{popup}{Property of the data to be shown in a popup.}
  \item{incl.data}{If \code{TRUE}, \code{data} is included in the HTML file itself.}
}
\author{Christian Graul}
\seealso{\code{\link{leaflet}}}
\keyword{methods}
\keyword{internal}