\encoding{UTF-8}
\name{styleSingle}
\alias{styleSingle}
\alias{singles}
\title{Single symbol styling}
\description{Creates a single symbol style.}
\usage{
styleSingle(col, lwd, alpha, fill, fill.alpha, rad)
singles(col, lwd, alpha, fill, fill.alpha, rad)
}
\arguments{
  \item{col}{Color used for lines, i.e. lines itself, borders of polygons and circle borders (points). Color might be given as name, number [0-8] or hexadecimal code. If \code{fill} is not specified, \code{col} is used for border and circle area. If \code{col} is \code{NA}, the border is omitted.}
  \item{lwd}{Line width in number of pixels -- default is \code{2}.}
  \item{alpha}{Opacity of a line or border, as numeric value between \code{0} (fully transparent) and \code{1} (opaque).}
  \item{fill}{Fill color used for polygons and circles (points). Color might be given as name, number [0-8] or hexadecimal code. If \code{fill} is \code{NA}, the circle area is left blank.}
  \item{fill.alpha}{Opacity of a polygon or circle area, as numeric value between \code{0} (fully transparent) and \code{1} (opaque).}
  \item{rad}{Radius of circles (points), in number of pixels -- default is \code{10}.}
}
\value{A single symbol style object.}
\note{Points are displayed as circles.}
\author{Christian Graul}
\seealso{\code{\link{styleGrad}}, \code{\link{styleCat}}, \code{\link{leaflet}}}
\examples{
\dontrun{
## point data ##
# prepare data
data(quakes)
dat <- toGeoJSON(data=quakes, dest=tempdir())

# change circle borders
# note: if fill color is not specified, col is also used as fill color 
sty <- styleSingle(col=2, lwd=1, alpha=1)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# change fill color, alpha and radius
sty <- styleSingle(fill="red", fill.alpha=1, rad=2)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# no border
sty <- styleSingle(col=NA)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# blank circle area
sty <- styleSingle(fill=NA)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# change all arguments
sty <- styleSingle(col="#d4d4d4", lwd=1, alpha=0.8, 
  fill="darkred", fill.alpha=0.4, rad=4)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)


## line data ##
# prepare data
dat <- toGeoJSON(data=system.file(package="leafletR", "files", 
  "lynx.zip"), name="Lynx telemetry", dest=tempdir())

# style
sty <- styleSingle(col="#bb650b", lwd=3, alpha=0.8)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)


## polygon data ##
# prepare data
dat <- system.file(package="leafletR", "files", "park_sk.geojson")

# change borders
# note: if fill color is not specified, col is also used as fill color 
sty <- styleSingle(col=3, lwd=2, alpha=1)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# change fill color and alpha
sty <- styleSingle(fill="darkgreen", fill.alpha=0.8)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# no border
sty <- styleSingle(col=NA)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# blank polygon area
sty <- styleSingle(fill=NA)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)

# change all arguments
sty <- styleSingle(col="#006400", lwd=5, alpha=0.8, 
  fill="darkgreen", fill.alpha=0.4)
map <- leaflet(data=dat, dest=tempdir(), style=sty)
browseURL(map)
}
}
\keyword{methods}
