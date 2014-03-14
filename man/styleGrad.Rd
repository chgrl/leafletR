\encoding{UTF-8}
\name{styleGrad}
\alias{styleGrad}
\alias{grads}
\title{Graduated styling}
\description{Creates a graduated style based on an attribute.}
\usage{
styleGrad(prop, breaks, right=TRUE, out=0, 
  style.par, style.val, leg, ...)
grads(prop, breaks, right=TRUE, out=0, 
  style.par, style.val, leg, ...)
}
\arguments{
  \item{prop}{Property (attribute) of the data to be styled, as string.}
  \item{breaks}{A vector giving the breakpoints between the desired classes.}
  \item{right}{If \code{TRUE} (default) classes are right-closed (left-open) intervals (>= breakpoint). Otherwise classes are left-closed (right-open) intervals (> breakpoint).}
  \item{out}{Handling of data outside the edges of \code{breaks}. One of \code{0} (left and right-closed), \code{1} (left-closed, right-open), \code{2} (left-open, right-closed) or \code{3} (left and right-open). Default is \code{0}.}
  \item{style.par}{Styling parameter as string. One of \code{"col"} (graduated color) or \code{"rad"} (graduated radius). Graduated radius can only be applied to points.}
  \item{style.val}{Styling values, a vector of colors or radii applied to the classes.}
  \item{leg}{Legend title as string. The line break sequence \code{\\n} may be used for line splitting.}
  \item{\dots}{Additional styling parameters, see \code{\link{styleSingle}} for details.}
}
\value{A graduated style object.}
\author{Christian Graul}
\seealso{\code{\link{styleSingle}}, \code{\link{styleCat}}, \code{\link{leaflet}}}
\examples{
\dontrun{
# prepare data
data(quakes)
qks <- toGeoJSON(data=quakes, dest=tempdir())

# prepare style
range(quakes$mag)	# gives 4.0 and 6.4
sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), 
  style.val=rev(heat.colors(5)), leg="Richter Magnitude")

# create map
map <- leaflet(data=qks, dest=tempdir(), 
  title="Fiji Earthquakes", style=sty)
browseURL(map)

# left-closed intervals
# note the gray points on the map: magnitude of 4 is outside the breaks
# (which are >4.0, >4.5, >5.0, >5.5, >6.0 and >6.5)
sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), right=FALSE, 
  style.val=rev(heat.colors(5)), leg="Richter Magnitude")
map <- leaflet(data=qks, dest=tempdir(), 
  title="Fiji Earthquakes", style=sty)
browseURL(map)

# handle outliers
# include outliers of the upper example by left-opening the break edges 
# and adding a sixth color
sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), right=FALSE, 
  out=2, style.val=rev(heat.colors(6)), leg="Richter Magnitude")
map <- leaflet(data=qks, dest=tempdir(), 
  title="Fiji Earthquakes", style=sty)
browseURL(map)

# graduated radius
sty <- styleGrad(prop="mag", breaks=seq(4, 6.5, by=0.5), style.par="rad", 
  style.val=c(2,5,9,14,20), leg="Richter Magnitude")
map <- leaflet(data=qks, dest=tempdir(), 
  title="Fiji Earthquakes", style=sty)
browseURL(map)

# additional styling parameters
peak <- toGeoJSON(data=system.file(package="leafletR", "files", 
  "peak_sk.kml"), dest=tempdir())
sty <- styleGrad(prop="Name", breaks=seq(750, 2500, by=250), out=3, 
  style.val=terrain.colors(9), leg="Elevation", 
  col=NA, fill.alpha=1, rad=3)
map <- leaflet(data=peak, dest=tempdir(), title="Peak elevation", 
  base.map="mqsat", style=sty, popup="Name")
browseURL(map)
}
}
\keyword{methods}
