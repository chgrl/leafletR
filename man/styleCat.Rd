\encoding{UTF-8}
\name{styleCat}
\alias{styleCat}
\alias{cats}
\title{Categorized styling}
\description{Creates a categorized style based on an attribute}
\usage{
styleCat(prop, val, style.par, style.val, leg, ...)
cats(prop, val, style.par, style.val, leg, ...)
}
\arguments{
  \item{prop}{Property (attribute) of the data to be styled, as string.}
  \item{val}{A vector giving the data values to be used as categories.}
  \item{style.par}{Styling parameter as string. One of \code{"col"} (categorized color) or \code{"rad"} (categorized radius). Categorized radius can only be applied to points.}
  \item{style.val}{Styling values, a vector of colors or radii applied to the categories given by \code{val}. See details for unspecified data values.}
  \item{leg}{Legend title as string. The line break sequence \code{\\n} may be used for line splitting.}
  \item{\dots}{Additional styling parameters, see \code{\link{styleSingle}} for details.}
}
\details{
If \code{val} does not cover all data values, the unspecified data values are colored gray. By adding an extra color for unspecified data values to \code{style.val}, an "other"-category is shown in the legend.
}
\value{A categorized style object.}
\author{Christian Graul}
\seealso{\code{\link{styleSingle}}, \code{\link{styleGrad}}, \code{\link{leaflet}}}
\examples{
\dontrun{
# prepare data
dat <- system.file(package="leafletR", "files", "park_sk.geojson")

# simple categorizing
sty <- styleCat(prop="lynx", val=c("yes", "no"), 
  style.val=c("green", "red"), leg="Lynx occurrence")
map <- leaflet(data=dat, dest=tempdir(), title="Lynx", 
  style=sty)

# just one category
sty <- styleCat(prop="wisent", val="yes", style.val="red", 
  leg="Wisent occurrence")
map <- leaflet(data=dat, dest=tempdir(), title="Wisent", 
  style=sty)

# get nice colors using ColorBrewer
require(RColorBrewer)
pal <- brewer.pal(7, "Dark2")
sty <- styleCat(prop="year", val=c("1949", "1967", "1978", "1988", 
  "1997", "1998", "2002"), style.val=pal, leg="established:\n")
map <- leaflet(data=dat, dest=tempdir(), 
  title="National parks", style=sty)

# add 'other'-category to legend
require(RColorBrewer)
pal <- brewer.pal(7, "Dark2")
sty <- styleCat(prop="year", val=c("1997", "1998", "2002"), 
  style.val=pal, leg="established:\n")
map <- leaflet(data=dat, dest=tempdir(), 
  title="National parks", style=sty)

# additional styling parameters
sty <- styleCat(prop="brown_bear", val=c("yes", "no"), 
  style.val=c("darkgreen", "red"), leg="Brown bear\noccurrence", 
  alpha=1, lwd=4, fill=NA)
map <- leaflet(data=dat, dest=tempdir(), title="Brown bear", 
  style=sty)
}
}
\keyword{methods}