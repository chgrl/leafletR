\encoding{UTF-8}
\name{getTopologies}
\alias{getTopologies}
\alias{topo}
\title{Print topology objects of a TopoJSON file}
\description{Prints the names of all available topology objects of a TopoJSON file.}
\usage{
getTopologies(data, print=TRUE)
topo(data, print=TRUE)
}
\arguments{
  \item{data}{Name of data file as string or TopoJSON object.}
  \item{print}{If \code{TRUE} (default), the property names are printed.}
}
\value{Topology object names as string vector.}
\author{Christian Graul}
\examples{
\dontrun{
#getTopologies()
}}
\keyword{methods}