\encoding{UTF-8}
\name{getHex}
\alias{getHex}
\title{Internal function for color conversion}
\description{Converts different color values to hexadecimal colors. Used by \code{\link{styleSingle}}, \code{\link{styleGrad}} and \code{\link{styleCat}}.}
\usage{getHex(col)}
\arguments{
  \item{col}{Color value as number, name or hex code with alpha.}
}
\value{Hexadecimal color value as string.}
\author{Christian Graul}
\seealso{\code{\link{styleSingle}}, \code{\link{styleGrad}}, \code{\link{styleCat}}}
\keyword{methods}
\keyword{internal}