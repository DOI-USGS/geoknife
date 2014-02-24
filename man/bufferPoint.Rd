\name{bufferPoint}
\alias{bufferPoint}
\title{Create linear ring from point}
\usage{
  bufferPoint(point)
}
\arguments{
  \item{A}{vector of longitude and latitude values}
}
\value{
  A linear ring (with closure) as a numeric vector
}
\description{
  Buffer ring creation from point
}
\details{
  bufferPoint is a function that takes a longitude and
  latitude pair and creates a buffered ring (i.e., a
  feature with non-zero area) centered.
}
\examples{
linearRing = bufferPoint(c(-111.48, 36.95))
}
\seealso{
  \code{setFeature}
}
\keyword{bufferPoint}

