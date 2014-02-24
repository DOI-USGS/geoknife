\name{bufferRing}
\alias{bufferRing}
\title{Create linear ring from point}
\usage{
  bufferRing(point)
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
  bufferRing is a function that takes a longitude and
  latitude pair and creates a buffered ring (i.e., a
  feature with non-zero area) centered.
}
\seealso{
  \code{setFeature}
}
\keyword{bufferRing}

