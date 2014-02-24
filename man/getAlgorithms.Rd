\docType{methods}
\name{getAlgorithms}
\alias{getAlgorithms}
\title{Get processing algorithms}
\arguments{
  \item{\code{geoknife}}{object with a valid WPS url.}
}
\value{
  list of available algorithms for the \code{geoknife} WPS
  url.
}
\description{
  Get algorithms for \code{geoknife}
}
\details{
  getAlgorithms is a method for finding algorithm names and
  locations for a \code{geoknife} object.
}
\examples{
gk <- geoknife() # create geoknife object
getAlgorithms(gk) # list Geo Data Portal processing algorithms
}
\seealso{
  \code{setAlgorithm}
}
\keyword{getAlgorithms}

