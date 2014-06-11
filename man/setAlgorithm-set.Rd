\docType{methods}
\name{setAlgorithm<-}
\alias{setAlgorithm<-}
\title{set processing algorithm}
\arguments{
  \item{An}{\code{geoknife} object.}

  \item{a}{list for a valid algorithm, including values for
  name & location}
}
\value{
  An \code{geoknife} object.
}
\description{
  set processing algorithm
}
\details{
  method for setting the process algorithm of the
  \code{geoknife} object.
}
\examples{
\dontrun{
gk <- geoknife() # create geoknife object
algorithm <- list("Area Grid Statistics (weighted)"=
"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
setAlgorithm(gk) <- algorithm
}
}
\keyword{setAlgorithm}

