\name{printProcessInputs}
\alias{printProcessInputs}
\title{printProcessInputs}
\usage{
  printProcessInputs(.Object)
}
\arguments{
  \item{An}{\code{geoknife} object.}
}
\description{
  function for printing processXML \code{geoknife} object.
}
\examples{
gk <- geoknife() # create geoknife object
linearRing = bufferPoint(c(-111.48,36.95))
setFeature(gk) <- list(LinearRing=linearRing)

algorithm <- list("Area Grid Statistics (weighted)"=
"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
setAlgorithm(gk) <- algorithm
setProcessInputs(gk) <- list("DATASET_URI" = "dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best")
printProcessInputs(gk)
}
\keyword{printProcessInputs}

