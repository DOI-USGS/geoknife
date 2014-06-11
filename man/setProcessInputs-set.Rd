\docType{methods}
\name{setProcessInputs<-}
\alias{setProcessInputs<-}
\title{set inputs for web processing}
\arguments{
  \item{An}{\code{geoknife} object.}

  \item{a}{list of valid processInputs.}
}
\value{
  An \code{geoknife} object with updated postInputs.
}
\description{
  set inputs for web processing
}
\details{
  method for setting the (non-feature related) post inputs
  of the \code{geoknife} object.
}
\examples{
\dontrun{
gk <- geoknife() # create geoknife object
algorithm <- list("Area Grid Statistics (weighted)"=
                   "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
setAlgorithm(gk) <- algorithm
# set the post inputs for the processing dataset
setProcessInputs(gk) <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
                                       'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
                                       'TIME_START'='2010-01-01T00:00:00Z',
                                       'TIME_END'='2010-01-01T23:00:00Z',
                                       'DELIMITER'='TAB')
gk # print geoknife object contents
}
}
\keyword{setProcessInputs}

