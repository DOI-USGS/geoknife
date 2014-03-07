\docType{methods}
\name{setProcessInputs<-}
\alias{setProcessInputs<-}
\title{set inputs for web processing}
\arguments{
  \item{An}{\code{geoknife} object.}

  \item{a}{list of valid processInputs.}
}
\value{
  An \code{geoknife} object.
}
\description{
  set inputs for web processing
}
\details{
  method for setting the (non-feature related) post inputs
  of the \code{geoknife} object.
}
\examples{
geoknife <- geoknife() # create geoknife object

setAlgorithm(geoknife) <- getAlgorithms(geoknife)[4] # feature weighted

# set the post inputs for the processing dataset
setProcessInputs(geoknife) <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
                                       'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
                                       'TIME_START'='2010-01-01T00:00:00Z',
                                       'TIME_END'='2010-01-01T23:00:00Z',
                                       'DELIMITER'='TAB')
geoknife # print geoknife object contents
}
\keyword{setProcessInputs}

