\docType{methods}
\name{startProcess}
\alias{startProcess}
\title{Submit a GDP web processing request}
\arguments{
  \item{\code{geoknife}}{object ot be used to formulate GDP
  process request.}
}
\value{
  An \code{geoknife} object with an active GDP process
  request.
}
\description{
  Start process for \code{geoknife}
}
\details{
  startProcess is a method for submitting a web processing
  request for the \code{geoknife} object.
}
\examples{
geoknife <- geoknife() # create geoknife object
geoknife # print geoknife object

linearRing = bufferPoint(c(-111.48,36.95))
geoknife <- setFeature(geoknife,list(LinearRing=linearRing))

#get a list of available processing algorithms
getAlgorithms(geoknife)

#set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
geoknife <- setAlgorithm(geoknife,getAlgorithms(geoknife)[4]) # feature weighted

# set the post inputs for the processing dataset
geoknife <-  setProcessInputs(geoknife,list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
                                       'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
                                       'TIME_START'='2010-01-01T00:00:00Z',
                                       'TIME_END'='2010-01-01T23:00:00Z',
                                       'DELIMITER'='TAB'))
geoknife # print geoknife object contents

# kick off your request
geoknife <- startProcess(geoknife)
geoknife # display contents
}
\seealso{
  \code{checkProcess}
}
\keyword{startProcess}

