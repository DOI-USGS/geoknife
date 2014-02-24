\docType{methods}
\name{setWPS}
\alias{setWPS}
\title{set web processing service location}
\arguments{
  \item{\code{geoknife}}{object.}

  \item{a}{Web Processing Service (WPS) endpoint.}
}
\value{
  An \code{geoknife} object.
}
\description{
  set web processing service location
}
\details{
  method for setting the web processing service (WPS)
  endpoint for a \code{geoknife} object.
}
\examples{
gk <- geoknife() # create geoknife object
WPS <- 'http://cida-eros-gdp2qa.er.usgs.gov/gdp/process/WebProcessingService'
gk <- setWPS(gk,WPS)
}
\keyword{setWPS}

