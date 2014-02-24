\docType{methods}
\name{setWFS}
\alias{setWFS}
\title{set web feature service location}
\arguments{
  \item{\code{geoknife}}{object.}

  \item{a}{Web Feature Service (WFS) endpoint.}
}
\value{
  An \code{geoknife} object.
}
\description{
  set web feature service location
}
\details{
  method for setting the web feature service (WFS) endpoint
  for a \code{geoknife} object.
}
\examples{
gk <- geoknife() # create geoknife object
WFS <- 'https://www.sciencebase.gov/catalogMaps/mapping/ows/51b0f5e5e4b030b51983cda1'
gk <- setWFS(gk,WFS)
}
\keyword{setWFS}

