\docType{methods}
\name{getAttributes}
\alias{getAttributes}
\title{get attributes from a shapefile at a web location}
\arguments{
  \item{\code{geoknife}}{object with a valid WFS url.}

  \item{a}{valid shapefile name.}
}
\value{
  list of attributes for the given shapefile at the
  \code{geoknife} WFS url.
}
\description{
  get attributes from a shapefile at a web location
}
\details{
  a \code{geoknife} method for finding attribute names for
  a given shapefile at a valid WFS endpoint.
}
\examples{
gk<- geoknife() # create geoknife object
shps <- getShapefiles(gk) # get shapefile names
getAttributes(gk,shapefile=shps[2])
}
\keyword{getAttributes}

