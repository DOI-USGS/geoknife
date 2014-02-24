\docType{methods}
\name{checkProcess}
\alias{checkProcess}
\title{Check status of processing request}
\arguments{
  \item{\code{geoknife}}{object with an active GDP process
  request.}
}
\value{
  \code{process}, a list containing \code{status} and
  \code{URL}.
}
\description{
  Check status of processing request
}
\details{
  checkProcess is a method for checking the process status
  of an active (executed) \code{geoknife} object. The
  method returns \code{process}, which is a list containing
  two fields: \code{status} and \code{URL}. If the
  \code{geoknife} object has not been executed (see
  \code{executePost}), this method returns
  \code{status}='none' and \code{URL}=NULL.
}
\seealso{
  \code{executePost}
}
\keyword{checkProcess}

