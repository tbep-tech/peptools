% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dodat.R
\docType{data}
\name{dodat}
\alias{dodat}
\title{Dissolved oxygen data for USGS stations}
\format{
A \code{data.frame} object
}
\usage{
dodat
}
\description{
Dissolved oxygen data for USGS stations
}
\examples{
\dontrun{

# 01304562 is Peconic River, 01304200 is Orient Harbor, 01304650 is Shelter Island
dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'), 
  nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'))

save(dodat, file = 'data/dodat.RData', compress = 'xz')

}
}
\concept{data}
\keyword{datasets}
