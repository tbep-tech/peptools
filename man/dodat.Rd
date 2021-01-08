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
# get DO data from USGS stations
# 01304562 is Peconic River, 01304200 is Orient Harbor
# downloaded data are in UTC
# for cd column, codes are often â€œAâ€ (approved) or â€œPâ€ (provisional)
# from vignette https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
# takes a few minutes to dl
dodat <- readNWISuv(siteNumbers = '01304562', parameterCd = '00300') \%>\% 
  select(site = site_no, DateTime = dateTime, do_mgl = X_00300_00000) \%>\%
  mutate(DateTime = with_tz(DateTime, tzone = 'America/Jamaica'))

save(dodat, file = 'data/dodat.RData', compress = 'xz')

}
}
\seealso{
Other utilities: 
\code{\link{entdat}},
\code{\link{pepstations}},
\code{\link{peptargets}},
\code{\link{rawdat}}
}
\concept{utilities}
\keyword{datasets}