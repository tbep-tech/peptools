% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_pepwq.R
\name{read_pepwq}
\alias{read_pepwq}
\title{Import raw water quality data}
\usage{
read_pepwq(path)
}
\arguments{
\item{path}{chr string of path for excel file}
}
\value{
data.frame
}
\description{
Import raw water quality data
}
\details{
Raw data from here \url{https://gis.suffolkcountyny.gov/portal/home/item.html?id=5d4b53ec44204219a8da685f1859e096}

All data prior to 1990 are removed - some exist but the data are scarce.
}
\examples{
path <- system.file("extdata", "currentdata.xlsx", package="peptools")
dat <- read_pepwq(path)
dat
}
\concept{read}
