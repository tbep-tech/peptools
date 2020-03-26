#' Raw data from Suffolk County
#'
#' Raw data from Suffolk County
#'
#' @format A \code{data.frame} object
#' @family utilities
#' @examples 
#' \dontrun{
#' path <- system.file("extdata", "currentdata.xlsx", package="pepreporting")
#' rawdat <- read_pepwq(path)
#' rawdat
#' save(rawdat, file = 'data/rawdat.RData', compress = 'xz')
#' }
"rawdat"