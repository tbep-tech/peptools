#' Raw data from Suffolk County
#'
#' Raw data from Suffolk County
#'
#' @format A \code{data.frame} object
#' @family utilities
#' @examples 
#' \dontrun{
#' rawdat <- read_xlsx('data/data-raw/Peconics SCDHS WQ data - up to 2019 so far.xlsx', col_types = 'text')
#' save(rawdat, file = 'data/rawdat.RData', compress = 'xz')
#' }
"rawdat"