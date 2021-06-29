#' Dissolved oxygen data for USGS stations
#'
#' Dissolved oxygen data for USGS stations
#'
#' @format A \code{data.frame} object
#' @concept data
#' @examples 
#' \dontrun{
#' 
#' # 01304562 is Peconic River, 01304200 is Orient Harbor
#' dodat <- read_pepdo(site = c('01304562', '01304200'), nms = c('Peconic River', 'Orient Harbor'))
#' 
#' save(dodat, file = 'data/dodat.RData', compress = 'xz')
#'
#'}
"dodat"
