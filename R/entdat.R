#' Raw beach pathogen data from Suffolk County
#'
#' Raw beach pathogen data from Suffolk County
#'
#' @format A \code{data.frame} object with columns for beach name (\code{Name}), station code (\code{FieldNum}), sample date (\code{Date}), Enterococcus concentration as cfu/100 mL (\code{value}), and whether the observation was above, below, or at the reported value (\code{status} as \code{>}, \code{<}, or \code{=})
#' @concept data
#' @details Data are from the Suffolk County Department of Health Services Bathing Beach Program, available at \url{https://gis.suffolkcountyny.gov/portal/home/item.html?id=b5d1e5c88d8c4fb48a3732cd44cf4007}. See \code{\link{read_pepent}} for details.
#' @examples
#' \dontrun{
#' entdat <- read_pepent()
#'
#' save(entdat, file = 'data/entdat.RData', compress = 'xz')
#' }
"entdat"
