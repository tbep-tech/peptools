#' Raw beach pathogen data from Suffolk County
#'
#' Raw beach pathogen data from Suffolk County
#'
#' @format A \code{data.frame} object
#' @family utilities
#' @examples 
#' \dontrun{
#' path <- system.file("extdata", "enterodata.xlsx", package="pepreporting")
#' entdat <- read_pepent(path)
#' save(entdat, file = 'data/entdat.RData', compress = 'xz')
#' }
"entdat"