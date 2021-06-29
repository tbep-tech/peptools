#' Bay segment targets
#'
#' Bay segment targets 
#' 
#' @format A \code{data.frame} object
#' @concept data
#' @examples
#' \dontrun{
#' peptargets <- tibble::tibble(
#'   bay_segment = factor(c('Western', 'Central', 'Eastern'), 
#'     levels = c('Western', 'Central', 'Eastern')), 
#'   name = factor(c('Western', 'Central', 'Eastern'), 
#'     levels = c('Western', 'Central', 'Eastern')), 
#'   sd_thresh = c(6.5, 6.5, 6.5),
#'   chla_thresh = c(5.5, 5.5, 5.5)
#' )
#' save(peptargets, file = 'data/peptargets.RData', compress= 'xz')
#' }
#' @concept data
"peptargets"
