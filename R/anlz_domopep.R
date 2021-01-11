#' Analyze monthly DO values relative to threshold
#' 
#' Analyze monthly DO values relative to threshold
#'
#' @param dodat result returned from \code{\link{read_pepdo}}
#' @param thr numeric indicating appropriate dissolved oxygen thresholds, usually 2.3 mg/L for acute, 4.8 mg/L for chronic
#'
#' @return data.frame
#' @export
#' 
#' @details The \code{\link{dodat}} data object can be used as input without downloading USGS data
#'
#' @importFrom dplyr "%>%"
#' 
#' @family analyze
#' 
#' @examples
#' data(dodat)
#' dat <- anlz_domopep(dodat)
#' dat
anlz_domopep <- function(dodat, thr = 2.3){
  
  out <- anlz_dodlypep(dodat, thr = thr) %>% 
    dplyr::group_by(
      site,
      yr = lubridate::year(Date), 
      mo = lubridate::month(Date, label = T, abbr = T)
    ) %>% 
    dplyr::summarise(
      do_mgl = mean(do_mgl), 
      below_ave = mean(below), 
      below_maxrun = max(below_cumsum), 
      .groups = 'drop'
    )
  
  return(out)
  
}