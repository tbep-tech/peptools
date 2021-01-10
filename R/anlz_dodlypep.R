#' Analyze daily DO values relative to threshold
#' 
#' Analyze daily DO values relative to threshold
#'
#' @param dodat result returned from \code{\link{read_pepdo}}
#' @param thr numeric indicating appropriate dissolved oxygen thresholds, usually 3 mg/L for acute, 4.8 mg/L for chronic
#'
#' @return data.frame
#' @export
#' 
#' @details The \code{\link{dodat}} data object can be used as input without downloading USGS data
#'
#' @importFrom dplyr "%>%"
#' 
#' @family read
#' 
#' @examples
#' data(dodat)
#' dat <- anlz_dodlypep(dodat)
#' dat
anlz_dodlypep <- function(dodat, thr = 3){
    
  out <- dodat %>% 
    dplyr::mutate(
      Date = as.Date(DateTime, tz = 'America/Jamaica')
    ) %>% 
    dplyr::group_by(site, Date) %>% 
    dplyr::summarise(
      below = ifelse(any(do_mgl < !thr), 1, 0), 
      do_mgl = mean(do_mgl), 
      .groups = 'drop'
    ) %>% 
    dplyr::arrange(site, Date) %>% 
    dplyr::group_by(site, grp = cumsum(below == 0), yr = year(Date), mo = month(Date)) %>% 
    dplyr::mutate(
      below_cumsum = cumsum(below)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(site, Date, yr, mo, do_mgl, below, below_cumsum)
  
  return(out)
  
}