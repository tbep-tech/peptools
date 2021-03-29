#' Analyze monthly DO values relative to threshold
#' 
#' Analyze monthly DO values relative to threshold
#'
#' @param dodat result returned from \code{\link{read_pepdo}}
#' @param thr numeric indicating appropriate dissolved oxygen thresholds, usually 3 mg/L for acute, 4.8 mg/L for chronic
#' @param impute logical indicating of missing dissolved oxygen values are imputed with the year, month, site average
#'
#' @return data.frame
#' @export
#' 
#' @details The \code{\link{dodat}} data object can be used as input without downloading USGS data
#' 
#' The date are summarized as three different values, where \code{do_mgl} is the average of all daily DO averages across the month, \code{below_ave} is the proportion of days in a month when concentrations in a given day fell below the threshold (1 would mean all days had an instance of DO below the threshold, 0 would mean none), and \code{below_maxrun} is the maximum number of sequential days in a month when concentrations in a given day fell below the threshold (30 or 31, depending on month, would indicate all days in a month had an instance of DO below the threshold). 
#' 
#' If \code{impute = TRUE}, missing dissolved oxygen values in the complete daily time series are imputed to the average for the year, month, site combination.  This is often necessary to create summary values that make sense. For example, if a month has incomplete data, the \code{below_ave} summary may indicate a value of one if all daily averages in the available data are below the threshold, whereas the \code{below_maxrun} summary may indicate a maximum run of days not equal to the number of days in the month. 
#'
#' @importFrom dplyr "%>%"
#' 
#' @family analyze
#' 
#' @examples
#' data(dodat)
#' dat <- anlz_domopep(dodat)
#' dat
anlz_domopep <- function(dodat, thr = 3, impute = TRUE){
  
  out <- anlz_dodlypep(dodat, thr = thr, impute = impute) %>% 
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