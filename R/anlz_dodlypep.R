#' Analyze daily DO values relative to threshold
#' 
#' Analyze daily DO values relative to threshold
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
#' If \code{impute = TRUE}, missing dissolved oxygen values in the complete daily time series are imputed to the average for the year, month, site combination.  This is often necessary to create summary values from \code{\link{anlz_domopep}} that make sense. For example, if a month has incomplete data, the \code{below_ave} summary from \code{\link{anlz_domopep}} may indicate a value of one if all daily averages in the available data are below the threshold, whereas the \code{below_maxrun} summary may indicate a maximum run of days not equal to the number of days in the month. 
#'
#' @importFrom dplyr "%>%"
#' 
#' @family analyze
#' 
#' @examples
#' data(dodat)
#' dat <- anlz_dodlypep(dodat)
#' dat
anlz_dodlypep <- function(dodat, thr = 3, impute = TRUE){

  # average to day
  avgs <- dodat %>% 
    dplyr::mutate(
      Date = as.Date(DateTime, tz = 'America/Jamaica')
    ) %>% 
    dplyr::group_by(site, Date) %>% 
    dplyr::summarise(
      do_mgl = mean(do_mgl, na.rm = TRUE), 
      .groups = 'drop'
    ) %>% 
    dplyr::arrange(site, Date) 
  
  # create complete time series, add avg by mo, yr, site if missing
  cmplt <- avgs %>% 
    dplyr::group_by(site) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(
      data = purrr::map(data, function(x){
        
        out <- x
        
        if(impute){
          
          rng <- range(x$Date, na.rm = TRUE)
        
          out <- seq.Date(rng[1], rng[2], by = 'day') %>% 
            dplyr::tibble(Date = .) %>% 
            dplyr::left_join(x, by = 'Date') 
        
        }
  
        out <- out %>% 
          dplyr::mutate(
            yr = lubridate::year(Date), 
            mo = lubridate::month(Date)
          ) %>% 
          dplyr::group_by(yr, mo) %>% 
          dplyr::mutate(
            do_mgl = dplyr::case_when(
              is.na(do_mgl) ~ mean(do_mgl, na.rm = TRUE), 
              TRUE ~ do_mgl
            )
          )
 
        return(out)
        
      })
    ) %>% 
    tidyr::unnest('data') %>% 
    dplyr::ungroup()

  # summarise do below and cumulative sum
  out <- cmplt %>% 
    dplyr::mutate(
      below = ifelse(any(do_mgl < thr), 1, 0), 
    ) %>% 
    dplyr::group_by(
      site, 
      grp = cumsum(below == 0)
    ) %>% 
    dplyr::mutate(
      below_cumsum = cumsum(below)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(site, Date, yr, mo, do_mgl, below, below_cumsum)
  
  return(out)
  
}