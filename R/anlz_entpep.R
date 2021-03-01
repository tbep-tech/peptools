#' Count beach exceedances for enterococcus
#'
#' @param entdat result returned from \code{\link{read_pepent}}
#' @param thr numeric value defining threshold for exceedance
#' 
#' @return A \code{data.frame} with counts of exceedances per year for each beach
#' @export
#'
#' @details 
#' The exceedance threshold is set by default as 104 cfu/100 ml criterion.  This is simply based on counts in a year when any value at any station was above the threshold for each 24 hour period in the record.
#' 
#' The \code{samples} column shows how many days of the year were sampled at each beach and the \code{exceedances} column shows how many samples were above the threshold.
#' 
#' @family analyze
#'
#' @examples
#' anlz_entpep(entdat)
anlz_entpep <- function(entdat, thr = 104){

  names <- beaches$Name

  out <- entdat %>% 
    dplyr::filter(Name %in% names) %>% 
    dplyr::mutate(
      Date = as.Date(Date),
      Name = gsub('Beach$|\\sBeach', '', Name)
    ) %>% 
    dplyr::group_by(Name, Date) %>% 
    dplyr::summarise(
      exceeds = any(value > thr), 
      .groups = 'drop'
    ) %>% 
    dplyr::mutate(
      yr = lubridate::year(Date)
    ) %>% 
    dplyr::group_by(Name, yr) %>% 
    dplyr::summarise(
      samples = dplyr::n(),
      exceedances = sum(exceeds),
      .groups = 'drop'
    )
  
  return(out)
  
}
