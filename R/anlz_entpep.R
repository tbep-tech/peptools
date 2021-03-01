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

  beaches <- c("Alberts Landing Beach", "Camp Baiting Hollow Beach (Lake)", "Camp Blue Bay Beach", "Camp Quinipet Beach", "Clearwater Beach",
      "Cornell Cooperative Extension Marine Center Beach", "Crescent Beach - Suffolk", "Culloden Shores Beach", "Devon Yacht Club Beach",
      "East Lake Drive Beach", "Emma Rose Elliston Memorial Park Beach", "Fifth Street Park Beach", "Fleets Neck Beach", "Foster Memorial Beach",
      "Founders Landing Beach", "Goose Creek Beach", "Havens Beach", "John Schiff Scout Reservation Beach", "Maidstone Beach",
      "Meschutt Beach", "Nassau Point Causeway Beach", "New Suffolk Beach", "Norman E. Klipp Park Beach",
      "Perlman Music Camp Beach", "Pridwin Hotel Beach", "Shelter Island Heights Beach Club Beach", "Silver Sands Motel Beach", "South Jamesport Beach",
      "Southampton Peconic Beach & Tennis Club Beach", "Veteran's Memorial Park Beach", "Wades Beach")

  out <- entdat %>% 
    dplyr::filter(Name %in% beaches) %>% 
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
