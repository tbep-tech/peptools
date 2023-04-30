#' Import dissolved oxygen data
#' 
#' Import dissolved oxygen data
#'
#' @param site chr string of site numbers for USGS stations
#' @param nms chr string vector of names to reassign to site numbers
#' @param startDate chr string of start date in YYYY-MM-DD
#' @param endDate chr string of end date in YYYY-MM-DD
#'
#' @return data.frame
#' @export
#'
#' @importFrom dplyr "%>%"
#' 
#' @details Raw data are downloaded using the USGS dataRetrieval R package, this function is a simple wrapper to the \code{\link[dataRetrieval]{readNWISuv}} function.
#' 
#' Note that downloading the station data with the default arguments may take a few minutes. Sites are 01304562 for Peconic River, 01304200 for Orient Harbor, 01304650 for Shelter Island.
#' 
#' @concept read
#' 
#' @examples
#' \dontrun{
#' dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'), 
#'   nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'), 
#'   StartDate = '2020-06-01', endDate = '2021-06-30')
#' dodat
#' }
read_pepdo <- function(site = c('01304562', '01304200', '01304650'), nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'), startDate = '2013-01-01', endDate = '2022-12-31'){
  
  # get DO data from USGS stations
  # 01304562 is Peconic River, 01304200 is Orient Harbor, 01304650 is Shelter Island
  # downloaded data are in UTC
  # for cd column, codes are often “A” (approved) or “P” (provisional)
  # from vignette https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
  # takes a few minutes to dl
  out <- dataRetrieval::readNWISuv(siteNumbers = site, parameterCd = '00300', startDate = startDate, endDate = endDate) %>%
    dplyr::select(site = site_no, DateTime = dateTime, do_mgl = X_00300_00000) %>%
    dplyr::mutate(DateTime = lubridate::with_tz(DateTime, tzone = 'America/Jamaica'))

  if(!is.null(nms))
    out <- out %>% 
      dplyr::mutate(
        site = factor(site, levels = !!site, labels = nms)
      )
  
  return(out)
  
}
