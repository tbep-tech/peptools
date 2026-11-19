#' Import dissolved oxygen data
#'
#' Import dissolved oxygen data
#'
#' @param site chr string of site numbers for USGS stations
#' @param nms chr string vector of names to reassign to site numbers
#' @param startDate chr string of start date in YYYY-MM-DD
#' @param endDate chr string of end date in YYYY-MM-DD, data for the entire day are included
#'
#' @return data.frame
#' @export
#'
#' @importFrom dplyr "%>%"
#'
#' @details Raw data are downloaded from the USGS Water Data API using the dataRetrieval R package, this function is a simple wrapper to the \code{\link[dataRetrieval]{read_waterdata_continuous}} function. Start and end dates are for full days in Eastern Standard Time (no daylight savings), consistent with the \code{DateTime} column in the output.
#'
#' Note that downloading the station data with the default arguments may take several minutes. Sites are 01304562 for Peconic River, 01304200 for Orient Harbor, 01304650 for Shelter Island.
#'
#' The USGS Water Data API limits the number of requests per hour for users without an API key.  A key can be obtained from \url{https://api.waterdata.usgs.gov/signup/} and set with \code{Sys.setenv(API_USGS_PAT = "your_key")} if the limit is exceeded.
#'
#' @concept read
#'
#' @examples
#' \dontrun{
#' dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'),
#'   nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'),
#'   startDate = '2020-06-01', endDate = '2021-06-30')
#' dodat
#' }
read_pepdo <- function(site = c('01304562', '01304200', '01304650'), nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'), startDate = '2013-01-01', endDate = '2025-12-31'){

  if(!is.null(nms) && length(nms) != length(site))
    stop('nms must be the same length as site')

  strt <- as.Date(startDate, format = '%Y-%m-%d')
  ends <- as.Date(endDate, format = '%Y-%m-%d')
  if(is.na(strt) | is.na(ends))
    stop('startDate and endDate must be YYYY-MM-DD')
  if(strt > ends)
    stop('startDate must be before endDate')

  # get DO data from USGS stations
  # 01304562 is Peconic River, 01304200 is Orient Harbor, 01304650 is Shelter Island
  # downloaded data are in UTC, start and end dates are for days in EST
  # takes several minutes to dl
  # API limits queries to 1100 days, so requests are made by calendar year
  yrs <- seq(lubridate::year(strt), lubridate::year(ends))
  chkstr <- pmax(as.Date(paste0(yrs, '-01-01')), strt)
  chkend <- pmin(as.Date(paste0(yrs, '-12-31')), ends)

  out <- lapply(seq_along(yrs), function(i){

      dat <- dataRetrieval::read_waterdata_continuous(
        monitoring_location_id = paste0('USGS-', site),
        parameter_code = '00300',
        time = c(paste0(chkstr[i], 'T05:00:00Z'), paste0(chkend[i] + 1, 'T04:59:59Z')), # EST days as UTC
        properties = c('monitoring_location_id', 'time', 'value')
      )

      dat <- as.data.frame(dat)
      if(nrow(dat) == 0)
        return(NULL)

      dat[, c('monitoring_location_id', 'time', 'value')]

    }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(site = monitoring_location_id, DateTime = time, do_mgl = value) %>%
    dplyr::mutate(
      site = gsub('^USGS-', '', site),
      DateTime = lubridate::with_tz(DateTime, tzone = 'America/Jamaica'),
      do_mgl = as.numeric(do_mgl)
    ) %>%
    dplyr::arrange(match(site, !!site), DateTime)

  if(!is.null(nms))
    out <- out %>%
      dplyr::mutate(
        site = factor(site, levels = !!site, labels = nms)
      )

  return(out)

}
