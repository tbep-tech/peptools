#' Dissolved oxygen data for USGS stations
#'
#' Dissolved oxygen data for USGS stations
#'
#' @format A \code{data.frame} object
#' @family utilities
#' @examples 
#' \dontrun{
#' # get DO data from USGS stations
#' # 01304562 is Peconic River, 01304200 is Orient Harbor
#' # downloaded data are in UTC
#' # for cd column, codes are often “A” (approved) or “P” (provisional)
#' # from vignette https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
#' # takes a few minutes to dl
#' dodat <- readNWISuv(siteNumbers = '01304562', parameterCd = '00300', startDate = '2013-01-01', endDate = '2020-12-31') %>% 
#'   select(site = site_no, DateTime = dateTime, do_mgl = X_00300_00000) %>%
#'   mutate(DateTime = with_tz(DateTime, tzone = 'America/Jamaica'))
#' 
#' save(dodat, file = 'data/dodat.RData', compress = 'xz')
#'
#'}
"dodat"