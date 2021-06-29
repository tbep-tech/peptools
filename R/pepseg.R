#' Polygon shapefile of segment boundaries
#'
#' Polygon shapefile of segment boundaries
#'
#' @format A \code{sf} object
#' @concept data
#' @examples 
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' 
#' pepseg <- st_read('~/Desktop/TBEP/Peconic/PEPSegments/PEP_Segments.shp') %>%
#'   st_transform(crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>% 
#'   mutate(
#'     bay_segment = case_when(
#'       NAME == 'Segment3' ~ 'Eastern', 
#'       NAME == 'Segment2' ~ 'Central', 
#'       NAME == 'Segment1' ~ 'Western', 
#'     )
#'   ) %>% 
#'   select(bay_segment) %>% 
#'   st_buffer(dist = 0) %>% 
#'   st_crop(xmin = -72.76, xmax = -71.8, ymin = 40.83, ymax = 41.2)
#'  
#' save(pepseg, file = 'data/pepseg.RData', compress = 'xz')
#' }
"pepseg"
