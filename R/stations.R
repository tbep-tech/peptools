#' Bay stations by segment
#'
#' Bay stations by segment
#'
#' @format A \code{data.frame} object
#' @family utilities
#' @examples 
#' \dontrun{
#' #' library(dplyr)
#' library(tidyr)
#' library(sf)
#' library(readxl)
#' library(mapedit)
#' library(mapview)
#' 
#' prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
#' 
#' rawdat <- read_xlsx('data/data-raw/Peconics SCDHS WQ data - up to 2019 so far.xlsx', col_types = 'text') %>% 
#'   dplyr::select(Date, BayStation, sd = Secchi, chla = `Chlorophyll A - Total`) %>% 
#'   tidyr::pivot_longer(c('sd', 'chla')) %>% 
#'   na.omit() %>% 
#'   dplyr::mutate(
#'     value = gsub('>|<', '', value), 
#'     value = as.numeric(value)
#'   ) %>% 
#'   dplyr::group_by(Date, BayStation, name) %>%
#'   dplyr::summarise(value = mean(value, na.rm = T)) %>% 
#'   ungroup %>% 
#'   dplyr::mutate(
#'     Date = as.Date(as.numeric(Date), origin = '1899-12-30')
#'   )
#' 
#' locs <- read_xlsx('data/data-raw/MASTER LIST of stations & Lat Longs  020118.xlsx') %>% 
#'   filter(BayStation %in% rawdat$BayStation) %>% 
#'   select(BayStation, StationName, Latitude, Longitude) %>% 
#'   mutate(
#'     Latitude = as.numeric(Latitude), 
#'     Longitude = as.numeric(Longitude), 
#'     Longitude = case_when(
#'       sign(Longitude) == 1 ~ -1 * Longitude, 
#'       T ~ Longitude
#'     )
#'   ) %>% 
#'   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = prj)
#' 
#' # use this for spatial selection
#' p <- mapview(locs)
#' e1 <- editMap(p)
#' locs[e1$finished, ] %>% pull(BayStation) %>% dput
#' 
#' stations <- list(
#'   Western = c("60069", "60100", "60101", "60102", "60103", "60104", "60110", 
#'               "60113", "60120", "60130", "60140", "60148", "60150", "60160", 
#'               "60170", "60180", "60190", "60200", "60210", "60220", "60230", 
#'               "60240", "60250", "60255", "60260", "60265", "60266", "60270", 
#'               "60275", "60280", "60285", "60290"),
#'   Central = c("60081", "60105", "60106", "60107", "60108", "60109", "60114", 
#'               "60119", "60121", "60123", "60124", "60125", "60128", "60129", 
#'               "60300", "60310", "60320", "60340", "60350"), 
#'   Eastern = c("60111", "60112", "60115", "60116", "60117", "60118", "60122", 
#'               "60126", "60127", "60131", "60132", "60133", "60134", "60135", 
#'               "60136", "60137", "60138", "60139", "60141", "60142", "60143", 
#'               "60144", "60145", "60147", "60330", "60360", "60370")
#' ) %>% 
#'   tibble::enframe('bay_segment', 'BayStation') %>% 
#'   unnest(BayStation) %>% 
#'   left_join(locs, ., by = 'BayStation')
#' crds <- st_coordinates(stations)  
#' stations <- stations %>% 
#'   st_set_geometry(NULL) %>% 
#'   mutate(
#'     Longitude = crds[, 1],
#'     Latitude = crds[, 2]
#'   )
#' 
#' save(stations, file = 'data/stations.RData', compress = 'xz')
#' }
"stations"