#' Bay stations by segment
#'
#' Bay stations by segment
#'
#' @format A \code{data.frame} object
#' @concept data
#' @examples 
#' \dontrun{
#' library(tidyr)
#' library(sf)
#' library(dplyr)
#' library(readxl)
#' library(mapedit)
#' library(mapview)
#' 
#' prj <- 4326
#' 
#' locs <- read_xlsx('inst/extdata/stationmeta.xlsx') %>%
#'   select(BayStation = `Station Number`, StationName, Longitude = LON, Latitude = LAT) %>%
#'   mutate(
#'     BayStation = as.character(BayStation)
#'   ) %>% 
#'   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = prj)
#' 
#' # # use this for spatial selection
#' # p <- mapview(pepseg) + mapview(locs)
#' # e1 <- editMap(p)
#' # locs[e1$finished, ] %>% pull(BayStation) %>% dput
#' 
#' pepstations <- list(
#'   `1a` = c("60280", "60275", "60270", "60265", "60266", "60260", "60250", 
#'               "60240", "60230", "60210", "60220"),
#'   `1b` = c("60170", "60101", "60130", "60290", "60148"), 
#'   `2` = c("60102", "60103", "60113", "60104", "60300", "60105", "60106", 
#'               "60107", "60310", "60320", "60114", "60109", "60340", "60121", 
#'               "60124", "60119", "60127", "60126", "60111", "60131", "60118"),
#'   `3` = c("60122", "60115", "60116", "60330", "60132", "60137", "60133", 
#'               "60134", "60135")
#' ) %>%
#'   tibble::enframe('bay_segment', 'BayStation') %>%
#'   unnest(BayStation) %>%
#'   left_join(locs, ., by = 'BayStation')
#' crds <- st_coordinates(pepstations)
#' pepstations <- pepstations %>%
#'   st_set_geometry(NULL) %>%
#'   mutate(
#'     Longitude = crds[, 1],
#'     Latitude = crds[, 2], 
#'     bay_segment = factor(bay_segment, levels = c('1a', '1b', '2', '3'))
#'   )
#' 
#' save(pepstations, file = 'data/pepstations.RData', compress = 'xz')
#'}
"pepstations"
