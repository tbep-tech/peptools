#' Map water quality data for a selected year
#'
#' Map water quality data for a selected year
#'
#' @param dat data frame of data returned by \code{\link{read_pepwq}}
#' @param yrsel numeric for year to plot
#' @param param chr string indicating which water quality value to plot, one of "chla" for chlorophyll and "sd" for secchi depth
#' @param bay_segment chr string for the bay segment, one or all of "Western", "Central", or "Eastern"
#'
#' @details Year estimates for the selected parameter are based on median observations across months.  The color ramp is reversed for Secchi observations.
#' 
#' @family visualize
#'
#' @return A \code{\link[leaflet]{leaflet}} object
#'
#' @export
#'
#' @examples
#' show_sitemappep(rawdat, yrsel = 2018)
show_sitemappep <- function(dat, yrsel, param = c('chla', 'sd'), bay_segment = c('Western', 'Central', 'Eastern')){
  
  # sanity check
  if(!yrsel %in% dat$yr)
    stop(yrsel, ' not in year range for input data')
  
  param <- match.arg(param)
  
  mptyps <- c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap")
  prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

  # get site averages for selected year
  locs <- dat %>%
    dplyr::select(BayStation, bay_segment, yr, val = !!param) %>% 
    dplyr::filter(bay_segment %in% !!bay_segment) %>% 
    dplyr::filter(yr %in% !!yrsel) %>% 
    dplyr::group_by(BayStation, yr) %>% 
    dplyr::summarise(val = median(val, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(pepstations, .,by = c('BayStation')) %>% 
    na.omit() %>% 
    dplyr::mutate(
      cexs = scales::rescale(val, to = c(4, 17), from = range(val))
    ) %>% 
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

  # colors, reverse if chla
  cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')
  if(param == 'chla')
    cols <- rev(cols)
  colfun <- colorRampPalette(cols)
  
  # legend label
  lbs <- c('chla', 'sd')
  names(lbs) <- c('chl-a (ug/L)', 'secchi (ft)')
  nms <- names(lbs)[lbs == param]
  leglab <- paste0(yrsel, ' ', nms)
  
  # hover point labels
  labs <- paste('Bay station ', locs$BayStation, ', ', nms, ' ', round(locs$val, 2))
  
  # map
  out <- mapview::mapview(locs, cex = locs$cexs, legend = T, layer.name = leglab, zcol= 'val', col.regions = colfun, 
                        homebutton = F, map.types = mptyps, label = labs)
  
  return(out)

}