#' Map water quality data for a selected year
#'
#' Map water quality data for a selected year
#'
#' @param dat data frame of data returned by \code{\link{read_pepwq}}
#' @param yrsel numeric for year to plot
#' @param param chr string indicating which water quality value to plot, one of "chla" for chlorophyll and "sd" for secchi depth
#' @param bay_segment chr string for the bay segment, one or all of "Western", "Central", or "Eastern"
#' @param maxrel numeric for the maximum quantile value for scaling if \code{relative = T}, this prevents outliers from skewing the scale
#' @param relative logical indicating if sizes and colors are relative to the entire water quality data base, otherwise scaling is relative only for the points on the map
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
show_sitemappep <- function(dat, yrsel, param = c('chla', 'sd'), bay_segment = c('Western', 'Central', 'Eastern'), maxrel = 0.99, relative = FALSE){

  # sanity check
  if(!yrsel %in% dat$yr)
    stop(yrsel, ' not in year range for input data')
  
  param <- match.arg(param)
  
  mptyps <- c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap")
  prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  
  # get data to process
  locs <- dat %>%
    dplyr::filter(name %in% !!param) %>% 
    dplyr::filter(bay_segment %in% !!bay_segment) %>% 
    dplyr::filter(yr %in% !!yrsel) %>% 
    dplyr::select(BayStation, bay_segment, yr, val = value, status) 
  
  if(param == 'chla')
    locs <- locs %>% 
      dplyr::group_by(BayStation, yr) %>% 
      dplyr::summarise(val = median(val, na.rm = TRUE)) %>% 
      dplyr::ungroup()
  
  if(param == 'sd')
    locs <- locs %>% 
      dplyr::mutate(
        status = dplyr::case_when(
          status == '>' ~ 0, 
          T ~ 1
        )
      ) %>% 
      survival::survfit(survival::Surv(val, status) ~ BayStation + yr, data = .) %>% 
      summary() %>% 
      .$table %>% 
      as.data.frame %>% 
      dplyr::select(
        val = median
      ) %>% 
      tibble::rownames_to_column(var = 'tosep') %>% 
      dplyr::mutate(tosep = gsub('BayStation=|yr=', '', tosep)) %>% 
      tidyr::separate(tosep, into = c('BayStation', 'yr'), sep = ', ') %>% 
      dplyr::mutate(
        yr = as.numeric(yr)
      ) 

  # get max/min values for scaling, uses whole dataset if relative is true
  relvls <- range(locs$val, na.rm = TRUE)
  if(relative){
    relvls <- dat %>% 
      dplyr::filter(name %in% param) %>% 
      dplyr::pull(value)
    relvls <- c(min(relvls), quantile(relvls, maxrel, na.rm = T))
  }

  # join with pepstations, make sf
  locs <- locs %>% 
    dplyr::left_join(pepstations, .,by = c('BayStation')) %>% 
    na.omit() %>% 
    dplyr::mutate(
      val2 = pmin(val, max(relvls)), 
      cexs = scales::rescale(val2, to = c(4, 17), from = relvls)
    ) %>% 
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

  # reverse size scale if sd
  if(param == 'sd')
    locs <- locs %>% 
      dplyr::mutate(
        cexs = max(cexs, na.rm = T) - cexs + min(cexs, na.rm = T)
      )
  
  # colors, reverse if chla
  cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')
  if(param == 'chla')
    cols <- rev(cols)
  colfun <- leaflet::colorNumeric(palette = cols, domain = relvls)

  # legend label
  lbs <- c('chla', 'sd')
  names(lbs) <- c('chl-a (ug/L)', 'secchi (ft)')
  nms <- names(lbs)[lbs == param]
  leglab <- paste0(yrsel, ' ', nms)
  
  # hover point labels
  labs <- paste('Bay station ', locs$BayStation, ', ', nms, ' ', round(locs$val, 2))

  # map
  out <- mapview::mapview(locs, legend = F, homebutton = F, map.types = mptyps) %>% 
    .@map %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addCircleMarkers(
      data = locs,
      stroke = TRUE,
      color = 'black',
      fill = TRUE,
      fillColor = ~colfun(val2),
      weight = 1,
      fillOpacity = 0.8,
      radius = ~cexs,
      label = labs
    ) %>%  
    leaflet::addLegend('bottomright', title = leglab, pal = colfun, values = locs$val2)
  
  return(out)

}
