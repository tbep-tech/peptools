#' Map water quality data for a selected year
#'
#' Map water quality data for a selected year
#'
#' @param dat data frame of data returned by \code{\link{read_pepwq}}
#' @param yrsel numeric for years to plot, see details
#' @param mosel numeric for months to plot, see details
#' @param param chr string indicating which water quality value to plot, one of "chla" for chlorophyll, "sd" for secchi depth, or "tn" for total nitrogen
#' @param bay_segment chr string for the bay segment, one or all of "Western", "Central", or "Eastern"
#' @param maxrel numeric for the maximum quantile value for scaling if \code{relative = T}, this prevents outliers from skewing the scale
#' @param relative logical indicating if sizes and colors are relative to the entire water quality data base, otherwise scaling is relative only for the points on the map
#' 
#' @details 
#' Year estimates for the selected parameter are based on median observations across months. All twelve months are used if \code{mosel = NULL} (default). Monthly estimates for the selected parameter are based on median observations across years.  All years are used if \code{yrsel = NULL} (default).
#' 
#' The color ramp is reversed for Secchi observations.
#' 
#' @concept visualize
#'
#' @return A \code{\link[leaflet]{leaflet}} object
#'
#' @export
#'
#' @examples
#' 
#' # 2018, all months
#' show_sitemappep(rawdat, yrsel = 2018)
#' 
#' # 2018, July only
#' show_sitemappep(rawdat, yrsel = 2018, mosel = 7)
#' 
#' # July only, all years
#' show_sitemappep(rawdat, mosel = 7)
show_sitemappep <- function(dat, yrsel = NULL, mosel = NULL, param = c('chla', 'sd', 'tn'), bay_segment = c('Western', 'Central', 'Eastern'), maxrel = 0.99, relative = FALSE){

  param <- match.arg(param)
  
  # mosel full range if null
  if(!is.null(mosel)){
    if(!any(mosel %in% seq(1, 12)))
      stop('mosel must be from 1 to 12')
    if(!all(abs(diff(mosel)) == 1))
      stop('mosel must be in a sequence')
  }
  
  if(is.null(mosel))
    mosel <- seq(1, 12)

  # full range yrsel if null
  yrrng <- range(dat$yr, na.rm = TRUE)
  if(!is.null(yrsel)){
    if(!any(yrsel %in% seq(yrrng[1], yrrng[2])))
      stop(yrsel, ' not in range ', paste(yrrng, collapse = ' to '))
    if(!all(abs(diff(yrsel)) == 1))
      stop('yrsel must be in a sequence')
  }
  
  if(is.null(yrsel))
    yrsel <- seq(yrrng[1], yrrng[2])

  mptyps <- c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap")
  prj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  
  # get data to process
  locs <- dat %>%
    dplyr::filter(name %in% !!param) %>% 
    dplyr::filter(bay_segment %in% !!bay_segment) %>% 
    dplyr::filter(yr %in% !!yrsel) %>% 
    dplyr::filter(mo %in% !!mosel) %>% 
    dplyr::select(BayStation, bay_segment, yr, mo, val = value, status) 
  
  if(param %in% c('chla', 'tn'))
    locs <- locs %>% 
      dplyr::group_by(BayStation) %>% 
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
      dplyr::filter(yr %in% yrsel & mo %in% mosel) %>% 
      survival::survfit(survival::Surv(val, status) ~ BayStation, data = .) %>% 
      summary() %>% 
      .$table %>% 
      as.data.frame %>% 
      tibble::rownames_to_column(var = 'tosep') %>%   
      dplyr::select(
        BayStation = tosep,
        val = median
        ) %>% 
      dplyr::mutate(BayStation = gsub('BayStation=', '', BayStation))

  # get max/min values for scaling, uses whole dataset if relative is true
  relvls <- range(locs$val, na.rm = TRUE)
  if(relative){
    relvls <- dat %>% 
      dplyr::filter(name %in% param) %>% 
      dplyr::pull(value)
    relvls <- c(min(relvls, na.rm = T), quantile(relvls, maxrel, na.rm = T))
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
  
  # colors, reverse if chla, tn
  cols <- RColorBrewer::brewer.pal(11, 'RdYlBu')
  if(param %in% c('chla', 'tn'))
    cols <- rev(cols)
  colfun <- leaflet::colorNumeric(palette = cols, domain = relvls)

  ##
  # legend label
  
  lbs <- c('chla', 'sd', 'tn')
  names(lbs) <- c('chl-a (ug/L)', 'secchi (ft)', 'TN (mg/L)')
  nms <- names(lbs)[lbs == param]
  
  if(length(yrsel) > 1)
    yrlab <- paste(range(yrsel), collapse = '-')
  if(length(yrsel) == 1)
    yrlab <- yrsel
  if(length(mosel) > 1)
    molab <- paste(lubridate::month(range(mosel), label = TRUE), collapse = '-')
  if(length(mosel) == 1)
    molab <- lubridate::month(mosel, label = TRUE)
  
  leglab <- paste0('Median ', nms, '</br>', yrlab, ', ', molab)
  
  # hover point labels
  labs <- paste('Bay station ', locs$BayStation, ', ', nms, ' ', round(locs$val, 2))

  # map
  out <- mapview::mapview(locs, legend = F, homebutton = F, map.types = mptyps, alpha.regions = 0, alpha = 0, popup = F, label = F, cex = -1) %>% 
    .@map %>% 
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
