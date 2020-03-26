#' Estimate annual means
#'
#' Estimate annual means by segment for chlorophyll and secchi data
#'
#' @param dat\code{data.frame} formatted from \code{read_pepwq}
#'
#' @return Mean estimates for chlorophyll and secchi
#'
#' @family analyze
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' # view average estimates
#' anlz_avepep(rawdat)
anlz_avepep <- function(dat){
  
  # chlorophyll monthly averages
  monchla <- dat %>%
    dplyr::select(yr, mo, bay_segment, chla) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(yr, mo, bay_segment) %>%
    dplyr::summarise(mean_chla = mean(chla)) %>%
    dplyr::ungroup()
  
  # chlorophyll annual values
  yrchla <- monchla %>%
    dplyr::select(bay_segment, yr, mean_chla) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr) %>%
    dplyr::summarise(mean_chla = mean(mean_chla)) %>%
    dplyr::ungroup()
  
  # secchi monthly averages
  monsd <- dat %>%
    dplyr::select(yr, mo, bay_segment, sd) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(yr, mo, bay_segment) %>%
    dplyr::summarise(mean_sd = mean(sd)) %>%
    dplyr::ungroup()
  
  # secchi annual values
  yrsd <- monsd %>%
    dplyr::select(bay_segment, yr, mean_sd) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr) %>%
    dplyr::summarise(mean_sd = mean(mean_sd)) %>%
    dplyr::ungroup()
  
  # combine chla and sd monthly data
  chlamodata <- monchla %>% 
    tidyr::pivot_longer(mean_chla, names_to = 'var', values_to = 'val') 
  sdmodata <- monsd %>% 
    tidyr::pivot_longer(mean_sd, names_to = 'var', values_to = 'val')
  moout <- bind_rows(chlamodata, sdmodata)
  
  # combine mtb ests with others, annual
  # geisson factors for secchi depth for % light availability
  chladata <- yrchla %>% 
    tidyr::pivot_longer(mean_chla, names_to = 'var', values_to = 'val') 
  sddata <-  yrsd %>% 
    tidyr::pivot_longer(mean_sd, names_to = 'var', values_to = 'val') 
  anout <- bind_rows(chladata, sddata)
  
  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )
  
  return(out)
  
}