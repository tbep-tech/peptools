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
#' anlz_medpep(rawdat)
anlz_medpep <- function(dat){

  # chlorophyll monthly averages
  monchla <- dat %>%
    dplyr::select(bay_segment, yr, mo, chla) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr, mo) %>%
    dplyr::summarise(val = median(chla, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      est = 'medv',
      var = 'chla'
      ) 
  
  # chlorophyll annual values
  yrchla <-  dat %>%
    dplyr::select(bay_segment, yr, chla) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr) %>%
    tidyr::nest() %>% 
    dplyr::mutate(
      mdv = purrr::map(data, function(x){

        try({DescTools::MedianCI(x$chla)}, silent = TRUE)
      
      })
    ) %>% 
    dplyr::select(-data) %>% 
    dplyr::rowwise() %>% 
    filter(!inherits(mdv, 'try-error')) %>% 
    dplyr::ungroup() %>% 
    tidyr::unnest_longer(mdv, values_to = 'val', indices_to = 'est') %>% 
    dplyr::mutate(
      est = dplyr::case_when(
        est %in% 'median' ~ 'medv', 
        TRUE ~ est
      ),
      var = 'chla'
    )
  
  # secchi monthly averages
  monsd <- dat %>%
    dplyr::select(bay_segment, yr, mo, sd) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr, mo) %>%
    dplyr::summarise(val = median(sd, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      est = 'medv',
      var = 'sd'
    ) 
  
  # secchi annual values
  yrsd <- dat %>%
    dplyr::select(bay_segment, yr, sd) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr) %>%
    tidyr::nest() %>% 
    dplyr::mutate(
      mdv = purrr::map(data, function(x){
        
        try({DescTools::MedianCI(x$sd)}, silent = TRUE)
        
      })
    ) %>% 
    dplyr::select(-data) %>% 
    dplyr::rowwise() %>% 
    filter(!inherits(mdv, 'try-error')) %>% 
    dplyr::ungroup() %>% 
    tidyr::unnest_longer(mdv, values_to = 'val', indices_to = 'est') %>% 
    dplyr::mutate(
      est = dplyr::case_when(
        est %in% 'median' ~ 'medv', 
        TRUE ~ est
      ),
      var = 'sd'
    )
  
  # combine chla and sd monthly data
  moout <- bind_rows(monchla, monsd)
  
  # combine mtb ests with others, annual
  # geisson factors for secchi depth for % light availability
  anout <- bind_rows(yrchla, yrsd)
  
  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )
  
  return(out)
  
}