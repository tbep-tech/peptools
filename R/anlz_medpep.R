#' Estimate annual means
#'
#' Estimate annual means by segment for chlorophyll and secchi data
#'
#' @param dat \code{data.frame} formatted from \code{\link{read_pepwq}}
#'
#' @return Mean estimates for chlorophyll and secchi
#'
#' @family analyze
#'
#' @export
#'
#' @examples
#' # view average estimates
#' anlz_medpep(rawdat)
anlz_medpep <- function(dat){

  # chlorophyll monthly averages
  monchla <- dat %>%
    dplyr::filter(name %in% 'chla') %>% 
    dplyr::select(bay_segment, yr, mo, value) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr, mo) %>%
    dplyr::summarise(val = median(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      est = 'medv',
      var = 'chla'
      ) 
  
  # chlorophyll annual values
  yrchla <-  dat %>%
    dplyr::filter(name %in% 'chla') %>% 
    dplyr::select(bay_segment, yr, mo, value) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr) %>%
    tidyr::nest() %>% 
    dplyr::mutate(
      mdv = purrr::map(data, function(x){

        try({DescTools::MedianCI(x$value)}, silent = TRUE)
      
      })
    ) %>% 
    dplyr::select(-data) %>% 
    dplyr::filter(purrr::map(mdv, length) == 3) %>%
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
    dplyr::filter(name %in% 'sd') %>% 
    dplyr::select(bay_segment, yr, mo, value) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr, mo) %>%
    dplyr::summarise(val = median(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      est = 'medv',
      var = 'sd'
    ) 
  
  # secchi annual values
  yrsd <- dat %>%
    dplyr::filter(name %in% 'sd') %>% 
    dplyr::select(bay_segment, yr, mo, value) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(bay_segment, yr) %>%
    tidyr::nest() %>% 
    dplyr::mutate(
      mdv = purrr::map(data, function(x){
        
        try({DescTools::MedianCI(x$value)}, silent = TRUE)
        
      })
    ) %>% 
    dplyr::select(-data) %>% 
    dplyr::filter(purrr::map(mdv, length) == 3) %>%
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
  # create complete year cases
  moout <- dplyr::bind_rows(monchla, monsd) %>%
    dplyr::mutate(
      yr = factor(yr, levels = seq(min(yr), max(yr)))
    ) %>% 
    tidyr::complete(bay_segment, yr, mo, est, var) %>% 
    dplyr::mutate(
      yr = as.numeric(as.character(yr))
    ) %>% 
    dplyr::select(bay_segment, yr, mo, val, est, var) %>% 
    dplyr::arrange(var, bay_segment, yr, mo)
  
  # combine chla and sd monthly data
  # create complete year cases
  anout <- dplyr::bind_rows(yrchla, yrsd) %>% 
    dplyr::mutate(
      yr = factor(yr, levels = seq(min(yr), max(yr)))
    ) %>% 
    tidyr::complete(bay_segment, yr, est, var) %>% 
    dplyr::mutate(
      yr = as.numeric(as.character(yr))
    ) %>% 
    dplyr::select(bay_segment, yr, val, est, var) %>% 
    dplyr::arrange(var, bay_segment, yr)
  
  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )
  
  return(out)
  
}