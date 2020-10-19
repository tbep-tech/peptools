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
    dplyr::select(bay_segment, yr, mo, value, status) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      status = dplyr::case_when(
        status == '>' ~ 0, 
        T ~ 1
      )
    )
  monsdfit <- survival::survfit(survival::Surv(value, status) ~ bay_segment + yr + mo, data = monsd)
  medv <- summary(monsdfit)$table
  monsd <- monsdfit$strata %>% 
    names %>% 
    tibble::tibble(tosep = .) %>%
    dplyr::mutate(tosep = gsub('bay\\_segment=|yr=|mo=', '', tosep)) %>% 
    tidyr::separate(tosep, into = c('bay_segment', 'yr', 'mo'), sep = ', ') %>% 
    dplyr::mutate(
      yr = as.numeric(yr), 
      mo = as.numeric(mo), 
      val = as.numeric(medv[, 'median']),
      est = 'medv', 
      var = 'sd'
    )

  # secchi monthly averages
  monsd <- dat %>%
    dplyr::filter(name %in% 'sd') %>%
    dplyr::select(bay_segment, yr, mo, value, status) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      status = dplyr::case_when(
        status == '>' ~ 0, 
        T ~ 1
      )
    ) %>% 
    survival::survfit(survival::Surv(value, status) ~ bay_segment + yr + mo, data = .) %>% 
    summary() %>% 
    .$table %>% 
    as.data.frame %>% 
    dplyr::select(
      medv = median
    ) %>% 
    tibble::rownames_to_column(var = 'tosep') %>% 
    dplyr::mutate(tosep = gsub('bay\\_segment=|yr=|mo=', '', tosep)) %>% 
    tidyr::separate(tosep, into = c('bay_segment', 'yr', 'mo'), sep = ', ') %>% 
    tidyr::pivot_longer(-c('bay_segment', 'yr', 'mo'), names_to = 'est', values_to = 'val') %>% 
    dplyr::mutate(
      yr = as.numeric(yr),
      mo = as.numeric(mo),
      var= 'sd', 
      bay_segment = factor(bay_segment, levels = levels(yrchla$bay_segment))
    ) 

  # secchi yearly averages
  yrsd <- dat %>%
    dplyr::filter(name %in% 'sd') %>%
    dplyr::select(bay_segment, yr, value, status) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      status = dplyr::case_when(
        status == '>' ~ 0, 
        T ~ 1
      )
    ) %>% 
    survival::survfit(survival::Surv(value, status) ~ bay_segment + yr, data = .) %>% 
    summary() %>% 
    .$table %>% 
    as.data.frame %>% 
    dplyr::select(
      medv = median, 
      lwr.ci = `0.95LCL`,
      upr.ci = `0.95UCL`
      ) %>% 
    tibble::rownames_to_column(var = 'tosep') %>% 
    dplyr::filter(!is.na(lwr.ci) & !is.na(upr.ci)) %>%
    dplyr::mutate(tosep = gsub('bay\\_segment=|yr=', '', tosep)) %>% 
    tidyr::separate(tosep, into = c('bay_segment', 'yr'), sep = ', ') %>% 
    tidyr::pivot_longer(-c('bay_segment', 'yr'), names_to = 'est', values_to = 'val') %>% 
    dplyr::mutate(
      yr = as.numeric(yr),
      var= 'sd', 
      bay_segment = factor(bay_segment, levels = levels(yrchla$bay_segment))
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
    dplyr::arrange(var, bay_segment, -yr, -mo)
  
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
    dplyr::arrange(var, bay_segment, -yr)

  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )
  
  return(out)
  
}