#' Get attainment categories
#'
#' Get attainment categories for each year and bay segment using chlorophyll and secchi depth
#'
#' @param meddat result returned from \code{\link{anlz_medpep}}
#' @param magdurout logical indicating if the separate magnitude and duration estimates are returned
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{peptargets}}
#'
#' @return A \code{data.frame} for each year and bay segment showing the attainment category
#' @export
#'
#' @family analyze
#'
#' @examples
#' meddat <- anlz_medpep(rawdat)
#' anlz_attainpep(meddat)
anlz_attainpep <- function(meddat, magdurout = FALSE, trgs = NULL){
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- peptargets
  
  # format targets
  trgs <- trgs %>%
    tidyr::pivot_longer(c('sd_thresh','chla_thresh'), names_to = 'var', values_to = 'val') %>%
    tidyr::separate(var, c('var', 'trgtyp'), sep = '_') %>%
    tidyr::pivot_wider(names_from = 'trgtyp', values_from = 'val') %>%
    dplyr::select(bay_segment, var, thresh)

  # get annual averages, join with targets
  annave <- meddat$ann %>%
    dplyr::left_join(trgs, by = c('bay_segment', 'var')) %>% 
    tidyr::pivot_wider(names_from = 'est', values_from = 'val')

  # get magnitude and durations
  magdur <- annave %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      mags = ifelse(!is.na(medv), findInterval(thresh, c(lwr.ci, upr.ci)), NA),
      mags = dplyr::case_when(
        var %in% 'chla' ~ as.integer(abs(mags - 2)), # reverse chloropyll
        T ~ mags
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(bay_segment, var) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, function(data){

        out <- data %>%
          dplyr::mutate(
            durats = stats::filter(mags > 0, filter = rep(1, 4), sides = 1)
          )
        
        return(out)
        
      })
    ) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        is.na(durats) & mags == 2 ~ 3L,
        is.na(durats) & mags == 1 ~ 2L,
        is.na(durats) & mags == 0 ~ 0L,
        mags == 2 & durats == 4 ~ 3L,
        mags == 2 & durats < 4 ~ 2L,
        mags == 1 & durats == 4 ~ 2L,
        mags == 1 & durats < 4 ~ 1L,
        mags == 0 ~ 0L
      )
    ) %>%
    dplyr::ungroup()
  
  if(magdurout)
    return(magdur)
  
  # get final outcomes
  out <- magdur %>%
    dplyr::select(bay_segment, yr, var, outcome) %>%
    tidyr::spread(var, outcome) %>%
    # na.omit %>%
    tidyr::unite('chla_sd', chla, sd) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        chla_sd %in% '0_0' ~ 'green',
        chla_sd %in% c('1_0', '2_0', '3_0', '0_1', '1_1', '2_1', '0_2', '1_2', '0_3') ~ 'yellow',
        chla_sd %in% c('3_1', '2_2', '3_2', '1_3', '2_3', '3_3') ~ 'red'
      )
    )
  
  return(out)
  
}