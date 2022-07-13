#' @title Plot annual water quality value and thresholds for all segments
#'
#' @description Plot annual water quality values and thresholds for all bay segments
#'
#' @param dat data frame of data returned by \code{\link{read_pepwq}}
#' @param param chr string indicating which water quality value and appropriate threshold to plot, one of "chla" for chlorophyll, "sd" for secchi depth, or "tn" for total nitrogen
#' @param trgs optional \code{data.frame} for annual bay segment water quality thresholds, defaults to \code{\link{peptargets}}, only applies if \code{param} is \code{"chla"} or \code{"sd"}
#' @param yrrng numeric vector indicating min, max years to include
#' @param family optional chr string indicating font family for text labels
#' @param labelexp logical indicating if y axis and target labels are plotted as expressions, default \code{TRUE}
#' @param txtlab logical indicating if a text label for the target value is shown in the plot, only applies if \code{param} is \code{"chla"} or \code{"sd"}
#'
#' @concept visualize
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @details This function is conceptually similar to \code{\link{show_thrpep}}, but results are shown as annual medians across all bay segments for the selected parameter.
#' 
#' @export
#'
#' @examples
#' show_allthrpep(rawdat, param = 'chl')
show_allthrpep <- function(dat, param = c('chla', 'sd', 'tn'), trgs = NULL, yrrng = NULL, family = NA, labelexp = TRUE, txtlab = TRUE){
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- peptargets
  
  # get years from data if yrrng not provided
  if(is.null(yrrng))
    yrrng <- range(dat$yr, na.rm = T)
  
  # check if yrrng has two values
  if(length(yrrng) != 2)
    stop('yrrng must have two values')
  
  # sort year range
  yrrng <- sort(yrrng)
  
  # wq to plot
  param <- match.arg(param)

  # chlorophyll, tn annual values
  yrchlatn <-  dat %>%
    dplyr::filter(name %in% c('chla', 'tn')) %>%
    dplyr::select(bay_segment, var = name, yr, mo, value) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(yr, var) %>%
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
      )
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
    survival::survfit(survival::Surv(value, status) ~ yr, data = ., conf.type = 'plain', robust = T) %>%
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
    dplyr::mutate(tosep = gsub('yr=', '', tosep)) %>%
    dplyr::rename(yr = tosep) %>%
    tidyr::pivot_longer(-c('yr'), names_to = 'est', values_to = 'val') %>%
    dplyr::mutate(
      yr = as.numeric(yr),
      var= 'sd'
    )
  
  # combine chla and sd annual data
  # create complete year cases
  anout <- dplyr::bind_rows(yrchlatn, yrsd) %>%
    dplyr::mutate(
      yr = factor(yr, levels = seq(min(yr), max(yr)))
    ) %>%
    tidyr::complete(yr, est, var) %>%
    dplyr::mutate(
      yr = as.numeric(as.character(yr))
    ) %>%
    dplyr::select(yr, val, est, var) %>%
    dplyr::arrange(var, -yr)
  
  # axis label
  if(labelexp)
    axlab <- dplyr::case_when(
      param == 'chla' ~ expression("Median Chl-a ("~ mu * "g\u00B7L"^-1 *")"),
      param == 'sd' ~ expression("Median Secchi (ft)"), 
      param == 'tn' ~ expression("Median TN (mg\u00B7L"^-1 *")")
    )
  if(!labelexp)
    axlab <- dplyr::case_when(
      param == 'chla' ~ "Median Chl-a (ug/L)",
      param == 'sd' ~ "Median Secchi (ft)", 
      param == 'tn' ~ "Median TN (mg/L)"
    )
  
  # get data to plot
  toplo <- anout %>%
    dplyr::filter(grepl(param, var)) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(est, val)
  
  # colors
  cols <- c("Annual Median"="red")

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = toplo, ggplot2::aes(x = yr, y = medv, colour = "Annual Median"), size = 3) +
    ggplot2::geom_line(data = toplo, ggplot2::aes(x = yr, y = medv, colour = "Annual Median"), linetype = 'solid', size = 0.75) +
    ggplot2::geom_errorbar(data = toplo, ggplot2::aes(x = yr, ymin = lwr.ci, ymax = upr.ci, colour = "Annual Median"), size = 0.5, width = 0.5) +
    ggplot2::labs(y = axlab, title = 'All segments') +
    ggplot2::scale_x_continuous(breaks = seq(yrrng[1], yrrng[2], by = 1)) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = '#ECECEC'),
      legend.position = 'top',#c(0.85, 0.95),
      legend.background = ggplot2::element_rect(fill=NA),
      legend.key = ggplot2::element_rect(fill = '#ECECEC'),
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, size = 7, hjust = 1)
    ) 
  
  if(param != 'tn'){
    
    cols <- c(cols, "Threshold"="blue")

    thrnum <- trgs %>% dplyr::pull(!!paste0(param, '_thresh')) %>% unique
    
    # threshold label
    if(labelexp)
      thrlab <- dplyr::case_when(
        param == 'chla' ~ paste(thrnum, "~ mu * g%.%L^{-1}"),
        param == 'sd' ~ paste(thrnum, "~ft")
      )
    if(!labelexp)
      thrlab <- dplyr::case_when(
        param == 'chla' ~ paste(thrnum, "ug/L"),
        param == 'sd' ~ paste(thrnum, "~ft")
      )
    
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept = thrnum, colour = 'Threshold'), linetype = 'dotted', size = 0.75) +
      ggplot2::scale_colour_manual(values = cols, labels = factor(names(cols), levels = names(cols))) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(
          shape = c(19, NA),
          colour = cols,
          linetype = c('solid', 'dotted'),
          size = c(0.75, 0.75)
        )
      ))
  
    if(txtlab)
      p <- p +
        ggplot2::geom_text(ggplot2::aes(yrrng[2], max(toplo$upr.ci, na.rm = T), label = thrlab), parse = labelexp, hjust = 1, vjust = 1, family = family, colour = 'blue')
    
  }
  
  if(param == 'tn')
    p <- p +
      ggplot2::scale_colour_manual(values = cols, labels = factor(names(cols), levels = names(cols))) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(
          shape = c(19),
          colour = cols,
          linetype = c('solid'),
          size = c(0.75)
        )
      ))
  
  return(p)
  
}