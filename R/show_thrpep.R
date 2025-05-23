#' @title Plot annual water quality value and thresholds for a segment
#'
#' @description Plot annual water quality values and thresholds for a bay segment
#'
#' @param dat data frame of data returned by \code{\link{read_pepwq}}
#' @param bay_segment chr string for the bay segment, one of "1a", "1b", "2", or "3"
#' @param param chr string indicating which water quality value and appropriate threshold to plot, one of "chla" for chlorophyll, "sd" for secchi depth, or "tn" for total nitrogen
#' @param ylim numeric vector of length two indicating range for the y-axis
#' @param trgs optional \code{data.frame} for annual bay segment water quality thresholds, defaults to \code{\link{peptargets}}
#' @param yrrng numeric vector indicating min, max years to include
#' @param family optional chr string indicating font family for text labels
#' @param labelexp logical indicating if y axis and target labels are plotted as expressions, default \code{TRUE}
#' @param txtlab logical indicating if a text label for the target value is shown in the plot
#'
#' @concept visualize
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @export
#'
#' @examples
#' show_thrpep(rawdat, bay_segment = '1a', param = 'chl')
show_thrpep <- function(dat, bay_segment = c('1a', '1b', '2', '3'), param = c('chla', 'sd', 'tn'), ylim = NULL, trgs = NULL, yrrng = NULL, family = NA, labelexp = TRUE, txtlab = TRUE){
  
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
  
  # segment
  bay_segment <- match.arg(bay_segment)
  
  # wq to plot
  param <- match.arg(param)
  
  # colors
  cols <- c("Annual Median"="red")
  
  # averages
  aves <- anlz_medpep(dat)
  
  # axis label
  if(labelexp)
    axlab <- ifelse(param == 'chla', expression("Median Chl-a ("~ mu * "g\u00B7L"^-1 *")"),
      ifelse(param == 'sd', expression("Median Secchi (ft)"),
        ifelse(param == 'tn', expression("Median TN (mg\u00B7L"^-1 *")"),
          NA_character_
        )
      )
    )
  if(!labelexp)
    axlab <- dplyr::case_when(
      param == 'chla' ~ "Median Chl-a (ug/L)",
      param == 'sd' ~ "Median Secchi (ft)", 
      param == 'tn' ~ "Median TN (mg/L)"
    )

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name) %>% 
    paste(., 'segment')
  
  # get data to plo
  toplo <- aves$ann %>%
    dplyr::filter(grepl(param, var)) %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(est, val)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = toplo, ggplot2::aes(x = yr, y = medv, colour = "Annual Median"), size = 3) +
    ggplot2::geom_line(data = toplo, ggplot2::aes(x = yr, y = medv, colour = "Annual Median"), linetype = 'solid', size = 0.75) +
    ggplot2::geom_errorbar(data = toplo, ggplot2::aes(x = yr, ymin = lwr.ci, ymax = upr.ci, colour = "Annual Median"), size = 0.5, width = 0.5) +
    ggplot2::labs(y = axlab, title = ttl) +
    ggplot2::scale_x_continuous(breaks = seq(yrrng[1], yrrng[2], by = 1)) +
    ggplot2::coord_cartesian(ylim = ylim) + 
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

  # colors
  cols <- c(cols, "Threshold"="blue")
  
  # get lines to plot
  toln <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment)
  thrnum <- toln %>% dplyr::pull(!!paste0(param, '_thresh'))
  
  # threshold label
  if(labelexp)
    thrlab <- dplyr::case_when(
      param == 'chla' ~ paste(thrnum, "~ mu * g%.%L^{-1}"),
      param == 'sd' ~ paste(thrnum, "~ft"), 
      param == 'tn' ~ paste(thrnum, "~ mg%.%L^{-1}")
    )
  if(!labelexp)
    thrlab <- dplyr::case_when(
      param == 'chla' ~ paste(thrnum, "ug/L"),
      param == 'sd' ~ paste(thrnum, "~ft"), 
      param == 'tn' ~ paste(thrnum, "mg/L")
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
  
  return(p)
  
}
