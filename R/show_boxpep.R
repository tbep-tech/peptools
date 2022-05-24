#' @title Plot monthly chlorophyll, secchi, or tn values values for a segment
#'
#' @description Plot monthly chlorophyl, secchi, or tn values values for a bay segment
#'
#' @param dat data frame of data returned by \code{\link{read_pepwq}}
#' @param param chr string indicating which water quality value and appropriate threshold to plot, one of "chla" for chlorophyll, "sd" for secchi depth, or "tn" for total nitrogen
#' @param yrsel numeric for year to emphasize, shown as separate red points on the plot
#' @param yrrng numeric vector indicating min, max years to include
#' @param ptsz numeric indicating point size of observations not in \code{yrsel}
#' @param bay_segment chr string for the bay segment, one of "Western", "Central", or "Eastern"
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{peptargets}}, only applies if \code{param} is \code{"chla"} or \code{"sd"}
#' @param family optional chr string indicating font family for text labels
#' @param labelexp logical indicating if y axis and target labels are plotted as expressions, default \code{TRUE}
#' @param txtlab logical indicating if a text label for the target value is shown in the plot, only applies if \code{param} is \code{"chla"} or \code{"sd"}
#'
#' @concept visualize
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @details
#' Points not included in \code{yrsel} are plotted over the box plots using \code{\link[ggplot2]{position_jitter}}. Use \code{ptsz = -1} to suppress.  The dotted line in the plot shows the threshold if \code{param} is \code{"chla"} or \code{"sd"}.
#'
#' @export
#'
#' @examples
#' show_boxpep(rawdat, bay_segment = 'Western')
show_boxpep <- function(dat, param = c('chla', 'sd', 'tn'),  yrsel = NULL, yrrng = NULL, ptsz = 0.5, bay_segment = c('Western', 'Central', 'Eastern'), trgs = NULL, family = NA, labelexp = TRUE, txtlab = TRUE){
  
  # parameter
  param <- match.arg(param)
  
  # segment
  bay_segment <- match.arg(bay_segment)
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- peptargets
  
  # select curyr as max of yrrng if null
  if(is.null(yrsel))
    yrsel <- max(dat$yr, na.rm = TRUE)

  # get years from data if yrrng not provided
  if(is.null(yrrng))
    yrrng <- range(dat$yr, na.rm = T)
  
  # check if yrrng has two values
  if(length(yrrng) != 2)
    stop('yrrng must have two values')
  
  # monthly averages
  aves <- anlz_medpep(dat) %>%
    .$'mos' %>%
    dplyr::filter(var %in% !!param) %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::mutate(
      mo = lubridate::month(mo, label = T)
    )
  
  # sort year range
  yrrng <- sort(yrrng)
  
  # yrsel not in dat
  if(!yrsel %in% dat$yr)
    stop(paste('Check yrsel is within', paste(range(dat$yr, na.rm = TRUE), collapse = '-')))
  
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

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name) %>% 
    paste(., 'segment')
  
  # toplo1 is all but current year
  toplo1 <- aves %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(!yr %in% yrsel)
  
  # toplo2 is current year
  toplo2 <- aves %>%
    dplyr::filter(yr %in% yrsel)
  
  # colors and legend names
  cols <- c("black", "red")
  names(cols)[1] <- dplyr::case_when(
    yrsel == yrrng[1] ~ paste(yrrng[1] + 1, yrrng[2], sep = '-'),
    yrsel == yrrng[2] ~ paste(yrrng[1], yrrng[2] - 1, sep = '-'),
    yrsel > yrrng[1] & yrsel < yrrng[2] ~ paste(paste(yrrng[1], yrsel - 1, sep = '-'), paste(yrsel + 1, yrrng[2], sep = '-'), sep = ', '),
    T ~ paste(yrrng, collapse = '-')
  )
  names(cols)[2] <- as.character(yrsel)

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = toplo1, ggplot2::aes(x = mo, y = val, colour = names(cols)[1]), outlier.colour = NA) +
    ggplot2::geom_point(data = toplo1, ggplot2::aes(x = mo, y = val, group = yr, colour = names(cols)[1]), position = ggplot2::position_jitter(width = 0.2), size = ptsz) +
    ggplot2::geom_point(data = toplo2, ggplot2::aes(x = mo, y = val, group = yr, fill = names(cols)[2]), pch = 21, color = cols[2], size = 3, alpha = 0.7) +
    ggplot2::labs(y = axlab, title = ttl) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = '#ECECEC'),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = ggplot2::element_rect(fill=NA),
          legend.key = ggplot2::element_rect(fill = '#ECECEC'),
          legend.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, size = 8, hjust = 1)
    ) +
    ggplot2::scale_colour_manual(values = cols[1]) +
    ggplot2::scale_fill_manual(values = cols[2]) 

  if(param != 'tn'){
    
    # get lines to plot
    thrnum <- trgs %>%
      dplyr::filter(bay_segment %in% !!bay_segment) %>%
      dplyr::pull(!!paste0(param, '_thresh'))
    
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
      ggplot2::geom_hline(ggplot2::aes(yintercept = thrnum, linetype = 'Threshold'), colour = 'blue', size = 0.75)+
      ggplot2::scale_linetype_manual(values = 'dotted') +
      ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(colour = 'blue')))
      
    if(txtlab)
      p <- p +
        ggplot2::geom_text(ggplot2::aes(x = factor('Dec'), max(toplo1$val, na.rm = TRUE)), 
                           parse = labelexp, label = thrlab, hjust = 1, vjust = 1, colour = 'blue', family = family)
    
  }
  
  return(p)
  
}
