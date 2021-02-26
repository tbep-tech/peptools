#' @title Create a colorized table for beach pathogen exceedances
#'
#' @description Create a colorized table for beach pathogen exceedances
#'
#' @param entdat data frame of enterococcus data returned by \code{\link{read_pepent}}
#' @param txtsz numeric for size of text in the plot, applies only if \code{asreact = FALSE}
#' @param thr numeric value defining threshold for exceedance
#' @param yrrng numeric vector indicating min, max years to include
#' @param family optional chr string indicating font family for text labels
#'
#' @family visualize
#'
#' @return A static \code{\link[ggplot2]{ggplot}} object is returned if \code{asreact = FALSE}, otherwise a \code{\link[reactable]{reactable}} table is returned
#' 
#' @seealso \code{\link{anlz_entpep}}
#' @export
#'
#' @examples
#' show_entmatrix(entdat)
show_entmatrix <- function(entdat, txtsz = 2, thr = 104, yrrng = NULL, family = NA){

  # upper limit for color ramp
  top <- 10
  
  # process data to plot
  entpep <- anlz_entpep(entdat, thr = thr)
  
  # get years from data if yrrng not provided
  if(is.null(yrrng))
    yrrng <- range(entpep$yr, na.rm = T)
  
  # check if yrrng has two values
  if(length(yrrng) != 2)
    stop('yrrng must have two values')
  
  # sort year range
  yrrng <- sort(yrrng)
  
  toplo <- entpep %>% 
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) 
  
  toplo <- toplo %>% 
    dplyr::mutate(
      Name = factor(Name, levels = rev(unique(toplo$Name))), 
      lbs = exceedances, 
      exceedances = pmin(exceedances, top)
    )
  
  # ggplot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = factor(yr), y = Name, fill = exceedances)) +
    ggplot2::geom_tile(colour = 'black') +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = 'top') +
    ggplot2::scale_fill_gradient(paste('No. of samples'), low = 'white', high = 'blue', limits = c(0, top)) +
    ggplot2::theme_bw(base_family = family) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      # legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 0)
    ) + 
    ggplot2::labs(
      caption = paste0('Values are relative to a threshold of ', thr , ' cfu/100 mL.')
    )
  
  if(!is.null(txtsz))
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = lbs), size = txtsz, family = family)
  
  return(p)
  
}
  