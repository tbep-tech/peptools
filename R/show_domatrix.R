#' @title Create a colorized table for reporting dissolved oxygen data 
#'
#' @description Create a colorized table for reporting dissolved oxygen data
#'
#' @param dodat data frame of dissolved oxygen data returned by \code{\link{read_pepdo}}
#' @param show chr string indicating which summary value to plot from \code{\link{anlz_domopep}}, one of \code{'below_ave'} or \code{'below_maxrun'}
#' @param txtsz numeric for size of text in the plot, applies only if \code{asreact = FALSE}
#' @param thr numeric indicating appropriate dissolved oxygen thresholds, usually 3 mg/L for acute, 4.8 mg/L for chronic
#' @param yrrng numeric vector indicating min, max years to include
#' @param family optional chr string indicating font family for text labels
#'
#' @family visualize
#'
#' @return A static \code{\link[ggplot2]{ggplot}} object is returned.
#' 
#' @seealso \code{\link{anlz_domopep}}, \code{\link{anlz_dodlypep}}
#' @export
#'
#' @examples
#' show_domatrix(dodat)
show_domatrix <- function(dodat, show = c('below_ave', 'below_maxrun'), txtsz = 3, thr = 3, yrrng = c(2013, 2020), family = NA){
  
  show <- match.arg(show)
  
  # process data to plot
  dat <- anlz_domopep(dodat, thr = thr)
  toplo <- dat %>% 
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>% 
    dplyr::mutate(
      below_ave = round(below_ave, 2)
    )
  
  # ggplot
  pthm <- ggplot2::theme_bw(base_family = family) + 
    ggplot2::theme(
      axis.title = ggplot2::element_blank(), 
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      legend.title = ggplot2::element_blank(), 
      strip.background = ggplot2::element_blank()
    )
  
  if(show == 'below_ave')
    mid <- 0.5
  if(show == 'below_maxrun')
    mid <- 15
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes_string(x = 'mo', y = 'yr', fill = show)) + 
    ggplot2::geom_tile(colour = 'black') + 
    ggplot2::facet_wrap(~site, ncol = 1) + 
    ggplot2::scale_x_discrete(expand = c(0, 0)) + 
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(yrrng[1], yrrng[2]), trans = 'reverse') + 
    ggplot2::scale_fill_gradient2(low = 'green', mid = 'yellow', high = 'red', midpoint = mid) +
    ggplot2::labs(
      caption = paste0('Values are relative to a threshold of ', thr , ' mg/L.')
    ) +
    pthm  
  
  if(!is.null(txtsz))
    p <- p +
      ggplot2::geom_text(ggplot2::aes_string(label = show), size = txtsz, family = family)
  
  return(p)
  
}
