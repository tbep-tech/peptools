#' @title Create a colorized table for beach pathogen exceedances
#'
#' @description Create a colorized table for beach pathogen exceedances
#'
#' @param entdat data frame of enterococcus data returned by \code{\link{read_pepent}}
#' @param txtsz numeric for size of text in the plot, applies only if \code{asreact = FALSE}
#' @param thr numeric value defining threshold for exceedance
#' @param yrrng numeric vector indicating min, max years to include
#' @param nrows if \code{asreact = TRUE}, a numeric specifying number of rows in the table
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
show_entmatrix <- function(entdat, txtsz = 3, thr = 104, cats = c(0, 1, 2), yrrng = c(2010, 2020), nrows = 10, family = NA){

  # process data to plot
  entpep <- anlz_entpep(entdat, thr = thr)
  toplo <- entpep %>% 
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) 
  
  toplo <- toplo %>% 
    dplyr::mutate(
      Name = factor(Name, levels = rev(unique(toplo$Name))), 
      perexceedances = round(perexceedances, 2)
    )
  
  # ggplot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = factor(yr), y = Name, fill = perexceedances)) +
    ggplot2::geom_tile(colour = 'black') +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = 'top') +
    ggplot2::scale_fill_gradient('Prop. of samples\nexceeding', low = 'white', high = 'green', limits = c(0, 1)) +
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
      ggplot2::geom_text(ggplot2::aes(label = perexceedances), size = txtsz, family = family)
  
  return(p)
  
}
  