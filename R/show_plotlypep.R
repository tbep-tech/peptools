#' Plot chlorophyll and secchi data together with matrix outcomes
#'
#' @param dat \code{data.frame} formatted from \code{\link{read_pepwq}}
#' @param bay_segment chr string for the bay segment, one of "1a", "1b", "2", or "3"
#' @param yrrng numeric for year range to plot
#' @param chllim numeric vector of length two indicating range for the chlorophyll y-axis
#' @param seclim numeric vector of length two indicating range for the secchi y-axis
#'
#' @return An interactive plotly object
#' 
#' @concept visualize
#' 
#' @export
#'
#' @examples
#' show_plotlypep(rawdat)
show_plotlypep <- function(dat, bay_segment = c('1a', '1b', '2', '3'), yrrng = NULL, chllim = NULL,
                           seclim = NULL){
  
  bay_segment <- match.arg(bay_segment)

  # get years from data if yrrng not provided
  if(is.null(yrrng))
    yrrng <- range(dat$yr, na.rm = T)
  
  # check if yrrng has two values
  if(length(yrrng) != 2)
    stop('yrrng must have two values')
  
  # sort year range
  yrrng <- sort(yrrng)
  
  suppressMessages({
      
    p1 <- show_thrpep(dat, bay_segment = bay_segment, param = "chla", yrrng = yrrng, ylim = chllim, txtlab = F, labelexp = F) + 
      ggtitle(NULL) +
      scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(yrrng[1], yrrng[2]))
    p2 <- show_thrpep(dat, bay_segment = bay_segment, param = "sd", yrrng = yrrng, ylim = seclim, txtlab = F, labelexp = F) + 
      ggtitle(NULL) +
      scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(yrrng[1], yrrng[2]))
    
    p3 <- show_segmatrixpep(dat, bay_segment = bay_segment, yrrng = yrrng, txtsz = NULL) + 
      scale_y_continuous(expand = c(0,0), breaks = c(yrrng[1]:yrrng[2])) +
      coord_flip() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text = element_text(size = 12)
      ) 
    
  })
  
  p3 <- plotly::ggplotly(p3, tooltip = 'Result') 
  for(i in 1:length(p3$x$data)) p3$x$data[[i]]$showlegend <- FALSE    
  
  p1 <- plotly::ggplotly(p1)
  p2 <- plotly::ggplotly(p2)
  p2$x$data[[1]]$showlegend <- FALSE
  p2$x$data[[2]]$showlegend <- FALSE
  p2$x$data[[3]]$showlegend <- FALSE
  
  # remove unnecessary hover text
  p1$x$data[[1]]$text <- gsub('colour:\\sAnnual\\sMean$', '', p1$x$data[[1]]$text)
  p2$x$data[[1]]$text <- gsub('colour:\\sAnnual\\sMean$', '', p2$x$data[[1]]$text)
  
  out <- plotly::subplot(p1, p3, p2, nrows = 3, heights = c(0.4, 0.2, 0.4), shareX = T, titleY = TRUE)
  
  return(out)
  
}
