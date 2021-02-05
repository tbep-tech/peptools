#' Plot chlorophyll and secchi data together with matrix outcomes
#'
#' @param dat \code{data.frame} formatted from \code{\link{read_pepwq}}
#' @param bay_segment chr string for the bay segment, one of "Western", "Central", or "Eastern"
#' @param yrrng numeric for year range to plot
#'
#' @return An interactive plotly object
#' 
#' @family visualize
#' 
#' @export
#'
#' @examples
#' show_plotlypep(rawdat)
show_plotlypep <- function(dat, bay_segment = c('Western', 'Central', 'Eastern'), yrrng = c(1990, 2020)){
  
  bay_segment <- match.arg(bay_segment)

  suppressMessages({
      
    p1 <- show_thrpep(dat, bay_segment = bay_segment, param = "chla", yrrng = yrrng, txtlab = F, labelexp = F) + 
      ggtitle(NULL) +
      scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(yrrng[1], yrrng[2]))
    p2 <- show_thrpep(dat, bay_segment = bay_segment, param = "sd", yrrng = yrrng, txtlab = F, labelexp = F) + 
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
