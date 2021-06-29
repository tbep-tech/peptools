#' @title Create a colorized table for chlorophyll or secchi exceedances
#'
#' @description Create a colorized table for chlorophyll or secchi exceedances
#'
#' @param dat \code{data.frame} formatted from \code{\link{read_pepwq}}
#' @param param chr string for which parameter to plot, one of \code{"chla"} for chlorophyll or \code{"sd"} for secchi
#' @param txtsz numeric for size of text in the plot, applies only if \code{tab = FALSE}
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{peptargets}}
#' @param yrrng numeric vector indicating min, max years to include
#' @param alpha numeric indicating color transparency
#' @param bay_segment chr string for bay segments to include, one to all of "Western", "Central", or "Eastern"
#' @param asreact logical indicating if a \code{\link[reactable]{reactable}} object is returned
#' @param nrows if \code{asreact = TRUE}, a numeric specifying number of rows in the table
#' @param family optional chr string indicating font family for text labels
#'
#' @concept visualize
#'
#' @return A static \code{\link[ggplot2]{ggplot}} object is returned if \code{asreact = FALSE}, otherwise a \code{\link[reactable]{reactable}} table is returned
#'
#' @seealso \code{\link{show_matrixpep}}, \code{\link{show_segmatrixpep}}
#' @export
#'
#' @importFrom dplyr "%>%"
#'
#' @import ggplot2
#'
#' @examples
#' show_wqmatrixpep(rawdat)
show_wqmatrixpep <- function(dat, param = c('chla', 'sd'), txtsz = 3, trgs = NULL, yrrng = NULL, alpha = 1, bay_segment = c("Western", "Central", "Eastern"), asreact = FALSE, nrows = 10, family = NA){

  # sanity checks
  param <- match.arg(param)
  
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
  
  # process data to plot
  meddat <- anlz_medpep(dat) %>%
    .$ann
  toplo <- meddat %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::filter(var %in% !!param) %>%
    tidyr::pivot_wider(names_from = 'est', values_from = 'val') %>% 
    dplyr::left_join(trgs, by = 'bay_segment') %>%
    dplyr::select(bay_segment, yr, var, lwr.ci, medv, upr.ci, thresh = !!paste0(param, '_thresh')) %>%
    dplyr::mutate(
      bay_segment = factor(bay_segment, levels = c("Western", "Central", "Eastern")),
      outcome = dplyr::case_when(
        var == 'sd' & medv >= thresh ~ 'green',
        var == 'sd' & medv < thresh ~ 'red', 
        var == 'chla' & medv < thresh ~ 'green',
        var == 'chla' & medv >= thresh ~ 'red'
      ),
      outcometxt = dplyr::case_when(
        outcome == 'red' ~ 'not met',
        outcome == 'green' ~ 'met'
      )
    )
  
  # reactable object
  if(asreact){
    
    totab <- toplo %>%
      dplyr::select(bay_segment, yr, outcometxt) %>%
      tidyr::spread(bay_segment, outcometxt)
    
    colfun <- function(x){
      
      out <- dplyr::case_when(
        x %in% c('not met') ~ scales::alpha('#FF3333', alpha),
        x %in% c('met') ~ scales::alpha('#33FF3B', alpha)
      )
      
      return(out)
      
    }
    
    # make reactable
    out <- show_reactablepep(totab, colfun, nrows = nrows)
    
    return(out)
    
  }
  
  # add descriptive labels, Result
  lbs <- dplyr::tibble(
    outcome = c('red', 'green'),
    Result = c('Not met', 'Met')
  )
  if(param == 'chla')
    rndval <- 1
  if(param == 'sd')
    rndval <- 2
  toplo <- toplo %>%
    dplyr::left_join(lbs, by = 'outcome') %>%
    dplyr::mutate(
      val = paste0('Median: ', round(medv, rndval)),
      thresh = paste0('Threshold: ', round(thresh, rndval))
    ) %>%
    tidyr::unite(segval, c('val', 'thresh'), sep = ', ') %>%
    dplyr::mutate(
      segval = paste0('(', segval, ')')
    ) %>%
    tidyr::unite(Result, c('Result', 'segval'), sep = ' ')
  
  # ggplot
  p <- ggplot(toplo, aes(x = bay_segment, y = yr, fill = outcome)) +
    geom_tile(aes(group = Result), colour = 'black', alpha = alpha) +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = 'red', green = 'green')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none', 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  
  if(!is.null(txtsz))
    p <- p +
      geom_text(aes(label = outcometxt), size = txtsz, family = family)
  
  return(p)
  
}
