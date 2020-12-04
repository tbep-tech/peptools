#' @title Create a colorized table for indicator reporting
#'
#' @description Create a colorized table for indicator reporting
#'
#' @param dat data frame of water quality data returned by \code{\link{read_pepwq}}
#' @param txtsz numeric for size of text in the plot, applies only if \code{asreact = FALSE}
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{peptargets}}
#' @param yrrng numeric vector indicating min, max years to include
#' @param bay_segment chr string for bay segments to include, one to all of "Western", "Central", or "Eastern"
#' @param asreact logical indicating if a \code{\link[reactable]{reactable}} object is returned
#' @param nrows if \code{asreact = TRUE}, a numeric specifying number of rows in the table
#' @param abbrev logical indicating if text labels in the plot are abbreviated as the first letter
#' @param family optional chr string indicating font family for text labels
#'
#' @family visualize
#'
#' @return A static \code{\link[ggplot2]{ggplot}} object is returned if \code{asreact = FALSE}, otherwise a \code{\link[reactable]{reactable}} table is returned
#' 
#' @export
#'
#' @examples
#' show_matrixpep(rawdat)
show_matrixpep <- function(dat, txtsz = 3, trgs = NULL, yrrng = c(1990, 2019), bay_segment = c('Western', 'Central', 'Eastern'), asreact = FALSE, nrows = 10, abbrev = FALSE, family = NA){
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- peptargets
  
  # process data to plot
  medpep <- anlz_medpep(dat)
  toplo <- anlz_attainpep(medpep, trgs = trgs) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::mutate(
      bay_segment = factor(bay_segment, levels = c('Western', 'Central', 'Eastern'))
    )
  
  # add abbreviations if true
  if(abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = dplyr::case_when(
        outcome == 'red' ~ 'R',
        outcome == 'yellow' ~ 'Y',
        outcome == 'green' ~ 'G'
      )
    )
  if(!abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = outcome
    )
  
  # reactable object
  if(asreact){
    
    totab <- toplo %>%
      dplyr::select(bay_segment, yr, outcometxt) %>%
      tidyr::spread(bay_segment, outcometxt)
    
    colfun <- function(x){
      
      out <- dplyr::case_when(
        x %in% c('R', 'red') ~ '#FF3333',
        x %in% c('Y', 'yellow') ~ '#F9FF33',
        x %in% c('G', 'green') ~ '#33FF3B'
      )
      
      return(out)
      
    }
    
    
    # make reactable
    out <- show_reactablepep(totab, colfun, nrows = nrows)
    
    return(out)
    
  }
  
  # add descriptive labels, Action
  lbs <- dplyr::tibble(
    outcome = c('red', 'yellow', 'green'),
    Action = c('On Alert', 'Caution', 'Stay the Course')
  )
  toplo <- toplo %>%
    dplyr::left_join(lbs, by = 'outcome') %>%
    tidyr::separate(chla_sd, c('chla', 'sd'), sep = '_', remove = F) %>%
    dplyr::mutate(
      chla = paste0('chla: ', chla),
      sd = paste0('sd: ', sd)
    ) %>%
    tidyr::unite(chla_sd, c('chla', 'sd'), sep = ', ') %>%
    dplyr::mutate(
      chla_sd = paste0('(', chla_sd, ')')
    ) %>%
    tidyr::unite(Action, c('Action', 'chla_sd'), sep = ' ')
  
  # ggplot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = bay_segment, y = yr, fill = outcome)) +
    ggplot2::geom_tile(ggplot2::aes(group = Action), colour = 'black') +
    ggplot2::scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = 'top') +
    ggplot2::scale_fill_manual(values = c(red = 'red', yellow = 'yellow', green = 'green')) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      legend.position = 'none', 
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank()
    )
  
  if(!is.null(txtsz))
    p <- p +
    ggplot2::geom_text(ggplot2::aes(label = outcometxt), size = txtsz, family = family)
  
  return(p)
  
}