#' @title Create a colorized table for water quality outcomes and exceedances by segment
#'
#' @description Create a colorized table for water quality outcomes by segment that includes the management action and chlorophyll, and secchi exceedances
#'
#' @param dat \code{data.frame} formatted from \code{\link{read_pepwq}}
#' @param txtsz numeric for size of text in the plot, applies only if \code{tab = FALSE}
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{peptargets}}
#' @param yrrng numeric vector indicating min, max years to include
#' @param bay_segment chr string for bay segments to include, one to all of "Western", "Central", or "Eastern"
#' @param abbrev logical indicating if text labels in the plot are abbreviated as the first letter
#' @param family optional chr string indicating font family for text labels
#'
#' @return A static \code{\link[ggplot2]{ggplot}} object is returned
#'
#' @family visualize
#'
#' @details This function provides a combined output for the \code{\link{show_wqmatrixpep}} and \code{\link{show_matrixpep}} functions. Only one bay segment can be plotted for each function call.
#'
#' @seealso \code{\link{show_wqmatrixpep}}, \code{\link{show_matrixpep}}
#'
#' @importFrom magrittr "%>%"
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' show_segmatrixpep(rawdat, bay_segment = 'Western')
show_segmatrixpep <- function(dat, txtsz = 3, trgs = NULL, yrrng = c(1990, 2019), bay_segment = c('Western', 'Central', 'Eastern'), abbrev = FALSE, family = NA) {
  
  bay_segment <- match.arg(bay_segment)
  
  # outcome data	
  outdat <- show_matrixpep(dat, bay_segment = bay_segment, txtsz = NULL, trgs = trgs, yrrng = yrrng, abbrev = abbrev)	
  outdat <- outdat$data %>%	
    dplyr::mutate(	
      var = 'outcome',	
      bay_segment = as.character(bay_segment)	
    ) %>%	
    dplyr::select(bay_segment, yr, var, Result = Action, outcome, outcometxt)
  
  # chloropyll and sd data
  chldat <- show_wqmatrixpep(dat, param = 'chl', bay_segment = bay_segment, trgs = trgs, txtsz = NULL, yrrng = yrrng, abbrev = abbrev)
  chldat <- chldat$data
  sddat <- show_wqmatrixpep(dat, param = 'sd', bay_segment = bay_segment, trgs = trgs, txtsz = NULL, yrrng = yrrng, abbrev = abbrev)
  sddat <- sddat$data
  wqdat <- dplyr::bind_rows(chldat, sddat) %>%
    dplyr::select(-Result) %>%
    dplyr::mutate(
      var = gsub('^mean\\_', '', var),
      bay_segment = as.character(bay_segment)
    )
  
  # outcome results for chlorophyll and sd, e.g., large/small, short/long exceedances
  vals <- anlz_medpep(dat) %>%
    anlz_attainpep(magdurout = T) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::mutate(
      mags = factor(mags, levels = c(0, 1, 2), labels = c('none', 'small', 'large')),
      mags = as.character(mags),
      mags = paste0('Exceedance: ', mags, ' (size)'),
      durats = dplyr::case_when(
        durats %in% 0 ~ 'none',
        durats %in% c(1, 2, 3) ~ 'short',
        durats %in% 4 ~ 'long'
      ),
      durats = paste0(durats, ' (length)'),
      outcome = paste0('Outcome: ', outcome), 
      bay_segment = as.character(bay_segment)
    ) %>%
    dplyr::select(-medv, -lwr.ci, -upr.ci, -thresh) %>%
    tidyr::unite(Result, c('outcome', 'mags', 'durats'), sep = ', ')
  
  # combine wqdat with outcome results and outcome data
  toplo <- wqdat %>%
    dplyr::left_join(vals, by = c('bay_segment', 'yr', 'var')) %>%
    dplyr::bind_rows(outdat) %>%
    dplyr::mutate(
      var = factor(var, levels = c('sd', 'outcome', 'chla'), labels = c('Secchi', 'Outcome', 'Chlorophyll-a'))
    )
  
  # create plot
  p <- ggplot(toplo, aes(x = var, y = yr, fill = outcome)) +
    geom_tile(aes(group = Result), colour = 'black') +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = 'red', yellow = 'yellow', green = 'green')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  # text if not null
  if(!is.null(txtsz))
    p <- p +
    geom_text(aes(label = outcometxt), size = txtsz, family = family)
  
  return(p)
  
}