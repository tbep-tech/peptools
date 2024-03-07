#' Raw beach pathogen data from Suffolk County
#'
#' Raw beach pathogen data from Suffolk County
#'
#' @format A \code{data.frame} object
#' @concept data
#' @examples 
#' \dontrun{
#' library(dplyr)
#' entdat1 <- read_pepent() %>% 
#'   filter(lubridate::year(Date) < 2022)
#' 
#' entdat2 <- read_pepent(path = '~/Desktop/Enterodata_2023.xlsx') %>%
#'   filter(lubridate::year(Date) >= 2022)
#' 
#' entdat <- bind_rows(entdat1, entdat2)
#' 
#' save(entdat, file = 'data/entdat.RData', compress = 'xz')
#' }
"entdat"
