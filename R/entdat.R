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
#' 
#' # or the file can be manually downloaded from here (format is not the same as path input from SCM)
#' # https://gis.suffolkcountyny.gov/portal/home/item.html?id=e3b344ff82b74762b625cacaa3e9621a
#' entdat <- read.csv('~/Desktop/Beach_Water_Quality_Data.csv', header = T) |> 
#'   dplyr::filter(Type %in% c('Enterococcus', 'Enterococci')) %>% 
#'   dplyr::select(
#'     Name,
#'     FieldNum, 
#'     Date = ColDate, 
#'     value = Result, 
#'     status = Character
#'   ) %>% 
#'   dplyr::mutate(
#'     status = gsub('[[:digit:]]+|\\.', '', value), 
#'     status = ifelse(status == '', '=', status), 
#'     value = as.numeric(gsub('>|<', '', value)),
#'     Date = suppressWarnings({dplyr::case_when(
#'       grepl('\\/', Date) ~ lubridate::mdy(Date), 
#'       grepl("^[[:digit:]]+$", Date) ~ as.Date(as.numeric(Date), origin = "1899-12-30"),
#'       T ~ NA
#'     )}), 
#'     Name = dplyr::case_when(
#'       Name == 'Crescent Beach - Shelter Island' ~ 'Crescent Beach - Suffolk', 
#'       T ~ Name
#'     )
#'   ) %>% 
#'   dplyr::filter(Name %in% beaches$Name) |> 
#'   dplyr::arrange(Name, FieldNum, Date)
#' 
#' save(entdat, file = 'data/entdat.RData')
#' }
"entdat"
