#' Import raw water quality data
#' 
#' Import raw water quality data
#'
#' @param path chr string of path for excel file
#'
#' @return data.frame
#' @export
#' 
#' @importFrom dplyr "%>%"
#' 
#' @details Raw data from here \url{https://gisportal.suffolkcountyny.gov/gis/home/item.html?id=8107f192ffac406380b6d61d3d3dbf7d}
#' 
#' All data prior to 1990 are removed - some exist but the data are scarce.
#' 
#' @family read
#' 
#' @examples
#' path <- system.file("extdata", "currentdata.xlsx", package="peptools")
#' dat <- read_pepwq(path)
#' dat
read_pepwq <- function(path){

  out <- readxl::read_xlsx(path, col_types = 'text') %>% 
    dplyr::select(Date, BayStation, sd = `Secchi\r\n(ft)`, chla = `T. Chl-a\r\n(ug/l)`) %>% 
    dplyr::filter(BayStation %in% pepstations$BayStation) %>% 
    tidyr::pivot_longer(c('sd', 'chla')) %>% 
    na.omit %>% 
    dplyr::mutate(
      status = stringr::str_extract(value, '>|<'),
      value = gsub('>|<', '', value), 
      value = as.numeric(value), 
    ) %>% 
    dplyr::group_by(Date, BayStation, name) %>%
    dplyr::summarise(
      value = mean(value, na.rm = T), 
      status = paste(unique(na.omit(status)), collapse = ', ')
      ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      Date = as.Date(as.numeric(Date), origin = '1899-12-30'),
      yr = lubridate::year(Date), 
      mo = lubridate::month(Date)
    ) %>% 
    dplyr::filter(yr >= 1990) %>% 
    dplyr::left_join(pepstations, by = 'BayStation')
  
  return(out)
  
}