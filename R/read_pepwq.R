#' Import raw data
#' 
#' Impot raw data
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
#' @family read
#' 
#' @examples
#' path <- system.file("extdata", "currentdata.xlsx", package="pepreporting")
#' dat <- read_pepwq(path)
#' dat
read_pepwq <- function(path){
  
  out <- readxl::read_xlsx(path, col_types = 'text') %>% 
    dplyr::select(Date, BayStation, sd = Secchi, chla = `Chlorophyll A - Total`) %>% 
    dplyr::filter(BayStation %in% stations$BayStation) %>% 
    tidyr::pivot_longer(c('sd', 'chla')) %>% 
    na.omit() %>% 
    dplyr::mutate(
      value = gsub('>|<', '', value), 
      value = as.numeric(value)
    ) %>% 
    dplyr::group_by(Date, BayStation, name) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      Date = as.Date(as.numeric(Date), origin = '1899-12-30'),
      yr = lubridate::year(Date), 
      mo = lubridate::month(Date)
    ) %>% 
    dplyr::left_join(stations, by = 'BayStation') %>% 
    dplyr::select(BayStation, bay_segment, Date, yr, mo, name, value) %>% 
    tidyr::pivot_wider()
  
  return(out)
  
}