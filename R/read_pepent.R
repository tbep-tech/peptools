#' Import raw enterococcus data
#' 
#' Import raw enterococcus data
#'
#' @param path chr string of path for excel file
#'
#' @return data.frame
#' @export
#'
#' @importFrom dplyr "%>%"
#' 
#' @details Raw data from here \url{https://gisportal.suffolkcountyny.gov/gis/home/item.html?id=025cb4dadb57413980dbd7e760b94da8}
#' 
#' @family read
#' 
#' @examples
#' path <- system.file("extdata", "enterodata.xlsx", package="peptools")
#' entdat <- read_pepent(path)
#' entdat
read_pepent <- function(path){

  out <- readxl::read_xlsx(path, col_types = c('text', 'text', 'text', 'date', 'text', 'text', 'text')) %>% 
    dplyr::select(Name, FieldNum, Date = ColDate, Time, value = Result) %>% 
    dplyr::mutate(
      Date = as.character(as.Date(Date)),
      Time = as.numeric(Time),
      Time =  as.POSIXct((Time) * 86400, origin = "1970-01-01", tz = 'UTC'),
      Time = format(Time, "%H:%M"),
      status = gsub('[0-9]+|\\.|\\s+', '', value),
      value = gsub('>|<|N', '', value), 
      value = as.numeric(value), 
    ) %>% 
    tidyr::unite('Date', Date, Time, sep = ' ') %>% 
    dplyr::mutate(
      Date = lubridate::ymd_hm(Date, tz = 'America/Caracas')
    ) %>% 
    dplyr::arrange(Name, FieldNum, Date)
  
  return(out)
  
}
