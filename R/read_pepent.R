#' Import raw enterococcus data
#' 
#' Import raw enterococcus data
#'
#' @param path chr string of path for excel file (optional)
#' 
#' @return data.frame
#' @export
#'
#' @importFrom dplyr "%>%"
#' 
#' @details Data are from the ArcGIS REST Services here \url{https://gis.suffolkcountyny.gov/hosted/rest/services/Hosted/Beach_Water_Quality_Data/FeatureServer/}.  
#' 
#' The API is queried by beach names in the \code{\link{beaches}} data object. The queries are done individually for each beach to not exceed the 2000 record limit. 
#' 
#' Data can also be imported from an Excel if the \code{path} argument is used for the location to the file.  
#' 
#' @concept read
#' 
#' @import httr jsonlite
#' 
#' @examples
#' \dontrun{
#' entdat <- read_pepent()
#' }
#' head(entdat)
read_pepent <- function(path = NULL){

  names <- beaches$Name
  
  # use API
  if(is.null(path)){
    
    path <- 'https://gis.suffolkcountyny.gov/hosted/rest/services/Hosted/Beach_Water_Quality_Data/FeatureServer/0/query?'
    
    out <- NULL
    for(beach in names){
      
      # cat(beach, '\n')
      
      # use two single quotes if one quote found, otherwise query doesn't work
      if(grepl("\\'", beach))
        beach <- gsub("\\'", "''" ,beach)
      
      qry <- paste0("name='", beach, "'")
      
      request <- GET(
        url = path,
        query= list(       
          where = qry,
          outFields = '*',
          f = 'pjson'
        ), 
        ssl_verifypeer = 0L
      )
      
      response <- content(request, as = "text", encoding = "UTF-8")
      results <- fromJSON(response, flatten = T)
      
      exceeds <- results$exceededTransferLimit
      
      # stop if limit exceeded
      if(exceeds)
        stop('request limit exceeded for ', beach)
      
      # format results
      df <-results$features %>% 
        dplyr::select(
          Name = attributes.name, 
          FieldNum = attributes.fieldnum,
          Date = attributes.coldate, 
          value = attributes.result,
          status = attributes.character_
        ) %>% 
        dplyr::mutate(
          Date = Date / 1000,
          Date = as.POSIXlt(Date, origin = '1970-01-02', tz = 'America/Jamaica') # these were off a day from raw data if 1970-01-01
        )
      
      out <- rbind(out, df)
      
    }
      
  }
  
  # use file import
  if(!is.null(path)){
    out <- readxl::read_excel(path, col_types = 'text') %>% 
      dplyr::select(
        Name = Beach_Name,
        FieldNum = Station_Name, 
        Date = Start_Date, 
        value = Result_Value, 
        status = Result_Type
      ) %>% 
      dplyr::mutate(
        status = gsub('[[:digit:]]+|\\.', '', value), 
        status = ifelse(status == '', '=', status), 
        value = as.numeric(gsub('>|<', '', value)),
        Date = suppressWarnings({dplyr::case_when(
          grepl('\\/', Date) ~ lubridate::mdy(Date), 
          grepl("^[[:digit:]]+$", Date) ~ as.Date(as.numeric(Date), origin = "1899-12-30"),
          T ~ NA
        )})
      ) %>% 
      dplyr::filter(Name %in% names)
    
      if(any(is.na(out$Date)))
        warning('Date conversion failed for some entries, must be MM/DD/YYYY or numeric')
    
    }
  
  out <- out %>% 
    dplyr::arrange(Name, FieldNum, Date)

  return(out)
  
}
