#' Import raw enterococcus data
#' 
#' Import raw enterococcus data
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
#' @family read
#' 
#' @import httr jsonlite
#' 
#' @examples
#' entdat <- read_pepent()
#' head(entdat)
read_pepent <- function(){

  path <- 'https://gis.suffolkcountyny.gov/hosted/rest/services/Hosted/Beach_Water_Quality_Data/FeatureServer/0/query?'
  
  names <- beaches$Name
  
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
      )
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
  
  out <- out %>% 
    dplyr::arrange(Name, FieldNum, Date)
  
  return(out)
  
}
