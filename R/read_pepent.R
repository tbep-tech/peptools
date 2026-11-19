#' Import raw enterococcus data
#'
#' Import raw enterococcus data
#'
#' @param path chr string of path to a local copy of the data (optional), see details
#' @param url chr string of url for downloading the data, used only if \code{path = NULL}
#'
#' @return data.frame
#' @export
#'
#' @importFrom dplyr "%>%"
#'
#' @details Data are from the Suffolk County Department of Health Services (SCDHS) Bathing Beach Program, available at \url{https://gis.suffolkcountyny.gov/portal/home/item.html?id=b5d1e5c88d8c4fb48a3732cd44cf4007}.  The data are provided as a zip file that is downloaded to a temporary directory if \code{path = NULL} (default).  The zip file includes historical data as a csv file, more recent data as one Excel file per year, and an Excel file of beach sampling locations.  The historical and recent data files use different formats, both of which are handled by this function.
#'
#' Alternatively, the \code{path} argument can point to a local copy of the zip file, a folder with the extracted contents of the zip file, or a vector of the individual data files.  The sampling locations file must be included if any of the data files use the recent format.
#'
#' Records are matched to the beaches in the \code{\link{beaches}} data object by station code (\code{FieldNum}), not beach name.  Beach names in the source data have changed over time and are not unique between data files (e.g., "Crescent Beach - Suffolk" is a Huntington beach in the source data).  Station codes for records in the recent format are obtained from the sampling locations file using the EPA beach id.  The beach names in the output are those in \code{\link{beaches}}.  Only Enterococcus records are returned.
#'
#' @concept read
#'
#' @examples
#' \dontrun{
#' entdat <- read_pepent()
#' }
#' head(entdat)
read_pepent <- function(path = NULL, url = 'https://gis.suffolkcountyny.gov/portal/sharing/rest/content/items/b5d1e5c88d8c4fb48a3732cd44cf4007/data'){

  # download and get files
  if(is.null(path)){

    tmpzip <- tempfile(fileext = '.zip')
    on.exit(unlink(tmpzip), add = TRUE)
    utils::download.file(url, tmpzip, mode = 'wb', quiet = TRUE)
    path <- tmpzip

  }

  fls <- NULL
  for(pth in path){

    if(!file.exists(pth))
      stop('file or folder not found: ', pth)

    # unzip to temporary folder
    if(grepl('\\.zip$', pth, ignore.case = TRUE)){
      tmpdir <- tempfile()
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
      utils::unzip(pth, exdir = tmpdir)
      pth <- tmpdir
    }

    if(dir.exists(pth))
      pth <- list.files(pth, recursive = TRUE, full.names = TRUE)

    fls <- c(fls, pth)

  }

  fls <- grep('\\.(csv|xlsx?)$', fls, ignore.case = TRUE, value = TRUE)
  fls <- fls[!grepl('^~\\$', basename(fls))]
  if(length(fls) == 0)
    stop('no csv or Excel files found')

  # import each file, identify format by column names
  old <- list()
  new <- list()
  locs <- NULL
  for(fl in fls){

    if(grepl('\\.csv$', fl, ignore.case = TRUE))
      dat <- utils::read.csv(fl, check.names = FALSE, stringsAsFactors = FALSE)
    if(!grepl('\\.csv$', fl, ignore.case = TRUE))
      dat <- suppressWarnings(readxl::read_excel(fl, sheet = 1, guess_max = 1e5))

    dat <- as.data.frame(dat)
    names(dat) <- toupper(gsub('[[:space:]]+', '', names(dat)))

    if(all(c('FIELDNUM', 'COLDATE', 'CHARACTER', 'RESULT', 'TYPE') %in% names(dat)))
      old[[fl]] <- dat
    else if(all(c('EPA_BEACH_ID', 'SAMPLE_COLLECTION_DATE', 'INDICATOR_BACTERIA', 'REMARK', 'RESULT') %in% names(dat)))
      new[[fl]] <- dat
    else if(all(c('EPABEACHID', 'BEACHCODE') %in% names(dat)))
      locs <- dat
    else
      warning('format not recognized, skipping file: ', basename(fl))

  }

  if(length(old) + length(new) == 0)
    stop('no data files with a recognized format found')

  ents <- c('ENTEROCOCCUS', 'ENTEROCOCCI')

  # historical format
  oldout <- NULL
  if(length(old) > 0)
    oldout <- dplyr::bind_rows(old) %>%
      dplyr::filter(toupper(trimws(TYPE)) %in% ents) %>%
      dplyr::select(
        FieldNum = FIELDNUM,
        Date = COLDATE,
        value = RESULT,
        status = CHARACTER
      ) %>%
      dplyr::mutate(
        Date = pepent_date(Date),
        status = trimws(status)
      )

  # recent format
  newout <- NULL
  if(length(new) > 0){

    if(is.null(locs))
      stop('sampling locations file with EPA beach ids and beach codes is required for data in recent format')

    locs <- locs %>%
      dplyr::select(EPA_BEACH_ID = EPABEACHID, FieldNum = BEACHCODE) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) %>%
      dplyr::distinct()

    newout <- dplyr::bind_rows(lapply(new, function(x) dplyr::mutate(x, SAMPLE_COLLECTION_DATE = pepent_date(SAMPLE_COLLECTION_DATE)))) %>%
      dplyr::filter(toupper(trimws(INDICATOR_BACTERIA)) %in% ents) %>%
      dplyr::mutate(EPA_BEACH_ID = trimws(EPA_BEACH_ID)) %>%
      dplyr::left_join(locs, by = 'EPA_BEACH_ID') %>%
      dplyr::select(
        FieldNum,
        Date = SAMPLE_COLLECTION_DATE,
        value = RESULT,
        status = REMARK
      ) %>%
      dplyr::mutate(
        status = dplyr::case_when(
          grepl('less', status, ignore.case = TRUE) ~ '<',
          grepl('greater', status, ignore.case = TRUE) ~ '>',
          TRUE ~ '='
        )
      )

  }

  # combine, match to beaches by station code
  bchs <- sf::st_drop_geometry(beaches) %>%
    dplyr::select(Name, FieldNum)

  out <- dplyr::bind_rows(
      dplyr::mutate(oldout, value = as.character(value)),
      dplyr::mutate(newout, value = as.character(value))
    ) %>%
    dplyr::mutate(
      FieldNum = toupper(trimws(FieldNum)),
      value = suppressWarnings(as.numeric(gsub('[^0-9.]', '', value)))
    ) %>%
    dplyr::inner_join(bchs, by = 'FieldNum') %>%
    dplyr::select(Name, FieldNum, Date, value, status) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Name, FieldNum, Date)

  if(any(is.na(out$Date)))
    warning('date conversion failed for some entries')

  return(out)

}

#' Convert dates for beach pathogen data
#'
#' @param x vector of dates as Date, POSIXct, Excel numeric, or character in month/day/year or year-month-day format
#'
#' @return Date vector
#'
#' @noRd
pepent_date <- function(x){

  if(inherits(x, c('Date', 'POSIXt')))
    return(as.Date(format(x, '%Y-%m-%d')))

  if(is.numeric(x))
    return(as.Date(x, origin = '1899-12-30'))

  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  isnum <- grepl('^[[:digit:]]+(\\.[[:digit:]]+)?$', x)
  out[isnum] <- as.Date(as.numeric(x[isnum]), origin = '1899-12-30')
  out[!isnum] <- as.Date(suppressWarnings(lubridate::parse_date_time(x[!isnum], orders = c('mdy', 'ymd', 'mdy HMS', 'ymd HMS'))))

  return(out)

}
