#' Create reactable table from matrix data
#'
#' Create reactable table from matrix data
#'
#' @param totab A data frame in wide format of summarized results
#' @param colfun Function specifying how colors are treated in cell background
#' @param nrows numeric specifying number of rows in the table
#'
#' @importFrom reactable colDef
#'
#' @details This function is used internally within \code{\link{show_matrixpep}}
#'
#' @family visualize
#'
#' @return A \code{\link[reactable]{reactable}} table
#'
#' @export
show_reactablepep <- function(totab, colfun, nrows = 10) {
  
  out <- reactable::reactable(totab,
                              defaultPageSize = nrows,
                              columns = list(
                                yr = colDef(
                                  name = "Year"
                                )
                              ),
                              defaultColDef = colDef(
                                style = function(value){
                                  list(background = colfun(value))
                                }
                              )
  )
  
  return(out)
  
}