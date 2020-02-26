#' Extract lunaid_date from text
#' @param s (vector of) string to extract luna_yyyymmdd
#' @export
#' @examples 
#' ld8from("/path/to/12345_20191011/file.txt")
ld8from <- function(s) stringr::str_extract(s,'\\d{5}_\\d{8}')
