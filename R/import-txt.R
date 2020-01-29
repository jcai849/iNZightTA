#' Import text file 
#'
#' @param filepath a string indicating the relative or absolute
#'     filepath of the file to import
#' 
#' @return a [tibble][tibble::tibble-package] of each row
#'   corrresponding to a line of the text file, with the column named
#'   "text"
import_txt <- function(filepath){
  readLines(filepath) %>%
  #paste(readLines(filepath), collapse = "\n") %>%
    tibble::tibble(text=.)
}
