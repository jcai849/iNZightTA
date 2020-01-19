#' Base case for file import
#'
#' @param filepath string filepath of file for import
#'
#' @return imported file with document id
import_base_file <- function(filepath){
  filetype <- get_filetype(filepath)
  filename <- basename(filepath)
  
  if (filetype == "csv"){
    imported <- import_csv(filepath)
  } 
  
  else if (filetype == "xlsx" | filetype == "xls") {
    imported <- import_excel(filepath)
  } 
  
  ################
  else if (filetype == "pdf"){
    imported <- pdf_text(filepath) %>% 
      tibble::tibble(text=.)
  }

  ################
  else {
    imported <- import_txt(filepath)
  }

  imported
}

#' Import any number of files
#'
#' @param filepaths char vector of filepaths
#'
#' @return a [tibble][tibble::tibble-package] imported files with
#'   document id
#' 
#' @export
import_files <- function(filepaths){
  filepaths %>%
    purrr::map(import_base_file) %>%
    dplyr::bind_rows()
}
