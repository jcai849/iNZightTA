#' Base case for file import
#'
#' @param filepath string filepath of file for import
#'
#' @param filename character name of the file
#'
#' @return imported file with document id
import_base_file <- function(filepath, filename){
  filetype <- get_filetype(filepath)
  id <- basename(filename)
  
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

  imported %>%
    dplyr::mutate(id = id)
}

#' Import any number of files
#'
#' @param filepaths char vector of filepaths
#'
#' @param filenames char vector of file names
#'
#' @return a [tibble][tibble::tibble-package] imported files with
#'   document id
#' 
#' @export
import_files <- function(filepaths, filenames){
  purrr::map2(filepaths,filenames,import_base_file) %>%
    dplyr::bind_rows()
}



