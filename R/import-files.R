#' Base case for file import
#'
#' @param filepath string filepath of file for import
#'
#' @param filename character name of the file
#'
#' @return imported file with document id
import_base_file <- function(filepath, filename){
  filetype <- get_filetype(filepath)
  f_name <- basename(filename)
  
  if (filetype == "csv"){
    imported <- import_csv(filepath)
  } 
  
  else if (filetype == "xlsx" | filetype == "xls") {
    imported <- import_excel(filepath)
  } 
  
  ################
  else if (filetype == "pdf"){
    imported <- pdftools::pdf_text(filepath) %>% 
      tibble::tibble(text=.)
  }

  ################
  else {
    imported <- import_txt(filepath)
  }

  if ("id" %in% names(imported)){
    imported %>% dplyr::mutate(docname = f_name)
  }
  else{
    imported %>% dplyr::mutate(id = f_name)
  }
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



