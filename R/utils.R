#' scheme-like if expression, without restriction of returning same-size table of .test, as ifelse() does
#'
#' @param .test predicate to test
#'
#' @param true expression to return if .test evals to TRUE
#'
#' @param false expression to return if .test evals to TRUE
#'
#' @return either true or false
ifexp <- function(.test, true, false){
  if (.test) {
    return(true)
  } else {
    return(false)
  }
}

#' Get filetype
#'
#' @param filepath string filepath of document
#'
#' @return filetype (string) - NA if no extension
get_filetype <- function(filepath){
  filepath %>%
    basename %>%
    stringr::str_extract('[a-zA-Z0-9]+\\.[a-zA-Z0-9]+$') %>% #ensure filename.extension form
    stringr::str_extract('[a-zA-Z0-9]+$')                  #extract extension
}

#' Interactively determine and automatically mark the text column of a table
#'
#' @param data dataframe with column requiring marking
#'
#' @return same dataframe with text column renamed to "text"
table_textcol <- function(data){
cols <- colnames(data)
print("Please enter the number of the column you want selected for text analytics")
print(cols)
textcol_index <- get_valid_input(as.character(1:ncol(data))) %>%
  as.integer 
textcol <- cols[textcol_index]  
data %>%
    dplyr::rename(text = !! dplyr::sym(textcol))
}

#' helper function to get valid input (recursively)
#'
#' @param options vector of options that valid input should be drawn from
#'
#' @param init whether this is the initial attempt, used only as
#'   recursive information
#'
#' @return readline output that exists in the vector of options
get_valid_input <- function(options, init=TRUE){
  input <- ifelse(init,
		  readline(),
		  readline(prompt = "Invalid option. Please try again: "))
  ifelse(input %in% options,
	 input,
	 get_valid_input(options, init=FALSE))
}

#' helper function to ungroup for dplyr. functions equivalently to
#' group_by() but with standard (string) evaluation
#'
#' @param x tibble to perform function on
#'
#' @param ... string of groups to ungroup on
#'
#' @return x with ... no longer grouped upon
ungroup_by <- function(x,...){
dplyr::group_by_at(x, dplyr::group_vars(x)[!dplyr::group_vars(x) %in% ...])
}
