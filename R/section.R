#' creates a search closure to section text
#'
#' @param search a string regexp for the term to seperate on, e.g. "Chapter"
#'
#' @return closure over search expression 
get_search <- function(search){
  function(.data){
    .data %>%
      stringr::str_detect(search) %>%
      purrr::accumulate(sum, na.rm=TRUE)
    }
}

#' sections text based on chapters
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with chapter numbers
#'
#' @export
get_chapters <- get_search("^[\\s]*[Cc][Hh][Aa]?[Pp][Tt]([Ee][Rr])?")

#' sections text based on parts
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with part numbers
#'
#' @export
get_parts <- get_search("^[\\s]*[Pp]([Aa][Rr])?[Tt]")

#' sections text based on sections
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with section numbers
#'
#' @export
get_sections <- get_search("^[\\s]*([Ss][Ss])|([Ss][Ee][Cc][Tt][Ii][Oo][Nn])")

#' sections text based on cantos
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with canto numbers
#'
#' @export
get_cantos <- get_search("(?i)canto (XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$")

#' sections text based on book
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with book numbers
#'
#' @export
get_books <- get_search("(?i)book$")

#' Adds section column to dataframe
#'
#' @param .data dataframe formatted as per output of prep process
#'
#' @param section_by character name of what to section over
#'
#' @return input dataframe with additional section column
#'
#' @export
section <- function(.data, section_by){
    sec_table <- list("chapter" = get_chapters,
                      "part" = get_parts,
                      "section" = get_sections,
                      "canto" = get_cantos,
                      "book" = get_books)
    .data %>%
        dplyr::mutate(!! section_by := sec_table[[section_by]](word))
}
