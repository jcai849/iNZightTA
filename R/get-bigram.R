#' Determine bigrams
#'
#' @param .data character vector of words
#'
#' @return character vector of bigrams
#'
 get_bigram <- function(.data){
    concat_walk(.data, c(.data[-1], NA))
}
