#' Determine the number of terms at each aggregate level
#'
#' @param .data character vector of terms
#'
#' @param aggregate_on vector to split .data on for insight
#'
#' @return vector of number of terms for each aggregate level, same
#'   length as .data
#'
#' @export
term_count <- function(.data, aggregate_on){
  split(.data, aggregate_on) %>%
    purrr::map(function(x){rep(length(x), length(x))}) %>%
    dplyr::combine()
}
