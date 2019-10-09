#' Determine term frequency
#'
#' @param .data character vector of terms
#'
#' @return numeric vector of term frequencies
#'
#' @export
term_freq <- function(.data){
  .data %>%
    tibble::enframe() %>%
  dplyr::add_count(value) %>%
  dplyr::mutate(n = dplyr::if_else(is.na(value),
		     as.integer(NA),
		     n))  %>%
  dplyr::pull(n)
}
