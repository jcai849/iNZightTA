#' Determine textrank score for vector of words
#'
#' @param .data character vector of words
#'
#' @param summ_method method to use for summarisation: textrank or
#'     lexrank. Doesn't do anything yet
#'
#' @return vector of scores for each word
#'
#' @export
keywords_tr <- function(.data, summ_method){
  relevent <- !is.na(.data)
  tr <- textrank::textrank_keywords(.data, relevent, p=+Inf)
  score <- tr$pagerank$vector %>% tibble::enframe()
  data <- .data %>% tibble::enframe("number", "name")
  dplyr::full_join(data, score, by="name") %>%
    dplyr::pull(value)
}
