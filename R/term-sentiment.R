#' Determine sentiment of terms
#'
#' @param .data vector of terms
#'
#' @param lexicon sentiment lexicon to use, based on the corpus
#'   provided by tidytext
#' 
#' @return vector with sentiment score of each word in the vector
#'
#' @export
term_sentiment <- function(.data, lexicon="afinn"){
  data <- tibble::enframe(.data, "number", "word")
  tidytext::get_sentiments(lexicon) %>%
    dplyr::select(word, value) %>%
    dplyr::right_join(data, by="word") %>%
    dplyr::pull(value)
}
