#' Get statistics for sentiment over some group, such as sentence.
#'
#' @param .data character vector of words
#'
#' @param aggregate_on vector to aggregate .data over; ideally,
#'   sentence_id, but could be chapter, document, etc.
#'
#' @param lexicon as per term sentiment
#'
#' @param statistic function that accepts na.rm argument; e.g. mean,
#'   median, sd.
#'
#' @return sentiment of same length as input vector aggregated over the aggregate_on vector
#'
#' @export
aggregate_sentiment <- function(.data, aggregate_on, lexicon = "afinn", statistic = mean, to_scale = FALSE){
  agg_senti <- tibble::enframe(.data, "nil1", "word") %>%
    dplyr::bind_cols(tibble::enframe(aggregate_on, "nil2", "aggregate")) %>%
    dplyr::select(word, aggregate) %>%
    dplyr::mutate(sentiment = term_sentiment(word, lexicon)) %>%
    dplyr::group_by(aggregate) %>%
    dplyr::mutate(aggregate_sentiment =
		    (function(.x){
		      rep(statistic(.x, na.rm = TRUE), length(.x))
		    })(sentiment)) %>%
    dplyr::pull(aggregate_sentiment)
  if (to_scale == TRUE){
    agg_senti <- scale(agg_senti)
  }
  agg_senti
}
