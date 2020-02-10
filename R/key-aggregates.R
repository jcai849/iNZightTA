#' get score for key sentences as per Lexrank
#'
#' @param .data character vector of words
#'
#' @param summ_method method to use for summarisation: textrank or
#'     lexrank. Doesn't do anything yet
#'
#' @param aggregate_on vector to aggregate .data over; ideally, sentence_id
#'
#' @return lexrank scores of aggregates
#'
#' @export
key_aggregates <- function(.data, aggregate_on, summ_method){
  ## prepare .data for lexrank
  base <-  tibble::tibble(word = !! .data, aggregate = aggregate_on)
  aggregated <- base %>%
    dplyr::group_by(aggregate) %>%
    stats::na.omit() %>%
    dplyr::summarise(sentence = paste(word, collapse = " ")) %>%
    dplyr::mutate(sentence = paste0(sentence, "."))
  ## lexrank
  lr <- aggregated %>%
    dplyr::pull(sentence) %>%
    lexRankr::lexRank(., n=length(.),removePunc = FALSE, returnTies = FALSE,
	    removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
	    rmStopWords = FALSE, Verbose = TRUE, sentencesAsDocs = TRUE)
  ## match lexrank output to .data
  lr %>%
    dplyr::distinct(sentence, .keep_all = TRUE) %>% 
    dplyr::full_join(aggregated, by="sentence") %>%
    dplyr::full_join(base, by="aggregate") %>%
    dplyr::arrange(aggregate) %>%
    dplyr::pull(value)
}
