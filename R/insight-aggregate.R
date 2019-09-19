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
	    rmStopWords = FALSE, Verbose = TRUE)
  ## match lexrank output to .data
  lr %>%
    dplyr::distinct(sentence, .keep_all = TRUE) %>% 
    dplyr::full_join(aggregated, by="sentence") %>%
    dplyr::full_join(base, by="aggregate") %>%
    dplyr::arrange(aggregate) %>%
    dplyr::pull(value)
}

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
aggregate_sentiment <- function(.data, aggregate_on, lexicon = "afinn", statistic = mean){
  tibble::enframe(.data, "nil1", "word") %>%
    dplyr::bind_cols(tibble::enframe(aggregate_on, "nil2", "aggregate")) %>%
    dplyr::select(word, aggregate) %>%
    dplyr::mutate(sentiment = term_sentiment(word, lexicon)) %>%
    dplyr::group_by(aggregate) %>%
    dplyr::mutate(aggregate_sentiment =
		    (function(.x){
		      rep(statistic(.x, na.rm = TRUE), length(.x))
		    })(sentiment)) %>%
    dplyr::pull(aggregate_sentiment)
}

#' perform group-aware term operations on the data
#'
#' @param .data dataframe of terms as per output of text_prep
#'
#' @param operations character vector of term operations to perform
#'
#' @param ... additional arguments to the operation - only sensible for singular operations 
#'
#' @return .data with operation columns added
#'
#' @export
get_term_insight <- function(.data, operations, ...){
    opstable <- list("Term Frequency" = term_freq,
                     "n-gram Frequency" = ngram_freq,
                     "n-grams" = get_ngram,
                     "Key Words" = keywords_tr,
                     "Term Sentiment" = term_sentiment,
                     "Moving Average Term Sentiment" = lagged_term_sentiment)
    ops <- opstable[operations]
    lapply(seq(length(ops)),
           function(x){
               name <- dplyr::sym(names(ops[x]))
               operation <- ops[x][[1]]
               df <- dplyr::mutate(.data,
                                   !!name := operation(text, ...))
               df[names(ops[x])]
           }) %>%
        dplyr::bind_cols(.data, .)
}

#' perform group-aware aggregate operations on the data
#'
#' @param .data dataframe of terms as per output of text_prep
#'
#' @param operations character vector of operations to perform
#'
#' @param aggregate_on character name of the column to perform aggregate operations on
#'
#' @param ... additional arguments to the operation - only sensible for singular operations 
#'
#' @return .data with operation columns added
#'
#' @export
get_aggregate_insight <- function(.data, operations, aggregate_on, ...){
    opstable <- list("Aggregated Term Count" = term_count,
                     "Key Sections" = key_aggregates,
                     "Aggregated Sentiment" = aggregate_sentiment,
                     "Bound Aggregates" = bind_aggregation)
    ops <- opstable[operations]
    lapply(seq(length(ops)),
           function(x){
               name <- dplyr::sym(names(ops[x]))
               operation <- ops[x][[1]]
               agg_on <- dplyr::sym(aggregate_on)
               df <- if (names(ops[x]) == "Bound Aggregates"){
                         dplyr::mutate(.data,
                                       !!name := operation(word, !! agg_on))
                     } else {
                         dplyr::mutate(.data,
                                       !!name := operation(text, !! agg_on, ...))
                     }
               df[names(ops[x])]
           }) %>%
        dplyr::bind_cols(.data, .)
}

#' bind aggregate terms together
#'
#' @param data vector of terms
#'
#' @param aggregate_on vector of aggregations
#'
#' @return data with every aggregation bound, as in a sentence
#'
#'
bind_aggregation <- function(data, aggregate_on){
    tibble::tibble(data, agg = aggregate_on) %>%
        dplyr::group_by(agg) %>%
        dplyr::mutate(bound = paste(data, collapse = " ")) %>%
        dplyr::pull(bound)
}
