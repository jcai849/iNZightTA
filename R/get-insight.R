#' perform group-aware term operations on the data
#'
#' @param .data dataframe of terms as per output of format_data
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
                     "Moving Average Term Sentiment" = ma_term_sentiment)
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
#' @param .data dataframe of terms as per output of format_data
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
