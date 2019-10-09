#' Determine the lagged sentiment of terms
#'
#' @param .data vector of terms
#'
#' @param lexicon sentiment lexicon to use, based on the corpus
#'     provided by tidytext
#'
#' @param lag how many (inclusive) terms to compute statistic over
#'
#' @param statistic base statistic used to summarise the data, capable
#'     of taking an na.rm argument
#'
#' @return vector with lagged sentiment score of each term in the input vector
#'
#' @export
ma_term_sentiment <- function(.data, lexicon="afinn", lag = 10, statistic = mean){
    sents <- term_sentiment(.data, lexicon)
    ## lagged_sents <- rep(NA, length(sents))
    ## for (i in seq(lag, length(sents))){
    ##     lagged_sents[i] <- statistic(sents[(seq(i - lag + 1, i))], na.rm = TRUE)
    ## }
    ## lagged_sents
    c(rep(NA, lag - 1),
      sapply(seq(lag, length(sents)),
             function(i){x <- statistic(sents[(seq(i - lag + 1, i))],
                                        na.rm = TRUE)
                                        ifelse(is.nan(x),
                                               NA,
                                               x)
             }))
}
