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

#' Determine bigrams
#'
#' @param .data character vector of words
#'
#' @return character vector of bigrams
#'
 get_bigram <- function(.data){
    concat_walk(.data, c(.data[-1], NA))
}

#' concat list 1 and 2 at index, skipping NA values
#'
#' @param i numeric index to assess index at
#'
#' @param list1 list or vector for first token
#'
#' @param list2 list or vector for second token
#'
#' @return paste of list1 and list2 at index i, skipping NA's
concat_walk_i <- function(i, list1, list2){
  ifelse(length(list2) < i | is.na(list1[i]),
	  as.character(NA),
  ifelse(!(is.na(list1[i]) | is.na(list2[i])),
	 paste(list1[i], list2[i]),
	 concat_walk_i(i,list1, list2[-1])))
}

#' concat list 1 and 2, moving past NA values
#'
#' @param list1 list or vector for first bigram token
#'
#' @param list2 list or vector for second bigram token
#'
#' @return paste of list1 and list2, skipping NA's
concat_walk <- function(list1, list2){
    stopifnot(length(list1) == length(list2))
    sapply(seq_along(list1), concat_walk_i, list1, list2)
}

# NOT FOR PRODUCTION - STILL IN TESING. Returns the n-grams, skipping NA values
## get_ngram <- function(.data, n){
##     if (n == 2){
##         concat_walk(.data, c(.data[-1], NA))
##     } else {
##         concat_walk(get_ngram(.data, n - 1), c(.data[-seq(n-1)], rep(NA, n-1)))
##     }
## }

#' Returns the n-grams, skipping NA values
#'
#' @param .data vector to get n-grams from
#'
#' @param n number of n-grams to attain
#'
#' @return n-gram vector without NA values
#'
#' @export
get_ngram <- function(.data, n){
    main_n <- n
    ngrams <- rep(NA_character_, length(.data))
    for (i in seq_along(.data)){
        if (i - 1 >  length(.data) - n) break
        if (is.na(.data[i])){
            next
        } else {
            ngram <- .data[i]
        }
        j <- i + 1
        while(n > 1){
            if (j >  length(.data)){
                ngram <- NA_character_
                break
            }
            if (is.na(.data[j])){
                j <- j + 1
            } else {
                ngram <- paste(ngram, .data[j])
                n <- n - 1
                j <- j + 1
            }
        }
        ngrams[i] <- ngram
        n <- main_n
    }
    return(ngrams)
}

#' NOT FOR PRODUCTION - STILL IN TESING. Returns the count of n-grams, skipping NA values
#'
#' @param .data vector to get n-grams from
#'
#' @param n number of n-grams to attain
#'
#' @return count of each associated n-gram
#'
#' @export
ngram_freq <- function(.data, n){
  term_freq(get_ngram(.data, n))
}

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
lagged_term_sentiment <- function(.data, lexicon="afinn", lag = 10, statistic = mean){
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

phi_coef <- function(n00, n01, n11, n10){
    n0x <- n00 + n01
    n1x <- n10 + n11
    nx0 <- n00 + n10
    nx1 <- n01 + n11
    (n11 * n00 - n10 * n01) / sqrt(prod(n0x, n1x, nx0, nx1))
}

#' Determine term correlations - extremely slow
#'
#' @param .data character vector of terms
#'
#' @param term character to find correlations with
#'
#' @param aggregate_on vector to aggregate .data over; ideally,
#'   sentence_id, but could be chapter, document, etc.
#'
#' @return numeric vector of term correlations as per phi_coef
#'
#' @export
term_corr <- function(.data, term, aggregate_on){
    corrs <- numeric(length(.data))
    for (i in seq_along(.data)){
        other_term <- .data[i]
        n00 <- sum(tapply(.data, aggregate_on, function(x){!((term %in% x) | (other_term %in% x))}))
        n01 <- sum(tapply(.data, aggregate_on, function(x){(term %in% x) & !(other_term %in% x)}))
        n10 <- sum(tapply(.data, aggregate_on, function(x){!(term %in% x) & (other_term %in% x)}))
        n11 <- sum(tapply(.data, aggregate_on, function(x){(term %in% x) & (other_term %in% x)}))
        corrs[i] <- phi_coef(n00, n01, n11, n10)
    }
    return(corrs)
}

#' Determine term cooccurances - extremely slow
#'
#' @param .data character vector of terms
#'
#' @param term character to find correlations with
#'
#' @param aggregate_on vector to aggregate .data over; ideally,
#'   sentence_id, but could be chapter, document, etc.
#'
#' @return numeric vector of term correlations as per phi_coef
#'
#' @export
term_cooccurance <- function(.data, term, aggregate_on){
    corrs <- numeric(length(.data))
    for (i in seq_along(.data)){
        other_term <- .data[i]
        corrs[i] <- sum(tapply(.data, aggregate_on, function(x){(term %in% x) & (other_term %in% x)}))
    }
    return(corrs)
}
