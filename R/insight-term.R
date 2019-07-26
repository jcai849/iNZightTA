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
#' @export
get_bigram <- function(.data){
  1:length(.data) %>%
    purrr::map_chr(index_bigram, .data, .data[-1])
}

#' get bigram at index i of list1 & 2
#'
#' @param i numeric index to attain bigram at
#'
#' @param list1 list or vector for first bigram token
#'
#' @param list2 list or vector for second bigram token
#'
#' @return bigram of list1 and list2 at index i, skipping NA's
index_bigram <- function(i, list1, list2){
  ifelse(length(list2) < i | is.na(list1[i]),
	  as.character(NA),
  ifelse(!(is.na(list1[i]) | is.na(list2[i])),
	 paste(list1[i], list2[i]),
	 index_bigram(i,list1, list2[-1])))
}

#' Determine textrank score for vector of words
#'
#' @param .data character vector of words
#'
#' @return vector of scores for each word
#'
#' @export
keywords_tr <- function(.data){
  relevent <- !is.na(.data)
  tr <- textrank::textrank_keywords(.data, relevent, p=+Inf)
  score <- tr$pagerank$vector %>% tibble::enframe
  data <- .data %>% tibble::enframe("number", "name")
  dplyr::full_join(data, score, by="name") %>%
    dplyr::pull(value)
}

#' Determine sentiment of words
#'
#' @param .data vector of words
#'
#' @param lexicon sentiment lexicon to use, based on the corpus
#'   provided by tidytext
#' 
#' @return vector with sentiment score of each word in the vector
#'
#' @export
word_sentiment <- function(.data, lexicon="afinn"){
  data <- tibble::enframe(.data, "number", "word")
  tidytext::get_sentiments(lexicon) %>%
    dplyr::select(word, value) %>%
    dplyr::right_join(data, by="word") %>%
    dplyr::pull(value)
}
