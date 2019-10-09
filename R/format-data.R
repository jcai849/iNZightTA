#' takes imported one-line-per-row data and prepares it for later analysis
#'
#' @param .data tibble with one line of text per row
#'
#' @param lemmatize boolean, whether to lemmatize or not
#'
#' @param stopwords boolean, whether to remove stopwords or not
#'
#' @param sw_lexicon string, lexicon with which to remove stopwords
#'
#' @param addl_stopwords char vector of user-supplied stopwords
#'
#' @return a [tibble][tibble::tibble-package] with one token per line,
#'   stopwords removed leaving NA values, column for analysis named
#'   "text"
#'
#' @export
format_data <- function(.data, lemmatize=TRUE, stopwords=TRUE,
                      sw_lexicon="snowball", addl_stopwords=NA){
  formatted <- .data %>%
    text_prep()

  text <- ifexp(lemmatize,
		ifexp(stopwords,
		      dplyr::mutate(formatted,
                                    lemma = tolower(textstem::lemmatize_words(word)),
				    stopword = determine_stopwords(lemma,
                                                                   sw_lexicon,
                                                                   addl_stopwords),
				    text = dplyr::if_else(stopword,
						   as.character(NA),
						   lemma)),
		      dplyr::mutate(formatted,
                                    lemma = tolower(textstem::lemmatize_words(word)),
				    text = lemma)),
		ifexp(stopwords,
		      dplyr::mutate(formatted,
                                    stopword = determine_stopwords(word,
                                                                   sw_lexicon,
                                                                   addl_stopwords),
				    text = dplyr::if_else(stopword,
						   as.character(NA),
						   word)),
		      dplyr::mutate(formatted, text = word)))
  return(text)
}
