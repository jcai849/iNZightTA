## #' formats imported data into an analysis-ready format '
## #' @param data a tibble formatted with a text and (optional) group
## #'     column
## #'
## #' @return a [tibble][tibble::tibble-package] formatted such that
## #'     columns correspond to identifiers of group, line, sentence,
## #'     word (groups ignored)
## #'
## #' @export
## format_data <- function(data){
##   data %>%
##       dplyr::mutate(line_id = dplyr::row_number()) %>% 
##       tidytext::unnest_tokens(output = sentence, input = text,
##                               token = "sentences", to_lower = FALSE) %>%
##       dplyr::mutate(sentence_id = dplyr::row_number()) %>%
##       dplyr::group_by(sentence_id, add=TRUE) %>%
##       dplyr::group_modify(~ {
##           .x %>%
##               tidytext::unnest_tokens(output = word, input = sentence,
##                                       token = "words", to_lower=FALSE) %>%
##               dplyr::mutate(word_id = dplyr::row_number())
##       }) %>%
##       ungroup_by("sentence_id")
## }

#' formats imported data into an analysis-ready format '
#' @param data a tibble formatted with a text and (optional) group
#'     column
#'
#' @return a [tibble][tibble::tibble-package] formatted such that
#'     columns correspond to identifiers of group, sentence,
#'     word (groups ignored)
#'
#' @export
format_data <- function(data){
    data %>%
      tidytext::unnest_tokens(output = sentence, input = text,
                              token = "sentences", to_lower = FALSE) %>%
      dplyr::mutate(sentence_id = dplyr::row_number()) %>%
      dplyr::group_by(sentence_id, add=TRUE) %>%
      dplyr::group_modify(~ {
          .x %>%
              tidytext::unnest_tokens(output = word, input = sentence,
                                      token = "words", to_lower=FALSE) %>%
              dplyr::mutate(word_id = dplyr::row_number())
      }) %>%
      ungroup_by("sentence_id")
}

#' Gets stopwords from a default list and user-provided list
#'
#' @param lexicon a string name of a stopword list, one of "smart",
#'     "snowball", or "onix"
#'
#' @param addl user defined character vector of additional stopwords,
#'     each element being a stopword
#'
#' @return a [tibble][tibble::tibble-package] with one column named "word"
get_sw <- function(lexicon = "snowball", addl = NA){
  addl_char <- as.character(addl)
  tidytext::get_stopwords(source = lexicon) %>%
    dplyr::select(word) %>%
    dplyr::bind_rows(., tibble::tibble(word = addl_char)) %>%
    stats::na.omit() %>%
    purrr::as_vector() %>%
    tolower() %>%
    as.character()
}

#' determine stopword status
#'
#' @param .data vector of words
#'
#' @param ... arguments of get_sw
#'
#' @return a [tibble][tibble::tibble-package] equivalent to the input
#'   dataframe, with an additional stopword column
#'
#' @export
determine_stopwords <- function(.data, ...){
  sw_list <- get_sw(...)
  .data %in% sw_list
}

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
text_prep <- function(.data, lemmatize=TRUE, stopwords=TRUE,
                      sw_lexicon="snowball", addl_stopwords=NA){
  formatted <- .data %>%
    format_data()

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

#' creates a search closure to section text
#'
#' @param search a string regexp for the term to seperate on, e.g. "Chapter"
#'
#' @return closure over search expression 
get_search <- function(search){
  function(.data){
    .data %>%
      stringr::str_detect(search) %>%
      purrr::accumulate(sum, na.rm=TRUE)
    }
}

#' sections text based on chapters
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with chapter numbers
#'
#' @export
get_chapters <- get_search("^[\\s]*[Cc][Hh][Aa]?[Pp][Tt]([Ee][Rr])?")

#' sections text based on parts
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with part numbers
#'
#' @export
get_parts <- get_search("^[\\s]*[Pp]([Aa][Rr])?[Tt]")

#' sections text based on sections
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with section numbers
#'
#' @export
get_sections <- get_search("^[\\s]*([Ss][Ss])|([Ss][Ee][Cc][Tt][Ii][Oo][Nn])")

#' sections text based on cantos
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with canto numbers
#'
#' @export
get_cantos <- get_search("(?i)canto (XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$")

#' sections text based on book
#'
#' @param .data vector to section
#'
#' @return vector of same length as .data with book numbers
#'
#' @export
get_books <- get_search("(?i)book$")

#' Adds section column to dataframe
#'
#' @param .data dataframe formatted as per output of prep process
#'
#' @param section_by character name of what to section over
#'
#' @return input dataframe with additional section column
#'
#' @export
section <- function(.data, section_by){
    sec_table <- list("chapter" = get_chapters,
                      "part" = get_parts,
                      "section" = get_sections,
                      "canto" = get_cantos,
                      "book" = get_books)
    .data %>%
        dplyr::mutate(!! section_by := sec_table[[section_by]](word))
}
