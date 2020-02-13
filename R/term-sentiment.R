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
# term_sentiment <- function(.data, lexicon="afinn"){
#   data <- tibble::enframe(.data, "number", "word")
#   tidytext::get_sentiments(lexicon) %>%
#     dplyr::select(word, value) %>%
#     dplyr::right_join(data, by="word") %>%
#     dplyr::pull(value)
# }

term_sentiment <- function(.data, lexicon="afinn", senti = NULL){
   data <- tibble::enframe(.data, "number", "word")
   
   if (lexicon == "afinn"){ 
     tidytext::get_sentiments(lexicon) %>%
       dplyr::select(word, value) %>%
       dplyr::right_join(data, by="word") %>%
       dplyr::pull(value)
   }
   
   else if (lexicon == "bing"){ 
     tidytext::get_sentiments(lexicon) %>%
       dplyr::mutate(value = ifelse(sentiment == "negative", -1, 1)) %>%
       dplyr::select(word, value) %>%
       dplyr::right_join(data, by="word") %>%
       dplyr::distinct(number, .keep_all = TRUE) %>%
       dplyr::pull(value)
   }
   
   else if (lexicon == "nrc"){ 
     
      tidytext::get_sentiments(lexicon) %>%
        dplyr::filter(sentiment %in% c("positive", "negative")) %>%
        dplyr::mutate(value = ifelse(sentiment == "negative", -1, 1)) %>%
        dplyr::select(word, value) %>%
        dplyr::right_join(data, by="word") %>%
        dplyr::distinct(number, .keep_all = TRUE) %>%
        dplyr::pull(value)
    }
   
   
   else if (lexicon == "loughran"){
     tidytext::get_sentiments(lexicon) %>%
       dplyr::filter(sentiment %in% c("positive", "negative")) %>%
       dplyr::mutate(value = ifelse(sentiment == "negative", -1, 1)) %>%
       dplyr::select(word, value) %>%
       dplyr::right_join(data, by="word") %>%
       dplyr::distinct(number, .keep_all = TRUE) %>%
       dplyr::pull(value)
   }
   
   else if (lexicon == "nrc - all sentiments"){ 
      tidytext::get_sentiments("nrc") %>%
         dplyr::filter(sentiment == senti) %>%
         dplyr::right_join(data, by="word") %>%
         dplyr::distinct(number, .keep_all = TRUE) %>%
         dplyr::pull(sentiment)
   }
   
   else { # loughran - all sentiments
      tidytext::get_sentiments("loughran") %>%
         dplyr::filter(sentiment == senti) %>%
         dplyr::right_join(data, by="word") %>%
         dplyr::distinct(number, .keep_all = TRUE) %>%
         dplyr::pull(sentiment)
   }
 }
