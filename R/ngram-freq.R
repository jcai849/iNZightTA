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
