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
