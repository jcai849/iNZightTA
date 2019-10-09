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
