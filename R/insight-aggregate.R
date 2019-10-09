#' bind aggregate terms together
#'
#' @param data vector of terms
#'
#' @param aggregate_on vector of aggregations
#'
#' @return data with every aggregation bound, as in a sentence
#'
#'
bind_aggregation <- function(data, aggregate_on){
    tibble::tibble(data, agg = aggregate_on) %>%
        dplyr::group_by(agg) %>%
        dplyr::mutate(bound = paste(data, collapse = " ")) %>%
        dplyr::pull(bound)
}
