
#' @param .data dataframe of terms as per output of format_data
#' @param aggregate_on vector to aggregate .data over; ideally,
#'   sentence_id, but could be chapter, document, etc.
#'   
#' @return a matrix of the correlations of each word with every other


term_correlation <- function(.data, aggregate_on){
  corrs <- numeric(length(.data))
  samp <- vector("list")
  
  agg_counts <- get_aggregate_insight(.data, 
                                      operations = c("Bound Aggregates", "Aggregated Term Count"), 
                                      aggregate_on) %>%
                select(!! dplyr::sym(aggregate_on), `Bound Aggregates`, `Aggregated Term Count`) %>%
                distinct()
  
  term_counts <- .data %>%
    dplyr::group_by(!! dplyr::sym(aggregate_on)) %>%
    get_term_insight(operations = "Term Frequency") %>%
    select(text, !! dplyr::sym(aggregate_on), `Term Frequency`) %>%
    ungroup()

  # reshape term counts data frame
  term_counts2 <- melt(term_counts, names(term_counts)[-3], "Term Frequency", na.rm = TRUE)
  term_counts3 <- dcast(term_counts2, term_counts2[, 2] ~ text)
  
  names(term_counts3)[1] <- aggregate_on
  
  # replace the NAs in term_counts3 with 0's 
  term_counts3[is.na(term_counts3)] <- 0 
  
  abs_freq <- merge(agg_counts, term_counts3)
  
  rel_freq <- round(abs_freq[,-c(1:3)]/abs_freq[, 3], 3)
  cor(rel_freq)
}

