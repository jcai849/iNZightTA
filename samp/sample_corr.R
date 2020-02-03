#' 
#' #' @param .data dataframe of terms as per output of format_data
#' #' @param aggregate_on vector to aggregate .data over; ideally,
#' #'   sentence_id, but could be chapter, document, etc.
#' #'   
#' #' @return a matrix of the correlations of each word with every other
#' 
#' 
#' term_correlation <- function(.data, aggregate_on){
#'   corrs <- numeric(length(.data))
#'   samp <- vector("list")
#'   
#'   agg_counts <- get_aggregate_insight(.data, 
#'                                       operations = c("Bound Aggregates", "Aggregated Term Count"), 
#'                                       aggregate_on) %>%
#'                 select(!! dplyr::sym(aggregate_on), `Bound Aggregates`, `Aggregated Term Count`) %>%
#'                 distinct()
#'   
#'   term_counts <- .data %>%
#'     dplyr::group_by(!! dplyr::sym(aggregate_on)) %>%
#'     get_term_insight(operations = "Term Frequency") %>%
#'     select(text, !! dplyr::sym(aggregate_on), `Term Frequency`) %>%
#'     ungroup()
#' 
#'   # reshape term counts data frame
#'   term_counts2 <- melt(term_counts, names(term_counts)[-3], "Term Frequency", na.rm = TRUE)
#'   term_counts3 <- dcast(term_counts2, term_counts2[, 2] ~ text)
#'   
#'   names(term_counts3)[1] <- aggregate_on
#'   
#'   # replace the NAs in term_counts3 with 0's 
#'   term_counts3[is.na(term_counts3)] <- 0 
#'   
#'   abs_freq <- merge(agg_counts, term_counts3)
#'   
#'   rel_freq <- round(abs_freq[,-c(1:3)]/abs_freq[, 3], 3)
#'   cor(rel_freq)
#' }



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
                                      aggregate_on) 
  agg_counts <- data.table::setDT(agg_counts)
  select_vec <- c(aggregate_on, "Bound Aggregates", "Aggregated Term Count")
  agg_counts <- agg_counts[, select_vec, with=FALSE]
  agg_counts <- unique(agg_counts)
  
  # term_counts <- emma_dt[ , .SD[ , get_term_insight(.SD, operations = "Term Frequency")], by=sentence_id]
  
  term_counts <- .data %>%
    dplyr::group_by(!! dplyr::sym(aggregate_on)) %>%
    get_term_insight(operations = "Term Frequency") %>%
    select(text, !! dplyr::sym(aggregate_on), `Term Frequency`) %>%
    ungroup()
  
  term_counts <- data.table::setDT(term_counts)
  # reshape term counts data frame
  term_counts <- data.table::melt(term_counts, names(term_counts)[-3], "Term Frequency", na.rm = TRUE)
  term_counts <- data.table::dcast(term_counts, sentence_id ~ text)
  
  #names(term_counts)[1] <- aggregate_on
  
  # replace the NAs in term_counts3 with 0's 
  for (i in seq_along(term_counts)) {
    set(term_counts, i=which(is.na(term_counts[[i]])), j=i, value=0)
  }

  #term_counts[is.na(term_counts)] <- 0 
  
  abs_freq <- merge(agg_counts, term_counts, by = "sentence_id")
  
  abs_freq[ , (1:2) := NULL ]

  x <- c(1L, ncol(abs_freq))
  
  abs_freq[, names(abs_freq)[-x] := lapply(.SD, "/", abs_freq$`Aggregated Term Count`), .SDcols = -x]
  
  abs_freq[ , (1) := NULL ]
    WGCNA::cor(abs_freq)
}

