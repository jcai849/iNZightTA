#' concat list 1 and 2 at index, skipping NA values
#'
#' @param i numeric index to assess index at
#'
#' @param list1 list or vector for first token
#'
#' @param list2 list or vector for second token
#'
#' @return paste of list1 and list2 at index i, skipping NA's
concat_walk_i <- function(i, list1, list2){
  ifelse(length(list2) < i | is.na(list1[i]),
	  as.character(NA),
  ifelse(!(is.na(list1[i]) | is.na(list2[i])),
	 paste(list1[i], list2[i]),
	 concat_walk_i(i,list1, list2[-1])))
}

#' concat list 1 and 2, moving past NA values
#'
#' @param list1 list or vector for first bigram token
#'
#' @param list2 list or vector for second bigram token
#'
#' @return paste of list1 and list2, skipping NA's
concat_walk <- function(list1, list2){
    stopifnot(length(list1) == length(list2))
    sapply(seq_along(list1), concat_walk_i, list1, list2)
}
