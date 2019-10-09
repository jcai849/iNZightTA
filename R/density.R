#' output a histogram of the distribution of some function of words
#'
#' @param .data the standard dataframe, modified so the last column
#'     is the output of some insight function (eg. output from
#'     term_freq)
#'
#' @param col_name symbol name of the column insight was
#'     performed on
dist_density <- function(.data, col_name){
    q_col_name <- dplyr::enquo(col_name)
    .data %>%
        ggplot2::ggplot(ggplot2::aes(x = !! q_col_name)) +
        ggplot2::geom_density()
}
