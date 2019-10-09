#' output a ggplot time series plot of some insight function
#'
#' @param .data a dataframe containing "text" and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper
#'     function
#' 
#' @param y symbol name of the column insight was
#'     outputted to
#'
#' @export
  struct_time_series <- function(.data, y){
    q_y <- dplyr::enquo(y)
    .data %>%
        dplyr::filter(!is.na(!! q_y)) %>%
        dplyr::mutate(term = seq_along(!! q_y)) %>%
        ggplot2::ggplot(ggplot2::aes(term, !! q_y)) +
        ggplot2::geom_line(na.rm = TRUE)
}
