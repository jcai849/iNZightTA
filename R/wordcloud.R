#' output a ggplot wordcloud graph of the top texts from some insight function
#'
#' @param .data a dataframe containing "text" and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper
#'     function
#' 
#' @param y symbol name of the column insight was
#'     outputted to
#' 
#' @param x symbol name of column for insight labels
#'
#' @param n number of words to display
#'
#' @param shape character: shape of the wordcloud
#'
#' @export
score_wordcloud <- function(.data, y, n = 15,
                                x = text, shape = "circle"){
    text <- dplyr::enquo(x)
    insight_col <- dplyr::enquo(y)
    .data %>%
        dplyr::distinct(!! text, .keep_all=TRUE) %>%
        dplyr::arrange(dplyr::desc(!! insight_col)) %>%
        dplyr::group_modify(~{.x %>% head(n)}) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(label = stringr::str_wrap(!! text, 30), size = !! insight_col)) +
        ggwordcloud::geom_text_wordcloud(shape = shape, rm_outside = TRUE) +
        ggplot2::scale_size_area(max_size = 24)
}
