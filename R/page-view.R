#' Colours a ggpage based on an insight function
#'
#' @param .data a dataframe containing "word" and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper
#'     function
#'
#' @param col_name symbol name of the insight column intended to
#'     colour plot
#'
#' @param num_terms the number of terms to visualise
#'
#' @param term_index which term to start the visualisation from
#'
#' @param palette determine coloration of palette (not yet implemented)
#' 
#' @return ggplot object as per ggpage
#'
#' @export
struct_pageview <- function(.data, col_name, num_terms, term_index, palette){
    end <- min(nrow(.data), term_index + num_terms)
    q_col_name <- dplyr::enquo(col_name)
    .data[seq(term_index, end),] %>%
        dplyr::pull(word) %>%
        ggpage::ggpage_build() %>%
        dplyr::bind_cols(.data[seq(term_index, end),]) %>% 
        ggpage::ggpage_plot(ggplot2::aes(fill = !! q_col_name)) +
                ggplot2::geom_text(ggplot2::aes(label = word,
                              x = (xmax + xmin)/2,
                              y = (ymin + ymax)/2),
                          size = 4, color = "white")
}
