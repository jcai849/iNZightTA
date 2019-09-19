#' output a ggplot column graph of the top texts from some insight function
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
#' @param n number of bars to display
#'
#' @param desc bool: show bars in descending order
#'
#' @export
score_bar_ungrouped <- function(.data, y, n = 15,
                                x = text, desc = FALSE){
    wrap <- 50
    text <- dplyr::enquo(x)
    insight_col <- dplyr::enquo(y)
    .data %>%
        dplyr::distinct(!! text, .keep_all=TRUE) %>%
        dplyr::arrange(dplyr::desc(!! insight_col)) %>%
        dplyr::group_modify(~{.x %>% head(n)}) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(text = forcats::fct_reorder(shorten(!! text, wrap),
                                                  !! insight_col,
                                                  .desc = desc)) %>%
        ggplot2::ggplot(ggplot2::aes(x = text)) +
        ggplot2::geom_col(ggplot2::aes(y = !! insight_col)) +
        ggplot2::coord_flip()
}

#' Shorten some text up to n characters
#'
#' @param .data character vector
#' 
#' @param n wrap length of text
#'
#' @return shortened form of .data
#'
shorten <- function(.data, n){
    ifelse(nchar(.data) > n,
           paste(substr(.data, 1, n), "...", sep = ""),
           .data)
}

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
score_wordcloud_ungrouped <- function(.data, y, n = 15,
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

#' output a histogram of the distribution of some function of words
#'
#' @param .data the standard dataframe, modified so the last column
#'     is the output of some insight function (eg. output from
#'     term_freq)
#'
#' @param col_name symbol name of the column insight was
#'     performed on
dist_density_ungrouped <- function(.data, col_name){
    q_col_name <- dplyr::enquo(col_name)
    .data %>%
        ggplot2::ggplot(ggplot2::aes(x = !! q_col_name)) +
        ggplot2::geom_density()
}

#' output a histogram of the distribution of some function of words
#'
#' @param .data the standard dataframe, modified so the last column
#'     is the output of some insight function (eg. output from
#'     term_freq)
#'
#' @param col_name symbol name of the column insight was
#'     performed on
dist_hist_ungrouped <- function(.data, col_name){
    q_col_name <- dplyr::enquo(col_name)
    .data %>%
        ggplot2::ggplot(ggplot2::aes(x = !! q_col_name)) +
        ggplot2::geom_histogram()
}

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
  struct_ts_ungrouped <- function(.data, y){
    q_y <- dplyr::enquo(y)
    .data %>%
        dplyr::filter(!is.na(!! q_y)) %>%
        dplyr::mutate(term = seq_along(!! q_y)) %>%
        ggplot2::ggplot(ggplot2::aes(term, !! q_y)) +
        ggplot2::geom_line(na.rm = TRUE)
}

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
struct_ggpage_ungrouped <- function(.data, col_name, num_terms, term_index, palette){
    end <- min(nrow(.data), term_index + num_terms)
    q_col_name <- dplyr::enquo(col_name)
    .data[seq(term_index, end),] %>%
        dplyr::pull(word) %>%
        ggpage::ggpage_build() %>%
        dplyr::bind_cols(.data[seq(term_index, end),]) %>% 
        ggpage::ggpage_plot(ggplot2::aes(fill = !! q_col_name)) ## +
}

#' create a group-aware visualisation
#'
#' @param .data the standard dataframe, modified so the last column
#'     is the output of some insight function (eg. output from
#'     term_freq)
#'
#' @param vis character name of visualisation function
#'
#' @param col character name of the column to get insight from
#'
#' @param facet_by character name of the column to facet by
#'
#' @param scale_fixed force scales to be fixed in a facet
#'  
#' @param ... additional arguments to the visualisation
#'
#' @export
get_vis <- function(.data, vis, col, facet_by="", scale_fixed = TRUE, ...){
    vistable <- list("Page View" = struct_ggpage_ungrouped,
                     "Time Series" = struct_ts_ungrouped,
                     "Bar" = score_bar_ungrouped,
                     "Density" = dist_density_ungrouped,
                     "Histogram" = dist_hist_ungrouped,
                     "Word Cloud" = score_wordcloud_ungrouped)
    y <- dplyr::sym(col)
    chart <- vistable[[vis]](.data, !! y, ...)
    if (shiny::isTruthy(facet_by)){
        facet_name <- dplyr::sym(facet_by)
        q_facet_name <- dplyr::enquo(facet_name)
        return(chart + ggplot2::facet_wrap(ggplot2::vars(!! q_facet_name),
                                           scales = ifelse(vis == "struct_ggpage_ungrouped" | scale_fixed,
                                                           "fixed",
                                                           "free")))

    } else {
        return(chart)
    }
}
