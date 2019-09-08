#' output a ggplot column graph of the top texts from some insight function
#'
#' @param .data a dataframe containing "text" and insight columns as
#'     per the output of the get_(term|aggregate)_insight wrapper
#'     function
#' 
#' @param col_name symbol name of the column insight was
#'     outputted to
#'
#' @param n number of bars to display
#'
#' @param desc bool: show bars in descending order
#'
#' @export
score_bar_ungrouped <- function(.data, col_name,
                                n = 15, desc = FALSE){
    insight_col <- dplyr::enquo(col_name)
    dist <- .data %>%
        dplyr::distinct(text, .keep_all=TRUE)
    ## if (desc) {
    arr <-  dplyr::arrange(dist, dplyr::desc(!! insight_col))
    ## } else {
    ##     arr <- dplyr::arrange(dist, !! insight_col)
    ## }
    arr %>%
        dplyr::group_modify(~{.x %>% head(n)}) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(text = forcats::fct_reorder(text,
                                                  !! insight_col,
                                                  .desc = desc)) %>%
        ggplot2::ggplot(ggplot2::aes(x = text)) +
        ggplot2::geom_col(ggplot2::aes(y = !! insight_col)) +
        ggplot2::coord_flip()
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

struct_ts_ungrouped <- function(.data, col_name){
    q_col_name <- dplyr::enquo(col_name)
    .data %>%
        dplyr::filter(!is.na(!! q_col_name)) %>%
        dplyr::mutate(term = seq_along(!! q_col_name)) %>%
        ggplot2::ggplot(ggplot2::aes(term, !! q_col_name)) +
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
#' @return ggplot object as per ggpage
#'
#' @export
struct_ggpage_ungrouped <- function(.data, col_name){
    q_col_name <- dplyr::enquo(col_name)
    .data %>%
        dplyr::pull(word) %>%
        ggpage::ggpage_build() %>%
        dplyr::bind_cols(.data) %>% 
        ggpage::ggpage_plot(ggplot2::aes(fill = !! q_col_name)) ## +
        ## ggplot2::labs(title = "")
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
#' @export
get_vis <- function(.data, vis, col, facet_by="", scale_fixed = TRUE){
    vistable <- list("struct_ggpage_ungrouped" = struct_ggpage_ungrouped,
                     "struct_ts_ungrouped" = struct_ts_ungrouped,
                     "score_bar_ungrouped" = score_bar_ungrouped,
                     "dist_density_ungrouped" = dist_density_ungrouped,
                     "dist_hist_ungrouped" = dist_hist_ungrouped)
    col_name <- dplyr::sym(col)
    chart <- vistable[[vis]](.data, !! col_name)
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
