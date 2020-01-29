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
    vistable <- list("Page View" = struct_pageview,
                     "Time Series" = struct_time_series,
                     "Bar" = score_barplot,
                     "Density" = dist_density,
                     "Histogram" = dist_hist,
                     "Word Cloud" = score_wordcloud)
    y <- dplyr::sym(col)
    chart <- vistable[[vis]](.data, !! y, ...)
    if (shiny::isTruthy(facet_by)){
        facet_name <- dplyr::sym(facet_by)
        q_facet_name <- dplyr::enquo(facet_name)
        if (vis == "Time Series"){
            return(chart + ggplot2::facet_wrap(ggplot2::vars(!! q_facet_name),
                                               scales = ifelse(vis == "struct_pageview" | scale_fixed,
                                                               "fixed",
                                                               "free"), ncol = 1))
        }
        else{
            return(chart + ggplot2::facet_wrap(ggplot2::vars(!! q_facet_name),
                                               scales = ifelse(vis == "struct_pageview" | scale_fixed,
                                                               "fixed",
                                                               "free")))
        }
    } else {
        return(chart)
    }
}
