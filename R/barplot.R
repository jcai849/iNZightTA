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
score_barplot <- function(.data, y, n = 15,
                                x = text, desc = FALSE, fill = NULL){
    
    wrap <- 50
    text <- dplyr::enquo(x)
    insight_col <- dplyr::enquo(y)
    if (desc == FALSE){
        arranged <- .data %>%
            dplyr::distinct(!! text, .keep_all=TRUE) %>%
            dplyr::arrange(dplyr::desc(!! insight_col)) 
    }
    else {
        arranged <- .data %>%
            dplyr::distinct(!! text, .keep_all=TRUE) %>%
            dplyr::arrange(!! insight_col) 
    }
    
    if (fill != "") {
         #cols <- gg_cols(length(unique(.data[,fill])))
            
        arranged %>%
            dplyr::group_modify(~{.x %>% head(n)}) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(text = forcats::fct_reorder(shorten(!! text, wrap),
                                                      !! insight_col)) %>%
            ggplot2::ggplot(ggplot2::aes_string(x = "text", fill = fill)) +
            ggplot2::geom_col(ggplot2::aes(y = !! insight_col)) +
            ggplot2::coord_flip() + 
            ggplot2::theme(text = element_text(size=20)) + 
            #scale_fill_manual(cols) + 
            guides(fill=FALSE)
    }
    else {
        
        arranged %>%
                dplyr::group_modify(~{.x %>% head(n)}) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(text = forcats::fct_reorder(shorten(!! text, wrap),
                                                          !! insight_col)) %>%
                ggplot2::ggplot(ggplot2::aes(x = text)) +
                ggplot2::geom_col(ggplot2::aes(y = !! insight_col, fill = "#4682b4")) +
                ggplot2::coord_flip() + 
                ggplot2::theme(text = element_text(size=20)) + guides(fill=FALSE)
        }
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
