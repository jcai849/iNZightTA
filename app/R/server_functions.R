#########################################################
##################### Server Functions ##################
#########################################################

# This file contains the miscellaneous functions called in the server. 

#########################################################
##################### For calculating td-idf
##################### 
#########################################################

#' @param .data dataframe of terms as per output of format_data
#' 
#' @param grouping character string indicating how the text is grouped into documents
#' 
#' @return original data frame additional tf-idf column                

get_tf_idf <- function(.data, grouping){
  
  # count term frequency by group 
  df <- .data %>%
    dplyr::count(!! dplyr::sym(grouping), text) 
  
  # total number of words in each group (including the stopwords)
  tot_words <- .data %>%
    dplyr::count(!! dplyr::sym(grouping), word) %>%
    dplyr::group_by(!! dplyr::sym(grouping)) %>%
    dplyr::summarise(total_words = sum(n))
  
  # calculate term frequency tf 
  df <- left_join(df, tot_words) %>% 
    mutate(tf = n/total_words)
  
  # calculate inverse doc freq idf
  t <- df$text
  idf <- log(nrow(tot_words) / table(t))
  
  df$idf <- as.numeric(idf[t])
  df$`Term Frequency-Inverse Document Frequency` <- df$tf * df$idf
  df <- df %>% select(!! dplyr::sym(grouping), text, `Term Frequency-Inverse Document Frequency`)
  left_join(.data, df)
}

#########################################################
##################### For readability, word tree, and lexical 
##################### dispersion plot
#########################################################

#' Adds section column to dataframe
#'
#' @param .data data frame as per output of pre-processing (still with stopwords)
#'
#' @param section_by character name of what to section over
#'
#' @return input dataframe with additional section column
#'

section_for_merge_id  <- function(.data, section_by){
  sec_table <- list("chapter" = get_chapters,
                    "part" = get_parts,
                    "section" = get_sections,
                    "canto" = get_cantos,
                    "book" = get_books)
  .data %>%
    group_by(id) %>%
    dplyr::mutate(!! section_by := sec_table[[section_by]](text))
}


#' Groups the text by id and collapses them together into one long string
#'
#' @param x data frame as per output of pre-processing 
#'
#' @param source character name of text source
#'
#' @return data frame with new groupings and text within each group merged together
#' 

merge_id <- function(x, source){
  if (isTruthy(input$merge_id_grps)){
    if (input$section_by == input$merge_id_grps)
    {
      # if user sections the text by chapter/book/canto in the beginning, 
      # add in the column for the sectioning
      x <- x %>%
        section_for_merge_id(input$section_by)
        
      # and merge the text by these columns
      by_section <- x %>%
        group_by(id, !! dplyr::sym(input$section_by)) %>%
        mutate(text = paste(text, collapse = " ")) %>%
        distinct(text) %>% ungroup() %>%
        mutate(id = paste(id, input$section_by))
      by_section
    }
    
    else {
      by_chosen <- x %>% 
        group_by(!! dplyr::sym(input$merge_id_grps)) %>%
        mutate(text = paste(text, collapse = " ")) %>%
        distinct(text) %>% ungroup() %>%
        mutate(id = !! dplyr::sym(input$merge_id_grps))
      by_chosen
    }
  }
 
  else{
    all_merged <- x %>% mutate(text = paste(text, collapse = ". ")) %>%
      distinct(text) %>% mutate(id = "Text")
    all_merged
  }
  
}

# merge_id <- function(x, source){
#   if (source == "Project Gutenberg")
#   {
#     # if user sections the text by chapter/book/canto in the beginning, 
#     # add in the column for the sectioning
#     if (isTruthy(input$section_by)){
#       x <- x %>%
#         section_for_merge_id(input$section_by)
#       
#       # and merge the text by these columns
#       by_section <- x %>%
#         group_by(id, !! dplyr::sym(input$section_by)) %>%
#         mutate(text = paste(text, collapse = " ")) %>%
#         distinct(text) %>% ungroup() %>%
#         mutate(id = paste(id, input$section_by))
#       by_section
#     }
#     
#     
#     # Otherwise just merge together the text from the whole book. 
#     else {
#       
#       by_id <- x %>% group_by(id) %>%
#         mutate(text = paste(text, collapse = " ")) %>%
#         distinct(text)
#       
#       by_id
#       
#     }
#     
#   }
#   
#   else if (source %in% c("The Guardian Articles", "Spotify/Genius", "Upload .txt, .csv, .xlsx, or .xls file")){
#     by_id <- x %>% group_by(id) %>%
#       mutate(text = paste(text, collapse = " ")) %>%
#       distinct(text)
#     by_id
#   }
#   
#   # For tweets, comments, etc 
#   else{
#     all_merged <- x %>% mutate(text = paste(text, collapse = ". ")) %>%
#       distinct(text) %>% mutate(id = "Text")
#     all_merged
#   }
#   
# }


#' Creates kwic object to pass into textplot_xray() and for concordance table
#'
#' @param merged data frame as per output ofmerge_id()
#'
#' @param patt pattern to find in text
#' 
#' @param value type of pattern matching "glob", "regex", or "fixed"
#' 
#' @param window how many words displayed around keyword
#' 
#' @param case_ins case insensitive pattern matching?
#'
#' @return input dataframe with additional section column
#'
#' @export data frame with new groupings and text within each group merged together
#'

get_kwic <- function(merged, patt, value, window, case_ins){
  words <- phrase(unlist(strsplit(patt, split = ",")))
  corp <- corpus(merged, text_field = "text",
                 docid_field = "id")
  kwic(x = corp, pattern = words, window = window, valuetype = value, case_insensitive = case_ins)
  
}

#########################################################
##################### For readability
#########################################################

# After merge_id() merges all the text for each id into one long string, 
# books_with_samples() takes in this data frame containing the id and the merged text, 
# then calculates the Flesch Kincaid score for each. 
# Afterwards binding these together with the sample texts in samples.R and 
# arranging the texts in ascending Flesch Kincaid score. 

books_with_samples <- function(books){
  # calculate the FK score of the text for each id
  books$scores <- lapply(books$text, quanteda::textstat_readability, measure = "Flesch.Kincaid")
  
  # extract the scores and place them in their own column in the data frame
  books$FK <- unlist(lapply(books[["scores"]], function(x) x[[2]])) 
  
  # no sample excerpts for the texts provided
  books$excerpt <- ""
  books <- books %>% select(id, FK, excerpt)
  
  # bind the provided texts with the sample (reference) texts
  samps <- rbind.data.frame(samples, books)
  samps %>% arrange(FK)
}

#########################################################
##################### cleaning text
#########################################################

clean_for_app <- function(df){
  
  Encoding(df$text) <- "UTF-8"
  
  # replaces the fancy apostrophes (for replacing contractions later on)
  df$text <- gsub(intToUtf8(8217), "'", df$text, perl = TRUE)
  
  # For the guardian
  df$text <- gsub("<figcaption.+?</figcaption>|Related.+?</aside>", "", df$text)
  
  df$text <- trimws(gsub("<.+?>|_", "", df$text))

  # Decodes common HTML entities 
  df$text <- gsub("&amp;", "&", df$text)
  df$text <- gsub("&quot;", '"', df$text)
  df$text <- gsub("&#039;|&#39;", "'", df$text)
  
  df$text <- gsub("&.*\\w+?;", " ", df$text)
  
  df$text <- textclean::replace_contraction(df$text)
  
  # Replace Mr. with Mister ... for sentence tokenization 
  df$text <- qdap::replace_abbreviation(df$text)
  df$text <- gsub("^\"|\"$", "", df$text)
  
  return(df)
}

#########################################################
##################### get song lyrics. 
##################### genius_lyrics from genius package
#########################################################

get_lyrics <- function(artist,song){
  df <- genius_lyrics(artist = artist, song = song)
  names(df)[names(df) == "lyric"] <- "text"
  names(df)[names(df) == "track_title"] <- "title"
  df
}


#########################################################
##################### From quanteda - for lexical dispersion plot
##################### changed appearance 
#########################################################

textplot_xray <- function(..., scale = c("absolute", "relative"),
                          sort = FALSE) {
  UseMethod("textplot_xray")
}

#' @export
textplot_xray.default <- function(..., scale = c("absolute", "relative"),
                                  sort = FALSE) {
  stop(friendly_class_undefined_message(class(x), "textplot_xray"))
}

#' @export
textplot_xray.kwic <- function(..., scale = c("absolute", "relative"),
                               sort = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("You must have ggplot2 installed to make a dispersion plot.")
  if (!requireNamespace("grid", quietly = TRUE))
    stop("You must have grid installed to make a dispersion plot.")
  
  position <- from <- keyword <- docname <- ntokens <- NULL
  
  kwics <- list(...)
  if (!all(vapply(kwics, is.kwic, logical(1))))
    stop("objects to plot must be kwic objects")
  
  # create a data.table from the kwic arguments
  x <- data.table(do.call(rbind, kwics))
  # use old variable name
  x[, position := from]
  # get the vector of ntokens
  ntokensbydoc <- unlist(lapply(kwics, attr, "ntoken"))
  # add ntokens to data.table as an indexed "merge"
  x[, ntokens := ntokensbydoc[as.character(x[, docname])]]
  
  # replace "found" keyword with patterned keyword
  x[, keyword := unlist(lapply(kwics, function(l) l[["pattern"]]))]
  
  ###############################
  
  ###############################
  
  # pre-emptively convert keyword to factor before ggplot does it, so that we
  # can keep the order of the factor the same as the order of the kwic objects
  # x[, keyword := factor(keyword, levels = unique(keyword))]
  
  multiple_documents <- length(unique(x$docname)) > 1
  
  # Deal with the scale argument:
  # if there is a user-supplied value, use that after passing through
  # match.argj; if not, use relative for multiple documents and absolute
  # for single documents
  if (!missing(scale)) {
    scale <- match.arg(scale)
  }
  else {
    if (multiple_documents) {
      scale <- "relative"
    } else {
      scale <- "absolute"
    }
  }
  
  # Deal with the sort argument:
  if (sort) {
    x[, docname := factor(docname)] # levels are sorted by default
  } else {
    x[, docname := factor(docname, levels = unique(docname))]
  }
  
  if (scale == "relative")
    x[, position := position / ntokens]
  
  x[,"yvar"] = rnorm(nrow(x), mean = 0.2, sd = 0)
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x = position, y = yvar)) +
    ggplot2::geom_point(ggplot2::aes(size = 2), alpha = 0.13) +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.spacing = grid::unit(0.1, "lines"),
                   panel.border = ggplot2::element_rect(colour = "gray", fill = NA),
                   strip.text.y = ggplot2::element_text(angle = 0)
    ) + ggplot2::ylim(0, 0.4)
  
  if (scale == "absolute")
    plot <- plot +
    ggplot2::geom_rect(ggplot2::aes(xmin = ntokens, xmax = max(x$ntokens),
                                    ymin = 0, ymax = 0.4), fill = "gray90")
  
  if (multiple_documents) {
    # If there is more than one document, put documents on the panel y-axis
    # and keyword(s) on the panel x-axis
    plot <- plot + ggplot2::facet_grid(docname ~ keyword) +
      ggplot2::labs(y = "Document", title = paste("Lexical dispersion plot"))
  }
  else {
    # If not, put keywords on the panel y-axis and the doc name in the title
    plot <- plot + ggplot2::facet_grid(keyword~.) +
      ggplot2::labs(y = "", title = paste("Lexical dispersion plot, document:",
                                          x$docname[[1]]))
  }
  
  if (scale == "relative") {
    plot <- plot + ggplot2::labs(x = "Relative token index")
  }
  else {
    plot <- plot + ggplot2::labs(x = "Token index")
  }
  
  l = vector("list")
  l[[1]] = x
  l[[2]] = plot
  return(l)
}


#########################################################
##################### From pushshift API 
##################### importing data from reddit
#########################################################

#' gets the pushshift data
#'
#' @param postType "submission" or "comment"
#' 
#' @param title Character string to search for in post titles
#' 
#' @param size Number of results to return, maximum is 1000
#' 
#' @param q Character string with search query
#' 
#' @param after Only search for posts made after this data, specified as a UNIX epoch time
#' 
#' @param before Only search for posts made before this data, specified as a UNIX epoch time
#' 
#' @param subreddit Only return posts made in this subreddit
#' 
#' @param nest_level How deep to search? nest_level = 1 returns only top-level comments
#' 
#' @return data frame with desired comments and submissions

getPushshiftData <- function(postType,
                             title = NULL,
                             size = NULL,
                             q = NULL,
                             after = NULL,
                             before = NULL,
                             subreddit = NULL,
                             nest_level = NULL) {
  if(postType == "submission") {
    base_url <- "https://api.pushshift.io/reddit/search/submission/"
    httr::GET(url = base_url, 
              query = list(title = title,
                           size = size,
                           q = q,
                           after = after,
                           before = before,
                           subreddit = subreddit,
                           nest_level = nest_level, 
                           sort = "asc")) %>%
      .$url %>%
      jsonlite::fromJSON() %>%
      .$data %>%
      jsonlite::flatten(recursive = TRUE) %>%
      select(author, title, selftext, created_utc, permalink, num_comments, score, subreddit) %>%
      as_tibble() %>%
      rename(id = title) %>%
      rename(text = selftext) %>%
      arrange(created_utc)
    
  } 
  
  else {
    base_url <- "https://api.pushshift.io/reddit/search/comment/"
    httr::GET(url = base_url, 
              query = list(title = title,
                           size = size,
                           q = q,
                           after = after,
                           before = before,
                           subreddit = subreddit,
                           nest_level = nest_level, 
                           sort = "asc")) %>%
      .$url %>%
      fromJSON() %>%
      .$data %>%
      jsonlite::flatten(recursive = TRUE) %>%
      select(author, body, permalink, score, created_utc, subreddit) %>%
      as_tibble() %>%
      rename(id = permalink) %>%
      rename(text = body) %>%
      arrange(created_utc)
  }
}

getPushshiftDataRecursive <- function(postType = "submission",
                                      title = NULL,
                                      size = NULL,
                                      q = NULL,
                                      after = NULL,
                                      before = NULL,
                                      subreddit = NULL,
                                      nest_level = NULL,
                                      delay = 0) {

  tmp <- getPushshiftData(postType,
                          title,
                          size,
                          q,
                          after,
                          before,
                          subreddit,
                          nest_level)
  
  out <- tmp %>% filter(FALSE)
  on.exit(return(out), add = TRUE)
  after <- last(tmp$created_utc)
  
  while(nrow(tmp) > 0) {
    message(
      sprintf("%d %ss fetched, last date fetched: %s \n",
              nrow(tmp),
              postType,
              as.Date(as.POSIXct(as.numeric(after), origin = "1970-01-01"))))
    out <- rbind(out, tmp)
    after <- last(tmp$created_utc)
    tmp <- getPushshiftData(postType,
                            title,
                            size,
                            q,
                            after,
                            before,
                            subreddit,
                            nest_level)
  }
  Sys.sleep(delay)
}


#########################################################
##################### Error handling for plots in shiny
##################### (used in lexical div plot)
#########################################################

#' To present error message in plot outputs in shiny 
#'
#' @param ... Text strings to be printed in the plot window 
#' 
#' @param sep Character string to separate the strings provided in ...
#' 
#' @return Plot with text strings printed on it


plot_exception <-function(
  ...,
  sep=" "){      
  
  txt = paste(...,collapse = sep)
  print(ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = txt), color = "red", size = 6) + 
          ggplot2::theme_void())
  invisible(NULL)
}

#########################################################
##################### preserve the emojis in tweets
##################### package lexicon also has sentiment corresponding to 
##################### emojis (for use in sentimentr)
#########################################################

#' Convert the emojis in a text vector to ::their description::
#'
#' @param x A character vector
#' 
#' @param emoji_dt data frame where a column called "x" contains the emoji in 
#' bytes and another column called "y" with its description
#' 
#' @return Character vector with emojis replaced with ::description of emoji::

emoji_to_words <- function(x, emoji_dt = lexicon::hash_emojis){
  x <- iconv(x, "UTF-8", "ASCII", "byte")
  gsub("\\s+", " ", mgsub(x, emoji_dt[["x"]], paste0("::", emoji_dt[["y"]], "::")))
}

#' Same as above, but for emoticons 

emoticon_to_words <- function (x, emoticon_dt = lexicon::hash_emoticons) 
{
  gsub("\\s+", " ", .mgsub(emoticon_dt[["x"]], paste0(" ", emoticon_dt[["y"]], " "), 
                           x))
}
