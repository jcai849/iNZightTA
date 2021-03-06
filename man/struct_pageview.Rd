% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/page-view.R
\name{struct_pageview}
\alias{struct_pageview}
\title{Colours a ggpage based on an insight function}
\usage{
struct_pageview(.data, col_name, num_terms, term_index, word_size, palette)
}
\arguments{
\item{.data}{a dataframe containing "word" and insight columns as
per the output of the get_(term|aggregate)_insight wrapper
function}

\item{col_name}{symbol name of the insight column intended to
colour plot}

\item{num_terms}{the number of terms to visualise}

\item{term_index}{which term to start the visualisation from}

\item{word_size}{integer size of words}

\item{palette}{determine coloration of palette (not yet implemented)}
}
\value{
ggplot object as per ggpage
}
\description{
Colours a ggpage based on an insight function
}
