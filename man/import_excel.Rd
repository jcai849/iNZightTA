% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-excel.R
\name{import_excel}
\alias{import_excel}
\title{Import excel file}
\usage{
import_excel(filepath)
}
\arguments{
\item{filepath}{a string indicating the relative or absolute
filepath of the file to import}
}
\value{
a [tibble][tibble::tibble-package] of each row
    corrresponding to a line of the text file, with the column
    named "text"
}
\description{
Import excel file
}
