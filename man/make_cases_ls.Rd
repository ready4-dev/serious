% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_cases_ls}
\alias{make_cases_ls}
\title{Make cases list}
\usage{
make_cases_ls(
  data_tb,
  by_1L_chr = "year",
  case_1L_chr = "UID",
  date_1L_chr = "Date",
  index_1L_lgl = TRUE,
  index_day_1L_chr = "01",
  index_month_1L_chr = "07",
  end_dtm = NULL,
  start_dtm = NULL
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{by_1L_chr}{By (a character vector of length one), Default: 'year'}

\item{case_1L_chr}{Case (a character vector of length one), Default: 'UID'}

\item{date_1L_chr}{Date (a character vector of length one), Default: 'Date'}

\item{index_1L_lgl}{Index (a logical vector of length one), Default: TRUE}

\item{index_day_1L_chr}{Index day (a character vector of length one), Default: '01'}

\item{index_month_1L_chr}{Index month (a character vector of length one), Default: '07'}

\item{end_dtm}{End (a date vector), Default: NULL}

\item{start_dtm}{Start (a date vector), Default: NULL}
}
\value{
Cases (a list)
}
\description{
make_cases_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cases list. The function returns Cases (a list).
}
\keyword{internal}
