% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_aggregates_summary}
\alias{make_aggregates_summary}
\title{Make aggregates summary}
\usage{
make_aggregates_summary(
  data_tb,
  include_1L_chr,
  extras_chr = character(0),
  select_chr = character(0),
  item_1L_chr = "Item",
  subdivision_1L_chr = c("Subdivision", "Discipline")
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{include_1L_chr}{Include (a character vector of length one)}

\item{extras_chr}{Extras (a character vector), Default: character(0)}

\item{select_chr}{Select (a character vector), Default: character(0)}

\item{item_1L_chr}{Item (a character vector of length one), Default: 'Item'}

\item{subdivision_1L_chr}{Subdivision (a character vector of length one), Default: c("Subdivision", "Discipline")}
}
\value{
Summary (a list)
}
\description{
make_aggregates_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make aggregates summary. The function returns Summary (a list).
}
\keyword{internal}
