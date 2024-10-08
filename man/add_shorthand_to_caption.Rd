% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_shorthand_to_caption}
\alias{add_shorthand_to_caption}
\title{Add shorthand to caption}
\usage{
add_shorthand_to_caption(
  caption_1L_chr = "",
  data_tsb = NULL,
  min_1L_int = 3L,
  original_xx = character(0),
  shorten_1L_chr = character(0)
)
}
\arguments{
\item{caption_1L_chr}{Caption (a character vector of length one), Default: ''}

\item{data_tsb}{Data (a tsibble), Default: NULL}

\item{min_1L_int}{Minimum (an integer vector of length one), Default: 3}

\item{original_xx}{Original (an output object of multiple potential types), Default: character(0)}

\item{shorten_1L_chr}{Shorten (a character vector of length one), Default: character(0)}
}
\value{
Caption (a character vector of length one)
}
\description{
add_shorthand_to_caption() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add shorthand to caption. The function returns Caption (a character vector of length one).
}
\keyword{internal}
