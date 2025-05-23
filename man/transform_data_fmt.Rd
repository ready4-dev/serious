% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_transform.R
\name{transform_data_fmt}
\alias{transform_data_fmt}
\title{Transform data format}
\usage{
transform_data_fmt(
  data_xx,
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  type_1L_chr = c("output", "input")
)
}
\arguments{
\item{data_xx}{Data (an output object of multiple potential types)}

\item{X_Ready4useDyad}{PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()}

\item{type_1L_chr}{Type (a character vector of length one), Default: c("output", "input")}
}
\value{
Data (an output object of multiple potential types)
}
\description{
transform_data_fmt() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform data format. The function returns Data (an output object of multiple potential types).
}
\keyword{internal}
