% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_plot.R
\name{plot_residuals}
\alias{plot_residuals}
\title{Plot residuals}
\usage{
plot_residuals(
  ts_models_ls = make_ts_models_ls(),
  var_1L_chr,
  model_1L_chr = "ARIMA",
  type_1L_chr = "innovation",
  ...
)
}
\arguments{
\item{ts_models_ls}{Time series models (a list), Default: make_ts_models_ls()}

\item{var_1L_chr}{Variable (a character vector of length one)}

\item{model_1L_chr}{Model (a character vector of length one), Default: 'ARIMA'}

\item{type_1L_chr}{Type (a character vector of length one), Default: 'innovation'}

\item{...}{Additional arguments}
}
\value{
Residuals (a plot)
}
\description{
plot_residuals() is a Plot function that plots data. Specifically, this function implements an algorithm to plot residuals. The function returns Residuals (a plot).
}
\keyword{internal}
