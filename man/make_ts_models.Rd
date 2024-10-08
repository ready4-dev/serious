% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_ts_models}
\alias{make_ts_models}
\title{Make time series models}
\usage{
make_ts_models(
  data_xx,
  approximation_xx = NULL,
  fill_gaps_1L_lgl = FALSE,
  frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly", "yearly"),
  index_1L_chr = character(0),
  key_vars_chr = character(0),
  key_totals_ls = NULL,
  metrics_chr = make_metric_vars(),
  models_chr = c("Mean", "Naïve", "Seasonal naïve", "Drift", "Trend", "LMTS", "ETS",
    "ARIMA", "NNTEAR", "Prophet", "Reg_ARIMA", "Reg_TSLM"),
  model_type_1L_chr = "multiplicative",
  order_1L_int = 2,
  period_1L_int = 12,
  stepwise_1L_lgl = TRUE,
  terms_1L_chr = character(0),
  test_1L_int = integer(0),
  type_1L_chr = c("totals", "key"),
  what_1L_chr = character(0)
)
}
\arguments{
\item{data_xx}{Data (an output object of multiple potential types)}

\item{approximation_xx}{Approximation (an output object of multiple potential types), Default: NULL}

\item{fill_gaps_1L_lgl}{Fill gaps (a logical vector of length one), Default: FALSE}

\item{frequency_1L_chr}{Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")}

\item{index_1L_chr}{Index (a character vector of length one), Default: character(0)}

\item{key_vars_chr}{Key variables (a character vector), Default: character(0)}

\item{key_totals_ls}{Key totals (a list), Default: NULL}

\item{metrics_chr}{Metrics (a character vector), Default: make_metric_vars()}

\item{models_chr}{Models (a character vector), Default: c("Mean", "Naïve", "Seasonal naïve", "Drift", "Trend", "LMTS",
"ETS", "ARIMA", "NNTEAR", "Prophet", "Reg_ARIMA", "Reg_TSLM")}

\item{model_type_1L_chr}{Model type (a character vector of length one), Default: 'multiplicative'}

\item{order_1L_int}{Order (an integer vector of length one), Default: 2}

\item{period_1L_int}{Period (an integer vector of length one), Default: 12}

\item{stepwise_1L_lgl}{Stepwise (a logical vector of length one), Default: TRUE}

\item{terms_1L_chr}{Terms (a character vector of length one), Default: character(0)}

\item{test_1L_int}{Test (an integer vector of length one), Default: integer(0)}

\item{type_1L_chr}{Type (a character vector of length one), Default: c("totals", "key")}

\item{what_1L_chr}{What (a character vector of length one), Default: character(0)}
}
\value{
Time series models (a list)
}
\description{
make_ts_models() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make time series models. The function returns Time series models (a list).
}
