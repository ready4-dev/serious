#' Calculate autocorrelations
#' @description calculate_autocorrelations() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate autocorrelations. The function returns Autocorrelations (a list).
#' @param data_xx Data (an output object of multiple potential types)
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param max_1L_int Maximum (an integer vector of length one), Default: NULL
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @return Autocorrelations (a list)
#' @rdname calculate_autocorrelations
#' @export 
#' @importFrom purrr map
#' @importFrom tsibble fill_gaps
#' @importFrom feasts ACF
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @keywords internal
calculate_autocorrelations <- function (data_xx, frequency_1L_chr = c("daily", "weekly", "monthly", 
    "quarterly", "yearly"), key_totals_ls = NULL, key_vars_chr = character(0), 
    max_1L_int = NULL, metrics_chr = make_metric_vars()) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    autocorrelations_ls <- metrics_chr %>% purrr::map(~get_tsibble(data_xx, 
        frequency_1L_chr = frequency_1L_chr, key_totals_ls = key_totals_ls, 
        key_vars_chr = key_vars_chr, metrics_chr = metrics_chr) %>% 
        tsibble::fill_gaps() %>% feasts::ACF(!!rlang::sym(.x), 
        lag_max = max_1L_int)) %>% stats::setNames(metrics_chr)
    return(autocorrelations_ls)
}
#' Calculate running totals
#' @description calculate_running_totals() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate running totals. The function is called for its side effects and does not return a value.
#' @param values_num PARAM_DESCRIPTION
#' @return Totals (a numeric)
#' @rdname calculate_running_totals
#' @export 
#' @importFrom purrr map list_c
#' @keywords internal
calculate_running_totals <- function (values_num) 
{
    totals_num <- 1:length(values_num) %>% purrr::map(~sum(values_num[1:.x])) %>% 
        purrr::list_c()
    return(totals_num)
}
