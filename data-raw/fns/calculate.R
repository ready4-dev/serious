calculate_autocorrelations <- function (data_xx, frequency_1L_chr = c("daily", "weekly", "monthly",
                                                                      "quarterly", "yearly"), key_totals_ls = NULL, key_vars_chr = character(0),
                                        max_1L_int = NULL, metrics_chr = make_metric_vars()) {
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  data_tsb <- get_tsibble(data_xx,
                          frequency_1L_chr = frequency_1L_chr, key_totals_ls = key_totals_ls,
                          key_vars_chr = key_vars_chr, metrics_chr = metrics_chr)
  autocorrelations_ls <- metrics_chr %>% purrr::map(~data_tsb %>%
                                                      tsibble::fill_gaps() %>% feasts::ACF(!!rlang::sym(.x),
                                                                                           lag_max = max_1L_int)) %>% stats::setNames(metrics_chr)
  return(autocorrelations_ls)
}
calculate_running_totals <- function(values_num){
  totals_num <- 1:length(values_num) %>% purrr::map(~sum(values_num[1:.x])) %>% purrr::list_c()
  return(totals_num)
}
