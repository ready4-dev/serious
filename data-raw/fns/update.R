update_for_price_year <- function (data_tb, cost_current_1L_chr = "Cost", cost_constant_1L_chr = "Cost",
                                   price_indices_dbl = numeric(0), price_ref_1L_int = 1L, time_var_1L_chr = "FiscalYear",
                                   total_1L_chr = character(0), years_are_cols_1L_lgl = F)
{
  if (!identical(price_indices_dbl, numeric(0))) {
    multipliers_dbl <- purrr::map_dbl(price_indices_dbl,
                                      ~price_indices_dbl[price_ref_1L_int]/.x)
    multipliers_tb <- tibble::tibble(PriceYearMultiplier = multipliers_dbl,
                                     `:=`(!!rlang::sym(time_var_1L_chr), names(price_indices_dbl)))
    if (years_are_cols_1L_lgl) {
      data_tb <- data_tb %>% tidyr::pivot_longer(intersect(names(data_tb),
                                                           names(price_indices_dbl)), names_to = time_var_1L_chr,
                                                 values_to = cost_current_1L_chr)
    }
    data_tb <- data_tb %>% dplyr::left_join(multipliers_tb)
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(cost_current_1L_chr),
                                              !!rlang::sym(cost_constant_1L_chr) * PriceYearMultiplier)) %>%
      dplyr::select(-PriceYearMultiplier)
    if (years_are_cols_1L_lgl) {
      data_tb <- data_tb %>% tidyr::pivot_wider(names_from = time_var_1L_chr,
                                                values_from = cost_current_1L_chr)
    }
    if (!identical(total_1L_chr, character(0))) {
      data_tb <- data_tb %>% dplyr::select(-tidyselect::any_of(total_1L_chr))
      data_tb <- data_tb %>% dplyr::rowwise() %>% dplyr::mutate(`:=`(!!rlang::sym(total_1L_chr),
                                                                     sum(dplyr::c_across(tidyselect::any_of(names(price_indices_dbl)))))) %>%
        dplyr::ungroup()
    }
  }
  return(data_tb)
}
update_medicare_data <- function(medicare_tb,
                                 measures_chr = character(0),
                                 years_chr = character(0)){
  medicare_tb <- medicare_tb %>%
    dplyr::mutate(Year_Quarter = dplyr::case_when(Quarter == "July to September" ~ "Q3",
                                                  Quarter == "October to December" ~ "Q4",
                                                  Quarter == "January to March" ~ "Q1",
                                                  Quarter == "April to June" ~ "Q2",
                                                  T  ~ Quarter)) %>%
    dplyr::filter(Year_Quarter %in% paste0("Q",1:4)) %>%
    dplyr::mutate(Year_Quarter = Year_Quarter %>% purrr::map2_chr(FinancialYear,
                                                                  ~paste0(ifelse(.x %in% c("Q1","Q2"),
                                                                                 paste0("202",stringr::str_sub(.y, start=7)),
                                                                                 stringr::str_sub(.y, end=4))," ",.x))) %>%
    dplyr::select(Year_Quarter, dplyr::everything()) %>%
    dplyr::mutate(Year_Quarter = tsibble::yearquarter(Year_Quarter)) %>%
    dplyr::mutate(Quarter = Year_Quarter) %>%
    dplyr::select(-Year_Quarter) %>%
    dplyr::select(FinancialYear, Quarter, dplyr::everything())
  if(!identical(measures_chr, character(0))){
    medicare_tb <- medicare_tb %>%
      dplyr::filter(Measure %in% measures_chr)
  }
  if(!identical(years_chr, character(0))){
    medicare_tb <- medicare_tb %>%
      dplyr::filter(FinancialYear %in% years_chr)
  }
  return(medicare_tb)
}
update_retainers_ds <- function (retainers_tb,
                                 cost_var_1L_chr = "Retainer amount",
                                 date_var_1L_chr = "Retainer date",
                                 end_date_dtm = NULL,
                                 price_indices_dbl = numeric(0),
                                 price_ref_1L_int = 1L,
                                 time_var_1L_chr = "FiscalYear")
{
  retainers_tb <- retainers_tb %>% dplyr::mutate(Clinicians = 1) %>%
    add_cyclic_cases(date_var_1L_chr = date_var_1L_chr, arrange_by_1L_chr = date_var_1L_chr,
                     new_zeros_chr = "Clinicians", end_date_dtm = end_date_dtm)  %>%
    add_temporal_vars(date_var_1L_chr = date_var_1L_chr) %>%
    update_for_price_year(cost_current_1L_chr = cost_var_1L_chr,
                          cost_constant_1L_chr = cost_var_1L_chr,
                          price_indices_dbl = price_indices_dbl,
                          price_ref_1L_int = price_ref_1L_int,
                          time_var_1L_chr = time_var_1L_chr) %>%
    dplyr::mutate(CumulativeRetainer = cumsum(!!rlang::sym(cost_var_1L_chr)),
                  CumulativeClinicians = cumsum(Clinicians)) %>%
    dplyr::mutate(Date = Day) %>% dplyr::select(tidyr::all_of(c("Date",names(retainers_tb), "Clinicians", "CumulativeRetainer", "CumulativeClinicians")), dplyr::everything())
  if (date_var_1L_chr != "Date") {
    retainers_tb <- retainers_tb %>% dplyr::select(-!!rlang::sym(date_var_1L_chr))
  }
  retainers_tb <- retainers_tb %>% dplyr::rename(Retainer = !!rlang::sym(cost_var_1L_chr))
  return(retainers_tb)
}
update_scenarios_tb <- function(scenarios_tb,
                                base_case_1L_chr = "Base case",
                                base_case_tb = NULL,
                                change_var_1L_chr = "Total Cost",
                                missing_1L_xx = NULL,
                                difference_1L_int = 1L,
                                outcomes_chr = "Budget Impact",
                                scenario_1L_chr = "Scenario",
                                tfmn_fn = NULL){
  if(!is.null(base_case_tb)){
    scenarios_tb <- dplyr::bind_rows(base_case_tb, scenarios_tb) %>%
      dplyr::mutate(!!rlang::sym(outcomes_chr[difference_1L_int]) := !!rlang::sym(change_var_1L_chr) - base_case_tb %>% dplyr::pull(!!rlang::sym(change_var_1L_chr)))
    extra_vars_chr <- outcomes_chr[-difference_1L_int]
    if(!identical(extra_vars_chr, character(0)) & !is.null(missing_1L_xx)){
      scenarios_tb <- extra_vars_chr %>% purrr::reduce(.init = scenarios_tb,
                                                       ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := dplyr::case_when(( .x %>% dplyr::pull(!!rlang::sym(scenario_1L_chr)) %>% purrr::map_lgl(~.x =="Base case")) & is.na(!!rlang::sym(.y)) ~ missing_1L_xx,
                                                                                                                   T ~ !!rlang::sym(.y))))
    }
  }
  if(!is.null(tfmn_fn)){
    scenarios_tb <- scenarios_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tfmn_fn(.x)))
  }
  scenarios_tb <- scenarios_tb %>%
    dplyr::select(tidyr::any_of(c(scenario_1L_chr, outcomes_chr)))
  return(scenarios_tb)
}
update_start_end_date <- function(end_dtm,
                                  start_dtm,
                                  index_day_1L_chr = "01",
                                  index_month_1L_chr = "07"){
  end_1L_chr <- end_dtm %>% as.character()
  start_1L_chr <- start_dtm %>% as.character()
  index_end_1L_chr <- end_1L_chr
  index_start_1L_chr <- start_1L_chr
  if(!is.na(index_day_1L_chr)){
    index_start_1L_chr <-  paste0(stringr::str_sub(index_start_1L_chr, end=8), index_day_1L_chr)
  }
  if(!is.na(index_month_1L_chr)){
    index_start_1L_chr <- paste0(stringr::str_sub(index_start_1L_chr, end=5), index_month_1L_chr, stringr::str_sub(index_start_1L_chr, start=8))
  }
  index_start_dtm <- as.POSIXct(index_start_1L_chr, tz = start_dtm %>% attr("tzone"))
  if(index_start_dtm > start_dtm ){
    index_year_1L_chr <- index_start_dtm %>% lubridate::year() -1
    index_start_1L_chr <- paste0(index_year_1L_chr, stringr::str_sub(index_start_1L_chr, start=5))
    index_start_dtm <- as.POSIXct(index_start_1L_chr, tz = start_dtm %>% attr("tzone"))
  }
  start_dtm <- index_start_dtm
  index_end_dtm <- start_dtm - lubridate::days(1)
  index_end_1L_chr <- index_end_dtm %>% as.character()
  index_end_1L_chr <- paste0(end_dtm %>% lubridate::year(),stringr::str_sub(index_end_1L_chr, start=5))
  index_end_dtm <- as.POSIXct(index_end_1L_chr, tz = end_dtm %>% attr("tzone"))
  if(index_end_dtm < end_dtm){
    index_year_1L_chr <- index_end_dtm %>% lubridate::year() + 1
    index_end_1L_chr <- paste0(index_year_1L_chr, stringr::str_sub(index_end_1L_chr, start=5))
    index_end_dtm <- as.POSIXct(index_end_1L_chr, tz = end_dtm %>% attr("tzone"))
  }
  end_dtm <- index_end_dtm
  new_dates_ls <- list(start = start_dtm,
                       end = end_dtm)
  return(new_dates_ls)
}
update_temporal_vars <- function(frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal"),
                                 temporal_vars_chr = make_temporal_vars()){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  date_var_1L_chr <- get_new_index(frequency_1L_chr)
  if("Weekday" %in% temporal_vars_chr){
    weekday_1L_chr <- "Weekday"
    temporal_vars_chr <- setdiff(temporal_vars_chr, "Weekday")
  }else{
    weekday_1L_chr <- character(0)
  }
  if(date_var_1L_chr %in% temporal_vars_chr){
    temporal_vars_chr <- temporal_vars_chr[which(temporal_vars_chr==date_var_1L_chr):length(temporal_vars_chr)]
  }
  if("Day" %in% temporal_vars_chr){
    temporal_vars_chr <- c(weekday_1L_chr,temporal_vars_chr)
  }
  return(temporal_vars_chr)
}
update_to_full_tenure <- function(data_tb,
                                  date_var_1L_chr = "Date",
                                  drop_derived_1L_lgl = TRUE,
                                  end_date_dtm = NULL,
                                  start_date_dtm = NULL,
                                  tenure_var_1L_chr = "Tenure",
                                  uid_var_1L_chr = "UID",
                                  unit_1L_chr = "year"){
  if(is.null(end_date_dtm)){
    end_date_dtm <- max(data_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)))
  }
  if(!is.null(start_date_dtm)){
    data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr)>=start_date_dtm)
  }
  if(!tenure_var_1L_chr %in% names(data_tb)){
    data_tb <- data_tb %>% add_tenure(date_var_1L_chr = date_var_1L_chr, tenure_var_1L_chr = tenure_var_1L_chr, uid_var_1L_chr = uid_var_1L_chr, unit_1L_chr = unit_1L_chr)
    derived_1L_chr <- tenure_var_1L_chr
  }else{
    derived_1L_chr <- character(0)
  }
  data_tb <- data_tb %>%
    dplyr::group_by(!!rlang::sym(uid_var_1L_chr)) %>%
    dplyr::mutate(Last_Period_lgl = !!rlang::sym(tenure_var_1L_chr) >= max(floor(!!rlang::sym(tenure_var_1L_chr))))
  summary_lup <- data_tb %>%
    dplyr::summarise(!!rlang::sym(uid_var_1L_chr) := dplyr::first(!!rlang::sym(uid_var_1L_chr)),
                     Index_For_Cut_int = which(Last_Period_lgl)[1],
                     End_This_Cycle_dtm = dplyr::first(!!rlang::sym(date_var_1L_chr)) + lubridate::duration(max(ceiling(!!rlang::sym(tenure_var_1L_chr))), units = unit_1L_chr))
  data_tb <- data_tb %>%
    dplyr::left_join(summary_lup)
  summary_lup <- data_tb %>%
    dplyr::mutate(Grouped_n_int = dplyr::row_number()) %>%
    dplyr::filter(Index_For_Cut_int == Grouped_n_int) %>%
    dplyr::mutate(Cut_Off_dtm = !!rlang::sym(date_var_1L_chr)) %>%
    dplyr::select(!!rlang::sym(uid_var_1L_chr), Cut_Off_dtm)
  data_tb <- data_tb %>%
    dplyr::left_join(summary_lup)
  data_tb <- data_tb %>%
    dplyr::mutate(After_Start_Cut_Off_lgl = !!rlang::sym(date_var_1L_chr) >= Cut_Off_dtm) %>%
    dplyr::mutate(Drop_lgl = purrr::map2_lgl(After_Start_Cut_Off_lgl, End_This_Cycle_dtm, ~.x & (.y > end_date_dtm))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!Drop_lgl)
  if(drop_derived_1L_lgl)
    data_tb <- data_tb %>% dplyr::select(-tidyselect::all_of(c("Last_Period_lgl", "Cut_Off_dtm", "After_Start_Cut_Off_lgl", "Drop_lgl", "Index_For_Cut_int", "End_This_Cycle_dtm", derived_1L_chr)))
  return(data_tb)
}
update_with_imputed <- function(data_xx,
                                impute_ls,
                                lup_ls,
                                args_ls = (list(NULL)),
                                fns_ls = list(sample)
){
  if(inherits(data_xx,"Ready4useDyad")){
    X_Ready4useDyad <- data_xx
  }else{
    X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
  }
  X_Ready4useDyad@ds_tb <- 1:length(impute_ls) %>%
    purrr::reduce(.init = X_Ready4useDyad@ds_tb,
                  ~ {
                    variables_chr <- impute_ls %>% purrr::pluck(.y)
                    lup <- lup_ls %>% purrr::pluck(.y)
                    .x %>%
                      dplyr::mutate(dplyr::across(variables_chr, ~ dplyr::case_when(is.na(.)  ~ "BATMAN",
                                                                                    TRUE ~ .)))
                  })

  if(inherits(data_xx,"Ready4useDyad")){
    data_xx <- X_Ready4useDyad
  }else{
    data_xx <- X_Ready4useDyad@ds_tb
  }
  return(data_xx)

}
