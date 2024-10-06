update_medicare_data <- function(medicare_tb,
                                 measures_chr = character(0),
                                 years_chr = character(0)){
  medicare_tb <- medicare_tb %>%
    dplyr::mutate(Year_Quarter = dplyr::case_when(Quarter == "July to September" ~ "Q3",
                                                  Quarter == "October to December" ~ "Q4",
                                                  Quarter == "January to March" ~ "Q1",
                                                  T  ~ "Q2")) %>%
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
update_retainers_ds <- function(retainers_tb,
                                cost_var_1L_chr = "Retainer amount",
                                date_var_1L_chr = "Retainer date",
                                end_date_dtm = NULL){
  retainers_tb <- retainers_tb %>%
    dplyr::mutate(Clinicians = 1) %>%
    add_cyclic_cases(date_var_1L_chr = date_var_1L_chr, arrange_by_1L_chr = date_var_1L_chr, new_zeros_chr = "Clinicians", end_date_dtm = end_date_dtm) %>%
    dplyr::mutate(CumulativeRetainer = cumsum(!!rlang::sym(cost_var_1L_chr)),
                  CumulativeClinicians = cumsum(Clinicians)) %>%
    add_temporal_vars(date_var_1L_chr = date_var_1L_chr) %>%
    dplyr::mutate(Date = Day) %>%
    dplyr::select(Date, dplyr::everything())
  if(date_var_1L_chr!= "Date"){
    retainers_tb <- retainers_tb %>%
      dplyr::select(-!!rlang::sym(date_var_1L_chr))
  }
  retainers_tb <- retainers_tb %>%
    dplyr::rename(Retainer = !!rlang::sym(cost_var_1L_chr))
  return(retainers_tb)
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
  #end_penultimate_dtm <- end_date_dtm - lubridate::duration(1, units = unit_1L_chr)
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
    #dplyr::mutate(Censored_lgl = !!rlang::sym(date_var_1L_chr) >= end_penultimate_dtm) %>%
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
                    # variables_chr %>%
                    #   purrr::reduce(.init = .x,
                    #                 ~ {
                    #                   dplyr::mutate(.x, !!rlang::sym(.y) = case_when())
                    #
                    #
                    #                 })
                  })

  if(inherits(data_xx,"Ready4useDyad")){
    data_xx <- X_Ready4useDyad
  }else{
    data_xx <- X_Ready4useDyad@ds_tb
  }
  return(data_xx)

}
