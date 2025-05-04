make_age_bands_lup <- function(bands_chr,
                               values_ls,
                               fractions_ls = NULL,
                               type_1L_chr = "by_year"){
  if(type_1L_chr=="by_year"){
    age_bands_lup  <- tibble::tibble(Name = bands_chr,
                                     Range = values_ls)
  }
  if(type_1L_chr=="by_group"){
    age_bands_lup <- tibble::tibble(Name = bands_chr,
                                    Source = values_ls,
                                    Fraction = fractions_ls)
  }
  return(age_bands_lup)
}
make_aggregates_summary <- function(data_tb,
                                    include_1L_chr,
                                    extras_chr = character(0),
                                    select_chr = character(0),
                                    item_1L_chr = "Item",
                                    subdivision_1L_chr = c("Subdivision", "Discipline")){
  subdivision_1L_chr <- match.arg(subdivision_1L_chr)
  select_chr <- c(include_1L_chr, select_chr) %>% unique()
  categories_chr <- data_tb$Category %>% unique()
  divisions_ls <- categories_chr %>% purrr::map(~{
    category_1L_chr <- .x
    subcategories_chr <- data_tb %>% dplyr::filter(Category == category_1L_chr) %>% dplyr::pull(Subcategory) %>% unique()
    subdisions_ls <- subcategories_chr %>% purrr::map(~data_tb %>% dplyr::filter(Category == category_1L_chr, Subcategory == .x) %>% dplyr::pull(!!rlang::sym(subdivision_1L_chr)) %>% unique() %>% sort()) %>% stats::setNames(subcategories_chr)
    subdisions_ls
  }) %>% stats::setNames(categories_chr)
  summary_tb <- data_tb %>% dplyr::select(tidyselect::any_of(c("UID", "Category", "Subcategory", subdivision_1L_chr, item_1L_chr, select_chr, extras_chr))) %>% tidyr::complete(UID, Category, Subcategory, !!rlang::sym(subdivision_1L_chr))
  summary_tb <- summary_tb %>%
    dplyr::mutate(Allowed = purrr::pmap_lgl(., ~ifelse(!(..3 %in% names(divisions_ls[[..2]])),
                                                       F,
                                                       ifelse(!(..4 %in% divisions_ls[[..2]][[..3]]),F,T)
    ))) %>%
    dplyr::filter(Allowed) %>% dplyr::select(-Allowed)
  totals_tb <- summary_tb %>%
    dplyr::filter(!!rlang::sym(subdivision_1L_chr) == "Total")
  summary_tb <- summary_tb %>%
    dplyr::filter(!!rlang::sym(subdivision_1L_chr) != "Total") %>%
    dplyr::group_by(UID, Category, Subcategory, !!rlang::sym(subdivision_1L_chr)) %>%
    dplyr::summarise(!!rlang::sym(item_1L_chr) := "Total",
                     dplyr::across(tidyselect::any_of(select_chr), ~ sum(.x, na.rm = TRUE)))
  summary_tb <- rbind(summary_tb, totals_tb) %>% dplyr::arrange(UID, Category, Subcategory, !!rlang::sym(subdivision_1L_chr), !!rlang::sym(item_1L_chr)) %>%
    dplyr::mutate(Included = !is.na(!!rlang::sym(include_1L_chr)))
  summary_ls <- list(sudbivisions_tb = summary_tb)
  totals_tb <- summary_tb %>%
    dplyr::filter(Subcategory == "Total") %>%
    dplyr::select(-c(!!rlang::sym(subdivision_1L_chr), Included))
  summary_tb <- summary_tb %>%
    dplyr::filter(Subcategory != "Total") %>%
    dplyr::group_by(UID, Category, Subcategory) %>% dplyr::summarise(!!rlang::sym(subdivision_1L_chr) := "Total",
                                                                     dplyr::across(tidyselect::any_of(select_chr), ~ sum(.x, na.rm = TRUE)))
  summary_tb <- rbind(summary_tb, totals_tb) %>% dplyr::arrange(UID, Category, Subcategory) %>%
    dplyr::mutate(Included = !is.na(!!rlang::sym(include_1L_chr)))
  summary_ls$subcategories_tb <- summary_tb
  summary_tb <- summary_tb %>%
    dplyr::group_by(UID, Category) %>% dplyr::summarise(Subcategory = "Total",
                                                        dplyr::across(tidyselect::any_of(select_chr), ~ sum(.x, na.rm = TRUE))
                                                        # `$` = sum(`$`, na.rm = T)
    ) %>% dplyr::arrange(UID, Category) %>%
    dplyr::mutate(Included = !is.na(!!rlang::sym(include_1L_chr)))
  summary_ls$Categories_tb <- summary_tb
  summary_ls <- summary_ls %>% purrr::map(~dplyr::left_join(.x, data_tb %>% dplyr::select(UID, Program, Service, Provider, FirstEight, Year, Period, Start, End) %>% dplyr::distinct()) %>%
                                            dplyr::mutate(Date = purrr::map2(Start, End, ~
                                                                               {
                                                                                 interval_dtm <- lubridate::interval(lubridate::ymd(.x),lubridate::ymd(.y))
                                                                                 interval_dtm@start + lubridate::as.duration(interval_dtm)/2

                                                                               }) %>% purrr::map_vec(~as.Date(.x)) ) %>%
                                            dplyr::mutate(Months = Period,
                                                          Period = UID %>% stringr::str_sub(start=-11)) %>%
                                            dplyr::ungroup() %>%
                                            dplyr::select(tidyselect::any_of(c("UID", "Program", "Service", "Provider", "FirstEight", "Year", "Months","Period", "Start", "End", "Date", "Category", "Subcategory", subdivision_1L_chr, item_1L_chr, select_chr, "Included"))))

  return(summary_ls)

}
make_base_case_tb <- function(forecast_tb,
                              add_ls = NULL,
                              scenario_1L_chr = "Base case",
                              use_1L_chr = "Mean"){
  base_case_tb <- forecast_tb %>%
    dplyr::select(dplyr::all_of(c("Parameter", use_1L_chr))) %>%
    t() %>% as.data.frame() %>%
    janitor::row_to_names(1) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric))
  bind_ls <- list(Scenario = scenario_1L_chr)
  if(is.list(add_ls)){
    bind_ls <- append(bind_ls, add_ls)
  }
  base_case_tb <- base_case_tb %>% dplyr::bind_cols(tibble::as_tibble(bind_ls)) %>%
    dplyr::select(Scenario, dplyr::everything())
  return(base_case_tb)
}
make_cases_ls <- function(data_tb,
                          by_1L_chr = "year",
                          case_1L_chr = "UID",
                          date_1L_chr = "Date",
                          index_1L_lgl = TRUE,
                          index_day_1L_chr = "01",
                          index_month_1L_chr = "07",
                          end_dtm = NULL,
                          start_dtm = NULL
){
  start_end_ls <- make_start_end_ls(data_tb,
                                    by_1L_chr = by_1L_chr,
                                    case_1L_chr = case_1L_chr,
                                    index_1L_lgl = index_1L_lgl,
                                    index_day_1L_chr = index_day_1L_chr,
                                    index_month_1L_chr = index_month_1L_chr,
                                    end_dtm = end_dtm,
                                    start_dtm = start_dtm)
  cases_ls <- purrr::map2(start_end_ls$start, start_end_ls$end,
                          ~ {
                            data_tb %>%
                              dplyr::filter(!!rlang::sym(date_1L_chr)>=.x & !!rlang::sym(date_1L_chr)<=.y) %>%
                              dplyr::pull(!!rlang::sym(case_1L_chr)) %>%
                              unique()  %>%
                              purrr::discard(is.na)
                          })
  return(cases_ls)
}
make_composite_forecast <- function(forecasts_tb){
  forecasts_tb <- forecasts_tb %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~mean(.x, na.rm = TRUE)))
  return(forecasts_tb)
}
make_cost_tb <- function (data_tsb, cost_1L_chr = "Cost",
                          frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal"), group_by_chr = character(0),
                          group_fn = sum, metrics_chr = make_metric_vars(), temporal_vars_chr = make_temporal_vars(),
                          unit_1L_chr = character(0), unit_cost_1L_chr = "UnitCost")
{
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  date_var_1L_chr <- get_new_index(frequency_1L_chr)
  temporal_vars_chr <- update_temporal_vars(frequency_1L_chr,
                                            temporal_vars_chr = temporal_vars_chr)
  metrics_chr <- intersect(metrics_chr, names(data_tsb))
  cost_tb <- get_tsibble(data_tsb, frequency_1L_chr = frequency_1L_chr,
                         metrics_chr = metrics_chr) %>% add_temporal_vars(date_var_1L_chr = date_var_1L_chr,
                                                                          temporal_vars_chr = temporal_vars_chr) %>% tsibble::as_tibble()
  if (!identical(group_by_chr, character(0))) {
    cost_tb <- cost_tb %>% dplyr::group_by(dplyr::across(tidyr::all_of(group_by_chr))) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric),
                                     group_fn))
  }
  if (!identical(unit_1L_chr, character(0))) {
    cost_tb <- cost_tb %>% dplyr::mutate(`:=`(!!rlang::sym(unit_cost_1L_chr),
                                              !!rlang::sym(cost_1L_chr)/!!rlang::sym(unit_1L_chr)))
  }
  return(cost_tb)
}
make_cumulatives <- function(prefix_1L_chr = "Cumulative",
                             separation_after_dbl = numeric(0)){
  episodes_vars_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl) %>% sort()
  active_vars_chr <- episodes_vars_chr[startsWith(episodes_vars_chr,"Active")]
  cumulatives_chr <- paste0(prefix_1L_chr, c(episodes_vars_chr[startsWith(episodes_vars_chr,"Episodes")],
                                             c("Appointments", "Cancellations", "Referrals", "Cost"),
                                             episodes_vars_chr[startsWith(episodes_vars_chr,"Separations")]))
  return(cumulatives_chr)
}
make_date_tfmn_fn <- function(format_1L_chr = "%d-%b-%y"){
  date_tfmn_fn <- eval(parse(text=paste0("function(x){format(x,\"",format_1L_chr,"\") %>% as.Date(\"",format_1L_chr,"\")}")))
  return(date_tfmn_fn)
}
make_duplicates_ls <- function(data_tb){
  categories_chr <- data_tb %>% dplyr::pull(Category) %>% unique() %>% sort()
  duplicates_ls <- categories_chr %>% purrr::map(~{
    category_1L_chr <- .x
    subcategories_chr <- data_tb %>% dplyr::filter(Category == category_1L_chr) %>% dplyr::pull(Subcategory) %>% unique() %>% sort()
    subcategories_chr %>% purrr::map(~make_duplicates_tb(data_tb, category_1L_chr = category_1L_chr, subcategory_1L_chr = .x)) %>%
      stats::setNames(subcategories_chr)
  }) %>%
    stats::setNames(categories_chr)
  return(duplicates_ls)
}
make_duplicate_totals_tb <- function(data_tb,
                                     type_1L_chr = c("Category", "Subcategory","Subdivision", "Discipline")){
  type_1L_chr <- match.arg(type_1L_chr)
  groupings_chr <- c("Category", "Subcategory",
                     ifelse(type_1L_chr == "Discipline","Discipline","Subdivision"))
  pick_1L_int <- which(groupings_chr==type_1L_chr)
  aggregate_at_1L_chr <- groupings_chr[pick_1L_int]
  groupings_chr <- c("UID", groupings_chr[ifelse(pick_1L_int-1>0,1,0):(pick_1L_int-1)])
  duplicate_totals_tb <- data_tb %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(groupings_chr))) %>%
    dplyr::summarise(!!rlang::sym(aggregate_at_1L_chr) := list(!!rlang::sym(aggregate_at_1L_chr))) %>%
    dplyr::filter(!!rlang::sym(aggregate_at_1L_chr) %>% purrr::map_lgl(~("Total" %in% .x) & length(.x)>1))
  return(duplicate_totals_tb)
}
make_duplicates_tb <- function(data_tb,
                               category_1L_chr,
                               subcategory_1L_chr){
  ids_chr <- data_tb %>% dplyr::filter(Subcategory == subcategory_1L_chr) %>% dplyr::pull(UID) %>% unique()
  duplicates_tb <- data_tb %>% dplyr::filter(UID %in% ids_chr & Category == category_1L_chr) %>% dplyr::arrange(UID)
  duplicates_tb <- duplicates_tb %>% dplyr::group_by(UID,
                                                     Subcategory ###
  ) %>% dplyr::mutate(Count = dplyr::n()) %>% dplyr::ungroup() %>% dplyr::arrange(UID)
  duplicates_tb <- duplicates_tb %>% dplyr::filter(Count>1)
  return(duplicates_tb)
}
make_episodes_vars <- function(active_var_1L_chr = "Active",
                               episode_var_1L_chr = "Episodes",
                               flatten_1L_lgl = TRUE,
                               prefix_1L_chr = "",
                               separation_after_dbl = numeric(0),
                               separations_var_1L_chr = "Separations",
                               suffix_1L_chr = ""){
  if(identical(separation_after_dbl, numeric(0))){
    episodes_vars_xx <- paste0(paste0(prefix_1L_chr, c(active_var_1L_chr, episode_var_1L_chr, separations_var_1L_chr)),suffix_1L_chr)
  }else{
    episodes_vars_xx <- 1:length(separation_after_dbl) %>% purrr::map(~make_episodes_vars(suffix_1L_chr = ifelse(.x==1,"",paste0("_",separation_after_dbl[.x]))))
    if(flatten_1L_lgl){
      episodes_vars_xx <- episodes_vars_xx %>% purrr::flatten_chr()
    }
  }
  return(episodes_vars_xx)
}
make_erp_ds <- function (erp_raw_tb = get_raw_erp_data(region_chr = "AUS"),
                         age_range_int = 1:115, age_bands_lup = NULL, age_tfmn_fn = as.integer,
                         as_dyad_1L_lgl = TRUE, frequency_1L_chr = "quarterly", measure_1L_chr = "count",
                         select_chr = c("time_period", "age", "sex", "obs_value"),
                         sex_chr = c("Female", "Male", "Total"), summarise_1L_lgl = FALSE,
                         var_ctg_chr = c("Temporal", "Key", "Key", "Metric"))
{
  measure_1L_int <- switch(measure_1L_chr, count = 1, change = 2,
                           `%change` = 3)
  period_1L_chr <- get_new_index(frequency_1L_chr)
  temporal_fn <- get_temporal_fn(period_1L_chr)
  value_1L_chr <- ifelse(measure_1L_int < 3, "Persons", "Percentage")
  erp_tb <- erp_raw_tb %>%
    haven::zap_labels() %>%
    dplyr::filter(measure == measure_1L_int) %>%
    dplyr::select(tidyselect::all_of(select_chr)) %>%
    dplyr::mutate(sex = dplyr::case_when(sex == 1 ~ sex_chr[2],
                                         sex == 2 ~ sex_chr[1], sex == 3 ~ sex_chr[3])) %>%
    dplyr::mutate(age = age_tfmn_fn(age)) %>% dplyr::arrange(time_period,
                                                             age, sex) %>% dplyr::rename(`:=`(!!rlang::sym(period_1L_chr),
                                                                                              time_period), `:=`(!!rlang::sym(value_1L_chr), obs_value),
                                                                                         Age = age, Sex = sex) %>% dplyr::mutate(`:=`(!!rlang::sym(period_1L_chr),
                                                                                                                                      temporal_fn(Quarter))) %>% dplyr::filter(Age %in% age_range_int)
  if (!is.null(age_bands_lup)) {
    erp_tb <- erp_tb %>% dplyr::mutate(Age = Age %>% purrr::map_chr(~{
      age_1L_int <- .x
      age_bands_lup$Name[which(age_bands_lup$Range %>%
                                 purrr::map_lgl(~age_1L_int %in% .x[1]:.x[2]))]
    }))
  }
  if (summarise_1L_lgl) {
    erp_tb <- erp_tb %>% dplyr::group_by(dplyr::across(setdiff(names(erp_tb),
                                                               value_1L_chr))) %>% dplyr::summarise(`:=`(!!rlang::sym(value_1L_chr),
                                                                                                         sum(!!rlang::sym(value_1L_chr)))) %>% dplyr::ungroup()
  }
  if (as_dyad_1L_lgl) {
    erp_xx <- ready4use::Ready4useDyad(ds_tb = erp_tb) %>%
      ready4use::add_dictionary(var_ctg_chr = var_ctg_chr)
  }
  else {
    erp_xx <- erp_tb
  }
  return(erp_xx)
}
make_forecast_cost_tb <- function (fabels_ls, unit_cost_1L_dbl, fixed_cost_1L_dbl = 0,
                                   what_1L_chr = "Appointments")
{
  forecast_mean_tb <- make_forecasts_tb(fabels_ls, tfmn_args_ls = list(y = unit_cost_1L_dbl),
                                        tfmn_fn = `*`, tfmn_pattern_1L_chr = "Cost_{.col}", type_1L_chr = "both", what_1L_chr = what_1L_chr)
  forecast_cost_tb <- forecast_mean_tb %>% dplyr::mutate(dplyr::across(dplyr::starts_with("Cost"),
                                                                       ~.x + fixed_cost_1L_dbl, .names = "Total{.col}"))
  forecast_cost_tb <- forecast_cost_tb %>% tidyr::pivot_longer(cols = names(forecast_cost_tb),
                                                               names_to = "Parameter") %>% dplyr::mutate(Statistic = Parameter %>%
                                                                                                           stringr::str_remove("TotalCost_") %>% stringr::str_remove("Cost_") %>%
                                                                                                           stringr::str_replace_all("_", " ") %>% stringr::str_replace_all(".mean",
                                                                                                                                                                           "Mean")) %>% dplyr::mutate(Parameter = dplyr::case_when(startsWith(Parameter,
                                                                                                                                                                                                                                              "Cost") ~ paste0(what_1L_chr, " Cost"), startsWith(Parameter,
                                                                                                                                                                                                                                                                                                 "Total") ~ "Total Cost", T ~ what_1L_chr)) %>% tidyr::pivot_wider(names_from = "Statistic",
                                                                                                                                                                                                                                                                                                                                                                   values_from = "value")
  return(forecast_cost_tb)
}
make_forecast_growth <- function(forecasts_tb,
                                 reference_1L_chr = character(0),
                                 reference_1L_dbl = numeric(0),
                                 tfmn_fn = scales::percent){
  if(!identical(reference_1L_chr, character(0))){
    reference_1L_int <- which(forecasts_tb$Scenario==reference_1L_chr)
    forecasts_tb <- forecasts_tb %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tfmn_fn((.x- dplyr::nth(.x, reference_1L_int))/dplyr::nth(.x, reference_1L_int)))) %>%
      dplyr::filter(Scenario != reference_1L_chr)
  }else{
    forecasts_tb <- forecasts_tb %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~tfmn_fn((.x - reference_1L_dbl)/reference_1L_dbl)))
  }
  return(forecasts_tb)
}
make_forecasts_tb <- function(fabels_ls,
                              group_by_1L_chr = character(0),
                              group_fn = sum,
                              summary_fn = mean,
                              tfmn_args_ls = NULL,
                              tfmn_fn = NULL,
                              tfmn_pattern_1L_chr = "Transformed_{.col}",
                              type_1L_chr = c("default", "grouped", "summary", "both"),
                              what_1L_chr = "Appointments"){
  type_1L_chr <- match.arg(type_1L_chr)
  forecast_tb <- fabels_ls %>% purrr::pluck(what_1L_chr) %>% fabletools::hilo() %>% fabletools::unpack_hilo(c(`80%`, `95%`)) %>%
    tsibble::as_tibble()
  group_by_chr <- c(".model", group_by_1L_chr)
  if(!is.null(tfmn_fn)){
    forecast_tb <- forecast_tb  %>%
      dplyr::mutate(dplyr::across(c(".mean","80%_lower", "80%_upper", "95%_lower", "95%_upper"), ~ rlang::exec(tfmn_fn, .x, !!!tfmn_args_ls), .names = tfmn_pattern_1L_chr))
  }
  if(type_1L_chr %in% c("grouped", "both")){
    forecast_tb <- forecast_tb %>%
      dplyr::group_by(dplyr::across(tidyr::all_of(group_by_chr))) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), group_fn))
  }
  if(type_1L_chr %in% c("summary", "both")){
    forecast_tb <- forecast_tb %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), summary_fn))
  }
  return(forecast_tb)
}
make_medicare_ds <- function(mbs_raw_tb = get_medicare_data(clean_1L_lgl = TRUE),
                             as_dyad_1L_lgl = TRUE,
                             erp_tb = make_erp_ds(as_dyad_1L_lgl = FALSE),
                             age_bands_lup = NULL,
                             age_group_var_1L_chr = "AgeGroup",
                             index_1L_chr = "Quarter",
                             key_vars_chr = c("Sex", "Age"),
                             metrics_chr = c("Patients", "Services"),
                             new_metrics_chr = c("ServicesPerPatient", "PatientsPerPerson", "ServicesPerPerson"),
                             names_from_1L_chr = "Measure",
                             population_var_1L_chr = "Persons",
                             provider_var_1L_chr = "ProviderType",
                             rename_age_to_1L_chr = "Age",
                             rename_provider_to_1L_chr = "ServiceType",
                             scale_1L_int = 1,
                             scaled_naming_1L_chr = "{.col}Scaled",
                             values_to_1L_chr = "Value",
                             years_chr = c("2021-22","2022-23")){
  if(identical(rename_provider_to_1L_chr, character(0))){
    rename_provider_to_1L_chr <- provider_var_1L_chr
  }
  mbs_tb <- mbs_raw_tb %>%
    update_medicare_data(measures_chr = metrics_chr, years_chr = years_chr)
  if(!is.null(age_bands_lup)){
    mbs_tb <- mbs_tb %>%
      transform_age_groups(age_group_var_1L_chr = age_group_var_1L_chr,
                           age_bands_lup = age_bands_lup,
                           index_1L_chr = index_1L_chr,
                           key_vars_chr = c(provider_var_1L_chr, setdiff(key_vars_chr, c(ifelse(identical(rename_age_to_1L_chr, character(0)), age_group_var_1L_chr, rename_age_to_1L_chr), provider_var_1L_chr))),
                           names_from_1L_chr = names_from_1L_chr,
                           rename_to_1L_chr = rename_age_to_1L_chr,
                           values_to_1L_chr = values_to_1L_chr)
  }else{
    mbs_tb <- mbs_tb %>%
      dplyr::rename(!!rlang::sym(rename_provider_to_1L_chr) := !!rlang::sym(provider_var_1L_chr))
    if(!identical(rename_age_to_1L_chr, character(0))){
      mbs_tb %>%
        dplyr::rename(!!rlang::sym(rename_age_to_1L_chr) := !!rlang::sym(age_group_var_1L_chr))
    }
    mbs_tb <- mbs_tb %>%
      tidyr::pivot_wider(names_from = names_from_1L_chr,
                         values_from = values_to_1L_chr)
  }
  mbs_tb <- make_metrics_summary(mbs_tb,
                                 index_1L_chr = index_1L_chr,
                                 key_vars_chr = c(provider_var_1L_chr, key_vars_chr) %>% unique(),
                                 metrics_chr = metrics_chr)
  mbs_tb <- mbs_tb %>%
    dplyr::mutate(!!rlang::sym(rename_provider_to_1L_chr) := dplyr::case_when(!!rlang::sym(provider_var_1L_chr) == "Psychiatrists" ~ "Psychiatry",
                                                                              !!rlang::sym(provider_var_1L_chr) %in% c("Clinical psychologists", "Other psychologists") ~ "Psychology (all)",
                                                                              !!rlang::sym(provider_var_1L_chr) == "All providers" ~ "All services",
                                                                              TRUE ~ "Other services"))
  mbs_tb <- mbs_tb %>%
    dplyr::select(-!!rlang::sym(provider_var_1L_chr))
  mbs_tb <- mbs_tb %>%
    dplyr::filter(!!rlang::sym(rename_provider_to_1L_chr) %in% c("Psychiatry", "Psychology (all)"))
  mbs_tb <- make_metrics_summary(mbs_tb,
                                 index_1L_chr = index_1L_chr,
                                 key_vars_chr = c(rename_provider_to_1L_chr, setdiff(key_vars_chr, c(provider_var_1L_chr, rename_provider_to_1L_chr))),
                                 metrics_chr = metrics_chr)  %>%
    dplyr::mutate(!!rlang::sym(new_metrics_chr[1]) := !!rlang::sym(metrics_chr[2]) / !!rlang::sym(metrics_chr[1]))
  if(!is.null(erp_tb)){
    mbs_tb <- mbs_tb %>%
      dplyr::inner_join(erp_tb)
    mbs_tb <- mbs_tb %>%
      dplyr::mutate(!!rlang::sym(new_metrics_chr[2]) := !!rlang::sym(metrics_chr[1]) / !!rlang::sym(population_var_1L_chr),
                    !!rlang::sym(new_metrics_chr[3]) := !!rlang::sym(metrics_chr[2]) / !!rlang::sym(population_var_1L_chr))
    if(scale_1L_int != 1){
      mbs_tb <- mbs_tb %>%
        dplyr::mutate(dplyr::across(new_metrics_chr[2:3], ~ .x * scale_1L_int, .names = scaled_naming_1L_chr))
    }

  }
  if(as_dyad_1L_lgl){
    keys_1L_int <- c(rename_provider_to_1L_chr, key_vars_chr) %>% unique() %>% length()
    var_ctg_chr <- c("Temporal", rep("Key", times = keys_1L_int), rep("Metric", times = length(names(mbs_tb))-keys_1L_int-1))
    mbs_xx <- ready4use::Ready4useDyad(ds_tb = mbs_tb) %>% ready4use::add_dictionary(var_ctg_chr = var_ctg_chr)
  } else{
    mbs_xx <- mbs_tb
  }
  return(mbs_xx)
}
make_metric_vars <- function(type_1L_chr = c("main", "all","eoc"),
                             appointments_var_1L_chr = "Appointments",
                             cancellations_var_1L_chr = "Cancellations",
                             cost_var_1L_chr = "Cost",
                             referrals_var_1L_chr = "Referrals",
                             separation_after_dbl = numeric(0)){
  type_1L_chr <- match.arg(type_1L_chr)
  episodes_vars_chr <- make_episodes_vars(separation_after_dbl = separation_after_dbl)
  if(type_1L_chr!="eoc"){
    metric_vars_chr <- c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr)
  }else{
    metric_vars_chr <- character(0)
  }
  if(type_1L_chr!="main"){
    metric_vars_chr <- c(episodes_vars_chr, metric_vars_chr)
  }
  return(metric_vars_chr)
}
make_metrics_summary <- function(data_tb,
                                 index_1L_chr,
                                 key_vars_chr,
                                 metrics_chr){
  data_tb <- dplyr::arrange(data_tb, !!rlang::sym(index_1L_chr))
  summary_tb <- dplyr::select(data_tb, tidyselect::all_of(c(index_1L_chr,
                                                            metrics_chr,
                                                            key_vars_chr))) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(index_1L_chr,
                                                       key_vars_chr)))) %>%
    dplyr::summarise(dplyr::across(tidyselect::all_of(metrics_chr), ~sum(.x, na.rm = T)), .groups = 'drop')
  return(summary_tb)
}
make_new_cases_ls <- function(cases_ls){
  new_cases_ls <- 1:length(cases_ls) %>%
    purrr::map(~{
      if(.x==1){
        exclude_chr <- character(0)
      }else{
        exclude_chr <- cases_ls[1:(.x-1)] %>% purrr::flatten_chr() %>% unique()
      }
      setdiff(cases_ls %>% purrr::pluck(.x), exclude_chr)
    })
  return(new_cases_ls)
}
make_new_correspondences <- function(data_tb = NULL,
                                     key_1L_chr = character(0),
                                     min_1L_int = 3L,
                                     original_xx = character(0)){
  if(identical(original_xx, character(0))){
    original_xx <- data_tb %>% dplyr::pull(!!rlang::sym(key_1L_chr)) %>%
      unique() %>% sort()
  }
  if(is.character(original_xx)){
    abbreviations_chr <- original_xx %>% stringr::str_trim() %>% abbreviate(minlength = min_1L_int)
  }else{
    original_xx <- original_xx %>% as.character()
    abbreviations_chr <- original_xx
  }
  x_ready4show_correspondences <- ready4show::ready4show_correspondences() %>%
    ready4show::renew.ready4show_correspondences(old_nms_chr = original_xx,
                                                 new_nms_chr = unname(abbreviations_chr))
  return(x_ready4show_correspondences)
}
make_rebate_cap_params <- function(data_tb,
                                   scenarios_xx,
                                   appointments_1L_chr = "Appointments",
                                   appointments_cdn_1L_chr = 'Activity == "Appointment"',
                                   cancellations_cdn_1L_chr = 'Activity == "Cancellation"',
                                   uids_xx = NULL){
  appointments_tb <- data_tb %>% dplyr::filter(eval(parse(text = appointments_cdn_1L_chr)))
  cancellations_tb <- data_tb %>% dplyr::filter(eval(parse(text = cancellations_cdn_1L_chr)))
  appointment_cost_1L_dbl <- appointments_tb %>% dplyr::summarise(Cost = mean(Cost)) %>% dplyr::pull(Cost)
  max_cost_1L_dbl <- appointments_tb %>% dplyr::summarise(Cost = max(Cost)) %>% dplyr::pull(Cost)
  cancellation_cost_1L_dbl <- cancellations_tb %>% dplyr::summarise(Cost = mean(Cost)) %>% dplyr::pull(Cost)
  scenario_costs_dbl <- scenarios_xx %>%
    purrr::map_dbl(~{
      max_1L_dbl <-.x
      (appointments_tb %>% dplyr::mutate(Cost = Cost %>% purrr::map_dbl(~min(.x,max_1L_dbl))) %>%
          dplyr::summarise(Cost = mean(Cost)) %>% dplyr::pull(Cost))
    })
  ratios_dbl <- scenario_costs_dbl/appointment_cost_1L_dbl
  mean_copays_dbl <- appointment_cost_1L_dbl - scenario_costs_dbl
  max_copays_dbl <- max_cost_1L_dbl - scenario_costs_dbl
  cancellation_copays_dbl <- (1-ratios_dbl) * cancellation_cost_1L_dbl
  params_ls <- list(appointment_cost_1L_dbl  = appointment_cost_1L_dbl,
                    appointments_ratio_dbl = rep(1, length(scenarios_xx)),
                    cancellation_cost_1L_dbl = cancellation_cost_1L_dbl,
                    cancellation_copays_dbl = cancellation_copays_dbl,
                    max_cost_1L_dbl = max_cost_1L_dbl,
                    max_copays_dbl= max_copays_dbl,
                    mean_copays_dbl = mean_copays_dbl,

                    scenario_costs_dbl = scenario_costs_dbl,
                    ratios_dbl = ratios_dbl)
  return(params_ls)
}
make_retainers <- function (retainers_tb, as_tsibble_1L_lgl = FALSE, censor_1L_lgl = FALSE,
                            cumulatives_1L_lgl = FALSE, cost_var_1L_chr = "Retainer amount",
                            data_tb = NULL, date_var_1L_chr = "Retainer date", default_1L_dbl = numeric(0),
                            dyad_1L_lgl = FALSE, end_date_dtm = NULL, fill_gaps_1L_lgl = FALSE,
                            fiscal_start_1L_int = 7L, index_1L_chr = "Date", offset_1L_int = integer(0),
                            price_indices_dbl = numeric(0),
                            price_ref_1L_int = 1L,
                            provider_id_1L_chr = "ProviderID",
                            # reset_new_1L_lgl = TRUE,
                            start_date_dtm = NULL,      time_var_1L_chr = "FiscalYear", unit_1L_chr = "days"){
  if (is.null(end_date_dtm)) {
    end_date_dtm <- max(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)))
  }
  if (is.null(start_date_dtm)) {
    start_date_dtm <- min(min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))),
                          ifelse(!is.null(data_tb), min(data_tb %>% dplyr::pull(!!rlang::sym(index_1L_chr))) %>%
                                   as.POSIXct(), Inf))
  }
  if (!is.null(start_date_dtm)) {
    retainers_tb <- retainers_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >=  start_date_dtm)
    if (!is.null(data_tb)) {
      data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(index_1L_chr) >= start_date_dtm)
    }
  }
  if (!is.null(data_tb)) {
    itemised_chr <- retainers_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>%
      unique() %>% purrr::discard(is.na)
    active_chr <- data_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>%
      unique() %>% purrr::discard(is.na)
    intersecting_chr <- intersect(itemised_chr, active_chr)
    if (identical(default_1L_dbl, numeric(0))) {
      default_1L_dbl <- mean(retainers_tb %>% dplyr::pull(!!rlang::sym(cost_var_1L_chr)),
                             na.rm = TRUE)
    }
    filtered_tb <- retainers_tb %>% dplyr::filter(!!rlang::sym(provider_id_1L_chr) %in% active_chr) %>%
      dplyr::left_join(data_tb %>% dplyr::select(tidyr::all_of(c(provider_id_1L_chr, index_1L_chr))) %>%
                         dplyr::rename(TESTDATE = !!rlang::sym(index_1L_chr)) %>%
                         dplyr::group_by(!!rlang::sym(provider_id_1L_chr)) %>%
                         dplyr::summarise(TESTDATE = min(TESTDATE))) %>%
      dplyr::filter(!!rlang::sym(date_var_1L_chr) <= TESTDATE) %>%
      dplyr::select(-TESTDATE)
    filtered_chr <- filtered_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>%
      unique() %>% purrr::discard(is.na)
    additional_chr <- setdiff(active_chr, filtered_chr)
    if (!identical(additional_chr, character(0))) {
      if (!is.null(start_date_dtm) & identical(offset_1L_int, integer(0))) {
        due_date_dtm <- start_date_dtm
      } else {
        due_date_dtm <- min(filtered_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))) +
          lubridate::duration(offset_1L_int, units = unit_1L_chr)
      }
      preexisting_tb <-  data_tb %>%
        dplyr::filter(!!rlang::sym(provider_id_1L_chr) %in% additional_chr) %>%
        dplyr::group_by(!!rlang::sym(provider_id_1L_chr)) %>%
        dplyr::select(!!rlang::sym(index_1L_chr)) %>%
        dplyr::summarise(!!rlang::sym(index_1L_chr) := min(!!rlang::sym(index_1L_chr)))
      preexisting_tb <- preexisting_tb %>% dplyr::left_join(retainers_tb %>% dplyr::select(tidyr::all_of(c(provider_id_1L_chr, cost_var_1L_chr)))) %>%
        dplyr::mutate(!!rlang::sym(cost_var_1L_chr) := dplyr::case_when(is.na(!!rlang::sym(cost_var_1L_chr)) ~ default_1L_dbl,
                                                                        T ~ !!rlang::sym(cost_var_1L_chr))) %>%
        dplyr::rename(!!rlang::sym(date_var_1L_chr) := !!rlang::sym(index_1L_chr))
      filtered_tb <-  filtered_tb %>% dplyr::bind_rows(preexisting_tb) %>% dplyr::arrange(!!rlang::sym(date_var_1L_chr))
    }
  }else{
    filtered_tb <- retainers_tb
  }
  retainers_tsb <- update_retainers_ds(filtered_tb, cost_var_1L_chr = cost_var_1L_chr,
                                       date_var_1L_chr = date_var_1L_chr, end_date_dtm = end_date_dtm,
                                       price_indices_dbl = price_indices_dbl,
                                       price_ref_1L_int = price_ref_1L_int,
                                       time_var_1L_chr = time_var_1L_chr) %>%
    transform_to_tsibble(index_1L_chr = index_1L_chr, metrics_chr = c("Clinicians",
                                                                      "Retainer")) %>% dplyr::select(!!rlang::sym(index_1L_chr),
                                                                                                     Clinicians, Retainer)
  if (start_date_dtm < min(retainers_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)))) {
    lubridate::interval(start_date_dtm, min(retainers_tsb %>%
                                              dplyr::pull(!!rlang::sym(index_1L_chr)))) %>% lubridate::as.duration()
    steps_1L_int <- lubridate::interval(start_date_dtm, min(retainers_tsb %>%
                                                              dplyr::pull(!!rlang::sym(index_1L_chr)))) %>% lubridate::as.duration() %>%
      lubridate::duration() %>% as.numeric(unit_1L_chr)
    retainers_tsb <- tsibble::append_row(retainers_tsb, n = -steps_1L_int) %>%
      dplyr::mutate(dplyr::across(c("Clinicians", "Retainer"),
                                  ~dplyr::case_when(is.na(.x) ~ 0, T ~ .x)))
  }
  if (fill_gaps_1L_lgl) {
    retainers_tsb <- retainers_tsb %>% tsibble::fill_gaps(Clinicians = 0,
                                                          Retainer = 0)
  }
  retainers_tsb <- add_temporal_vars(retainers_tsb, date_var_1L_chr = index_1L_chr,
                                     fiscal_start_1L_int = fiscal_start_1L_int)
  if (cumulatives_1L_lgl) {
    retainers_tsb <- retainers_tsb %>% dplyr::mutate(CumulativeRetainer = cumsum(Retainer),
                                                     CumulativeClinicians = cumsum(Clinicians))
    retainers_tsb <- retainers_tsb %>% dplyr::select(!!rlang::sym(index_1L_chr),
                                                     Clinicians, Retainer, CumulativeClinicians, CumulativeRetainer,
                                                     dplyr::everything())
  }
  if (censor_1L_lgl)
    retainers_tsb <- retainers_tsb %>% dplyr::filter(!!rlang::sym(index_1L_chr) >= min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))))
  if (!as_tsibble_1L_lgl) {
    data_xx <- retainers_tsb %>% tsibble::as_tibble()
  }  else {
    data_xx <- retainers_tsb
  }
  if (dyad_1L_lgl) {
    data_xx <- ready4use::Ready4useDyad(ds_tb = data_xx) %>%
      ready4use::add_dictionary(var_ctg_chr = "Temporal")
    data_xx@dictionary_r3 <- data_xx@dictionary_r3 %>% dplyr::mutate(var_ctg_chr = dplyr::case_when(var_nm_chr %in%
                                                                                                      c("Clinicians", "Retainer", "CumulativeRetainer",
                                                                                                        "CumulativeClinicians") ~ "Metrics", TRUE ~ var_ctg_chr))
  }
  return(data_xx)
}
make_roll_backs_ls <- function(data_xx,
                               groupings_chr,
                               exclude_chr = character(0),
                               minimum_1L_int = 10L){
  if(inherits(data_xx,"Ready4useDyad")){
    X_Ready4useDyad <- data_xx
  }else{
    X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
  }
  X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::ungroup()
  missing_int <- sapply(X_Ready4useDyad@ds_tb, function(x) sum(is.na(x)))  %>% purrr::discard(.<1)
  missing_chr <- setdiff(names(missing_int), exclude_chr)
  intersected_chr <- intersect(groupings_chr, missing_chr)
  roll_backs_ls <- list(main_lup = make_roll_back_lup(X_Ready4useDyad,
                                                      groupings_chr = groupings_chr,
                                                      values_chr = setdiff(missing_chr, intersected_chr),
                                                      minimum_1L_int = minimum_1L_int))
  if(!identical(intersected_chr, character(0))){
    roll_backs_ls <- append(roll_backs_ls, intersected_chr %>%
                              purrr::map(~{
                                make_roll_back_lup(X_Ready4useDyad,
                                                   groupings_chr = setdiff(groupings_chr, .x),
                                                   values_chr = .x,
                                                   minimum_1L_int = minimum_1L_int)
                              }) %>% stats::setNames(intersected_chr))
  }
  return(roll_backs_ls)
}
make_roll_back_lup <- function(data_xx,
                               groupings_chr,
                               values_chr,
                               minimum_1L_int = 10L){
  if(inherits(data_xx,"Ready4useDyad")){
    X_Ready4useDyad <- data_xx
  }else{
    X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
  }
  X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::ungroup()

  all_tb <- X_Ready4useDyad@ds_tb %>%
    tidyr::expand(tidyr::nesting(!!!rlang::syms(groupings_chr))) %>%
    tidyr::drop_na()
  all_tb <- length(groupings_chr):1 %>%
    purrr::map_dfr(~all_tb %>%
                     dplyr::select(groupings_chr[1:.x]) %>% dplyr::distinct())
  roll_back_ls <- values_chr %>%
    purrr::map( ~ {
      var_1L_chr <- .x
      one_var_lup <- length(groupings_chr):0 %>%
        purrr::reduce(.init = dplyr::mutate(all_tb, !!rlang::sym(var_1L_chr) := list(NULL)),
                      ~{
                        complete_tb <- .x %>%
                          dplyr::filter(!!rlang::sym(var_1L_chr) %>% purrr::map_lgl(~!is.null(.x)))
                        if(nrow(complete_tb) < nrow(all_tb)){
                          if(.y>0){
                            partial_tb <- .x  %>%
                              dplyr::filter(!!rlang::sym(var_1L_chr) %>% purrr::map_lgl(~is.null(.x))) %>%
                              dplyr::select(!!!rlang::syms(groupings_chr[1:.y]))
                          }
                          tfmn_fn <- ifelse(is.numeric(X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(var_1L_chr))),
                                            as.numeric,
                                            ifelse(is.character(X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(var_1L_chr))),
                                                   as.character,
                                                   ifelse(is.logical(X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(var_1L_chr))),
                                                          as.logical,
                                                          identity)))
                          if(.y>0){
                            matched_tb <- X_Ready4useDyad@ds_tb %>%
                              tidyr::drop_na(!!!rlang::syms(c(groupings_chr[1:.y], var_1L_chr))) %>%
                              dplyr::group_by(!!!rlang::syms(groupings_chr[1:.y])) %>%
                              dplyr::summarise(!!rlang::sym(var_1L_chr) := list(table(!!rlang::sym(var_1L_chr)) %>% as.data.frame() %>% tibble::as_tibble() %>%
                                                                                  dplyr::mutate(!!rlang::sym(var_1L_chr) := tfmn_fn(!!rlang::sym(var_1L_chr))) %>%
                                                                                  dplyr::rename(Value = !!rlang::sym(var_1L_chr), Frequency = Freq))) %>%
                              dplyr::ungroup() %>%
                              dplyr::filter(!!rlang::sym(var_1L_chr) %>% purrr::map_lgl(~sum(.x$Frequency)>=minimum_1L_int))
                            complete_tb <- dplyr::bind_rows(complete_tb,
                                                            partial_tb %>% dplyr::right_join(matched_tb))
                          }else{
                            complete_tb <- dplyr::bind_rows(complete_tb, X_Ready4useDyad@ds_tb %>%
                                                              dplyr::summarise(dplyr::across(tidyselect::all_of(groupings_chr), ~NA),
                                                                               !!rlang::sym(var_1L_chr) := list(table(!!rlang::sym(var_1L_chr)) %>% as.data.frame() %>% tibble::as_tibble() %>%
                                                                                                                  dplyr::mutate(!!rlang::sym(var_1L_chr) := tfmn_fn(!!rlang::sym(var_1L_chr))) %>%
                                                                                                                  dplyr::rename(Value = !!rlang::sym(var_1L_chr), Frequency = Freq))))
                          }

                        }
                        complete_tb
                      })
      # all_missing_tb <- purrr::reduce(groupings_chr, .init = X_Ready4useDyad@ds_tb, ~dplyr::filter(.x, is.na(!!rlang::sym(.y)))) %>%
      #   dplyr::select(tidyselect::all_of(c(groupings_chr, var_1L_chr)))
      # if(nrow(all_missing_tb)>0){
      # }
    })

  roll_back_lup <- roll_back_ls %>%
    purrr::reduce(~dplyr::full_join(.x,.y))
  return(roll_back_lup)
}
make_sampled_values <- function(roll_back_xx,
                                draws_int,
                                fail_with_xx = NULL,
                                filter_cdn_ls = NULL,
                                variable_1L_chr = character(0)
){
  if(!is.null(filter_cdn_ls)){
    sampled_xx <- purrr::reduce(1:length(filter_cdn_ls),
                                .init = roll_back_xx,
                                ~ {
                                  value_1L_xx <- filter_cdn_ls[[.y]]

                                  .x %>%
                                    dplyr::filter(!!rlang::sym(names(filter_cdn_ls)[.y]) %>% purrr::map_lgl(~identical(.x,value_1L_xx)))
                                }
    ) %>%
      dplyr::pull(!!rlang::sym(variable_1L_chr)) %>% purrr::pluck(1)
  }else{
    sampled_xx <- roll_back_xx
  }
  if(!is.null(sampled_xx)){
    sampled_xx <- sampled_xx %>% purrr::pmap(~rep(..1,..2)) %>% purrr::flatten() %>% unlist() %>%
      sample(size = draws_int, replace = TRUE)
  }else{
    sampled_xx <- fail_with_xx
  }
  return(sampled_xx)
}
make_sampling_lup <- function(shares_dbl,
                              values_xx,
                              var_nm_1L_chr,
                              prefix_1L_chr = "Client_",
                              scale_1L_dbl = 100,
                              uid_var_nm_1L_chr = "Client ID"){
  sampling_lup <- values_xx %>%
    purrr::map2_dfr(shares_dbl,
                    ~ tibble::tibble(!!rlang::sym(var_nm_1L_chr) := rep(.x, scale_1L_dbl*.y)))
  sampling_lup <- youthvars::add_uids_to_tbs_ls(list(sampling_lup), prefix_1L_chr = prefix_1L_chr, id_var_nm_1L_chr = uid_var_nm_1L_chr) %>% purrr::pluck(1) %>%
    dplyr::select(!!rlang::sym(uid_var_nm_1L_chr), dplyr::everything())
  return(sampling_lup)
}
make_scaled_forecasts <- function(forecasts_tb,
                                  predictors_tb,
                                  after_1L_chr = character(0),
                                  before_1L_chr = character(0),
                                  bind_1L_lgl = TRUE,
                                  positive_1L_lgl = FALSE,
                                  prefix_1L_chr = "scenario_",
                                  reference_1L_chr = "Status quo",
                                  scale_fn = function(x) x * 1,
                                  scaled_fn_ls = NULL,
                                  tfmn_1_fn = identity,
                                  tfmn_2_fn = identity){
  original_tb <- forecasts_tb
  if(is.null(scaled_fn_ls)){
    forecasts_tb <- dplyr::bind_rows(forecasts_tb %>% dplyr::filter(Scenario == reference_1L_chr),
                                     make_forecast_growth(predictors_tb, reference_1L_chr = reference_1L_chr, tfmn_fn = scale_fn) %>%
                                       update_scenario_names(after_1L_chr = after_1L_chr, before_1L_chr = before_1L_chr, prefix_1L_chr = prefix_1L_chr,
                                                             reference_1L_chr = reference_1L_chr, tfmn_1_fn = tfmn_1_fn, tfmn_2_fn = tfmn_2_fn))
    reference_1L_int <- which(forecasts_tb$Scenario==reference_1L_chr)
    forecasts_tb <- forecasts_tb %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ dplyr::case_when(dplyr::row_number() != reference_1L_int ~ dplyr::nth(.x,reference_1L_int) + .x * dplyr::nth(.x,reference_1L_int), T ~ .x)))

    if (positive_1L_lgl) {
      forecasts_tb <- forecasts_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                                   ~.x %>% purrr::map_dbl(~max(.x, 0))))
    }
  }else{
    forecasts_tb <- 1:length(scaled_fn_ls) %>% purrr::reduce(.init = forecasts_tb,
                                                             ~ make_scaled_forecasts(forecasts_tb = .x,
                                                                                     predictors_tb = predictors_tb,
                                                                                     after_1L_chr = paste0(", ", names(scaled_fn_ls)[.y]),
                                                                                     before_1L_chr = before_1L_chr,
                                                                                     bind_1L_lgl = TRUE,
                                                                                     positive_1L_lgl = positive_1L_lgl,
                                                                                     prefix_1L_chr = prefix_1L_chr,
                                                                                     reference_1L_chr = reference_1L_chr,
                                                                                     scale_fn = scaled_fn_ls[[.y]],
                                                                                     scaled_fn_ls = NULL,
                                                                                     tfmn_1_fn = tfmn_1_fn,
                                                                                     tfmn_2_fn = tfmn_2_fn) %>% dplyr::distinct())
  }

  if(bind_1L_lgl){
    forecasts_tb <- dplyr::bind_rows(original_tb,
                                     forecasts_tb %>% dplyr::filter(Scenario != reference_1L_chr)) %>% dplyr::distinct()
  }
  return(forecasts_tb)

}
make_scenario_forecast_costs <- function(scenario_forecasts_ls,
                                         unit_costs_tb,
                                         what_1L_chr,
                                         fixed_cost_1L_dbl = 0,
                                         positive_1L_lgl = FALSE,
                                         predictors_chr = character(0),
                                         type_1L_chr = c("Variable", "Fixed")){
  type_1L_chr <- match.arg(type_1L_chr)
  forecast_costs_tb <- names(scenario_forecasts_ls) %>% purrr::map_dfr(~{
    name_1L_chr <- .x
    unique(unit_costs_tb$Scenario) %>%
      purrr::map_dfr(~{
        costs_tb <- make_forecast_cost_tb(scenario_forecasts_ls %>% purrr::pluck(name_1L_chr) %>% purrr::pluck("fabels_ls"),
                                          fixed_cost_1L_dbl = fixed_cost_1L_dbl,
                                          unit_cost_1L_dbl = ready4::get_from_lup_obj(unit_costs_tb %>% dplyr::filter(Scenario==.x),
                                                                                      match_value_xx = type_1L_chr,
                                                                                      match_var_nm_1L_chr = "Type",
                                                                                      target_var_nm_1L_chr = "UnitCost"),
                                          what_1L_chr = what_1L_chr) %>%
          dplyr::select(-tidyselect::any_of(c(predictors_chr,"0")))
        if(fixed_cost_1L_dbl==0){
          costs_tb <- costs_tb  %>% dplyr::filter(Parameter != "Total Cost")
        }
        costs_tb %>%
          dplyr::mutate(Scenario = paste0(.x,"_",name_1L_chr)) %>%
          dplyr::select(Scenario, dplyr::everything())
      }

      )
  }) %>%
    dplyr::arrange(Scenario, Parameter)
  if(positive_1L_lgl){
    forecast_costs_tb <- forecast_costs_tb %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ .x %>% purrr::map_dbl(~max(.x,0))))
  }
  return(forecast_costs_tb)
}
make_scenario_forecasts <- function (scenario_forecasts_ls, what_1L_chr,
                                     bind_to_tb = NULL,
                                     date_end_dtm = NULL,
                                     date_start_dtm = NULL, date_var_1L_chr = "Day", group_by_1L_chr = character(0),
                                     group_fn = sum, positive_1L_lgl = FALSE, summarise_1L_lgl = FALSE,
                                     summary_fn = mean, summary_2_fn = sum, tfmn_args_ls = NULL,
                                     tfmn_fn = NULL, tfmn_pattern_1L_chr = "Transformed_{.col}",
                                     type_1L_chr = c("default", "grouped", "summary", "both"),
                                     predictors_chr = character(0))
{
  forecasts_tb <- names(scenario_forecasts_ls) %>% purrr::map_dfr(~{
    name_1L_chr <- .x
    make_forecasts_tb(scenario_forecasts_ls %>% purrr::pluck(name_1L_chr) %>%
                        purrr::pluck("fabels_ls"), group_by_1L_chr = group_by_1L_chr,
                      group_fn = group_fn, summary_fn = summary_fn, tfmn_args_ls = tfmn_args_ls,
                      tfmn_fn = tfmn_fn, tfmn_pattern_1L_chr = tfmn_pattern_1L_chr,
                      type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr) %>%
      dplyr::select(-tidyselect::any_of(c(predictors_chr,
                                          "0"))) %>% dplyr::rename(Distribution = !!rlang::sym(what_1L_chr),
                                                                   `:=`(!!rlang::sym(what_1L_chr), .mean)) %>% dplyr::mutate(Scenario = name_1L_chr) %>%
      dplyr::relocate(Scenario, .after = .model)
  })
  if (summarise_1L_lgl) {
    forecasts_tb <- forecasts_tb %>% dplyr::rename(.mean = what_1L_chr) %>%
      dplyr::select(-c("Distribution"))
    if((!is.null(date_start_dtm) | !is.null(date_end_dtm)) & !date_var_1L_chr %in% names(forecasts_tb)){
      forecasts_tb <- forecasts_tb %>% add_temporal_vars(temporal_vars_chr = date_var_1L_chr,
                                                         date_var_1L_chr = c(intersect(make_temporal_vars(), names(forecasts_tb)), "Date")[1])
    }
    if (!is.null(date_start_dtm)) {
      forecasts_tb <- forecasts_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >=
                                                       date_start_dtm)
    }
    if (!is.null(date_end_dtm)) {
      forecasts_tb <- forecasts_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) <=
                                                       date_end_dtm)
    }
    forecasts_tb <- forecasts_tb %>% dplyr::group_by(Scenario, .model)
    if (positive_1L_lgl) {
      forecasts_tb <- forecasts_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                                   ~.x %>% purrr::map_dbl(~max(.x, 0))))
    }
    forecasts_tb <- forecasts_tb %>% dplyr::summarise(dplyr::across(dplyr::where(is.numeric),
                                                                    ~summary_2_fn(.x)))
  }
  if(length(unique(forecasts_tb$.model))==1){
    forecasts_tb <- forecasts_tb %>% dplyr::select(-c(.model))
  }
  if(length(unique(forecasts_tb$Scenario))==1){
    forecasts_tb <- forecasts_tb %>% dplyr::select(-c(Scenario))
  }
  forecasts_tb <- forecasts_tb %>% dplyr::relocate(`95%_lower`, .before = `80%_lower`)
  if(!is.null(bind_to_tb)){
    forecasts_tb <- dplyr::bind_rows(bind_to_tb, forecasts_tb)
  }
  return(forecasts_tb)
}
make_scenarios_tb <- function(base_case_tb,
                              data_tb,
                              scenarios_xx,
                              add_cumulatives_1L_lgl = F,
                              add_fn = NULL,
                              data_filter_1L_chr = character(0),
                              extra_costs_1L_chr = character(0),
                              group_by_1L_chr = "UID",
                              new_filter_1L_chr = character(0),
                              prefix_1L_chr = "",
                              suffix_1L_chr = "",
                              uids_xx = NULL,
                              unit_1L_chr = "Appointments"){
  if(!identical(data_filter_1L_chr, character(0))){
    data_tb <- data_tb %>% dplyr::filter(eval(parse(text = data_filter_1L_chr)))
  }
  if(add_cumulatives_1L_lgl){
    data_tb <- add_cumulatives(data_tb, metrics_chr = unit_1L_chr, group_by_1L_chr = group_by_1L_chr)
  }
  scenarios_tb <- purrr::map_dfr(scenarios_xx,
                                 ~ {
                                   if(!identical(new_filter_1L_chr, character(0))){
                                     filter_cdn_1L_chr <- stringr::str_replace_all(new_filter_1L_chr, "\\.x",
                                                                                   ifelse(is.character(.x),paste0('"',.x,'"'),as.character(.x)))
                                     new_tb <- data_tb %>% dplyr::filter(eval(parse(text = filter_cdn_1L_chr)))

                                   }else{
                                     filter_cdn_1L_chr <- character(0)
                                     new_tb <- data_tb
                                   }
                                   partial_tb <- base_case_tb %>% dplyr::mutate(Scenario = paste0(prefix_1L_chr, .x, suffix_1L_chr))
                                   if(is.null(add_fn)){
                                     partial_tb <- partial_tb %>% dplyr::mutate(!!rlang::sym(unit_1L_chr) := nrow(new_tb)/nrow(data_tb) * !!rlang::sym(unit_1L_chr)) %>%
                                       add_scenario_activity_cost(base_case_tb = base_case_tb,
                                                                  unit_1L_chr = unit_1L_chr)
                                   }else{
                                     args_ls <- list(base_case_tb = base_case_tb, data_tb = data_tb, filter_cdn_1L_chr = filter_cdn_1L_chr, scenario_1L_xx = .x, scenarios_xx = scenarios_xx, uids_xx = uids_xx, unit_1L_chr = unit_1L_chr)
                                     partial_tb <- rlang::exec(add_fn, partial_tb, !!!args_ls)
                                   }
                                   if(!identical(extra_costs_1L_chr, character(0))){
                                     partial_tb <-  partial_tb %>% dplyr::mutate(`Total Cost` = !!rlang::sym(paste0(unit_1L_chr, " Cost")) + !!rlang::sym(extra_costs_1L_chr))
                                   }
                                   partial_tb
                                 })
  scenarios_tb <- scenarios_tb %>%
    dplyr::select(c(setdiff(names(scenarios_tb), "Total Cost"), "Total Cost"))
  return(scenarios_tb)
}
make_service_summary <- function(data_xx,
                                 active_base_1L_chr = "Active",
                                 max_periods_1L_int = integer(0),
                                 metrics_chr = character(0),
                                 metric_var_1L_chr = "Appointments",
                                 missing_val_1L_dbl = 0,
                                 patterns_ls = NULL,
                                 period_ctg_1L_chr = "Temporal",
                                 period_var_1L_chr = "Period",
                                 prefix_1L_chr = "",
                                 service_var_1L_chr = "Service",
                                 summary_fn = sum,
                                 tenure_var_1L_chr = "Tenure",
                                 uid_1L_chr = "UID",
                                 update_desc_1L_lgl = TRUE,
                                 var_ctg_chr = "Summary"){
  X_Ready4useDyad <- transform_data_fmt(data_xx,
                                        type_1L_chr = "input")
  X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(c(uid_1L_chr, service_var_1L_chr, metric_var_1L_chr)) %>%
    tidyr::pivot_wider(names_from = service_var_1L_chr, values_from = metric_var_1L_chr, values_fn = summary_fn, values_fill = missing_val_1L_dbl, names_prefix = prefix_1L_chr)
  X_Ready4useDyad <- X_Ready4useDyad %>%
    ready4use::add_dictionary(var_ctg_chr = var_ctg_chr)
  if(!is.null(patterns_ls)){
    X_Ready4useDyad <- ready4use::update_column_names(X_Ready4useDyad, patterns_ls = patterns_ls, update_desc_1L_lgl = update_desc_1L_lgl)
  }
  if(!identical(metrics_chr, character(0))){
    Y_Ready4useDyad <- transform_data_fmt(data_xx,
                                          type_1L_chr = "input")
    metrics_chr <- intersect(metrics_chr %>% purrr::discard(~startsWith(., active_base_1L_chr)), names(Y_Ready4useDyad@ds_tb))
    Y_Ready4useDyad <- Z_Ready4useDyad <- add_period(Y_Ready4useDyad,
                                                     period_ctg_1L_chr = period_ctg_1L_chr,
                                                     period_var_1L_chr = period_var_1L_chr,
                                                     tenure_var_1L_chr = tenure_var_1L_chr)
    Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad,"ds_tb",
                                 Y_Ready4useDyad@ds_tb %>%
                                   dplyr::group_by(dplyr::pick(tidyselect::all_of(c(uid_1L_chr, period_var_1L_chr)))) %>%
                                   dplyr::summarise(dplyr::across(tidyselect::any_of(metrics_chr), sum)) %>%
                                   dplyr::ungroup()
    )
    periods_xx <- Z_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(period_var_1L_chr)) %>% unique() %>% sort()
    dyads_ls <- periods_xx %>% purrr::map(~ {
      A_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb",
                                   Y_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(period_var_1L_chr) == .x) %>%
                                     dplyr::select(-tidyselect::all_of(period_var_1L_chr)))
      A_Ready4useDyad <- renewSlot(A_Ready4useDyad, "dictionary_r3", A_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% names(A_Ready4useDyad@ds_tb)))
      old_chr <- setdiff(names(A_Ready4useDyad@ds_tb), uid_1L_chr)
      new_chr <- paste0(period_var_1L_chr,.x, old_chr)
      A_Ready4useDyad <- renewSlot(A_Ready4useDyad, "ds_tb",
                                   1:length(old_chr) %>% purrr::reduce(.init = A_Ready4useDyad@ds_tb,
                                                                       ~ dplyr::rename(.x, !!rlang::sym(new_chr[.y]) := old_chr[.y]))) %>%
        renewSlot("dictionary_r3", 1:length(old_chr) %>% purrr::reduce(.init = A_Ready4useDyad@dictionary_r3,
                                                                       ~ dplyr::mutate(.x, var_nm_chr = var_nm_chr %>% stringr::str_replace_all(old_chr[.y], new_chr[.y]),
                                                                                       var_desc_chr = var_desc_chr %>% stringr::str_replace_all(old_chr[.y], new_chr[.y]))))
      B_Ready4useDyad <- make_service_summary(renewSlot(Z_Ready4useDyad,"ds_tb",
                                                        Z_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(period_var_1L_chr) == .x)),
                                              active_base_1L_chr = active_base_1L_chr,
                                              metric_var_1L_chr = metric_var_1L_chr, metrics_chr = character(0),
                                              missing_val_1L_dbl = missing_val_1L_dbl, patterns_ls = patterns_ls,
                                              period_ctg_1L_chr = period_ctg_1L_chr,
                                              period_var_1L_chr = period_var_1L_chr, prefix_1L_chr = paste0(period_var_1L_chr, .x),
                                              service_var_1L_chr = service_var_1L_chr, summary_fn = summary_fn,
                                              tenure_var_1L_chr = tenure_var_1L_chr, uid_1L_chr = uid_1L_chr,
                                              update_desc_1L_lgl = update_desc_1L_lgl, var_ctg_chr = var_ctg_chr)
      renewSlot(A_Ready4useDyad, "ds_tb",
                dplyr::inner_join(A_Ready4useDyad@ds_tb,
                                  B_Ready4useDyad@ds_tb) %>%
                  dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::coalesce(.x, 0)))) %>%
        ready4use::add_dictionary(new_cases_r3 = B_Ready4useDyad@dictionary_r3 %>% dplyr::filter(!var_nm_chr %in% A_Ready4useDyad@dictionary_r3$var_nm_chr))

    } ) %>%
      stats::setNames(paste0(period_var_1L_chr, periods_xx))
    if(!identical(max_periods_1L_int, integer(0))){
      dyads_ls <- dyads_ls %>% purrr::keep_at(1:min(max(periods_xx), max_periods_1L_int))
    }
    X_Ready4useDyad <- purrr::reduce(dyads_ls,
                                     .init = X_Ready4useDyad,
                                     ~
                                       renewSlot(.x, "ds_tb",
                                                 dplyr::left_join(.x@ds_tb,.y@ds_tb) %>%
                                                   dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::coalesce(.x, 0)))) %>%
                                       ready4use::add_dictionary(new_cases_r3 = .y@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% names(.y@ds_tb),
                                                                                                                   !var_nm_chr %in% names(.x@ds_tb)))
    )
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 dplyr::left_join(Y_Ready4useDyad@ds_tb %>%
                                                    dplyr::select(-period_var_1L_chr) %>%
                                                    dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>%
                                                    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) %>%
                                                    dplyr::ungroup(),X_Ready4useDyad@ds_tb) %>%
                                   dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::coalesce(.x, 0)))) %>%
      ready4use::add_dictionary(new_cases_r3 = Y_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% names(Y_Ready4useDyad@ds_tb),
                                                                                               !var_nm_chr %in% names(X_Ready4useDyad@ds_tb)))

  }
  data_xx <- transform_data_fmt(data_xx,
                                X_Ready4useDyad = X_Ready4useDyad)
  return(data_xx)
}
make_start_end_ls <- function(data_tb,
                              by_1L_chr = "year",
                              case_1L_chr = "UID",
                              date_1L_chr = "Date",
                              index_1L_lgl = TRUE,
                              index_day_1L_chr = "01",
                              index_month_1L_chr = "07",
                              end_dtm = NULL,
                              start_dtm = NULL){
  if (is.null(end_dtm)) {
    end_dtm <- max(data_tb %>% dplyr::pull(!!rlang::sym(date_1L_chr)))
  }
  if (is.null(start_dtm)) {
    start_dtm <- min(data_tb %>% dplyr::pull(!!rlang::sym(date_1L_chr)))
  }
  if(index_1L_lgl){
    new_dates_ls <- update_start_end_date(end_dtm = end_dtm,
                                          start_dtm = start_dtm,
                                          index_day_1L_chr = index_day_1L_chr,
                                          index_month_1L_chr = index_month_1L_chr)
    start_dtm <- new_dates_ls$start
    end_dtm <- new_dates_ls$end
  }
  start_dates_dtm <- seq(start_dtm, end_dtm, by = by_1L_chr)
  end_dates_dtm <- start_dates_dtm - lubridate::days(1) + lubridate::years(1)
  start_end_ls <- list(start = start_dates_dtm,
                       end = end_dates_dtm)

  return(start_end_ls)
}
make_summary_ds <- function (data_xx,
                             active_base_1L_chr = "Active",
                             add_with_join_xx = NULL,
                             join_before_dtm = NULL,
                             index_1L_chr = "Date",
                             key_vars_chr = character(0),
                             max_tenure_1L_dbl = numeric(0),
                             metrics_chr = make_metric_vars(),
                             patterns_ls = NULL,
                             prefix_1L_chr = "Cumulative",
                             separation_after_dbl = numeric(0),
                             tenure_var_1L_chr = "Tenure",
                             uid_1L_chr = "UID",
                             update_desc_1L_lgl = TRUE)
{
  X_Ready4useDyad <- transform_data_fmt(data_xx,
                                        type_1L_chr = "input")
  if(!is.null(add_with_join_xx)){
    Y_Ready4useDyad <- transform_data_fmt(add_with_join_xx,
                                          type_1L_chr = "input")
  }else{
    Y_Ready4useDyad <- NULL
  }
  all_vars_chr <- names(X_Ready4useDyad@ds_tb)
  active_vars_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl) %>%
    purrr::keep(~startsWith(., active_base_1L_chr)) %>% intersect(all_vars_chr)
  metrics_chr <- metrics_chr %>% intersect(all_vars_chr)
  key_vars_chr <- setdiff(key_vars_chr %>% intersect(all_vars_chr), c(tenure_var_1L_chr, active_vars_chr))
  if (!identical(max_tenure_1L_dbl, numeric(0))) {
    if(is.null(join_before_dtm)){
      join_before_dtm <- lubridate::as_date(Inf)
    }
    #cumulatives_chr <- make_cumulatives(separation_after_dbl = separation_after_dbl) %>% intersect(all_vars_chr)
    logicals_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl)[startsWith(make_metric_vars("eoc", separation_after_dbl = separation_after_dbl), active_base_1L_chr)] %>% intersect(all_vars_chr)
    key_vars_chr <- setdiff(key_vars_chr, c(#cumulatives_chr,
                                            logicals_chr))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 X_Ready4useDyad@ds_tb %>% dplyr::filter(!is.na(!!rlang::sym(uid_1L_chr))) %>%
                                   dplyr::filter(!!rlang::sym(tenure_var_1L_chr) <= max_tenure_1L_dbl) %>%
                                   # add_cumulatives(metrics_chr = metrics_chr,
                                   #                 arrange_by_1L_chr = "Date", prefix_1L_chr = "Cumulative",
                                   #                 group_by_1L_chr = uid_1L_chr) %>%
                                   dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>%
                                   dplyr::summarise(dplyr::across(tidyselect::any_of(c(index_1L_chr, key_vars_chr)), dplyr::first),
                                                    dplyr::across(tidyselect::any_of(c(tenure_var_1L_chr)), max), #, cumulatives_chr
                                                    dplyr::across(tidyselect::any_of(logicals_chr), sum)) %>%
                                   dplyr::mutate(dplyr::across(tidyselect::any_of(logicals_chr), as.logical)) %>% dplyr::ungroup() %>%
                                   dplyr::filter(!!rlang::sym(index_1L_chr) <= join_before_dtm))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "dictionary_r3", X_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb)))
  } else {
    cumulatives_chr <- make_cumulatives(prefix_1L_chr = prefix_1L_chr,
                                        separation_after_dbl = separation_after_dbl) %>% intersect(all_vars_chr)
    logicals_chr <- active_vars_chr
    key_vars_chr <- setdiff(key_vars_chr, c(cumulatives_chr, logicals_chr))
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 X_Ready4useDyad@ds_tb %>% dplyr::filter(!is.na(!!rlang::sym(uid_1L_chr))) %>%
                                   dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% dplyr::summarise(dplyr::across(tidyselect::any_of(c(index_1L_chr, key_vars_chr)), #ProviderID, ProviderState),
                                                                                                                dplyr::first),
                                                                                                  dplyr::across(tidyselect::any_of(c(tenure_var_1L_chr)),#, cumulatives_chr #c(Tenure, !!!rlang::syms(cumulatives_chr)),
                                                                                                                max),
                                                                                                  dplyr::across(tidyselect::any_of(logicals_chr), sum)) %>%
                                   dplyr::mutate(dplyr::across(tidyselect::any_of(logicals_chr),
                                                               as.logical)) %>% dplyr::ungroup())
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "dictionary_r3", X_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb)))
  }
  if(!is.null(Y_Ready4useDyad)){
    X_Ready4useDyad <- ready4use::add_with_join(X_Ready4useDyad, Y_Ready4useDyad)
  }
  if(!is.null(patterns_ls)){
    X_Ready4useDyad <- ready4use::update_column_names(X_Ready4useDyad, patterns_ls = patterns_ls, update_desc_1L_lgl = update_desc_1L_lgl)
  }
  data_xx <- transform_data_fmt(data_xx,
                                X_Ready4useDyad = X_Ready4useDyad)
  return(data_xx)
}
make_temporal_fns <- function(daily_fn = make_date_tfmn_fn(),
                              fiscal_start_1L_int = 7L,
                              monthly_fn = tsibble::yearmonth,
                              rename_1L_lgl = FALSE){
  temporal_fns_ls <- list(sub = lubridate::ymd_hms,
                          daily = daily_fn, #identity,
                          weekly = tsibble::yearweek,
                          monthly = monthly_fn,
                          quarterly = tsibble::yearquarter,
                          yearly = lubridate::year,
                          fiscal = function(x, y = fiscal_start_1L_int){tsibble::yearquarter(x, fiscal_start = y)},
                          ####
                          fiscalyear = function(x, y = fiscal_start_1L_int){
                            lubridate::quarter(x, fiscal_start = y, with_year = T) %>%
                              stringr::str_sub(start = 1, end = 4) %>% as.numeric() %>%
                              purrr::map_chr(~ifelse(y == 1, as.character(.x), paste0(as.character(.x-1),"-",as.character(.x))))},
                          fiscalquarter = function(x, y = fiscal_start_1L_int){lubridate::quarter(x, fiscal_start = y)}, ###
                          weekday = weekdays)
  if(rename_1L_lgl){
    temporal_fns_ls <- temporal_fns_ls %>% stats::setNames(names(temporal_fns_ls) %>% purrr::map_chr(~get_new_index(.x)))
  }
  return(temporal_fns_ls)
}
make_temporal_vars <- function(what_1L_chr = c("all", "core", "fiscal","days", "cf", "cd","fd"),
                               index_1L_chr = character(0),#"Date",
                               components_chr = c("Day", "Week", "Month", "Quarter", "Year"),#c("Year","Quarter", "Week"),
                               fiscal_chr = c("FiscalYQ", "FiscalQuarter", "FiscalYear"),
                               days_1L_chr = "Weekday"
){
  what_1L_chr <- match.arg(what_1L_chr)
  if(what_1L_chr %in% c("fiscal","days", "fd")){
    components_chr <- character(0)
  }
  if(what_1L_chr %in% c("core", "days",  "cd")){
    fiscal_chr <- character(0)
  }
  if(what_1L_chr %in% c("core", "fiscal", "cf")){
    days_1L_chr <- character(0)
  }
  temporal_vars_chr <- c(index_1L_chr, components_chr, fiscal_chr, days_1L_chr)
  return(temporal_vars_chr)
}
make_tfmn_args_ls <- function(fill_gaps_1L_lgl = logical(0),
                              frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly",
                                                   "yearly"),
                              join_to_xx = NULL, key_totals_ls = NULL,
                              key_vars_chr = character(0),
                              metrics_chr = character(0),
                              type_1L_chr = c("totals", "key", "wide", "main", "cumulative"),
                              what_1L_chr = character(0)){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  tfmn_args_ls <- list(fill_gaps_1L_lgl = fill_gaps_1L_lgl, frequency_1L_chr = frequency_1L_chr,
                       join_to_xx = join_to_xx, key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                       metrics_chr = metrics_chr, type_1L_chr = type_1L_chr,
                       what_1L_chr = what_1L_chr)
  return(tfmn_args_ls)
}
make_training_ds <- function(data_tsb,
                             index_1L_chr = "Date",
                             test_1L_int = integer(0)){
  if(!identical(test_1L_int, integer(0))){
    dates_chr <- data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% as.character()
    training_tsb <- data_tsb %>%
      tsibble::filter_index(dates_chr[1] ~ dates_chr[(length(dates_chr)-test_1L_int)])#-1
  }else{
    training_tsb <- data_tsb
  }
  return(training_tsb)
}
make_ts_models <- function (data_xx, approximation_xx = NULL, collapse_1L_lgl = T,
                            cumulatives_chr = character(0), fill_gaps_1L_lgl = FALSE,
                            frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly",
                                                 "yearly"), index_1L_chr = character(0), join_to_args_ls = make_tfmn_args_ls(),
                            key_vars_chr = character(0), key_totals_ls = NULL, metrics_chr = make_metric_vars(),
                            models_chr = c("Mean", "Naïve", "Seasonal naïve", "Drift",
                                           "Trend", "LMTS", "ETS", "ARIMA", "NENTAR", "Prophet",
                                           "Reg_ARIMA", "Reg_Prophet", "Reg_TSLM"), model_type_1L_chr = "multiplicative",
                            order_1L_int = 2, period_1L_int = 12, predictors_chr = character(0),
                            prefix_1L_chr = "Cumulative", stepwise_1L_lgl = TRUE, terms_1L_chr = character(0),
                            terms_ls = NULL, test_1L_int = integer(0), type_1L_chr = c("totals",
                                                                                       "key"), what_1L_chr = character(0))
{
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if (is.null(terms_ls)) {
    args_ls <- list(fill_gaps_1L_lgl = fill_gaps_1L_lgl,
                    frequency_1L_chr = frequency_1L_chr, key_totals_ls = key_totals_ls,
                    key_vars_chr = key_vars_chr, metrics_chr = metrics_chr,
                    type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)
    predictor_args_ls <- cumulatives_args_ls <- make_tfmn_args_ls()
    if (!identical(predictors_chr, character(0))) {
      predictor_args_ls <- args_ls
      predictor_args_ls$metrics_chr <- predictors_chr
    }
    if (!identical(cumulatives_chr, character(0))) {
      cumulatives_args_ls <- args_ls
      cumulatives_args_ls$type_1L_chr <- "cumulative"
      cumulatives_args_ls$metrics_chr <- cumulatives_chr
      cumulatives_args_ls$prefix_1L_chr <- "Cumulative"
    }
    if (!identical(join_to_args_ls, make_tfmn_args_ls())) {
      join_to_args_ls$frequency_1L_chr <- frequency_1L_chr
      if (identical(join_to_args_ls$fill_gaps_1L_lgl, logical(0))) {
        join_to_args_ls$fill_gaps_1L_lgl <- fill_gaps_1L_lgl
      }
      if (identical(join_to_args_ls$metrics_chr, character(0))) {
        join_to_args_ls$metrics_chr <- setdiff(names(join_to_args_ls$join_to_xx,
                                                     get_new_index(frequency_1L_chr)))
      }
    }
    if (identical(terms_1L_chr, character(0))) {
      models_chr <- setdiff(models_chr, c("Reg_TSLM", "Reg_Prophet",
                                          "Reg_ARIMA"))
    }
    ts_models_ls <- make_ts_models_ls(mabels_ls = list(),
                                      args_ls = args_ls, cumulatives_args_ls = cumulatives_args_ls,
                                      join_to_args_ls = join_to_args_ls, predictor_args_ls = predictor_args_ls,
                                      models_chr = models_chr, test_1L_int = test_1L_int)
    data_tsb <- transform_to_mdl_input(data_xx, ts_models_ls = ts_models_ls)
    if (identical(index_1L_chr, character(0))) {
      index_1L_chr <- get_new_index(frequency_1L_chr)
    }
    training_tsb <- make_training_ds(data_tsb, index_1L_chr = index_1L_chr,
                                     test_1L_int = test_1L_int)
    mabels_ls <- metrics_chr %>% purrr::map(~{
      mdl_1L_chr <- paste0(.x, " ~ ", terms_1L_chr)
      model_args_ls <- list()
      model_args_ls <- list(Mean = fable::MEAN(!!rlang::sym(.x)),
                            Naïve = fable::NAIVE(!!rlang::sym(.x)), `Seasonal naïve` = fable::SNAIVE(!!rlang::sym(.x)),
                            Drift = fable::NAIVE(!!rlang::sym(.x) ~ drift()),
                            Trend = fable::TSLM(!!rlang::sym(.x) ~ trend()),
                            LMTS = fable::TSLM(!!rlang::sym(.x) ~ trend() +
                                                 season()), ETS = fable::ETS(!!rlang::sym(.x)),
                            ARIMA = fable::ARIMA(!!rlang::sym(.x), approximation = approximation_xx,
                                                 stepwise = stepwise_1L_lgl), NNETAR = fable::NNETAR(!!rlang::sym(.x)),
                            NNTEAR = fable::NNETAR(!!rlang::sym(.x)), Prophet = eval(parse(text = paste0("fable.prophet::prophet(",
                                                                                                         .x, "~season(period=", period_1L_int, ", type='",
                                                                                                         model_type_1L_chr, "', order=", order_1L_int,
                                                                                                         "))"))))
      if (!identical(terms_1L_chr, character(0))) {
        model_args_ls <- append(model_args_ls, list(Reg_ARIMA = eval(parse(text = paste0("fable::ARIMA(",
                                                                                         mdl_1L_chr, ", approximation = approximation_xx, stepwise = stepwise_1L_lgl)"))),
                                                    Reg_TSLM = eval(parse(text = paste0("fable::TSLM(",
                                                                                        mdl_1L_chr, ")"))), Reg_Prophet = eval(parse(text = paste0("fable.prophet::prophet(",
                                                                                                                                                   mdl_1L_chr, " + season(period=", period_1L_int,
                                                                                                                                                   ", type='", model_type_1L_chr, "', order=",
                                                                                                                                                   order_1L_int, "))")))))
      }
      model_args_ls <- model_args_ls %>% purrr::keep_at(models_chr)
      rlang::exec(fabletools::model, .data = training_tsb,
                  !!!model_args_ls)
    }) %>% stats::setNames(metrics_chr)
    ts_models_ls$mabels_ls <- mabels_ls
    ts_models_xx <- ts_models_ls
  }  else {
    ts_models_xx <- terms_ls %>% purrr::map(~make_ts_models(data_xx,
                                                            approximation_xx = approximation_xx, cumulatives_chr = cumulatives_chr,
                                                            fill_gaps_1L_lgl = fill_gaps_1L_lgl, frequency_1L_chr = frequency_1L_chr,
                                                            index_1L_chr = index_1L_chr, join_to_args_ls = join_to_args_ls,
                                                            key_vars_chr = key_vars_chr, key_totals_ls = key_totals_ls,
                                                            metrics_chr = metrics_chr, models_chr = models_chr,
                                                            model_type_1L_chr = model_type_1L_chr, order_1L_int = order_1L_int,
                                                            period_1L_int = period_1L_int, predictors_chr = predictors_chr,
                                                            prefix_1L_chr = prefix_1L_chr, stepwise_1L_lgl = stepwise_1L_lgl,
                                                            terms_1L_chr = .x, terms_ls = NULL, test_1L_int = test_1L_int,
                                                            type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)) %>%
      stats::setNames(names(terms_ls))
    if (length(ts_models_xx) == 1) {
      ts_models_xx <- ts_models_xx %>% purrr::pluck(1)
    }    else {
      if (collapse_1L_lgl) {
        mabels_ls <- ts_models_xx %>% purrr::map2(names(ts_models_xx),
                                                  ~{
                                                    name_1L_chr <- .y
                                                    .x$mabels_ls %>% purrr::map(~{
                                                      mabel_tb <- .x
                                                      names(mabel_tb) <- paste0(names(mabel_tb),
                                                                                paste0("_", name_1L_chr))
                                                      mabel_tb
                                                    })
                                                  })
        vars_chr <- mabels_ls[[1]] %>% names()
        mabels_ls <- vars_chr %>% purrr::map(~{
          name_1L_chr <- .x
          mabels_ls %>% purrr::map_dfc(~.x %>% purrr::pluck(name_1L_chr))
        }) %>% stats::setNames(vars_chr)
        ts_models_xx <- ts_models_xx %>% purrr::pluck(1)
        ts_models_xx$mabels_ls <- mabels_ls
      }
    }
  }
  return(ts_models_xx)
}
make_ts_models_ls <- function(mabels_ls = list(), args_ls = make_tfmn_args_ls(), cumulatives_args_ls = make_tfmn_args_ls(), fabels_ls = list(), join_to_args_ls = make_tfmn_args_ls(),
                              predictor_args_ls = make_tfmn_args_ls(), models_chr = character(0), test_1L_int = integer(0)){
  ts_models_ls <- list(mabels_ls = mabels_ls, args_ls = args_ls, cumulatives_args_ls = cumulatives_args_ls, fabels_ls = fabels_ls, join_to_args_ls = join_to_args_ls,
                       predictor_args_ls = predictor_args_ls, models_chr = models_chr, test_1L_int = test_1L_int)
  return(ts_models_ls)
}
