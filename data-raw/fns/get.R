get_ds_from_web_zip <- function(file_1L_chr,
                                url_1L_chr,
                                mode_1L_chr = "wb",
                                read_fn = read.csv,
                                read_fn_args_ls = NULL){
  dir_1L_chr <- tempdir()
  zip_1L_chr <- tempfile()
  download.file(url_1L_chr, zip_1L_chr, quiet=TRUE, mode=mode_1L_chr)
  temp_path_1L_chr <-  unzip(file.path(zip_1L_chr), files = file_1L_chr, exdir = dir_1L_chr)
  ds_xx <- rlang::exec(read_fn, temp_path_1L_chr, !!!read_fn_args_ls)
  unlink(zip_1L_chr)
  unlink(temp_path_1L_chr)
  return(ds_xx)
}
get_index_type <- function(data_tsb,
                           index_1L_chr = character(0)){
  if(identical(index_1L_chr, character(0))){
    index_1L_chr <- data_tsb %>% tsibble::index() %>% as.character()
  }
  index_type_chr <- data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% class()
  index_type_1L_chr <- index_type_chr %>% intersect(c("Date",
                                                      "yearweek",
                                                      "yearmonth",
                                                      "yearquarter",
                                                      "numeric"
  )) %>%
    switch("Date" = "daily",
           "yearweek" = "weekly",
           "yearmonth" = "monthly",
           "yearquarter" = "quarterly",
           "numeric" = "yearly")
  # if(index_type_1L_chr == "daily"){
  #   if(!identical(data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)), data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr))  %>% lubridate::ymd())){
  #     index_type_1L_chr <- "sub"
  #   }
  # }
  if(index_type_1L_chr == "quarterly"){
    if(!identical(data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)), data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% tsibble::yearquarter(fiscal_start = 1))){
      index_type_1L_chr <- "fiscal"
    }
  }
  return(index_type_1L_chr)
}
get_medicare_data <- function(path_1L_chr = character(0),
                              clean_1L_lgl = FALSE,
                              file_1L_chr = "MH_MBS_QUARTERS_SEX_AGEGROUP_2223.csv",
                              url_1L_chr = character(0)){
  if(!identical(path_1L_chr, character(0))){
    medicare_tb <- read.csv(path_1L_chr, fileEncoding = "latin1")
  }else{
    if(identical(url_1L_chr, character(0))){
      url_1L_chr <- "https://www.aihw.gov.au/getmedia/285c5287-97ba-4acb-9003-e9edab1f61da/Medicare_mental_health_services_data_2223.zip"
    }
    medicare_df <- get_ds_from_web_zip("MH_MBS_QUARTERS_SEX_AGEGROUP_2223.csv",
                                       url_1L_chr = url_1L_chr,
                                       read_fn_args_ls = list(fileEncoding = "latin1"))
  }
  if(clean_1L_lgl){
    medicare_df <- medicare_df %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_replace_all(.x,"\u0096","-"))) %>%
      dplyr::mutate(ProviderType = gsub("\U00A0", " ", ProviderType)) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::mutate(AgeGroup = stringr::str_squish(AgeGroup))
  }
  medicare_tb <- medicare_df %>%
    tibble::as_tibble()
  return(medicare_tb)

}
get_new_index <- function(frequency_1L_chr = c("daily", "weekly",
                                               "monthly", "quarterly", "yearly", "fiscal",
                                               "sub", "fiscalyear", "fiscalquarter", "weekday")){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  new_index_1L_chr <- switch(frequency_1L_chr,
                             daily = "Day",
                             weekly = "Week",
                             monthly = "Month",
                             quarterly = "Quarter",
                             yearly = "Year",
                             fiscal = "FiscalYQ",
                             sub = "Sub",
                             fiscalyear = "FiscalYear",
                             fiscalquarter = "FiscalQuarter",
                             weekday = "Weekday")
  return(new_index_1L_chr)
}
get_performance <- function(ts_mdls_ls,
                            data_xx,
                            metric_1L_chr = make_metric_vars(),
                            rank_by_int = integer(0),
                            statistics_chr = c("RMSE", "MAE", "MPE", "MAPE")){
  metric_1L_chr <- match.arg(metric_1L_chr)
  data_tsb <- get_tsibble(data_xx, frequency_1L_chr = ts_mdls_ls$args_ls$frequency_1L_chr,
                          key_totals_ls = ts_mdls_ls$args_ls$key_totals_ls, key_vars_chr = ts_mdls_ls$args_ls$key_vars_chr,
                          type_1L_chr = ts_mdls_ls$args_ls$type_1L_chr, what_1L_chr = ts_mdls_ls$args_ls$what_1L_chr)
  performance_tb <- ts_mdls_ls$fabels_ls %>% purrr::pluck(metric_1L_chr) %>% fabletools::accuracy(data_tsb)
  if(!identical(statistics_chr, character(0))){
    performance_tb <- performance_tb %>% dplyr::select(.model, statistics_chr)
  }
  if(!identical(rank_by_int, integer(0))){
    performance_tb <- performance_tb %>% dplyr::arrange(!!rlang::sym(statistics_chr[rank_by_int]))
  }
  return(performance_tb)
}
get_raw_erp_data <- function(uid_1L_chr = "ERP_Q",
                             age_chr = as.character(1:115),
                             end_1L_chr = character(0),
                             frequency_1L_chr = "quarterly",
                             measure_chr = c("count", "change", "%change"),
                             #measure_int = 1:3,
                             region_chr = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT", "AUS"),
                             #region_int = 1:10, #c(as.character(1:9),"AUS"),
                             #sex_int = 1:3,
                             sex_chr = c("female", "male", "person"),
                             start_1L_chr = character(0),
                             version_1L_chr = NULL){
  frequency_1L_chr <- match.arg(frequency_1L_chr,
                                choices = c("yearly", "monthly", "quarterly"))  %>% purrr::map_chr(~ switch(.x, "yearly" = "A", "monthly" = "M", "quarterly" = "Q"))
  measure_chr <- match.arg(measure_chr, several.ok = T)
  measure_int <- measure_chr %>% purrr::map_int(~switch(.x, "count" = 1, "change" = 2, "%change" = 3))
  sex_chr <- match.arg(sex_chr, several.ok = T)
  region_chr <- match.arg(region_chr, several.ok = T)
  region_int <- region_chr %>% purrr::map_int(~which(.x==c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT", "AUS")))
  sex_int <- sex_chr %>% purrr::map_int(~switch(.x, "female" = 2, "male" = 1, "person" = 3))
  region_chr <- as.character(region_int) %>%
    stringr::str_replace("10","AUS")
  if(identical(end_1L_chr, character(0))){
    end_1L_chr <- NULL
  }
  if(identical(start_1L_chr, character(0))){
    start_1L_chr <- NULL
  }
  if(identical(version_1L_chr, character(0))){
    version_1L_chr <- NULL
  }
  erp_raw_tb <- get_gracefully(uid_1L_chr, fn = readabs::read_api, args_ls = list(#uid_1L_chr, # get_gracefully
                                                                                  datakey = list(measure = measure_int %>% as.list(),
                                                                                                 sex= sex_int %>% as.list(),
                                                                                                 age = age_chr %>% as.list(),
                                                                                                 region = region_chr %>% as.list(),
                                                                                                 freq = frequency_1L_chr %>% as.list()),
                                                                                  start_period = start_1L_chr,
                                                                                  end_period = end_1L_chr,
                                                                                  version = version_1L_chr),
                               not_chr_1L_lgl = TRUE,
                               tests_chr = c("cannot open the connection to ", "unknown input format",
                                             "Attempt to get feed was unsuccessful", "Not Found \\(HTTP 404\\)",
                                             "Bad Request \\(HTTP 400\\)","HTTP error 404", "Could not resolve host", "Could not parse",
                                             "Unknown HTTP verb", "Gateway Timeout \\(HTTP 504\\)")) # (HTTP 504)
  # erp_raw_tb <- readabs::read_api(uid_1L_chr, # get_gracefully
  #                                   datakey = list(measure = measure_int %>% as.list(),
  #                                                  sex= sex_int %>% as.list(),
  #                                                  age = age_chr %>% as.list(),
  #                                                  region = region_chr %>% as.list(),
  #                                                  freq = frequency_1L_chr %>% as.list()),
  #                                   start_period = start_1L_chr,
  #                                   end_period = end_1L_chr,
  #                                   version = version_1L_chr)

  return(erp_raw_tb)
}
get_temporal_fn <- function(period_1L_chr = make_temporal_vars(index_1L_chr = "Sub"),
                            temporal_fns_ls = NULL,
                            daily_fn = make_date_tfmn_fn(),
                            monthly_fn = tsibble::yearmonth,
                            fiscal_start_1L_int = 7L){
  period_1L_chr <- match.arg(period_1L_chr)
  if(is.null(temporal_fns_ls)){
    temporal_fns_ls <- make_temporal_fns(daily_fn = daily_fn,
                                         fiscal_start_1L_int = fiscal_start_1L_int,
                                         monthly_fn = monthly_fn,
                                         rename_1L_lgl = TRUE)
  }
  temporal_fn <- purrr::pluck(temporal_fns_ls, period_1L_chr)
  return(temporal_fn)

}
get_tsibble <- function(data_xx,
                        fill_gaps_1L_lgl = FALSE,
                        frequency_1L_chr = c("daily","weekly",
                                             "monthly", "quarterly",  "yearly", "fiscal"),
                        # index_type_1L_chr = c("daily","weekly",
                        #                       "monthly", "quarterly", "yearly", "fiscal"),
                        key_totals_ls = NULL,
                        key_vars_chr = character(0),
                        metrics_chr = make_metric_vars(),
                        prefix_1L_chr = "Cumulative",
                        type_1L_chr = c("totals","key", "wide", "main", "cumulative"),
                        what_1L_chr = character(0)){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  #index_type_1L_chr <- match.arg(index_type_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if(!tsibble::is_tsibble(data_xx) & !inherits(data_xx,"Ready4useDyad")){
    # else{
    assertthat::assert_that(is.list(data_xx))
    data_xx <- purrr::pluck(data_xx, paste0(type_1L_chr,"_dss_ls")) %>% purrr::pluck(frequency_1L_chr)
    # }
    if(!tsibble::is_tsibble(data_xx)){
      data_tsb <- purrr::pluck(data_xx, what_1L_chr)
    }else{
      data_tsb <- data_xx
    }
  }else{
    if(inherits(data_xx,"Ready4useDyad")){
      data_xx <- data_xx@ds_tb
    }
    data_tsb <- data_xx
    index_1L_chr <- data_tsb %>% tsibble::index() %>% as.character()
    index_type_1L_chr <- get_index_type(data_tsb, index_1L_chr = index_1L_chr)
    start_at_1L_int <- which(index_type_1L_chr==c("daily", "weekly", "monthly",
                                                  "quarterly", "yearly", "fiscal"))
    temporal_vars_chr <- make_temporal_vars()
    temporal_vars_chr <- temporal_vars_chr[start_at_1L_int:length(temporal_vars_chr)]
    if(start_at_1L_int > 1)
      temporal_vars_chr <- temporal_vars_chr[1:length(temporal_vars_chr)-1]


    if(identical(key_vars_chr, character(0))){
      if(!identical(what_1L_chr, character(0))){
        if(what_1L_chr %in% names(data_tsb)){
          key_vars_chr <- what_1L_chr
        }else{
          key_vars_chr <- get_vars(data_tsb, index_1L_chr = index_1L_chr, what_1L_chr = what_1L_chr)
        }
      }
    }
    if(!identical(key_vars_chr, character(0))){
      if(!is.null(key_totals_ls)){
        data_tsb <- purrr::reduce(1:length(key_totals_ls),
                                  .init = data_tsb,
                                  ~
                                    .x %>% dplyr::filter(!!rlang::sym(names(key_totals_ls)[.y]) != key_totals_ls[[.y]])

        )
      }
      data_tsb <- transform_to_tsibble(data_tsb, index_1L_chr = index_1L_chr, key_vars_chr = key_vars_chr, metrics_chr = metrics_chr, temporal_vars_chr = temporal_vars_chr)
    }
    #if(frequency_1L_chr != "daily"){
    new_index_1L_chr <- get_new_index(frequency_1L_chr)
    # switch(frequency_1L_chr,
    #                          daily = "Day",
    #                          weekly = "Week",
    #                          monthly = "Month",
    #                          quarterly = "Quarter",
    #                          yearly = "Year",
    #                          fiscal = "FiscalYQ")
    data_tsb <- data_tsb %>%
      # dplyr::select(Sex, tidyselect::all_of(metrics_chr), tidyselect::any_of(c("Week", "Month", "Quarter", "Year"))) %>%
      tsibble::group_by_key() %>%
      tsibble::index_by(!!rlang::sym(new_index_1L_chr)) %>% # monthly aggregates
      dplyr::summarise(dplyr::across(tidyselect::all_of(metrics_chr), ~sum(.x, na.rm = T)))  %>%
      dplyr::ungroup()
    if(type_1L_chr == "wide"){
      data_tsb <- data_tsb %>%
        dplyr::select(!!rlang::sym(new_index_1L_chr), dplyr::everything())
      data_tsb <- data_tsb %>%
        tsibble::update_tsibble(index = !!rlang::sym(new_index_1L_chr))
    }else{
      if(identical(key_vars_chr, character(0))){
        filtered_tb <- data_tsb %>%
          tsibble::as_tibble() %>%
          dplyr::select(!!rlang::sym(new_index_1L_chr), tidyselect::all_of(metrics_chr))
        key_vars_chr <- NULL
      }else{
        filtered_tb <- data_tsb %>%
          tsibble::as_tibble() %>%
          dplyr::select(!!rlang::sym(new_index_1L_chr), #!!rlang::sym(what_1L_chr),
                        tidyselect::all_of(c(key_vars_chr,metrics_chr)))
        #key_vars_chr <- what_1L_chr
      }
      data_tsb <- filtered_tb %>% transform_to_tsibble(index_1L_chr = new_index_1L_chr, metrics_chr = metrics_chr, temporal_vars_chr = character(0), key_vars_chr = key_vars_chr)
      if(type_1L_chr == "cumulative"){
        base_index_1L_chr <- tsibble::index(data_xx) %>% as.character()
        start_tsb <- data_xx %>% dplyr::filter(!!rlang::sym(base_index_1L_chr) == min(!!rlang::sym(base_index_1L_chr)))
        starting_dbl <- metrics_chr %>%
          purrr::map_dbl(~{
            ifelse(paste0(prefix_1L_chr,.x) %in% names(start_tsb),
                   dplyr::mutate(start_tsb, !!rlang::sym(paste0("PreExistingCumulative",.x)) := !!rlang::sym(paste0(prefix_1L_chr,.x)) - !!rlang::sym(.x)) %>% dplyr::pull(!!rlang::sym(paste0("PreExistingCumulative",.x))),
                   0)
          })
        data_tsb <- data_tsb %>% add_cumulatives(metrics_chr = metrics_chr, arrange_by_1L_chr = new_index_1L_chr, prefix_1L_chr = prefix_1L_chr, starting_dbl = starting_dbl) %>% dplyr::select(-tidyselect::all_of(metrics_chr))
      }
    }
    #}
  }
  if(fill_gaps_1L_lgl){
    data_tsb <- eval(parse(text = paste0("tsibble::fill_gaps(data_tsb, ",paste0(metrics_chr,"=0", collapse=","),")") ))
  }
  return(data_tsb)
}
get_vars <- function(data_df,
                     activity_1L_chr = "Activity",
                     appointments_var_1L_chr = "Appointments",
                     cancellations_var_1L_chr = "Cancellations",
                     clinical_team_1L_chr = "Clinical Team",
                     clinician_1L_chr = "Clinician",
                     clinician_discipline_1L_chr = "Service",
                     components_chr = c("Year","Quarter", "Week"),
                     cost_var_1L_chr = "Cost",
                     days_1L_chr = "Weekday",
                     duration_1L_chr = "Duration",
                     exclude_chr = character(0),
                     group_1L_chr = character(0),
                     index_1L_chr = "Date",
                     referrals_var_1L_chr = "Referrals",
                     referrers_1L_chr = "Referrer Role",
                     severity_1L_chr = "Severity",
                     team_disciplines_1L_chr = "Disciplines",
                     uid_var_1L_chr = "UID",
                     what_1L_chr = c("all","clinical","metrics", "sports", "temporal", "other")){
  what_1L_chr <- match.arg(what_1L_chr)
  vars_chr <- names(data_df)
  if(what_1L_chr != "all"){
    clinical_vars_chr <- make_clinical_vars(activity_1L_chr = activity_1L_chr,
                                            clinical_team_1L_chr = clinical_team_1L_chr,
                                            clinician_1L_chr = clinician_1L_chr,
                                            clinician_discipline_1L_chr = clinician_discipline_1L_chr,
                                            duration_1L_chr = duration_1L_chr,
                                            exclude_chr = exclude_chr,
                                            referrers_1L_chr = referrers_1L_chr,
                                            severity_1L_chr = severity_1L_chr,
                                            team_disciplines_1L_chr = team_disciplines_1L_chr)
    metric_vars_chr <- make_metric_vars(appointments_var_1L_chr = appointments_var_1L_chr,
                                        cancellations_var_1L_chr = cancellations_var_1L_chr,
                                        cost_var_1L_chr = cost_var_1L_chr,
                                        referrals_var_1L_chr = referrals_var_1L_chr)
    sports_vars_chr <- get_sports_vars(data_df, exclude_chr = exclude_chr, group_1L_chr = group_1L_chr)
    temporal_vars_chr <- make_temporal_vars(index_1L_chr = index_1L_chr,
                                            components_chr = components_chr,
                                            fiscal_chr = character(0),
                                            days_1L_chr = days_1L_chr)
    other_vars_chr <- setdiff(vars_chr, c(uid_var_1L_chr, clinical_vars_chr, metric_vars_chr, temporal_vars_chr, sports_vars_chr))
    if(what_1L_chr=="clinical"){
      vars_chr <- clinical_vars_chr
    }
    if(what_1L_chr=="metrics"){
      vars_chr <- metric_vars_chr
    }
    if(what_1L_chr=="sports"){
      vars_chr <- sports_vars_chr
    }
    if(what_1L_chr=="temporal"){
      vars_chr <- temporal_vars_chr
    }
    if(what_1L_chr=="other"){
      vars_chr <- other_vars_chr
    }
  }
  if(!identical(exclude_chr, character(0))){
    vars_chr <- setdiff(vars_chr,exclude_chr)
  }
  return(vars_chr)
}
