#' Get dataset from web zip
#' @description get_ds_from_web_zip() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get dataset from web zip. The function returns Dataset (an output object of multiple potential types).
#' @param file_1L_chr File (a character vector of length one)
#' @param url_1L_chr Url (a character vector of length one)
#' @param mode_1L_chr Mode (a character vector of length one), Default: 'wb'
#' @param read_fn Read (a function), Default: read.csv
#' @param read_fn_args_ls Read function arguments (a list), Default: NULL
#' @return Dataset (an output object of multiple potential types)
#' @rdname get_ds_from_web_zip
#' @export 
#' @importFrom rlang exec
#' @keywords internal
get_ds_from_web_zip <- function (file_1L_chr, url_1L_chr, mode_1L_chr = "wb", read_fn = read.csv, 
    read_fn_args_ls = NULL) 
{
    dir_1L_chr <- tempdir()
    zip_1L_chr <- tempfile()
    download.file(url_1L_chr, zip_1L_chr, quiet = TRUE, mode = mode_1L_chr)
    temp_path_1L_chr <- unzip(file.path(zip_1L_chr), files = file_1L_chr, 
        exdir = dir_1L_chr)
    ds_xx <- rlang::exec(read_fn, temp_path_1L_chr, !!!read_fn_args_ls)
    unlink(zip_1L_chr)
    unlink(temp_path_1L_chr)
    return(ds_xx)
}
#' Get index type
#' @description get_index_type() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get index type. The function returns Index type (a character vector of length one).
#' @param data_tsb Data (a tsibble)
#' @param index_1L_chr Index (a character vector of length one), Default: character(0)
#' @return Index type (a character vector of length one)
#' @rdname get_index_type
#' @export 
#' @importFrom tsibble index yearquarter
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
get_index_type <- function (data_tsb, index_1L_chr = character(0)) 
{
    if (identical(index_1L_chr, character(0))) {
        index_1L_chr <- data_tsb %>% tsibble::index() %>% as.character()
    }
    index_type_chr <- data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% 
        class()
    index_type_1L_chr <- index_type_chr %>% intersect(c("Date", 
        "yearweek", "yearmonth", "yearquarter", "numeric")) %>% 
        switch(Date = "daily", yearweek = "weekly", yearmonth = "monthly", 
            yearquarter = "quarterly", numeric = "yearly")
    if (index_type_1L_chr == "quarterly") {
        if (!identical(data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)), 
            data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% 
                tsibble::yearquarter(fiscal_start = 1))) {
            index_type_1L_chr <- "fiscal"
        }
    }
    return(index_type_1L_chr)
}
#' Get medicare data
#' @description get_medicare_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get medicare data. The function returns Medicare (a tibble).
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param clean_1L_lgl Clean (a logical vector of length one), Default: FALSE
#' @param file_1L_chr File (a character vector of length one), Default: 'MH_MBS_QUARTERS_SEX_AGEGROUP_2223.csv'
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @return Medicare (a tibble)
#' @rdname get_medicare_data
#' @export 
#' @importFrom dplyr mutate across everything
#' @importFrom stringr str_replace_all str_squish
#' @importFrom tibble as_tibble
get_medicare_data <- function (path_1L_chr = character(0), clean_1L_lgl = FALSE, file_1L_chr = "MH_MBS_QUARTERS_SEX_AGEGROUP_2223.csv", 
    url_1L_chr = character(0)) 
{
    if (!identical(path_1L_chr, character(0))) {
        medicare_df <- read.csv(path_1L_chr, fileEncoding = "latin1")
    }
    else {
        if (identical(url_1L_chr, character(0))) {
            url_1L_chr <- "https://www.aihw.gov.au/getmedia/285c5287-97ba-4acb-9003-e9edab1f61da/Medicare_mental_health_services_data_2223.zip"
        }
        medicare_df <- get_ds_from_web_zip("MH_MBS_QUARTERS_SEX_AGEGROUP_2223.csv", 
            url_1L_chr = url_1L_chr, read_fn_args_ls = list(fileEncoding = "latin1"))
    }
    if (clean_1L_lgl) {
        medicare_df <- medicare_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), 
            ~stringr::str_replace_all(.x, "\u0096", "-"))) %>% 
            dplyr::mutate(ProviderType = gsub(" ", " ", ProviderType)) %>% 
            dplyr::mutate(Value = as.numeric(Value)) %>% dplyr::mutate(AgeGroup = stringr::str_squish(AgeGroup))
    }
    medicare_tb <- medicare_df %>% tibble::as_tibble()
    return(medicare_tb)
}
#' Get new index
#' @description get_new_index() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get new index. The function returns New index (a character vector of length one).
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal", 
#'    "sub", "fiscalyear", "fiscalquarter", "weekday")
#' @return New index (a character vector of length one)
#' @rdname get_new_index
#' @export 
#' @keywords internal
get_new_index <- function (frequency_1L_chr = c("daily", "weekly", "monthly", 
    "quarterly", "yearly", "fiscal", "sub", "fiscalyear", "fiscalquarter", 
    "weekday")) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    new_index_1L_chr <- switch(frequency_1L_chr, daily = "Day", 
        weekly = "Week", monthly = "Month", quarterly = "Quarter", 
        yearly = "Year", fiscal = "FiscalYQ", sub = "Sub", fiscalyear = "FiscalYear", 
        fiscalquarter = "FiscalQuarter", weekday = "Weekday")
    return(new_index_1L_chr)
}
#' Get performance
#' @description get_performance() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get performance. The function returns Performance (a tibble).
#' @param ts_mdls_ls Time series models (a list)
#' @param data_xx Data (an output object of multiple potential types)
#' @param metric_1L_chr Metric (a character vector of length one), Default: make_metric_vars()
#' @param rank_by_int Rank by (an integer vector), Default: integer(0)
#' @param statistics_chr Statistics (a character vector), Default: c("RMSE", "MAE", "MPE", "MAPE")
#' @return Performance (a tibble)
#' @rdname get_performance
#' @export 
#' @importFrom purrr pluck
#' @importFrom fabletools accuracy
#' @importFrom dplyr select arrange
#' @importFrom rlang sym
#' @keywords internal
get_performance <- function (ts_mdls_ls, data_xx, metric_1L_chr = make_metric_vars(), 
    rank_by_int = integer(0), statistics_chr = c("RMSE", "MAE", 
        "MPE", "MAPE")) 
{
    metric_1L_chr <- match.arg(metric_1L_chr)
    data_tsb <- get_tsibble(data_xx, frequency_1L_chr = ts_mdls_ls$args_ls$frequency_1L_chr, 
        key_totals_ls = ts_mdls_ls$args_ls$key_totals_ls, key_vars_chr = ts_mdls_ls$args_ls$key_vars_chr, 
        type_1L_chr = ts_mdls_ls$args_ls$type_1L_chr, what_1L_chr = ts_mdls_ls$args_ls$what_1L_chr)
    performance_tb <- ts_mdls_ls$fabels_ls %>% purrr::pluck(metric_1L_chr) %>% 
        fabletools::accuracy(data_tsb)
    if (!identical(statistics_chr, character(0))) {
        performance_tb <- performance_tb %>% dplyr::select(.model, 
            statistics_chr)
    }
    if (!identical(rank_by_int, integer(0))) {
        performance_tb <- performance_tb %>% dplyr::arrange(!!rlang::sym(statistics_chr[rank_by_int]))
    }
    return(performance_tb)
}
#' Get raw Estimatedesident Population data
#' @description get_raw_erp_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get raw estimatedesident population data. The function returns Estimatedesident Population raw (a tibble).
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'ERP_Q'
#' @param age_chr Age (a character vector), Default: as.character(1:115)
#' @param end_1L_chr End (a character vector of length one), Default: character(0)
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: 'quarterly'
#' @param measure_chr Measure (a character vector), Default: c("count", "change", "%change")
#' @param region_chr Region (a character vector), Default: c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT", 
#'    "AUS")
#' @param sex_chr Sex (a character vector), Default: c("female", "male", "person")
#' @param start_1L_chr Start (a character vector of length one), Default: character(0)
#' @param version_1L_chr Version (a character vector of length one), Default: NULL
#' @return Estimatedesident Population raw (a tibble)
#' @rdname get_raw_erp_data
#' @export 
#' @importFrom purrr map_chr map_int
#' @importFrom stringr str_replace
#' @importFrom ready4 get_gracefully
#' @importFrom readabs read_api
get_raw_erp_data <- function (uid_1L_chr = "ERP_Q", age_chr = as.character(1:115), 
    end_1L_chr = character(0), frequency_1L_chr = "quarterly", 
    measure_chr = c("count", "change", "%change"), region_chr = c("NSW", 
        "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT", "AUS"), 
    sex_chr = c("female", "male", "person"), start_1L_chr = character(0), 
    version_1L_chr = NULL) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr, choices = c("yearly", 
        "monthly", "quarterly")) %>% purrr::map_chr(~switch(.x, 
        yearly = "A", monthly = "M", quarterly = "Q"))
    measure_chr <- match.arg(measure_chr, several.ok = T)
    measure_int <- measure_chr %>% purrr::map_int(~switch(.x, 
        count = 1, change = 2, `%change` = 3))
    sex_chr <- match.arg(sex_chr, several.ok = T)
    region_chr <- match.arg(region_chr, several.ok = T)
    region_int <- region_chr %>% purrr::map_int(~which(.x == 
        c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", 
            "OT", "AUS")))
    sex_int <- sex_chr %>% purrr::map_int(~switch(.x, female = 2, 
        male = 1, person = 3))
    region_chr <- as.character(region_int) %>% stringr::str_replace("10", 
        "AUS")
    if (identical(end_1L_chr, character(0))) {
        end_1L_chr <- NULL
    }
    if (identical(start_1L_chr, character(0))) {
        start_1L_chr <- NULL
    }
    if (identical(version_1L_chr, character(0))) {
        version_1L_chr <- NULL
    }
    erp_raw_tb <- ready4::get_gracefully(uid_1L_chr, fn = readabs::read_api, 
        args_ls = list(datakey = list(measure = measure_int %>% 
            as.list(), sex = sex_int %>% as.list(), age = age_chr %>% 
            as.list(), region = region_chr %>% as.list(), freq = frequency_1L_chr %>% 
            as.list()), start_period = start_1L_chr, end_period = end_1L_chr, 
            version = version_1L_chr), not_chr_1L_lgl = TRUE, 
        tests_chr = c("cannot open the connection to ", "unknown input format", 
            "Attempt to get feed was unsuccessful", "Not Found \\(HTTP 404\\)", 
            "Bad Request \\(HTTP 400\\)", "HTTP error 404", "Could not resolve host", 
            "Could not parse", "Unknown HTTP verb", "Gateway Timeout \\(HTTP 504\\)"))
    return(erp_raw_tb)
}
#' Get temporal function
#' @description get_temporal_fn() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get temporal function. The function returns Temporal (a function).
#' @param period_1L_chr Period (a character vector of length one), Default: make_temporal_vars(index_1L_chr = "Sub")
#' @param temporal_fns_ls Temporal functions (a list), Default: NULL
#' @param daily_fn Daily (a function), Default: make_date_tfmn_fn()
#' @param monthly_fn Monthly (a function), Default: tsibble::yearmonth
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @return Temporal (a function)
#' @rdname get_temporal_fn
#' @export 
#' @importFrom tsibble yearmonth
#' @importFrom purrr pluck
#' @keywords internal
get_temporal_fn <- function (period_1L_chr = make_temporal_vars(index_1L_chr = "Sub"), 
    temporal_fns_ls = NULL, daily_fn = make_date_tfmn_fn(), monthly_fn = tsibble::yearmonth, 
    fiscal_start_1L_int = 7L) 
{
    period_1L_chr <- match.arg(period_1L_chr)
    if (is.null(temporal_fns_ls)) {
        temporal_fns_ls <- make_temporal_fns(daily_fn = daily_fn, 
            fiscal_start_1L_int = fiscal_start_1L_int, monthly_fn = monthly_fn, 
            rename_1L_lgl = TRUE)
    }
    temporal_fn <- purrr::pluck(temporal_fns_ls, period_1L_chr)
    return(temporal_fn)
}
#' Get tsibble
#' @description get_tsibble() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get tsibble. The function returns Data (a tsibble).
#' @param data_xx Data (an output object of multiple potential types)
#' @param fill_gaps_1L_lgl Fill gaps (a logical vector of length one), Default: FALSE
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param type_1L_chr Type (a character vector of length one), Default: c("totals", "key", "wide", "main", "cumulative")
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @return Data (a tsibble)
#' @rdname get_tsibble
#' @export 
#' @importFrom tsibble is_tsibble index group_by_key index_by update_tsibble as_tibble
#' @importFrom assertthat assert_that
#' @importFrom purrr pluck reduce map_dbl
#' @importFrom dplyr filter summarise across ungroup select everything mutate pull
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
get_tsibble <- function (data_xx, fill_gaps_1L_lgl = FALSE, frequency_1L_chr = c("daily", 
    "weekly", "monthly", "quarterly", "yearly", "fiscal"), key_totals_ls = NULL, 
    key_vars_chr = character(0), metrics_chr = make_metric_vars(), 
    prefix_1L_chr = "Cumulative", type_1L_chr = c("totals", "key", 
        "wide", "main", "cumulative"), what_1L_chr = character(0)) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (!tsibble::is_tsibble(data_xx) & !inherits(data_xx, "Ready4useDyad")) {
        assertthat::assert_that(is.list(data_xx))
        data_xx <- purrr::pluck(data_xx, paste0(type_1L_chr, 
            "_dss_ls")) %>% purrr::pluck(frequency_1L_chr)
        if (!tsibble::is_tsibble(data_xx)) {
            data_tsb <- purrr::pluck(data_xx, what_1L_chr)
        }
        else {
            data_tsb <- data_xx
        }
    }
    else {
        if (inherits(data_xx, "Ready4useDyad")) {
            data_xx <- data_xx@ds_tb
        }
        data_tsb <- data_xx
        index_1L_chr <- data_tsb %>% tsibble::index() %>% as.character()
        index_type_1L_chr <- get_index_type(data_tsb, index_1L_chr = index_1L_chr)
        start_at_1L_int <- which(index_type_1L_chr == c("daily", 
            "weekly", "monthly", "quarterly", "yearly", "fiscal"))
        temporal_vars_chr <- make_temporal_vars()
        temporal_vars_chr <- temporal_vars_chr[start_at_1L_int:length(temporal_vars_chr)]
        if (start_at_1L_int > 1) 
            temporal_vars_chr <- temporal_vars_chr[1:length(temporal_vars_chr) - 
                1]
        if (identical(key_vars_chr, character(0))) {
            if (!identical(what_1L_chr, character(0))) {
                if (what_1L_chr %in% names(data_tsb)) {
                  key_vars_chr <- what_1L_chr
                }
                else {
                  key_vars_chr <- get_vars(data_tsb, index_1L_chr = index_1L_chr, 
                    what_1L_chr = what_1L_chr)
                }
            }
        }
        if (!identical(key_vars_chr, character(0))) {
            if (!is.null(key_totals_ls)) {
                data_tsb <- purrr::reduce(1:length(key_totals_ls), 
                  .init = data_tsb, ~.x %>% dplyr::filter(!!rlang::sym(names(key_totals_ls)[.y]) != 
                    key_totals_ls[[.y]]))
            }
            data_tsb <- transform_to_tsibble(data_tsb, index_1L_chr = index_1L_chr, 
                key_vars_chr = key_vars_chr, metrics_chr = metrics_chr, 
                temporal_vars_chr = temporal_vars_chr)
        }
        new_index_1L_chr <- get_new_index(frequency_1L_chr)
        data_tsb <- data_tsb %>% tsibble::group_by_key() %>% 
            tsibble::index_by(!!rlang::sym(new_index_1L_chr)) %>% 
            dplyr::summarise(dplyr::across(tidyselect::all_of(metrics_chr), 
                ~sum(.x, na.rm = T))) %>% dplyr::ungroup()
        if (type_1L_chr == "wide") {
            data_tsb <- data_tsb %>% dplyr::select(!!rlang::sym(new_index_1L_chr), 
                dplyr::everything())
            data_tsb <- data_tsb %>% tsibble::update_tsibble(index = !!rlang::sym(new_index_1L_chr))
        }
        else {
            if (identical(key_vars_chr, character(0))) {
                filtered_tb <- data_tsb %>% tsibble::as_tibble() %>% 
                  dplyr::select(!!rlang::sym(new_index_1L_chr), 
                    tidyselect::all_of(metrics_chr))
                key_vars_chr <- NULL
            }
            else {
                filtered_tb <- data_tsb %>% tsibble::as_tibble() %>% 
                  dplyr::select(!!rlang::sym(new_index_1L_chr), 
                    tidyselect::all_of(c(key_vars_chr, metrics_chr)))
            }
            data_tsb <- filtered_tb %>% transform_to_tsibble(index_1L_chr = new_index_1L_chr, 
                metrics_chr = metrics_chr, temporal_vars_chr = character(0), 
                key_vars_chr = key_vars_chr)
            if (type_1L_chr == "cumulative") {
                base_index_1L_chr <- tsibble::index(data_xx) %>% 
                  as.character()
                start_tsb <- data_xx %>% dplyr::filter(!!rlang::sym(base_index_1L_chr) == 
                  min(!!rlang::sym(base_index_1L_chr)))
                starting_dbl <- metrics_chr %>% purrr::map_dbl(~{
                  ifelse(paste0(prefix_1L_chr, .x) %in% names(start_tsb), 
                    dplyr::mutate(start_tsb, `:=`(!!rlang::sym(paste0("PreExistingCumulative", 
                      .x)), !!rlang::sym(paste0(prefix_1L_chr, 
                      .x)) - !!rlang::sym(.x))) %>% dplyr::pull(!!rlang::sym(paste0("PreExistingCumulative", 
                      .x))), 0)
                })
                data_tsb <- data_tsb %>% add_cumulatives(metrics_chr = metrics_chr, 
                  arrange_by_1L_chr = new_index_1L_chr, prefix_1L_chr = prefix_1L_chr, 
                  starting_dbl = starting_dbl) %>% dplyr::select(-tidyselect::all_of(metrics_chr))
            }
        }
    }
    if (fill_gaps_1L_lgl) {
        data_tsb <- eval(parse(text = paste0("tsibble::fill_gaps(data_tsb, ", 
            paste0(metrics_chr, "=0", collapse = ","), ")")))
    }
    return(data_tsb)
}
#' Get variables
#' @description get_vars() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get variables. The function returns Variables (a character vector).
#' @param data_df Data (a data.frame)
#' @param activity_1L_chr Activity (a character vector of length one), Default: 'Activity'
#' @param appointments_var_1L_chr Appointments variable (a character vector of length one), Default: 'Appointments'
#' @param cancellations_var_1L_chr Cancellations variable (a character vector of length one), Default: 'Cancellations'
#' @param clinical_team_1L_chr Clinical team (a character vector of length one), Default: 'Clinical Team'
#' @param clinician_1L_chr Clinician (a character vector of length one), Default: 'Clinician'
#' @param clinician_discipline_1L_chr Clinician discipline (a character vector of length one), Default: 'Service'
#' @param components_chr Components (a character vector), Default: c("Year", "Quarter", "Week")
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Cost'
#' @param days_1L_chr Days (a character vector of length one), Default: 'Weekday'
#' @param duration_1L_chr Duration (a character vector of length one), Default: 'Duration'
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param group_1L_chr Group (a character vector of length one), Default: character(0)
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param referrals_var_1L_chr Referrals variable (a character vector of length one), Default: 'Referrals'
#' @param referrers_1L_chr Referrers (a character vector of length one), Default: 'Referrer Role'
#' @param severity_1L_chr Severity (a character vector of length one), Default: 'Severity'
#' @param team_disciplines_1L_chr Team disciplines (a character vector of length one), Default: 'Disciplines'
#' @param uid_var_1L_chr Unique identifier variable (a character vector of length one), Default: 'UID'
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "clinical", "metrics", "sports", "temporal", "other")
#' @return Variables (a character vector)
#' @rdname get_vars
#' @export 
#' @keywords internal
get_vars <- function (data_df, activity_1L_chr = "Activity", appointments_var_1L_chr = "Appointments", 
    cancellations_var_1L_chr = "Cancellations", clinical_team_1L_chr = "Clinical Team", 
    clinician_1L_chr = "Clinician", clinician_discipline_1L_chr = "Service", 
    components_chr = c("Year", "Quarter", "Week"), cost_var_1L_chr = "Cost", 
    days_1L_chr = "Weekday", duration_1L_chr = "Duration", exclude_chr = character(0), 
    group_1L_chr = character(0), index_1L_chr = "Date", referrals_var_1L_chr = "Referrals", 
    referrers_1L_chr = "Referrer Role", severity_1L_chr = "Severity", 
    team_disciplines_1L_chr = "Disciplines", uid_var_1L_chr = "UID", 
    what_1L_chr = c("all", "clinical", "metrics", "sports", "temporal", 
        "other")) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    vars_chr <- names(data_df)
    if (what_1L_chr != "all") {
        clinical_vars_chr <- make_clinical_vars(activity_1L_chr = activity_1L_chr, 
            clinical_team_1L_chr = clinical_team_1L_chr, clinician_1L_chr = clinician_1L_chr, 
            clinician_discipline_1L_chr = clinician_discipline_1L_chr, 
            duration_1L_chr = duration_1L_chr, exclude_chr = exclude_chr, 
            referrers_1L_chr = referrers_1L_chr, severity_1L_chr = severity_1L_chr, 
            team_disciplines_1L_chr = team_disciplines_1L_chr)
        metric_vars_chr <- make_metric_vars(appointments_var_1L_chr = appointments_var_1L_chr, 
            cancellations_var_1L_chr = cancellations_var_1L_chr, 
            cost_var_1L_chr = cost_var_1L_chr, referrals_var_1L_chr = referrals_var_1L_chr)
        sports_vars_chr <- get_sports_vars(data_df, exclude_chr = exclude_chr, 
            group_1L_chr = group_1L_chr)
        temporal_vars_chr <- make_temporal_vars(index_1L_chr = index_1L_chr, 
            components_chr = components_chr, fiscal_chr = character(0), 
            days_1L_chr = days_1L_chr)
        other_vars_chr <- setdiff(vars_chr, c(uid_var_1L_chr, 
            clinical_vars_chr, metric_vars_chr, temporal_vars_chr, 
            sports_vars_chr))
        if (what_1L_chr == "clinical") {
            vars_chr <- clinical_vars_chr
        }
        if (what_1L_chr == "metrics") {
            vars_chr <- metric_vars_chr
        }
        if (what_1L_chr == "sports") {
            vars_chr <- sports_vars_chr
        }
        if (what_1L_chr == "temporal") {
            vars_chr <- temporal_vars_chr
        }
        if (what_1L_chr == "other") {
            vars_chr <- other_vars_chr
        }
    }
    if (!identical(exclude_chr, character(0))) {
        vars_chr <- setdiff(vars_chr, exclude_chr)
    }
    return(vars_chr)
}
