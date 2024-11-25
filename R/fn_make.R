#' Make age bands lookup table
#' @description make_age_bands_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make age bands lookup table. The function returns Age bands (a lookup table).
#' @param bands_chr Bands (a character vector)
#' @param values_ls Values (a list)
#' @param fractions_ls Fractions (a list), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: 'by_year'
#' @return Age bands (a lookup table)
#' @rdname make_age_bands_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_age_bands_lup <- function (bands_chr, values_ls, fractions_ls = NULL, type_1L_chr = "by_year") 
{
    if (type_1L_chr == "by_year") {
        age_bands_lup <- tibble::tibble(Name = bands_chr, Range = values_ls)
    }
    if (type_1L_chr == "by_group") {
        age_bands_lup <- tibble::tibble(Name = bands_chr, Source = values_ls, 
            Fraction = fractions_ls)
    }
    return(age_bands_lup)
}
#' Make cumulatives
#' @description make_cumulatives() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cumulatives. The function returns Cumulatives (a character vector).
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param separation_after_dbl Separation after (a double vector), Default: numeric(0)
#' @return Cumulatives (a character vector)
#' @rdname make_cumulatives
#' @export 
#' @keywords internal
make_cumulatives <- function (prefix_1L_chr = "Cumulative", separation_after_dbl = numeric(0)) 
{
    episodes_vars_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl) %>% 
        sort()
    active_vars_chr <- episodes_vars_chr[startsWith(episodes_vars_chr, 
        "Active")]
    cumulatives_chr <- paste0(prefix_1L_chr, c(episodes_vars_chr[startsWith(episodes_vars_chr, 
        "Episodes")], c("Appointments", "Cancellations", "Referrals", 
        "Cost"), episodes_vars_chr[startsWith(episodes_vars_chr, 
        "Separations")]))
    return(cumulatives_chr)
}
#' Make date transformation function
#' @description make_date_tfmn_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make date transformation function. The function returns Date transformation (a function).
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%y'
#' @return Date transformation (a function)
#' @rdname make_date_tfmn_fn
#' @export 
#' @keywords internal
make_date_tfmn_fn <- function (format_1L_chr = "%d-%b-%y") 
{
    date_tfmn_fn <- eval(parse(text = paste0("function(x){format(x,\"", 
        format_1L_chr, "\") %>% as.Date(\"", format_1L_chr, "\")}")))
    return(date_tfmn_fn)
}
#' Make episodes variables
#' @description make_episodes_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make episodes variables. The function returns Episodes variables (an output object of multiple potential types).
#' @param active_var_1L_chr Active variable (a character vector of length one), Default: 'Active'
#' @param episode_var_1L_chr Episode variable (a character vector of length one), Default: 'Episodes'
#' @param flatten_1L_lgl Flatten (a logical vector of length one), Default: TRUE
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: ''
#' @param separation_after_dbl Separation after (a double vector), Default: numeric(0)
#' @param separations_var_1L_chr Separations variable (a character vector of length one), Default: 'Separations'
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: ''
#' @return Episodes variables (an output object of multiple potential types)
#' @rdname make_episodes_vars
#' @export 
#' @importFrom purrr map flatten_chr
#' @keywords internal
make_episodes_vars <- function (active_var_1L_chr = "Active", episode_var_1L_chr = "Episodes", 
    flatten_1L_lgl = TRUE, prefix_1L_chr = "", separation_after_dbl = numeric(0), 
    separations_var_1L_chr = "Separations", suffix_1L_chr = "") 
{
    if (identical(separation_after_dbl, numeric(0))) {
        episodes_vars_xx <- paste0(paste0(prefix_1L_chr, c(active_var_1L_chr, 
            episode_var_1L_chr, separations_var_1L_chr)), suffix_1L_chr)
    }
    else {
        episodes_vars_xx <- 1:length(separation_after_dbl) %>% 
            purrr::map(~make_episodes_vars(suffix_1L_chr = ifelse(.x == 
                1, "", paste0("_", separation_after_dbl[.x]))))
        if (flatten_1L_lgl) {
            episodes_vars_xx <- episodes_vars_xx %>% purrr::flatten_chr()
        }
    }
    return(episodes_vars_xx)
}
#' Make Estimatedesident Population dataset
#' @description make_erp_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make estimatedesident population dataset. The function returns Estimatedesident Population (an output object of multiple potential types).
#' @param erp_raw_tb Estimatedesident Population raw (a tibble), Default: get_raw_erp_data(region_chr = "AUS")
#' @param age_range_int Age range (an integer vector), Default: 1:115
#' @param age_bands_lup Age bands (a lookup table), Default: NULL
#' @param age_tfmn_fn Age transformation (a function), Default: as.integer
#' @param as_dyad_1L_lgl As dyad (a logical vector of length one), Default: TRUE
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: 'quarterly'
#' @param measure_1L_chr Measure (a character vector of length one), Default: 'count'
#' @param select_chr Select (a character vector), Default: c("time_period", "age", "sex", "obs_value")
#' @param sex_chr Sex (a character vector), Default: c("Female", "Male", "Total")
#' @param summarise_1L_lgl Summarise (a logical vector of length one), Default: FALSE
#' @param var_ctg_chr Variable category (a character vector), Default: c("Temporal", "Key", "Key", "Metric")
#' @return Estimatedesident Population (an output object of multiple potential types)
#' @rdname make_erp_ds
#' @export 
#' @importFrom dplyr filter select mutate case_when arrange rename group_by across summarise ungroup
#' @importFrom tidyselect all_of
#' @importFrom haven zap_labels
#' @importFrom rlang sym
#' @importFrom purrr map_chr map_lgl
#' @importFrom ready4use Ready4useDyad add_dictionary
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
    erp_tb <- erp_raw_tb %>% dplyr::filter(measure == measure_1L_int) %>% 
        dplyr::select(tidyselect::all_of(select_chr)) %>% haven::zap_labels() %>% 
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
#' Make medicare dataset
#' @description make_medicare_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make medicare dataset. The function returns a Medicare Benefits Schedule (an output object of multiple potential types).
#' @param mbs_raw_tb Medicare Benefits Schedule raw (a tibble), Default: get_medicare_data(clean_1L_lgl = TRUE)
#' @param as_dyad_1L_lgl As dyad (a logical vector of length one), Default: TRUE
#' @param erp_tb Estimatedesident Population (a tibble), Default: make_erp_ds(as_dyad_1L_lgl = FALSE)
#' @param age_bands_lup Age bands (a lookup table), Default: NULL
#' @param age_group_var_1L_chr Age group variable (a character vector of length one), Default: 'AgeGroup'
#' @param index_1L_chr Index (a character vector of length one), Default: 'Quarter'
#' @param key_vars_chr Key variables (a character vector), Default: c("Sex", "Age")
#' @param metrics_chr Metrics (a character vector), Default: c("Patients", "Services")
#' @param new_metrics_chr New metrics (a character vector), Default: c("ServicesPerPatient", "PatientsPerPerson", "ServicesPerPerson")
#' @param names_from_1L_chr Names from (a character vector of length one), Default: 'Measure'
#' @param population_var_1L_chr Population variable (a character vector of length one), Default: 'Persons'
#' @param provider_var_1L_chr Provider variable (a character vector of length one), Default: 'ProviderType'
#' @param rename_age_to_1L_chr Rename age to (a character vector of length one), Default: 'Age'
#' @param rename_provider_to_1L_chr Rename provider to (a character vector of length one), Default: 'ServiceType'
#' @param scale_1L_int Scale (an integer vector of length one), Default: 1
#' @param scaled_naming_1L_chr Scaled naming (a character vector of length one), Default: '{.col}Scaled'
#' @param values_to_1L_chr Values to (a character vector of length one), Default: 'Value'
#' @param years_chr Years (a character vector), Default: c("2021-22", "2022-23")
#' @return a Medicare Benefits Schedule (an output object of multiple potential types)
#' @rdname make_medicare_ds
#' @export 
#' @importFrom dplyr rename mutate case_when select filter inner_join across
#' @importFrom rlang sym
#' @importFrom tidyr pivot_wider
#' @importFrom ready4use Ready4useDyad add_dictionary
make_medicare_ds <- function (mbs_raw_tb = get_medicare_data(clean_1L_lgl = TRUE), 
    as_dyad_1L_lgl = TRUE, erp_tb = make_erp_ds(as_dyad_1L_lgl = FALSE), 
    age_bands_lup = NULL, age_group_var_1L_chr = "AgeGroup", 
    index_1L_chr = "Quarter", key_vars_chr = c("Sex", "Age"), 
    metrics_chr = c("Patients", "Services"), new_metrics_chr = c("ServicesPerPatient", 
        "PatientsPerPerson", "ServicesPerPerson"), names_from_1L_chr = "Measure", 
    population_var_1L_chr = "Persons", provider_var_1L_chr = "ProviderType", 
    rename_age_to_1L_chr = "Age", rename_provider_to_1L_chr = "ServiceType", 
    scale_1L_int = 1, scaled_naming_1L_chr = "{.col}Scaled", 
    values_to_1L_chr = "Value", years_chr = c("2021-22", "2022-23")) 
{
    if (identical(rename_provider_to_1L_chr, character(0))) {
        rename_provider_to_1L_chr <- provider_var_1L_chr
    }
    mbs_tb <- mbs_raw_tb %>% update_medicare_data(measures_chr = metrics_chr, 
        years_chr = years_chr)
    if (!is.null(age_bands_lup)) {
        mbs_tb <- mbs_tb %>% transform_age_groups(age_group_var_1L_chr = age_group_var_1L_chr, 
            age_bands_lup = age_bands_lup, index_1L_chr = index_1L_chr, 
            key_vars_chr = c(provider_var_1L_chr, setdiff(key_vars_chr, 
                c(ifelse(identical(rename_age_to_1L_chr, character(0)), 
                  age_group_var_1L_chr, rename_age_to_1L_chr), 
                  provider_var_1L_chr))), names_from_1L_chr = names_from_1L_chr, 
            rename_to_1L_chr = rename_age_to_1L_chr, values_to_1L_chr = values_to_1L_chr)
    }
    else {
        mbs_tb <- mbs_tb %>% dplyr::rename(`:=`(!!rlang::sym(rename_provider_to_1L_chr), 
            !!rlang::sym(provider_var_1L_chr)))
        if (!identical(rename_age_to_1L_chr, character(0))) {
            mbs_tb %>% dplyr::rename(`:=`(!!rlang::sym(rename_age_to_1L_chr), 
                !!rlang::sym(age_group_var_1L_chr)))
        }
        mbs_tb <- mbs_tb %>% tidyr::pivot_wider(names_from = names_from_1L_chr, 
            values_from = values_to_1L_chr)
    }
    mbs_tb <- make_metrics_summary(mbs_tb, index_1L_chr = index_1L_chr, 
        key_vars_chr = c(provider_var_1L_chr, key_vars_chr) %>% 
            unique(), metrics_chr = metrics_chr)
    mbs_tb <- mbs_tb %>% dplyr::mutate(`:=`(!!rlang::sym(rename_provider_to_1L_chr), 
        dplyr::case_when(!!rlang::sym(provider_var_1L_chr) == 
            "Psychiatrists" ~ "Psychiatry", !!rlang::sym(provider_var_1L_chr) %in% 
            c("Clinical psychologists", "Other psychologists") ~ 
            "Psychology (all)", !!rlang::sym(provider_var_1L_chr) == 
            "All providers" ~ "All services", TRUE ~ "Other services")))
    mbs_tb <- mbs_tb %>% dplyr::select(-!!rlang::sym(provider_var_1L_chr))
    mbs_tb <- mbs_tb %>% dplyr::filter(!!rlang::sym(rename_provider_to_1L_chr) %in% 
        c("Psychiatry", "Psychology (all)"))
    mbs_tb <- make_metrics_summary(mbs_tb, index_1L_chr = index_1L_chr, 
        key_vars_chr = c(rename_provider_to_1L_chr, setdiff(key_vars_chr, 
            c(provider_var_1L_chr, rename_provider_to_1L_chr))), 
        metrics_chr = metrics_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(new_metrics_chr[1]), 
        !!rlang::sym(metrics_chr[2])/!!rlang::sym(metrics_chr[1])))
    if (!is.null(erp_tb)) {
        mbs_tb <- mbs_tb %>% dplyr::inner_join(erp_tb)
        mbs_tb <- mbs_tb %>% dplyr::mutate(`:=`(!!rlang::sym(new_metrics_chr[2]), 
            !!rlang::sym(metrics_chr[1])/!!rlang::sym(population_var_1L_chr)), 
            `:=`(!!rlang::sym(new_metrics_chr[3]), !!rlang::sym(metrics_chr[2])/!!rlang::sym(population_var_1L_chr)))
        if (scale_1L_int != 1) {
            mbs_tb <- mbs_tb %>% dplyr::mutate(dplyr::across(new_metrics_chr[2:3], 
                ~.x * scale_1L_int, .names = scaled_naming_1L_chr))
        }
    }
    if (as_dyad_1L_lgl) {
        keys_1L_int <- c(rename_provider_to_1L_chr, key_vars_chr) %>% 
            unique() %>% length()
        var_ctg_chr <- c("Temporal", rep("Key", times = keys_1L_int), 
            rep("Metric", times = length(names(mbs_tb)) - keys_1L_int - 
                1))
        mbs_xx <- ready4use::Ready4useDyad(ds_tb = mbs_tb) %>% 
            ready4use::add_dictionary(var_ctg_chr = var_ctg_chr)
    }
    else {
        mbs_xx <- mbs_tb
    }
    return(mbs_xx)
}
#' Make metric variables
#' @description make_metric_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make metric variables. The function returns Metric variables (a character vector).
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "all", "eoc")
#' @param appointments_var_1L_chr Appointments variable (a character vector of length one), Default: 'Appointments'
#' @param cancellations_var_1L_chr Cancellations variable (a character vector of length one), Default: 'Cancellations'
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Cost'
#' @param referrals_var_1L_chr Referrals variable (a character vector of length one), Default: 'Referrals'
#' @param separation_after_dbl Separation after (a double vector), Default: numeric(0)
#' @return Metric variables (a character vector)
#' @rdname make_metric_vars
#' @export 
make_metric_vars <- function (type_1L_chr = c("main", "all", "eoc"), appointments_var_1L_chr = "Appointments", 
    cancellations_var_1L_chr = "Cancellations", cost_var_1L_chr = "Cost", 
    referrals_var_1L_chr = "Referrals", separation_after_dbl = numeric(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    episodes_vars_chr <- make_episodes_vars(separation_after_dbl = separation_after_dbl)
    if (type_1L_chr != "eoc") {
        metric_vars_chr <- c(referrals_var_1L_chr, appointments_var_1L_chr, 
            cancellations_var_1L_chr, cost_var_1L_chr)
    }
    else {
        metric_vars_chr <- character(0)
    }
    if (type_1L_chr != "main") {
        metric_vars_chr <- c(episodes_vars_chr, metric_vars_chr)
    }
    return(metric_vars_chr)
}
#' Make metrics summary
#' @description make_metrics_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make metrics summary. The function returns Summary (a tibble).
#' @param data_tb Data (a tibble)
#' @param index_1L_chr Index (a character vector of length one)
#' @param key_vars_chr Key variables (a character vector)
#' @param metrics_chr Metrics (a character vector)
#' @return Summary (a tibble)
#' @rdname make_metrics_summary
#' @export 
#' @importFrom dplyr arrange select group_by across summarise
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
make_metrics_summary <- function (data_tb, index_1L_chr, key_vars_chr, metrics_chr) 
{
    data_tb <- dplyr::arrange(data_tb, !!rlang::sym(index_1L_chr))
    summary_tb <- dplyr::select(data_tb, tidyselect::all_of(c(index_1L_chr, 
        metrics_chr, key_vars_chr))) %>% dplyr::group_by(dplyr::across(tidyselect::all_of(c(index_1L_chr, 
        key_vars_chr)))) %>% dplyr::summarise(dplyr::across(tidyselect::all_of(metrics_chr), 
        ~sum(.x, na.rm = T)), .groups = "drop")
    return(summary_tb)
}
#' Make new correspondences
#' @description make_new_correspondences() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make new correspondences. The function is called for its side effects and does not return a value.
#' @param data_tb Data (a tibble), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: character(0)
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param original_xx Original (an output object of multiple potential types), Default: character(0)
#' @return X (Name correspondences lookup table)
#' @rdname make_new_correspondences
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom stringr str_trim
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
make_new_correspondences <- function (data_tb = NULL, key_1L_chr = character(0), min_1L_int = 3L, 
    original_xx = character(0)) 
{
    if (identical(original_xx, character(0))) {
        original_xx <- data_tb %>% dplyr::pull(!!rlang::sym(key_1L_chr)) %>% 
            unique() %>% sort()
    }
    if (is.character(original_xx)) {
        abbreviations_chr <- original_xx %>% stringr::str_trim() %>% 
            abbreviate(minlength = min_1L_int)
    }
    else {
        original_xx <- original_xx %>% as.character()
        abbreviations_chr <- original_xx
    }
    x_ready4show_correspondences <- ready4show::ready4show_correspondences() %>% 
        ready4show::renew.ready4show_correspondences(old_nms_chr = original_xx, 
            new_nms_chr = unname(abbreviations_chr))
    return(x_ready4show_correspondences)
}
#' Make retainers
#' @description make_retainers() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make retainers. The function returns Data (an output object of multiple potential types).
#' @param retainers_tb Retainers (a tibble)
#' @param as_tsibble_1L_lgl As tsibble (a logical vector of length one), Default: FALSE
#' @param censor_1L_lgl Censor (a logical vector of length one), Default: FALSE
#' @param cumulatives_1L_lgl Cumulatives (a logical vector of length one), Default: FALSE
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Retainer amount'
#' @param data_tb Data (a tibble), Default: NULL
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Retainer date'
#' @param default_1L_dbl Default (a double vector of length one), Default: numeric(0)
#' @param dyad_1L_lgl Dyad (a logical vector of length one), Default: FALSE
#' @param end_date_dtm End date (a date vector), Default: NULL
#' @param fill_gaps_1L_lgl Fill gaps (a logical vector of length one), Default: FALSE
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param offset_1L_int Offset (an integer vector of length one), Default: integer(0)
#' @param provider_id_1L_chr Provider identity (a character vector of length one), Default: 'ProviderID'
#' @param reset_new_1L_lgl Reset new (a logical vector of length one), Default: TRUE
#' @param start_date_dtm Start date (a date vector), Default: NULL
#' @param unit_1L_chr Unit (a character vector of length one), Default: 'days'
#' @return Data (an output object of multiple potential types)
#' @rdname make_retainers
#' @export 
#' @importFrom dplyr pull filter select mutate across case_when bind_rows arrange group_by summarise first everything left_join rename
#' @importFrom rlang sym
#' @importFrom lubridate interval as.duration duration
#' @importFrom tsibble append_row fill_gaps as_tibble as_tsibble
#' @importFrom purrr discard
#' @importFrom tibble tibble
#' @importFrom ready4use Ready4useDyad add_dictionary
#' @keywords internal
make_retainers <- function (retainers_tb, as_tsibble_1L_lgl = FALSE, censor_1L_lgl = FALSE, 
    cumulatives_1L_lgl = FALSE, cost_var_1L_chr = "Retainer amount", 
    data_tb = NULL, date_var_1L_chr = "Retainer date", default_1L_dbl = numeric(0), 
    dyad_1L_lgl = FALSE, end_date_dtm = NULL, fill_gaps_1L_lgl = FALSE, 
    fiscal_start_1L_int = 7L, index_1L_chr = "Date", offset_1L_int = integer(0), 
    provider_id_1L_chr = "ProviderID", reset_new_1L_lgl = TRUE, 
    start_date_dtm = NULL, unit_1L_chr = "days") 
{
    if (is.null(end_date_dtm)) {
        end_date_dtm <- max(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)))
    }
    if (is.null(start_date_dtm)) {
        start_date_dtm <- min(min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))), 
            ifelse(!is.null(data_tb), min(data_tb %>% dplyr::pull(!!rlang::sym(index_1L_chr))) %>% 
                as.POSIXct(), Inf))
    }
    if (!is.null(start_date_dtm)) {
        retainers_tb <- retainers_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >= 
            start_date_dtm)
        if (!is.null(data_tb)) {
            data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(index_1L_chr) >= 
                start_date_dtm)
        }
    }
    retainers_tsb <- update_retainers_ds(retainers_tb, cost_var_1L_chr = cost_var_1L_chr, 
        date_var_1L_chr = date_var_1L_chr, end_date_dtm = end_date_dtm) %>% 
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
    if (!is.null(data_tb)) {
        preexisting_chr <- setdiff(data_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>% 
            unique(), retainers_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>% 
            unique()) %>% purrr::discard(is.na)
        if (!identical(preexisting_chr, character(0))) {
            if (identical(default_1L_dbl, numeric(0))) {
                default_1L_dbl <- mean(retainers_tb %>% dplyr::pull(!!rlang::sym(cost_var_1L_chr)), 
                  na.rm = TRUE)
            }
            if (!is.null(start_date_dtm) & identical(offset_1L_int, 
                integer(0))) {
                due_date_dtm <- start_date_dtm
            }
            else {
                due_date_dtm <- min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))) + 
                  lubridate::duration(offset_1L_int, units = unit_1L_chr)
            }
            preexisting_tb <- retainers_tb %>% dplyr::filter(F) %>% 
                dplyr::bind_rows(tibble::tibble(`:=`(!!rlang::sym(date_var_1L_chr), 
                  due_date_dtm), `:=`(!!rlang::sym(provider_id_1L_chr), 
                  preexisting_chr), `:=`(!!rlang::sym(cost_var_1L_chr), 
                  default_1L_dbl)))
            preexisting_tsb <- preexisting_tb %>% make_retainers(as_tsibble_1L_lgl = T, 
                cumulatives_1L_lgl = F, censor_1L_lgl = T, cost_var_1L_chr = cost_var_1L_chr, 
                data_tb = NULL, date_var_1L_chr = date_var_1L_chr, 
                dyad_1L_lgl = FALSE, end_date_dtm = end_date_dtm, 
                fill_gaps_1L_lgl = fill_gaps_1L_lgl, fiscal_start_1L_int = fiscal_start_1L_int)
            retainers_tsb <- retainers_tsb %>% tsibble::as_tibble() %>% 
                dplyr::bind_rows(preexisting_tsb %>% tsibble::as_tibble()) %>% 
                dplyr::arrange(!!rlang::sym(index_1L_chr)) %>% 
                dplyr::group_by(!!rlang::sym(index_1L_chr)) %>% 
                dplyr::summarise(dplyr::across(setdiff(names(retainers_tsb), 
                  c(index_1L_chr, make_temporal_vars())), sum), 
                  dplyr::across(intersect(names(retainers_tsb), 
                    make_temporal_vars()), dplyr::first)) %>% 
                tsibble::as_tsibble(index = !!rlang::sym(index_1L_chr))
        }
    }
    if (cumulatives_1L_lgl) {
        retainers_tsb <- retainers_tsb %>% dplyr::mutate(CumulativeRetainer = cumsum(Retainer), 
            CumulativeClinicians = cumsum(Clinicians))
        retainers_tsb <- retainers_tsb %>% dplyr::select(!!rlang::sym(index_1L_chr), 
            Clinicians, Retainer, CumulativeClinicians, CumulativeRetainer, 
            dplyr::everything())
    }
    if (!is.null(data_tb)) {
        if (!identical(preexisting_chr, character(0))) {
            if (reset_new_1L_lgl) {
                retainers_tsb <- retainers_tsb %>% dplyr::left_join(preexisting_tsb %>% 
                  dplyr::select(Clinicians) %>% dplyr::rename(SubtractThisPlease = Clinicians)) %>% 
                  dplyr::mutate(Clinicians = dplyr::case_when(!is.na(SubtractThisPlease) ~ 
                    Clinicians - SubtractThisPlease, TRUE ~ Clinicians)) %>% 
                  dplyr::select(-SubtractThisPlease)
            }
            if (censor_1L_lgl) 
                retainers_tsb <- retainers_tsb %>% dplyr::filter(!!rlang::sym(index_1L_chr) >= 
                  min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))))
        }
    }
    if (!as_tsibble_1L_lgl) {
        data_xx <- retainers_tsb %>% tsibble::as_tibble()
    }
    else {
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
#' Make roll back lookup table
#' @description make_roll_back_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make roll back lookup table. The function returns Roll back (a lookup table).
#' @param data_xx Data (an output object of multiple potential types)
#' @param groupings_chr Groupings (a character vector)
#' @param values_chr Values (a character vector)
#' @param minimum_1L_int Minimum (an integer vector of length one), Default: 10
#' @return Roll back (a lookup table)
#' @rdname make_roll_back_lup
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom dplyr ungroup select distinct mutate filter pull group_by summarise rename bind_rows right_join across full_join
#' @importFrom tidyr expand nesting drop_na
#' @importFrom rlang syms sym
#' @importFrom purrr map_dfr map reduce map_lgl
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of
#' @keywords internal
make_roll_back_lup <- function (data_xx, groupings_chr, values_chr, minimum_1L_int = 10L) 
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::ungroup()
    all_tb <- X_Ready4useDyad@ds_tb %>% tidyr::expand(tidyr::nesting(!!!rlang::syms(groupings_chr))) %>% 
        tidyr::drop_na()
    all_tb <- length(groupings_chr):1 %>% purrr::map_dfr(~all_tb %>% 
        dplyr::select(groupings_chr[1:.x]) %>% dplyr::distinct())
    roll_back_ls <- values_chr %>% purrr::map(~{
        var_1L_chr <- .x
        one_var_lup <- length(groupings_chr):0 %>% purrr::reduce(.init = dplyr::mutate(all_tb, 
            `:=`(!!rlang::sym(var_1L_chr), list(NULL))), ~{
            complete_tb <- .x %>% dplyr::filter(!!rlang::sym(var_1L_chr) %>% 
                purrr::map_lgl(~!is.null(.x)))
            if (nrow(complete_tb) < nrow(all_tb)) {
                if (.y > 0) {
                  partial_tb <- .x %>% dplyr::filter(!!rlang::sym(var_1L_chr) %>% 
                    purrr::map_lgl(~is.null(.x))) %>% dplyr::select(!!!rlang::syms(groupings_chr[1:.y]))
                }
                tfmn_fn <- ifelse(is.numeric(X_Ready4useDyad@ds_tb %>% 
                  dplyr::pull(!!rlang::sym(var_1L_chr))), as.numeric, 
                  ifelse(is.character(X_Ready4useDyad@ds_tb %>% 
                    dplyr::pull(!!rlang::sym(var_1L_chr))), as.character, 
                    ifelse(is.logical(X_Ready4useDyad@ds_tb %>% 
                      dplyr::pull(!!rlang::sym(var_1L_chr))), 
                      as.logical, identity)))
                if (.y > 0) {
                  matched_tb <- X_Ready4useDyad@ds_tb %>% tidyr::drop_na(!!!rlang::syms(c(groupings_chr[1:.y], 
                    var_1L_chr))) %>% dplyr::group_by(!!!rlang::syms(groupings_chr[1:.y])) %>% 
                    dplyr::summarise(`:=`(!!rlang::sym(var_1L_chr), 
                      list(table(!!rlang::sym(var_1L_chr)) %>% 
                        as.data.frame() %>% tibble::as_tibble() %>% 
                        dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                          tfmn_fn(!!rlang::sym(var_1L_chr)))) %>% 
                        dplyr::rename(Value = !!rlang::sym(var_1L_chr), 
                          Frequency = Freq)))) %>% dplyr::ungroup() %>% 
                    dplyr::filter(!!rlang::sym(var_1L_chr) %>% 
                      purrr::map_lgl(~sum(.x$Frequency) >= minimum_1L_int))
                  complete_tb <- dplyr::bind_rows(complete_tb, 
                    partial_tb %>% dplyr::right_join(matched_tb))
                }
                else {
                  complete_tb <- dplyr::bind_rows(complete_tb, 
                    X_Ready4useDyad@ds_tb %>% dplyr::summarise(dplyr::across(tidyselect::all_of(groupings_chr), 
                      ~NA), `:=`(!!rlang::sym(var_1L_chr), list(table(!!rlang::sym(var_1L_chr)) %>% 
                      as.data.frame() %>% tibble::as_tibble() %>% 
                      dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                        tfmn_fn(!!rlang::sym(var_1L_chr)))) %>% 
                      dplyr::rename(Value = !!rlang::sym(var_1L_chr), 
                        Frequency = Freq)))))
                }
            }
            complete_tb
        })
    })
    roll_back_lup <- roll_back_ls %>% purrr::reduce(~dplyr::full_join(.x, 
        .y))
    return(roll_back_lup)
}
#' Make roll backs list
#' @description make_roll_backs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make roll backs list. The function returns Roll backs (a list).
#' @param data_xx Data (an output object of multiple potential types)
#' @param groupings_chr Groupings (a character vector)
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param minimum_1L_int Minimum (an integer vector of length one), Default: 10
#' @return Roll backs (a list)
#' @rdname make_roll_backs_ls
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom dplyr ungroup
#' @importFrom purrr discard map
#' @importFrom stats setNames
#' @keywords internal
make_roll_backs_ls <- function (data_xx, groupings_chr, exclude_chr = character(0), 
    minimum_1L_int = 10L) 
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::ungroup()
    missing_int <- sapply(X_Ready4useDyad@ds_tb, function(x) sum(is.na(x))) %>% 
        purrr::discard(. < 1)
    missing_chr <- setdiff(names(missing_int), exclude_chr)
    intersected_chr <- intersect(groupings_chr, missing_chr)
    roll_backs_ls <- list(main_lup = make_roll_back_lup(X_Ready4useDyad, 
        groupings_chr = groupings_chr, values_chr = setdiff(missing_chr, 
            intersected_chr), minimum_1L_int = minimum_1L_int))
    if (!identical(intersected_chr, character(0))) {
        roll_backs_ls <- append(roll_backs_ls, intersected_chr %>% 
            purrr::map(~{
                make_roll_back_lup(X_Ready4useDyad, groupings_chr = setdiff(groupings_chr, 
                  .x), values_chr = .x, minimum_1L_int = minimum_1L_int)
            }) %>% stats::setNames(intersected_chr))
    }
    return(roll_backs_ls)
}
#' Make sampled values
#' @description make_sampled_values() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sampled values. The function returns Sampled (an output object of multiple potential types).
#' @param roll_back_xx Roll back (an output object of multiple potential types)
#' @param draws_int Draws (an integer vector)
#' @param fail_with_xx Fail with (an output object of multiple potential types), Default: NULL
#' @param filter_cdn_ls Filter condition (a list), Default: NULL
#' @param variable_1L_chr Variable (a character vector of length one), Default: character(0)
#' @return Sampled (an output object of multiple potential types)
#' @rdname make_sampled_values
#' @export 
#' @importFrom purrr reduce map_lgl pluck pmap flatten
#' @importFrom dplyr filter pull
#' @importFrom rlang sym
#' @keywords internal
make_sampled_values <- function (roll_back_xx, draws_int, fail_with_xx = NULL, filter_cdn_ls = NULL, 
    variable_1L_chr = character(0)) 
{
    if (!is.null(filter_cdn_ls)) {
        sampled_xx <- purrr::reduce(1:length(filter_cdn_ls), 
            .init = roll_back_xx, ~{
                value_1L_xx <- filter_cdn_ls[[.y]]
                .x %>% dplyr::filter(!!rlang::sym(names(filter_cdn_ls)[.y]) %>% 
                  purrr::map_lgl(~identical(.x, value_1L_xx)))
            }) %>% dplyr::pull(!!rlang::sym(variable_1L_chr)) %>% 
            purrr::pluck(1)
    }
    else {
        sampled_xx <- roll_back_xx
    }
    if (!is.null(sampled_xx)) {
        sampled_xx <- sampled_xx %>% purrr::pmap(~rep(..1, ..2)) %>% 
            purrr::flatten() %>% unlist() %>% sample(size = draws_int, 
            replace = TRUE)
    }
    else {
        sampled_xx <- fail_with_xx
    }
    return(sampled_xx)
}
#' Make sampling lookup table
#' @description make_sampling_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sampling lookup table. The function returns Sampling (a lookup table).
#' @param shares_dbl Shares (a double vector)
#' @param values_xx Values (an output object of multiple potential types)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Client_'
#' @param scale_1L_dbl Scale (a double vector of length one), Default: 100
#' @param uid_var_nm_1L_chr Unique identifier variable name (a character vector of length one), Default: 'Client ID'
#' @return Sampling (a lookup table)
#' @rdname make_sampling_lup
#' @export 
#' @importFrom purrr map2_dfr pluck
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom youthvars add_uids_to_tbs_ls
#' @importFrom dplyr select everything
#' @keywords internal
make_sampling_lup <- function (shares_dbl, values_xx, var_nm_1L_chr, prefix_1L_chr = "Client_", 
    scale_1L_dbl = 100, uid_var_nm_1L_chr = "Client ID") 
{
    sampling_lup <- values_xx %>% purrr::map2_dfr(shares_dbl, 
        ~tibble::tibble(`:=`(!!rlang::sym(var_nm_1L_chr), rep(.x, 
            scale_1L_dbl * .y))))
    sampling_lup <- youthvars::add_uids_to_tbs_ls(list(sampling_lup), 
        prefix_1L_chr = prefix_1L_chr, id_var_nm_1L_chr = uid_var_nm_1L_chr) %>% 
        purrr::pluck(1) %>% dplyr::select(!!rlang::sym(uid_var_nm_1L_chr), 
        dplyr::everything())
    return(sampling_lup)
}
#' Make service summary
#' @description make_service_summary() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make service summary. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param active_base_1L_chr Active base (a character vector of length one), Default: 'Active'
#' @param max_periods_1L_int Maximum periods (an integer vector of length one), Default: integer(0)
#' @param metrics_chr Metrics (a character vector), Default: character(0)
#' @param metric_var_1L_chr Metric variable (a character vector of length one), Default: 'Appointments'
#' @param missing_val_1L_dbl Missing value (a double vector of length one), Default: 0
#' @param patterns_ls Patterns (a list), Default: NULL
#' @param period_ctg_1L_chr Period category (a character vector of length one), Default: 'Temporal'
#' @param period_var_1L_chr Period variable (a character vector of length one), Default: 'Period'
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: ''
#' @param service_var_1L_chr Service variable (a character vector of length one), Default: 'Service'
#' @param summary_fn Summary (a function), Default: sum
#' @param tenure_var_1L_chr Tenure variable (a character vector of length one), Default: 'Tenure'
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param update_desc_1L_lgl Update description (a logical vector of length one), Default: TRUE
#' @param var_ctg_chr Variable category (a character vector), Default: 'Summary'
#' @return Data (an output object of multiple potential types)
#' @rdname make_service_summary
#' @export 
#' @importFrom dplyr select group_by pick summarise across ungroup pull filter rename mutate inner_join coalesce left_join everything
#' @importFrom tidyr pivot_wider
#' @importFrom ready4use add_dictionary update_column_names
#' @importFrom purrr discard map reduce keep_at
#' @importFrom tidyselect all_of any_of
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all
#' @importFrom stats setNames
#' @keywords internal
make_service_summary <- function (data_xx, active_base_1L_chr = "Active", max_periods_1L_int = integer(0), 
    metrics_chr = character(0), metric_var_1L_chr = "Appointments", 
    missing_val_1L_dbl = 0, patterns_ls = NULL, period_ctg_1L_chr = "Temporal", 
    period_var_1L_chr = "Period", prefix_1L_chr = "", service_var_1L_chr = "Service", 
    summary_fn = sum, tenure_var_1L_chr = "Tenure", uid_1L_chr = "UID", 
    update_desc_1L_lgl = TRUE, var_ctg_chr = "Summary") 
{
    X_Ready4useDyad <- transform_data_fmt(data_xx, type_1L_chr = "input")
    X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::select(c(uid_1L_chr, 
        service_var_1L_chr, metric_var_1L_chr)) %>% tidyr::pivot_wider(names_from = service_var_1L_chr, 
        values_from = metric_var_1L_chr, values_fn = summary_fn, 
        values_fill = missing_val_1L_dbl, names_prefix = prefix_1L_chr)
    X_Ready4useDyad <- X_Ready4useDyad %>% ready4use::add_dictionary(var_ctg_chr = var_ctg_chr)
    if (!is.null(patterns_ls)) {
        X_Ready4useDyad <- ready4use::update_column_names(X_Ready4useDyad, 
            patterns_ls = patterns_ls, update_desc_1L_lgl = update_desc_1L_lgl)
    }
    if (!identical(metrics_chr, character(0))) {
        Y_Ready4useDyad <- transform_data_fmt(data_xx, type_1L_chr = "input")
        metrics_chr <- intersect(metrics_chr %>% purrr::discard(~startsWith(., 
            active_base_1L_chr)), names(Y_Ready4useDyad@ds_tb))
        Y_Ready4useDyad <- Z_Ready4useDyad <- add_period(Y_Ready4useDyad, 
            period_ctg_1L_chr = period_ctg_1L_chr, period_var_1L_chr = period_var_1L_chr, 
            tenure_var_1L_chr = tenure_var_1L_chr)
        Y_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
            Y_Ready4useDyad@ds_tb %>% dplyr::group_by(dplyr::pick(tidyselect::all_of(c(uid_1L_chr, 
                period_var_1L_chr)))) %>% dplyr::summarise(dplyr::across(tidyselect::any_of(metrics_chr), 
                sum)) %>% dplyr::ungroup())
        periods_xx <- Z_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(period_var_1L_chr)) %>% 
            unique() %>% sort()
        dyads_ls <- periods_xx %>% purrr::map(~{
            A_Ready4useDyad <- renewSlot(Y_Ready4useDyad, "ds_tb", 
                Y_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(period_var_1L_chr) == 
                  .x) %>% dplyr::select(-tidyselect::all_of(period_var_1L_chr)))
            A_Ready4useDyad <- renewSlot(A_Ready4useDyad, "dictionary_r3", 
                A_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% 
                  names(A_Ready4useDyad@ds_tb)))
            old_chr <- setdiff(names(A_Ready4useDyad@ds_tb), 
                uid_1L_chr)
            new_chr <- paste0(period_var_1L_chr, .x, old_chr)
            A_Ready4useDyad <- renewSlot(A_Ready4useDyad, "ds_tb", 
                1:length(old_chr) %>% purrr::reduce(.init = A_Ready4useDyad@ds_tb, 
                  ~dplyr::rename(.x, `:=`(!!rlang::sym(new_chr[.y]), 
                    old_chr[.y])))) %>% renewSlot("dictionary_r3", 
                1:length(old_chr) %>% purrr::reduce(.init = A_Ready4useDyad@dictionary_r3, 
                  ~dplyr::mutate(.x, var_nm_chr = var_nm_chr %>% 
                    stringr::str_replace_all(old_chr[.y], new_chr[.y]), 
                    var_desc_chr = var_desc_chr %>% stringr::str_replace_all(old_chr[.y], 
                      new_chr[.y]))))
            B_Ready4useDyad <- make_service_summary(renewSlot(Z_Ready4useDyad, 
                "ds_tb", Z_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(period_var_1L_chr) == 
                  .x)), active_base_1L_chr = active_base_1L_chr, 
                metric_var_1L_chr = metric_var_1L_chr, metrics_chr = character(0), 
                missing_val_1L_dbl = missing_val_1L_dbl, patterns_ls = patterns_ls, 
                period_ctg_1L_chr = period_ctg_1L_chr, period_var_1L_chr = period_var_1L_chr, 
                prefix_1L_chr = paste0(period_var_1L_chr, .x), 
                service_var_1L_chr = service_var_1L_chr, summary_fn = summary_fn, 
                tenure_var_1L_chr = tenure_var_1L_chr, uid_1L_chr = uid_1L_chr, 
                update_desc_1L_lgl = update_desc_1L_lgl, var_ctg_chr = var_ctg_chr)
            renewSlot(A_Ready4useDyad, "ds_tb", dplyr::inner_join(A_Ready4useDyad@ds_tb, 
                B_Ready4useDyad@ds_tb) %>% dplyr::mutate(dplyr::across(where(is.numeric), 
                ~dplyr::coalesce(.x, 0)))) %>% ready4use::add_dictionary(new_cases_r3 = B_Ready4useDyad@dictionary_r3 %>% 
                dplyr::filter(!var_nm_chr %in% A_Ready4useDyad@dictionary_r3$var_nm_chr))
        }) %>% stats::setNames(paste0(period_var_1L_chr, periods_xx))
        if (!identical(max_periods_1L_int, integer(0))) {
            dyads_ls <- dyads_ls %>% purrr::keep_at(1:min(max(periods_xx), 
                max_periods_1L_int))
        }
        X_Ready4useDyad <- purrr::reduce(dyads_ls, .init = X_Ready4useDyad, 
            ~renewSlot(.x, "ds_tb", dplyr::left_join(.x@ds_tb, 
                .y@ds_tb) %>% dplyr::mutate(dplyr::across(where(is.numeric), 
                ~dplyr::coalesce(.x, 0)))) %>% ready4use::add_dictionary(new_cases_r3 = .y@dictionary_r3 %>% 
                dplyr::filter(var_nm_chr %in% names(.y@ds_tb), 
                  !var_nm_chr %in% names(.x@ds_tb))))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            dplyr::left_join(Y_Ready4useDyad@ds_tb %>% dplyr::select(-period_var_1L_chr) %>% 
                dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% 
                dplyr::summarise(dplyr::across(dplyr::everything(), 
                  sum)) %>% dplyr::ungroup(), X_Ready4useDyad@ds_tb) %>% 
                dplyr::mutate(dplyr::across(where(is.numeric), 
                  ~dplyr::coalesce(.x, 0)))) %>% ready4use::add_dictionary(new_cases_r3 = Y_Ready4useDyad@dictionary_r3 %>% 
            dplyr::filter(var_nm_chr %in% names(Y_Ready4useDyad@ds_tb), 
                !var_nm_chr %in% names(X_Ready4useDyad@ds_tb)))
    }
    data_xx <- transform_data_fmt(data_xx, X_Ready4useDyad = X_Ready4useDyad)
    return(data_xx)
}
#' Make summary dataset
#' @description make_summary_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make summary dataset. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param active_base_1L_chr Active base (a character vector of length one), Default: 'Active'
#' @param add_with_join_xx Add with join (an output object of multiple potential types), Default: NULL
#' @param join_before_dtm Join before (a date vector), Default: NULL
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param max_tenure_1L_dbl Maximum tenure (a double vector of length one), Default: numeric(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param patterns_ls Patterns (a list), Default: NULL
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param separation_after_dbl Separation after (a double vector), Default: numeric(0)
#' @param tenure_var_1L_chr Tenure variable (a character vector of length one), Default: 'Tenure'
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param update_desc_1L_lgl Update description (a logical vector of length one), Default: TRUE
#' @return Data (an output object of multiple potential types)
#' @rdname make_summary_ds
#' @export 
#' @importFrom purrr keep
#' @importFrom lubridate as_date
#' @importFrom dplyr filter group_by summarise across first mutate ungroup
#' @importFrom rlang sym
#' @importFrom tidyselect any_of
#' @importFrom ready4use add_with_join update_column_names
#' @keywords internal
make_summary_ds <- function (data_xx, active_base_1L_chr = "Active", add_with_join_xx = NULL, 
    join_before_dtm = NULL, index_1L_chr = "Date", key_vars_chr = character(0), 
    max_tenure_1L_dbl = numeric(0), metrics_chr = make_metric_vars(), 
    patterns_ls = NULL, prefix_1L_chr = "Cumulative", separation_after_dbl = numeric(0), 
    tenure_var_1L_chr = "Tenure", uid_1L_chr = "UID", update_desc_1L_lgl = TRUE) 
{
    X_Ready4useDyad <- transform_data_fmt(data_xx, type_1L_chr = "input")
    if (!is.null(add_with_join_xx)) {
        Y_Ready4useDyad <- transform_data_fmt(add_with_join_xx, 
            type_1L_chr = "input")
    }
    else {
        Y_Ready4useDyad <- NULL
    }
    all_vars_chr <- names(X_Ready4useDyad@ds_tb)
    active_vars_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl) %>% 
        purrr::keep(~startsWith(., active_base_1L_chr)) %>% intersect(all_vars_chr)
    metrics_chr <- metrics_chr %>% intersect(all_vars_chr)
    key_vars_chr <- setdiff(key_vars_chr %>% intersect(all_vars_chr), 
        c(tenure_var_1L_chr, active_vars_chr))
    if (!identical(max_tenure_1L_dbl, numeric(0))) {
        if (is.null(join_before_dtm)) {
            join_before_dtm <- lubridate::as_date(Inf)
        }
        logicals_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl)[startsWith(make_metric_vars("eoc", 
            separation_after_dbl = separation_after_dbl), active_base_1L_chr)] %>% 
            intersect(all_vars_chr)
        key_vars_chr <- setdiff(key_vars_chr, c(logicals_chr))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(!is.na(!!rlang::sym(uid_1L_chr))) %>% 
                dplyr::filter(!!rlang::sym(tenure_var_1L_chr) <= 
                  max_tenure_1L_dbl) %>% dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% 
                dplyr::summarise(dplyr::across(tidyselect::any_of(c(index_1L_chr, 
                  key_vars_chr)), dplyr::first), dplyr::across(tidyselect::any_of(c(tenure_var_1L_chr)), 
                  max), dplyr::across(tidyselect::any_of(logicals_chr), 
                  sum)) %>% dplyr::mutate(dplyr::across(tidyselect::any_of(logicals_chr), 
                as.logical)) %>% dplyr::ungroup() %>% dplyr::filter(!!rlang::sym(index_1L_chr) <= 
                join_before_dtm))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "dictionary_r3", 
            X_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% 
                names(X_Ready4useDyad@ds_tb)))
    }
    else {
        cumulatives_chr <- make_cumulatives(prefix_1L_chr = prefix_1L_chr, 
            separation_after_dbl = separation_after_dbl) %>% 
            intersect(all_vars_chr)
        logicals_chr <- active_vars_chr
        key_vars_chr <- setdiff(key_vars_chr, c(cumulatives_chr, 
            logicals_chr))
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb", 
            X_Ready4useDyad@ds_tb %>% dplyr::filter(!is.na(!!rlang::sym(uid_1L_chr))) %>% 
                dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% 
                dplyr::summarise(dplyr::across(tidyselect::any_of(c(index_1L_chr, 
                  key_vars_chr)), dplyr::first), dplyr::across(tidyselect::any_of(c(tenure_var_1L_chr)), 
                  max), dplyr::across(tidyselect::any_of(logicals_chr), 
                  sum)) %>% dplyr::mutate(dplyr::across(tidyselect::any_of(logicals_chr), 
                as.logical)) %>% dplyr::ungroup())
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "dictionary_r3", 
            X_Ready4useDyad@dictionary_r3 %>% dplyr::filter(var_nm_chr %in% 
                names(X_Ready4useDyad@ds_tb)))
    }
    if (!is.null(Y_Ready4useDyad)) {
        X_Ready4useDyad <- ready4use::add_with_join(X_Ready4useDyad, 
            Y_Ready4useDyad)
    }
    if (!is.null(patterns_ls)) {
        X_Ready4useDyad <- ready4use::update_column_names(X_Ready4useDyad, 
            patterns_ls = patterns_ls, update_desc_1L_lgl = update_desc_1L_lgl)
    }
    data_xx <- transform_data_fmt(data_xx, X_Ready4useDyad = X_Ready4useDyad)
    return(data_xx)
}
#' Make temporal functions
#' @description make_temporal_fns() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make temporal functions. The function returns Temporal functions (a list).
#' @param daily_fn Daily (a function), Default: make_date_tfmn_fn()
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @param monthly_fn Monthly (a function), Default: tsibble::yearmonth
#' @param rename_1L_lgl Rename (a logical vector of length one), Default: FALSE
#' @return Temporal functions (a list)
#' @rdname make_temporal_fns
#' @export 
#' @importFrom tsibble yearmonth yearweek yearquarter
#' @importFrom lubridate ymd_hms year quarter
#' @importFrom stringr str_sub
#' @importFrom purrr map_chr
#' @importFrom stats setNames
#' @keywords internal
make_temporal_fns <- function (daily_fn = make_date_tfmn_fn(), fiscal_start_1L_int = 7L, 
    monthly_fn = tsibble::yearmonth, rename_1L_lgl = FALSE) 
{
    temporal_fns_ls <- list(sub = lubridate::ymd_hms, daily = daily_fn, 
        weekly = tsibble::yearweek, monthly = monthly_fn, quarterly = tsibble::yearquarter, 
        yearly = lubridate::year, fiscal = function(x, y = fiscal_start_1L_int) {
            tsibble::yearquarter(x, fiscal_start = y)
        }, fiscalyear = function(x, y = fiscal_start_1L_int) {
            lubridate::quarter(x, fiscal_start = y, with_year = T) %>% 
                stringr::str_sub(start = 1, end = 4) %>% as.numeric() %>% 
                purrr::map_chr(~ifelse(y == 1, as.character(.x), 
                  paste0(as.character(.x - 1), "-", as.character(.x))))
        }, fiscalquarter = function(x, y = fiscal_start_1L_int) {
            lubridate::quarter(x, fiscal_start = y)
        }, weekday = weekdays)
    if (rename_1L_lgl) {
        temporal_fns_ls <- temporal_fns_ls %>% stats::setNames(names(temporal_fns_ls) %>% 
            purrr::map_chr(~get_new_index(.x)))
    }
    return(temporal_fns_ls)
}
#' Make temporal variables
#' @description make_temporal_vars() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make temporal variables. The function returns Temporal variables (a character vector).
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "core", "fiscal", "days", "cf", "cd", "fd")
#' @param index_1L_chr Index (a character vector of length one), Default: character(0)
#' @param components_chr Components (a character vector), Default: c("Day", "Week", "Month", "Quarter", "Year")
#' @param fiscal_chr Fiscal (a character vector), Default: c("FiscalYQ", "FiscalQuarter", "FiscalYear")
#' @param days_1L_chr Days (a character vector of length one), Default: 'Weekday'
#' @return Temporal variables (a character vector)
#' @rdname make_temporal_vars
#' @export 
#' @keywords internal
make_temporal_vars <- function (what_1L_chr = c("all", "core", "fiscal", "days", "cf", 
    "cd", "fd"), index_1L_chr = character(0), components_chr = c("Day", 
    "Week", "Month", "Quarter", "Year"), fiscal_chr = c("FiscalYQ", 
    "FiscalQuarter", "FiscalYear"), days_1L_chr = "Weekday") 
{
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr %in% c("fiscal", "days", "fd")) {
        components_chr <- character(0)
    }
    if (what_1L_chr %in% c("core", "days", "cd")) {
        fiscal_chr <- character(0)
    }
    if (what_1L_chr %in% c("core", "fiscal", "cf")) {
        days_1L_chr <- character(0)
    }
    temporal_vars_chr <- c(index_1L_chr, components_chr, fiscal_chr, 
        days_1L_chr)
    return(temporal_vars_chr)
}
#' Make transformation arguments list
#' @description make_tfmn_args_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make transformation arguments list. The function returns Transformation arguments (a list).
#' @param fill_gaps_1L_lgl Fill gaps (a logical vector of length one), Default: logical(0)
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param join_to_xx Join to (an output object of multiple potential types), Default: NULL
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("totals", "key", "wide", "main", "cumulative")
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @return Transformation arguments (a list)
#' @rdname make_tfmn_args_ls
#' @export 
#' @keywords internal
make_tfmn_args_ls <- function (fill_gaps_1L_lgl = logical(0), frequency_1L_chr = c("daily", 
    "weekly", "monthly", "quarterly", "yearly"), join_to_xx = NULL, 
    key_totals_ls = NULL, key_vars_chr = character(0), metrics_chr = character(0), 
    type_1L_chr = c("totals", "key", "wide", "main", "cumulative"), 
    what_1L_chr = character(0)) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    tfmn_args_ls <- list(fill_gaps_1L_lgl = fill_gaps_1L_lgl, 
        frequency_1L_chr = frequency_1L_chr, join_to_xx = join_to_xx, 
        key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr, 
        metrics_chr = metrics_chr, type_1L_chr = type_1L_chr, 
        what_1L_chr = what_1L_chr)
    return(tfmn_args_ls)
}
#' Make training dataset
#' @description make_training_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make training dataset. The function returns Training (a tsibble).
#' @param data_tsb Data (a tsibble)
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param test_1L_int Test (an integer vector of length one), Default: integer(0)
#' @return Training (a tsibble)
#' @rdname make_training_ds
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom tsibble filter_index
#' @keywords internal
make_training_ds <- function (data_tsb, index_1L_chr = "Date", test_1L_int = integer(0)) 
{
    if (!identical(test_1L_int, integer(0))) {
        dates_chr <- data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% 
            as.character()
        training_tsb <- data_tsb %>% tsibble::filter_index(dates_chr[1] ~ 
            dates_chr[(length(dates_chr) - test_1L_int)])
    }
    else {
        training_tsb <- data_tsb
    }
    return(training_tsb)
}
#' Make time series models
#' @description make_ts_models() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make time series models. The function returns Time series models (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param approximation_xx Approximation (an output object of multiple potential types), Default: NULL
#' @param collapse_1L_lgl Collapse (a logical vector of length one), Default: T
#' @param cumulatives_chr Cumulatives (a character vector), Default: character(0)
#' @param fill_gaps_1L_lgl Fill gaps (a logical vector of length one), Default: FALSE
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param index_1L_chr Index (a character vector of length one), Default: character(0)
#' @param join_to_args_ls Join to arguments (a list), Default: make_tfmn_args_ls()
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param models_chr Models (a character vector), Default: c("Mean", "Naïve", "Seasonal naïve", "Drift", "Trend", "LMTS", 
#'    "ETS", "ARIMA", "NNTEAR", "Prophet", "Reg_ARIMA", "Reg_Prophet", 
#'    "Reg_TSLM")
#' @param model_type_1L_chr Model type (a character vector of length one), Default: 'multiplicative'
#' @param order_1L_int Order (an integer vector of length one), Default: 2
#' @param period_1L_int Period (an integer vector of length one), Default: 12
#' @param predictors_chr Predictors (a character vector), Default: character(0)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param stepwise_1L_lgl Stepwise (a logical vector of length one), Default: TRUE
#' @param terms_1L_chr Terms (a character vector of length one), Default: character(0)
#' @param terms_ls Terms (a list), Default: NULL
#' @param test_1L_int Test (an integer vector of length one), Default: integer(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("totals", "key")
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @return Time series models (an output object of multiple potential types)
#' @rdname make_ts_models
#' @export 
#' @importFrom purrr map keep_at pluck map2 map_dfc
#' @importFrom fable MEAN NAIVE SNAIVE TSLM ETS ARIMA NNETAR
#' @importFrom rlang sym exec
#' @importFrom fabletools model
#' @importFrom stats setNames
make_ts_models <- function (data_xx, approximation_xx = NULL, collapse_1L_lgl = T, 
    cumulatives_chr = character(0), fill_gaps_1L_lgl = FALSE, 
    frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly", 
        "yearly"), index_1L_chr = character(0), join_to_args_ls = make_tfmn_args_ls(), 
    key_vars_chr = character(0), key_totals_ls = NULL, metrics_chr = make_metric_vars(), 
    models_chr = c("Mean", "Naïve", "Seasonal naïve", "Drift", 
        "Trend", "LMTS", "ETS", "ARIMA", "NNTEAR", "Prophet", 
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
                  stepwise = stepwise_1L_lgl), NNTEAR = fable::NNETAR(!!rlang::sym(.x)), 
                Prophet = eval(parse(text = paste0("fable.prophet::prophet(", 
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
    }
    else {
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
        }
        else {
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
#' Make time series models list
#' @description make_ts_models_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make time series models list. The function returns Time series models (a list).
#' @param mabels_ls Mabels (a list), Default: list()
#' @param args_ls Arguments (a list), Default: make_tfmn_args_ls()
#' @param cumulatives_args_ls Cumulatives arguments (a list), Default: make_tfmn_args_ls()
#' @param fabels_ls Fabels (a list), Default: list()
#' @param join_to_args_ls Join to arguments (a list), Default: make_tfmn_args_ls()
#' @param predictor_args_ls Predictor arguments (a list), Default: make_tfmn_args_ls()
#' @param models_chr Models (a character vector), Default: character(0)
#' @param test_1L_int Test (an integer vector of length one), Default: integer(0)
#' @return Time series models (a list)
#' @rdname make_ts_models_ls
#' @export 
#' @keywords internal
make_ts_models_ls <- function (mabels_ls = list(), args_ls = make_tfmn_args_ls(), 
    cumulatives_args_ls = make_tfmn_args_ls(), fabels_ls = list(), 
    join_to_args_ls = make_tfmn_args_ls(), predictor_args_ls = make_tfmn_args_ls(), 
    models_chr = character(0), test_1L_int = integer(0)) 
{
    ts_models_ls <- list(mabels_ls = mabels_ls, args_ls = args_ls, 
        cumulatives_args_ls = cumulatives_args_ls, fabels_ls = fabels_ls, 
        join_to_args_ls = join_to_args_ls, predictor_args_ls = predictor_args_ls, 
        models_chr = models_chr, test_1L_int = test_1L_int)
    return(ts_models_ls)
}
