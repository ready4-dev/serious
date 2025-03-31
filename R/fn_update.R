#' Update for price year
#' @description update_for_price_year() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update for price year. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param cost_current_1L_chr Cost current (a character vector of length one), Default: 'Cost'
#' @param cost_constant_1L_chr Cost constant (a character vector of length one), Default: 'Cost'
#' @param price_indices_dbl Price indices (a double vector), Default: numeric(0)
#' @param price_ref_1L_int Price reference (an integer vector of length one), Default: 1
#' @param time_var_1L_chr Time variable (a character vector of length one), Default: 'FiscalYear'
#' @param total_1L_chr Total (a character vector of length one), Default: character(0)
#' @param years_are_cols_1L_lgl Years are columns (a logical vector of length one), Default: F
#' @return Data (a tibble)
#' @rdname update_for_price_year
#' @export 
#' @importFrom purrr map_dbl
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr left_join mutate select rowwise c_across ungroup
#' @importFrom tidyselect any_of
update_for_price_year <- function (data_tb, cost_current_1L_chr = "Cost", cost_constant_1L_chr = "Cost", 
    price_indices_dbl = numeric(0), price_ref_1L_int = 1L, time_var_1L_chr = "FiscalYear", 
    total_1L_chr = character(0), years_are_cols_1L_lgl = F) 
{
    if (!identical(price_indices_dbl, numeric(0))) {
        multipliers_dbl <- purrr::map_dbl(price_indices_dbl, 
            ~.x/price_indices_dbl[price_ref_1L_int])
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
#' Update medicare data
#' @description update_medicare_data() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update medicare data. The function returns Medicare (a tibble).
#' @param medicare_tb Medicare (a tibble)
#' @param measures_chr Measures (a character vector), Default: character(0)
#' @param years_chr Years (a character vector), Default: character(0)
#' @return Medicare (a tibble)
#' @rdname update_medicare_data
#' @export 
#' @importFrom dplyr mutate case_when filter select everything
#' @importFrom purrr map2_chr
#' @importFrom stringr str_sub
#' @importFrom tsibble yearquarter
#' @keywords internal
update_medicare_data <- function (medicare_tb, measures_chr = character(0), years_chr = character(0)) 
{
    medicare_tb <- medicare_tb %>% dplyr::mutate(Year_Quarter = dplyr::case_when(Quarter == 
        "July to September" ~ "Q3", Quarter == "October to December" ~ 
        "Q4", Quarter == "January to March" ~ "Q1", Quarter == 
        "April to June" ~ "Q2", T ~ Quarter)) %>% dplyr::filter(Year_Quarter %in% 
        paste0("Q", 1:4)) %>% dplyr::mutate(Year_Quarter = Year_Quarter %>% 
        purrr::map2_chr(FinancialYear, ~paste0(ifelse(.x %in% 
            c("Q1", "Q2"), paste0("202", stringr::str_sub(.y, 
            start = 7)), stringr::str_sub(.y, end = 4)), " ", 
            .x))) %>% dplyr::select(Year_Quarter, dplyr::everything()) %>% 
        dplyr::mutate(Year_Quarter = tsibble::yearquarter(Year_Quarter)) %>% 
        dplyr::mutate(Quarter = Year_Quarter) %>% dplyr::select(-Year_Quarter) %>% 
        dplyr::select(FinancialYear, Quarter, dplyr::everything())
    if (!identical(measures_chr, character(0))) {
        medicare_tb <- medicare_tb %>% dplyr::filter(Measure %in% 
            measures_chr)
    }
    if (!identical(years_chr, character(0))) {
        medicare_tb <- medicare_tb %>% dplyr::filter(FinancialYear %in% 
            years_chr)
    }
    return(medicare_tb)
}
#' Update retainers dataset
#' @description update_retainers_ds() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update retainers dataset. The function returns Retainers (a tibble).
#' @param retainers_tb Retainers (a tibble)
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Retainer amount'
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Retainer date'
#' @param end_date_dtm End date (a date vector), Default: NULL
#' @param price_indices_dbl Price indices (a double vector), Default: numeric(0)
#' @param price_ref_1L_int Price reference (an integer vector of length one), Default: 1
#' @param time_var_1L_chr Time variable (a character vector of length one), Default: 'FiscalYear'
#' @return Retainers (a tibble)
#' @rdname update_retainers_ds
#' @export 
#' @importFrom dplyr mutate select everything rename
#' @importFrom rlang sym
#' @importFrom tidyr all_of
#' @keywords internal
update_retainers_ds <- function (retainers_tb, cost_var_1L_chr = "Retainer amount", 
    date_var_1L_chr = "Retainer date", end_date_dtm = NULL, price_indices_dbl = numeric(0), 
    price_ref_1L_int = 1L, time_var_1L_chr = "FiscalYear") 
{
    retainers_tb <- retainers_tb %>% dplyr::mutate(Clinicians = 1) %>% 
        add_cyclic_cases(date_var_1L_chr = date_var_1L_chr, arrange_by_1L_chr = date_var_1L_chr, 
            new_zeros_chr = "Clinicians", end_date_dtm = end_date_dtm) %>% 
        add_temporal_vars(date_var_1L_chr = date_var_1L_chr) %>% 
        update_for_price_year(cost_current_1L_chr = cost_var_1L_chr, 
            cost_constant_1L_chr = cost_var_1L_chr, price_indices_dbl = price_indices_dbl, 
            price_ref_1L_int = price_ref_1L_int, time_var_1L_chr = time_var_1L_chr) %>% 
        dplyr::mutate(CumulativeRetainer = cumsum(!!rlang::sym(cost_var_1L_chr)), 
            CumulativeClinicians = cumsum(Clinicians)) %>% dplyr::mutate(Date = Day) %>% 
        dplyr::select(tidyr::all_of(c("Date", names(retainers_tb), 
            "Clinicians", "CumulativeRetainer", "CumulativeClinicians")), 
            dplyr::everything())
    if (date_var_1L_chr != "Date") {
        retainers_tb <- retainers_tb %>% dplyr::select(-!!rlang::sym(date_var_1L_chr))
    }
    retainers_tb <- retainers_tb %>% dplyr::rename(Retainer = !!rlang::sym(cost_var_1L_chr))
    return(retainers_tb)
}
#' Update scenarios tibble
#' @description update_scenarios_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update scenarios tibble. The function returns Scenarios (a tibble).
#' @param scenarios_tb Scenarios (a tibble)
#' @param base_case_1L_chr Base case (a character vector of length one), Default: 'Base case'
#' @param base_case_tb Base case (a tibble), Default: NULL
#' @param change_var_1L_chr Change variable (a character vector of length one), Default: 'Total Cost'
#' @param missing_1L_xx Missing length one (an output object of multiple potential types), Default: NULL
#' @param difference_1L_int Difference (an integer vector of length one), Default: 1
#' @param outcomes_chr Outcomes (a character vector), Default: 'Budget Impact'
#' @param scenario_1L_chr Scenario (a character vector of length one), Default: 'Scenario'
#' @param tfmn_fn Transformation (a function), Default: NULL
#' @return Scenarios (a tibble)
#' @rdname update_scenarios_tb
#' @export 
#' @importFrom dplyr bind_rows mutate pull case_when across where select
#' @importFrom rlang sym
#' @importFrom purrr reduce map_lgl
#' @importFrom tidyr any_of
#' @keywords internal
update_scenarios_tb <- function (scenarios_tb, base_case_1L_chr = "Base case", base_case_tb = NULL, 
    change_var_1L_chr = "Total Cost", missing_1L_xx = NULL, difference_1L_int = 1L, 
    outcomes_chr = "Budget Impact", scenario_1L_chr = "Scenario", 
    tfmn_fn = NULL) 
{
    if (!is.null(base_case_tb)) {
        scenarios_tb <- dplyr::bind_rows(base_case_tb, scenarios_tb) %>% 
            dplyr::mutate(`:=`(!!rlang::sym(outcomes_chr[difference_1L_int]), 
                !!rlang::sym(change_var_1L_chr) - base_case_tb %>% 
                  dplyr::pull(!!rlang::sym(change_var_1L_chr))))
        extra_vars_chr <- outcomes_chr[-difference_1L_int]
        if (!identical(extra_vars_chr, character(0)) & !is.null(missing_1L_xx)) {
            scenarios_tb <- extra_vars_chr %>% purrr::reduce(.init = scenarios_tb, 
                ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                  dplyr::case_when((.x %>% dplyr::pull(!!rlang::sym(scenario_1L_chr)) %>% 
                    purrr::map_lgl(~.x == "Base case")) & is.na(!!rlang::sym(.y)) ~ 
                    missing_1L_xx, T ~ !!rlang::sym(.y)))))
        }
    }
    if (!is.null(tfmn_fn)) {
        scenarios_tb <- scenarios_tb %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), 
            ~tfmn_fn(.x)))
    }
    scenarios_tb <- scenarios_tb %>% dplyr::select(tidyr::any_of(c(scenario_1L_chr, 
        outcomes_chr)))
    return(scenarios_tb)
}
#' Update start end date
#' @description update_start_end_date() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update start end date. The function returns New dates (a list).
#' @param end_dtm End (a date vector)
#' @param start_dtm Start (a date vector)
#' @param index_day_1L_chr Index day (a character vector of length one), Default: '01'
#' @param index_month_1L_chr Index month (a character vector of length one), Default: '07'
#' @return New dates (a list)
#' @rdname update_start_end_date
#' @export 
#' @importFrom stringr str_sub
#' @importFrom lubridate year days
#' @keywords internal
update_start_end_date <- function (end_dtm, start_dtm, index_day_1L_chr = "01", index_month_1L_chr = "07") 
{
    end_1L_chr <- end_dtm %>% as.character()
    start_1L_chr <- start_dtm %>% as.character()
    index_end_1L_chr <- end_1L_chr
    index_start_1L_chr <- start_1L_chr
    if (!is.na(index_day_1L_chr)) {
        index_start_1L_chr <- paste0(stringr::str_sub(index_start_1L_chr, 
            end = 8), index_day_1L_chr)
    }
    if (!is.na(index_month_1L_chr)) {
        index_start_1L_chr <- paste0(stringr::str_sub(index_start_1L_chr, 
            end = 5), index_month_1L_chr, stringr::str_sub(index_start_1L_chr, 
            start = 8))
    }
    index_start_dtm <- as.POSIXct(index_start_1L_chr, tz = start_dtm %>% 
        attr("tzone"))
    if (index_start_dtm > start_dtm) {
        index_year_1L_chr <- index_start_dtm %>% lubridate::year() - 
            1
        index_start_1L_chr <- paste0(index_year_1L_chr, stringr::str_sub(index_start_1L_chr, 
            start = 5))
        index_start_dtm <- as.POSIXct(index_start_1L_chr, tz = start_dtm %>% 
            attr("tzone"))
    }
    start_dtm <- index_start_dtm
    index_end_dtm <- start_dtm - lubridate::days(1)
    index_end_1L_chr <- index_end_dtm %>% as.character()
    index_end_1L_chr <- paste0(end_dtm %>% lubridate::year(), 
        stringr::str_sub(index_end_1L_chr, start = 5))
    index_end_dtm <- as.POSIXct(index_end_1L_chr, tz = end_dtm %>% 
        attr("tzone"))
    if (index_end_dtm < end_dtm) {
        index_year_1L_chr <- index_end_dtm %>% lubridate::year() + 
            1
        index_end_1L_chr <- paste0(index_year_1L_chr, stringr::str_sub(index_end_1L_chr, 
            start = 5))
        index_end_dtm <- as.POSIXct(index_end_1L_chr, tz = end_dtm %>% 
            attr("tzone"))
    }
    end_dtm <- index_end_dtm
    new_dates_ls <- list(start = start_dtm, end = end_dtm)
    return(new_dates_ls)
}
#' Update temporal variables
#' @description update_temporal_vars() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update temporal variables. The function returns Temporal variables (a character vector).
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal")
#' @param temporal_vars_chr Temporal variables (a character vector), Default: make_temporal_vars()
#' @return Temporal variables (a character vector)
#' @rdname update_temporal_vars
#' @export 
#' @keywords internal
update_temporal_vars <- function (frequency_1L_chr = c("daily", "weekly", "monthly", 
    "quarterly", "yearly", "fiscal"), temporal_vars_chr = make_temporal_vars()) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    date_var_1L_chr <- get_new_index(frequency_1L_chr)
    if ("Weekday" %in% temporal_vars_chr) {
        weekday_1L_chr <- "Weekday"
        temporal_vars_chr <- setdiff(temporal_vars_chr, "Weekday")
    }
    else {
        weekday_1L_chr <- character(0)
    }
    if (date_var_1L_chr %in% temporal_vars_chr) {
        temporal_vars_chr <- temporal_vars_chr[which(temporal_vars_chr == 
            date_var_1L_chr):length(temporal_vars_chr)]
    }
    if ("Day" %in% temporal_vars_chr) {
        temporal_vars_chr <- c(weekday_1L_chr, temporal_vars_chr)
    }
    return(temporal_vars_chr)
}
#' Update to full tenure
#' @description update_to_full_tenure() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update to full tenure. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param drop_derived_1L_lgl Drop derived (a logical vector of length one), Default: TRUE
#' @param end_date_dtm End date (a date vector), Default: NULL
#' @param start_date_dtm Start date (a date vector), Default: NULL
#' @param tenure_var_1L_chr Tenure variable (a character vector of length one), Default: 'Tenure'
#' @param uid_var_1L_chr Unique identifier variable (a character vector of length one), Default: 'UID'
#' @param unit_1L_chr Unit (a character vector of length one), Default: 'year'
#' @return Data (a tibble)
#' @rdname update_to_full_tenure
#' @export 
#' @importFrom dplyr pull filter group_by mutate summarise first left_join row_number select ungroup
#' @importFrom rlang sym
#' @importFrom lubridate duration
#' @importFrom purrr map2_lgl
#' @importFrom tidyselect all_of
#' @keywords internal
update_to_full_tenure <- function (data_tb, date_var_1L_chr = "Date", drop_derived_1L_lgl = TRUE, 
    end_date_dtm = NULL, start_date_dtm = NULL, tenure_var_1L_chr = "Tenure", 
    uid_var_1L_chr = "UID", unit_1L_chr = "year") 
{
    if (is.null(end_date_dtm)) {
        end_date_dtm <- max(data_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)))
    }
    if (!is.null(start_date_dtm)) {
        data_tb <- data_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >= 
            start_date_dtm)
    }
    if (!tenure_var_1L_chr %in% names(data_tb)) {
        data_tb <- data_tb %>% add_tenure(date_var_1L_chr = date_var_1L_chr, 
            tenure_var_1L_chr = tenure_var_1L_chr, uid_var_1L_chr = uid_var_1L_chr, 
            unit_1L_chr = unit_1L_chr)
        derived_1L_chr <- tenure_var_1L_chr
    }
    else {
        derived_1L_chr <- character(0)
    }
    data_tb <- data_tb %>% dplyr::group_by(!!rlang::sym(uid_var_1L_chr)) %>% 
        dplyr::mutate(Last_Period_lgl = !!rlang::sym(tenure_var_1L_chr) >= 
            max(floor(!!rlang::sym(tenure_var_1L_chr))))
    summary_lup <- data_tb %>% dplyr::summarise(`:=`(!!rlang::sym(uid_var_1L_chr), 
        dplyr::first(!!rlang::sym(uid_var_1L_chr))), Index_For_Cut_int = which(Last_Period_lgl)[1], 
        End_This_Cycle_dtm = dplyr::first(!!rlang::sym(date_var_1L_chr)) + 
            lubridate::duration(max(ceiling(!!rlang::sym(tenure_var_1L_chr))), 
                units = unit_1L_chr))
    data_tb <- data_tb %>% dplyr::left_join(summary_lup)
    summary_lup <- data_tb %>% dplyr::mutate(Grouped_n_int = dplyr::row_number()) %>% 
        dplyr::filter(Index_For_Cut_int == Grouped_n_int) %>% 
        dplyr::mutate(Cut_Off_dtm = !!rlang::sym(date_var_1L_chr)) %>% 
        dplyr::select(!!rlang::sym(uid_var_1L_chr), Cut_Off_dtm)
    data_tb <- data_tb %>% dplyr::left_join(summary_lup)
    data_tb <- data_tb %>% dplyr::mutate(After_Start_Cut_Off_lgl = !!rlang::sym(date_var_1L_chr) >= 
        Cut_Off_dtm) %>% dplyr::mutate(Drop_lgl = purrr::map2_lgl(After_Start_Cut_Off_lgl, 
        End_This_Cycle_dtm, ~.x & (.y > end_date_dtm))) %>% dplyr::ungroup() %>% 
        dplyr::filter(!Drop_lgl)
    if (drop_derived_1L_lgl) 
        data_tb <- data_tb %>% dplyr::select(-tidyselect::all_of(c("Last_Period_lgl", 
            "Cut_Off_dtm", "After_Start_Cut_Off_lgl", "Drop_lgl", 
            "Index_For_Cut_int", "End_This_Cycle_dtm", derived_1L_chr)))
    return(data_tb)
}
#' Update with imputed
#' @description update_with_imputed() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update with imputed. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param impute_ls Impute (a list)
#' @param lup_ls Lookup table list (a list of lookup tables)
#' @param args_ls Arguments (a list), Default: (
#'list(NULL)
#' @param fns_ls Functions (a list), Default: list(sample)
#' @return Data (an output object of multiple potential types)
#' @rdname update_with_imputed
#' @export 
#' @importFrom ready4use Ready4useDyad
#' @importFrom purrr reduce pluck
#' @importFrom dplyr mutate across case_when
#' @keywords internal
update_with_imputed <- function (data_xx, impute_ls, lup_ls, args_ls = (list(NULL)), 
    fns_ls = list(sample)) 
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    X_Ready4useDyad@ds_tb <- 1:length(impute_ls) %>% purrr::reduce(.init = X_Ready4useDyad@ds_tb, 
        ~{
            variables_chr <- impute_ls %>% purrr::pluck(.y)
            lup <- lup_ls %>% purrr::pluck(.y)
            .x %>% dplyr::mutate(dplyr::across(variables_chr, 
                ~dplyr::case_when(is.na(.) ~ "BATMAN", TRUE ~ 
                  .)))
        })
    if (inherits(data_xx, "Ready4useDyad")) {
        data_xx <- X_Ready4useDyad
    }
    else {
        data_xx <- X_Ready4useDyad@ds_tb
    }
    return(data_xx)
}
