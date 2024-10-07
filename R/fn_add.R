#' Add cumulatives
#' @description add_cumulatives() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cumulatives. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param metrics_chr Metrics (a character vector)
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: character(0)
#' @param dict_by_ctg_1L_chr Dictionary by category (a character vector of length one), Default: FALSE
#' @param group_by_1L_chr Group by (a character vector of length one), Default: character(0)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param starting_dbl Starting (a double vector), Default: 0
#' @return Data (an output object of multiple potential types)
#' @rdname add_cumulatives
#' @export
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary
#' @importFrom dplyr arrange group_by mutate across ungroup filter
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @importFrom purrr reduce
#' @keywords internal
add_cumulatives <- function (data_xx, metrics_chr, arrange_by_1L_chr = character(0),
    dict_by_ctg_1L_chr = FALSE, group_by_1L_chr = character(0),
    prefix_1L_chr = "Cumulative", starting_dbl = 0)
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    if (!identical(arrange_by_1L_chr, character(0))) {
        X_Ready4useDyad@ds_tb <- dplyr::arrange(X_Ready4useDyad@ds_tb,
            !!rlang::sym(arrange_by_1L_chr))
    }
    if (!identical(group_by_1L_chr, character(0))) {
        X_Ready4useDyad@ds_tb <- dplyr::group_by(X_Ready4useDyad@ds_tb,
            !!rlang::sym(group_by_1L_chr))
    }
    X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(metrics_chr),
        cumsum, .names = paste0(prefix_1L_chr, "{.col}")))
    if (length(starting_dbl) == 1 & length(metrics_chr) > 1) {
        starting_dbl <- rep(starting_dbl, length(metrics_chr))
    }
    X_Ready4useDyad@ds_tb <- 1:length(metrics_chr) %>% purrr::reduce(.init = X_Ready4useDyad@ds_tb,
        ~{
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(prefix_1L_chr,
                metrics_chr[.y])), !!rlang::sym(paste0(prefix_1L_chr,
                metrics_chr[.y])) + starting_dbl[.y]))
        })
    if (!identical(group_by_1L_chr, character(0))) {
        X_Ready4useDyad@ds_tb <- dplyr::ungroup(X_Ready4useDyad@ds_tb)
    }
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>%
            ready4use::renew.ready4use_dictionary(new_cases_r3 = X_Ready4useDyad@dictionary_r3 %>%
                dplyr::filter(var_nm_chr %in% metrics_chr, !var_nm_chr %in%
                  paste0(prefix_1L_chr, metrics_chr)) %>% dplyr::mutate(var_nm_chr = paste0(prefix_1L_chr,
                metrics_chr)))
        if (dict_by_ctg_1L_chr) {
            X_Ready4useDyad@dictionary_r3 <- dplyr::arrange(X_Ready4useDyad@dictionary_r3,
                var_ctg_chr)
        }
        data_xx <- X_Ready4useDyad
    }
    else {
        data_xx <- X_Ready4useDyad@ds_tb
    }
    return(data_xx)
}
#' Add cyclic cases
#' @description add_cyclic_cases() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add cyclic cases. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: character(0)
#' @param cycle_1L_chr Cycle (a character vector of length one), Default: c("years", "days", "weeks")
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param date_tfmn_fn Date transformation (a function), Default: lubridate::ymd
#' @param end_date_dtm End date (a date vector), Default: NULL
#' @param new_zeros_chr New zeros (a character vector), Default: character(0)
#' @return Data (a tibble)
#' @rdname add_cyclic_cases
#' @export
#' @importFrom lubridate ymd days weeks years interval
#' @importFrom dplyr pull mutate across bind_rows select arrange
#' @importFrom rlang sym
#' @importFrom purrr map_dfr
#' @importFrom tidyselect all_of
#' @keywords internal
add_cyclic_cases <- function (data_tb, arrange_by_1L_chr = character(0), cycle_1L_chr = c("years",
    "days", "weeks"), date_var_1L_chr = "Date", date_tfmn_fn = lubridate::ymd,
    end_date_dtm = NULL, new_zeros_chr = character(0))
{
    cycle_1L_chr <- match.arg(cycle_1L_chr)
    if (is.null(end_date_dtm)) {
        end_date_dtm <- max(data_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))) %>%
            date_tfmn_fn()
    }
    period_fn <- switch(cycle_1L_chr, days = lubridate::days,
        weeks = lubridate::weeks, years = lubridate::years)
    data_tb <- dplyr::mutate(data_tb, repeat_cycles_int = (lubridate::interval(data_tb %>%
        dplyr::pull(!!rlang::sym(date_var_1L_chr)) %>% date_tfmn_fn(),
        end_date_dtm)/period_fn(1)) %>% floor())
    extras_tb <- 1:nrow(data_tb) %>% purrr::map_dfr(~{
        cycles_1L_int <- data_tb[[.x, "repeat_cycles_int"]]
        extras_tb <- data_tb[rep(.x, cycles_1L_int), ]
        if (cycles_1L_int > 0) {
            extras_tb <- extras_tb %>% dplyr::mutate(`:=`(!!rlang::sym(date_var_1L_chr),
                !!rlang::sym(date_var_1L_chr) + period_fn(1:cycles_1L_int)))
        }
        if (!identical(new_zeros_chr, character(0))) {
            extras_tb <- extras_tb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(new_zeros_chr),
                ~0))
        }
    })
    data_tb <- dplyr::bind_rows(data_tb, extras_tb) %>% dplyr::select(-repeat_cycles_int)
    if (!identical(arrange_by_1L_chr, character(0))) {
        data_tb <- dplyr::arrange(data_tb, !!rlang::sym(arrange_by_1L_chr))
    }
    return(data_tb)
}
#' Add date variables
#' @description add_date_vars() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add date variables. The function returns Data.frame (a data.frame).
#' @param df Data.frame (a data.frame)
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @return Data.frame (a data.frame)
#' @rdname add_date_vars
#' @export
#' @importFrom dplyr mutate pull case_when
#' @importFrom rlang sym
#' @importFrom lubridate year isoweek
#' @importFrom purrr map_lgl
#' @keywords internal
add_date_vars <- function (df, date_var_1L_chr = "Date")
{
    df <- df %>% dplyr::mutate(Weekday = weekdays(df %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))))
    df <- df %>% dplyr::mutate(Year = lubridate::year(df %>%
        dplyr::pull(!!rlang::sym(date_var_1L_chr))) %>% as.integer())
    df <- df %>% dplyr::mutate(Week = lubridate::isoweek(df %>%
        dplyr::pull(!!rlang::sym(date_var_1L_chr))) %>% as.integer())
    df <- df %>% dplyr::mutate(Quarter = dplyr::case_when(Week %>%
        purrr::map_lgl(~.x < 14) ~ 1L, Week %>% purrr::map_lgl(~.x >
        13 && .x < 27) ~ 2L, Week %>% purrr::map_lgl(~.x > 26 &&
        .x < 40) ~ 3L, Week %>% purrr::map_lgl(~.x > 39) ~ 4L,
        T ~ NA_integer_))
    return(df)
}
#' Add disengaged
#' @description add_disengaged() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add disengaged. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param date_1L_chr Date (a character vector of length one)
#' @param category_1L_chr Category (a character vector of length one), Default: 'Healthcare'
#' @param date_tfmn_fn Date transformation (a function), Default: lubridate::ymd
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param description_1L_chr Description (a character vector of length one), Default: 'Disengaged'
#' @param dict_by_ctg_1L_chr Dictionary by category (a character vector of length one), Default: FALSE
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param var_nm_1L_chr Variable name (a character vector of length one), Default: 'Disengaged'
#' @return Data (an output object of multiple potential types)
#' @rdname add_disengaged
#' @export
#' @importFrom lubridate ymd
#' @importFrom ready4use Ready4useDyad renew.ready4use_dictionary ready4use_dictionary
#' @importFrom dplyr filter pull mutate arrange
#' @importFrom rlang sym
#' @importFrom tibble add_case
#' @keywords internal
add_disengaged <- function (data_xx, date_1L_chr, category_1L_chr = "Healthcare",
    date_tfmn_fn = lubridate::ymd, date_var_1L_chr = "Date",
    description_1L_chr = "Disengaged", dict_by_ctg_1L_chr = FALSE,
    uid_1L_chr = "UID", var_nm_1L_chr = "Disengaged")
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    active_xx <- X_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) >=
        date_tfmn_fn(date_1L_chr)) %>% dplyr::pull(!!rlang::sym(uid_1L_chr)) %>%
        unique()
    X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(var_nm_1L_chr),
        !(!!rlang::sym(uid_1L_chr) %in% active_xx)))
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>%
            dplyr::filter(var_nm_chr != var_nm_1L_chr) %>% ready4use::renew.ready4use_dictionary(new_cases_r3 = ready4use::ready4use_dictionary() %>%
            tibble::add_case(var_nm_chr = var_nm_1L_chr, var_ctg_chr = category_1L_chr,
                var_desc_chr = description_1L_chr, var_type_chr = "logical"))
        if (dict_by_ctg_1L_chr) {
            X_Ready4useDyad@dictionary_r3 <- dplyr::arrange(X_Ready4useDyad@dictionary_r3,
                var_ctg_chr)
        }
        else {
            X_Ready4useDyad@dictionary_r3 <- dplyr::arrange(X_Ready4useDyad@dictionary_r3,
                var_nm_chr)
        }
        data_xx <- X_Ready4useDyad
    }
    else {
        data_xx <- X_Ready4useDyad@ds_tb
    }
    return(data_xx)
}
#' Add episodes
#' @description add_episodes() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add episodes. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param separation_after_dbl Separation after (a double vector)
#' @param index_1L_int Index (an integer vector of length one), Default: integer(0)
#' @param active_var_1L_chr Active variable (a character vector of length one), Default: 'Active'
#' @param activity_var_1L_chr Activity variable (a character vector of length one), Default: 'Activity'
#' @param date_tfmn_fn Date transformation (a function), Default: lubridate::ymd
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param end_date_dtm End date (a date vector), Default: NULL
#' @param episode_var_1L_chr Episode variable (a character vector of length one), Default: 'Episodes'
#' @param episodes_vars_chr Episodes variables (a character vector), Default: character(0)
#' @param exclude_chr Exclude (a character vector), Default: 'Duration'
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param separations_var_1L_chr Separations variable (a character vector of length one), Default: 'Separations'
#' @param temporal_vars_chr Temporal variables (a character vector), Default: make_temporal_vars()
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID'
#' @param unit_1L_chr Unit (a character vector of length one), Default: 'month'
#' @return Data (an output object of multiple potential types)
#' @rdname add_episodes
#' @export
#' @importFrom lubridate ymd as.duration duration
#' @importFrom ready4use Ready4useDyad add_dictionary ready4use_dictionary make_pt_ready4use_dictionary
#' @importFrom dplyr pull mutate group_by arrange lag case_when row_number ungroup select filter summarise last across bind_rows
#' @importFrom rlang sym
#' @importFrom purrr reduce
#' @importFrom stringr str_remove
#' @importFrom tidyr fill
#' @importFrom tidyselect all_of
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
add_episodes <- function (data_xx, separation_after_dbl, index_1L_int = integer(0),
    active_var_1L_chr = "Active", activity_var_1L_chr = "Activity",
    date_tfmn_fn = lubridate::ymd, date_var_1L_chr = "Date",
    end_date_dtm = NULL, episode_var_1L_chr = "Episodes", episodes_vars_chr = character(0),
    exclude_chr = "Duration", fiscal_start_1L_int = 7L, metrics_chr = make_metric_vars(),
    prefix_1L_chr = "Cumulative", separations_var_1L_chr = "Separations",
    temporal_vars_chr = make_temporal_vars(), uid_1L_chr = "UID",
    unit_1L_chr = "month")
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    if (is.null(end_date_dtm)) {
        end_date_dtm <- max(X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)))
    }
    if (identical(index_1L_int, integer(0))) {
        episodes_vars_ls <- make_episodes_vars(active_var_1L_chr = active_var_1L_chr,
            episode_var_1L_chr = episode_var_1L_chr, separation_after_dbl = separation_after_dbl,
            separations_var_1L_chr = separations_var_1L_chr,
            flatten_1L_lgl = F)
        X_Ready4useDyad <- 1:length(separation_after_dbl) %>%
            purrr::reduce(.init = X_Ready4useDyad, ~.x %>% add_episodes(separation_after_dbl = separation_after_dbl,
                index_1L_int = .y, end_date_dtm = end_date_dtm,
                episodes_vars_chr = episodes_vars_ls[[.y]], unit_1L_chr = unit_1L_chr))
    }
    else {
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(episodes_vars_chr[1]),
            0), `:=`(!!rlang::sym(episodes_vars_chr[2]), 0),
            `:=`(!!rlang::sym(episodes_vars_chr[3]), 0), `:=`(!!rlang::sym(paste0(prefix_1L_chr,
                episodes_vars_chr[2])), 0), `:=`(!!rlang::sym(paste0(prefix_1L_chr,
                episodes_vars_chr[3])), 0)) %>% dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>%
            dplyr::arrange(!!rlang::sym(date_var_1L_chr)) %>%
            dplyr::mutate(Interval_Since_Last_OOS = ifelse(startsWith(dplyr::lag(!!rlang::sym(activity_var_1L_chr)),
                paste0("Separation", episodes_vars_chr[3] %>%
                  stringr::str_remove(separations_var_1L_chr))),
                NA, dplyr::lag(!!rlang::sym(date_var_1L_chr))) %>%
                as.Date()) %>% tidyr::fill(Interval_Since_Last_OOS) %>%
            dplyr::mutate(Interval_Since_Last_OOS = dplyr::case_when(is.na(Interval_Since_Last_OOS) ~
                !!rlang::sym(date_var_1L_chr), TRUE ~ Interval_Since_Last_OOS))
        X_Ready4useDyad@ds_tb$Interval_Since_Last_OOS <- (X_Ready4useDyad@ds_tb %>%
            dplyr::pull(!!rlang::sym(date_var_1L_chr)) - X_Ready4useDyad@ds_tb$Interval_Since_Last_OOS) %>%
            lubridate::as.duration()
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(episodes_vars_chr[2]),
            dplyr::case_when((Interval_Since_Last_OOS > lubridate::duration(separation_after_dbl[index_1L_int],
                units = unit_1L_chr)) & !startsWith(!!rlang::sym(activity_var_1L_chr),
                "Separation") ~ 1, dplyr::row_number() == 1 ~
                1, TRUE ~ 0))) %>% dplyr::mutate(`:=`(!!rlang::sym(episodes_vars_chr[1]),
            !!rlang::sym(episodes_vars_chr[2]))) %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(prefix_1L_chr,
            episodes_vars_chr[2])), cumsum(!!rlang::sym(episodes_vars_chr[2])))) %>%
            dplyr::ungroup() %>% dplyr::arrange(!!rlang::sym(uid_1L_chr)) %>%
            dplyr::select(-Interval_Since_Last_OOS)
        separations_tb <- X_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(activity_var_1L_chr) !=
            paste0("Separation", episodes_vars_chr[3] %>% stringr::str_remove(separations_var_1L_chr))) %>%
            dplyr::group_by(!!rlang::sym(uid_1L_chr), !!rlang::sym(paste0(prefix_1L_chr,
                episodes_vars_chr[2]))) %>% dplyr::summarise(`:=`(!!rlang::sym(date_var_1L_chr),
            (dplyr::last(!!rlang::sym(date_var_1L_chr)) + lubridate::duration(separation_after_dbl[index_1L_int],
                units = unit_1L_chr)) %>% as.Date() %>% date_tfmn_fn()),
            `:=`(!!rlang::sym(activity_var_1L_chr), paste0("Separation",
                episodes_vars_chr[3] %>% stringr::str_remove(separations_var_1L_chr))),
            dplyr::across(c(setdiff(metrics_chr, episodes_vars_chr),
                episodes_vars_chr[2]), ~0), `:=`(!!rlang::sym(episodes_vars_chr[3]),
                1), `:=`(!!rlang::sym(episodes_vars_chr[1]),
                -1), dplyr::across(setdiff(names(X_Ready4useDyad@ds_tb),
                c(uid_1L_chr, date_var_1L_chr, make_episodes_vars(active_var_1L_chr = active_var_1L_chr,
                  episode_var_1L_chr = episode_var_1L_chr, separation_after_dbl = separation_after_dbl,
                  separations_var_1L_chr = separations_var_1L_chr,
                  flatten_1L_lgl = T), paste0(prefix_1L_chr,
                  episodes_vars_chr[3]), activity_var_1L_chr,
                  temporal_vars_chr, metrics_chr, exclude_chr,
                  paste0(prefix_1L_chr, episodes_vars_chr[2]))),
                dplyr::last), dplyr::across(intersect(setdiff(make_episodes_vars(active_var_1L_chr = active_var_1L_chr,
                episode_var_1L_chr = episode_var_1L_chr, separation_after_dbl = separation_after_dbl,
                separations_var_1L_chr = separations_var_1L_chr,
                flatten_1L_lgl = T), episodes_vars_chr), names(X_Ready4useDyad@ds_tb)),
                ~0)) %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(prefix_1L_chr,
            episodes_vars_chr[3])), cumsum(!!rlang::sym(episodes_vars_chr[3])))) %>%
            dplyr::ungroup() %>% dplyr::filter(!!rlang::sym(date_var_1L_chr) <=
            end_date_dtm) %>% add_temporal_vars(date_var_1L_chr = date_var_1L_chr,
            fiscal_start_1L_int = fiscal_start_1L_int, temporal_vars_chr = temporal_vars_chr)
        X_Ready4useDyad@ds_tb <- X_Ready4useDyad@ds_tb %>% dplyr::bind_rows(separations_tb) %>%
            dplyr::arrange(!!rlang::sym(uid_1L_chr), !!rlang::sym(date_var_1L_chr)) %>%
            dplyr::select(tidyselect::all_of(c(uid_1L_chr, date_var_1L_chr,
                setdiff(names(X_Ready4useDyad@ds_tb), c(uid_1L_chr,
                  date_var_1L_chr, activity_var_1L_chr, episodes_vars_chr[2],
                  metrics_chr, c(episodes_vars_chr[3], episodes_vars_chr[1]),
                  paste0(prefix_1L_chr, c(metrics_chr, episodes_vars_chr[2:3])),
                  temporal_vars_chr)), c(activity_var_1L_chr,
                  episodes_vars_chr[2], metrics_chr, c(episodes_vars_chr[3],
                    episodes_vars_chr[1]), paste0(prefix_1L_chr,
                    c(episodes_vars_chr[2], metrics_chr, episodes_vars_chr[3])),
                  temporal_vars_chr))))
        if (inherits(data_xx, "Ready4useDyad")) {
            X_Ready4useDyad <- ready4use::add_dictionary(X_Ready4useDyad,
                new_cases_r3 = ready4use::ready4use_dictionary(ready4use::make_pt_ready4use_dictionary(var_nm_chr = c(episodes_vars_chr,
                  paste0(prefix_1L_chr, episodes_vars_chr[2:3])),
                  var_ctg_chr = ready4::get_from_lup_obj(X_Ready4useDyad@dictionary_r3,
                    match_var_nm_1L_chr = "var_nm_chr", match_value_xx = metrics_chr[1],
                    target_var_nm_1L_chr = "var_ctg_chr"), var_desc_chr = c(episodes_vars_chr,
                    paste0(prefix_1L_chr, episodes_vars_chr[2:3])),
                  var_type_chr = "numeric")))
        }
    }
    if (inherits(data_xx, "Ready4useDyad")) {
        data_xx <- X_Ready4useDyad
    }
    else {
        data_xx <- X_Ready4useDyad@ds_tb
    }
    return(data_xx)
}
#' Add fabels
#' @description add_fabels() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add fabels. The function returns Time series models (a list).
#' @param ts_models_ls Time series models (a list)
#' @param data_xx Data (an output object of multiple potential types), Default: NULL
#' @param periods_1L_int Periods (an integer vector of length one), Default: integer(0)
#' @return Time series models (a list)
#' @rdname add_fabels
#' @export
#' @importFrom purrr map2
#' @importFrom fabletools forecast
#' @importFrom stats setNames
#' @keywords internal
add_fabels <- function (ts_models_ls, data_xx = NULL, periods_1L_int = integer(0))
{
    if (identical(periods_1L_int, integer(0))) {
        periods_1L_int <- ts_models_ls$test_1L_int
    }
    if (identical(periods_1L_int, integer(0))) {
        stop("Supply a positive integer value to periods_1L_int")
    }
    if (!is.null(data_xx)) {
        new_data_tsb <- get_tsibble(data_xx, frequency_1L_chr = ts_models_ls$args_ls$frequency_1L_chr,
            key_totals_ls = ts_models_ls$args_ls$key_totals_ls,
            key_vars_chr = ts_models_ls$args_ls$key_vars_chr,
            type_1L_chr = ts_models_ls$args_ls$type_1L_chr, what_1L_chr = ts_models_ls$args_ls$what_1L_chr)
        if (!identical(ts_models_ls$test_1L_int, integer(0))) {
            new_data_tsb <- new_data_tsb %>% tail(ts_models_ls$test_1L_int)
        }
    }
    else {
        new_data_tsb <- NULL
    }
    fabels_ls <- ts_models_ls$mabels_ls %>% purrr::map2(names(ts_models_ls$mabels_ls),
        ~.x %>% fabletools::forecast(h = periods_1L_int, new_data = new_data_tsb)) %>%
        stats::setNames(names(ts_models_ls$mabels_ls))
    ts_models_ls <- append(ts_models_ls, list(fabels_ls = fabels_ls))
    return(ts_models_ls)
}
#' Add new unique identifier
#' @description add_new_uid() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add new unique identifier. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param uid_vars_chr Unique identifier variables (a character vector)
#' @param uid_pfx_1L_chr Unique identifier prefix (a character vector of length one)
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: character(0)
#' @param drop_old_uids_1L_lgl Drop old unique identifiers (a logical vector of length one), Default: FALSE
#' @param new_uid_var_1L_chr New unique identifier variable (a character vector of length one), Default: 'UID'
#' @param imputed_uid_pfx_chr Imputed unique identifier prefix (a character vector), Default: 'UNK'
#' @param place_first_1L_lgl Place first (a logical vector of length one), Default: TRUE
#' @param recode_1L_lgl Recode (a logical vector of length one), Default: FALSE
#' @param recode_pfx_1L_chr Recode prefix (a character vector of length one), Default: 'Person_'
#' @return Data (a tibble)
#' @rdname add_new_uid
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr pull select mutate case_when filter bind_rows arrange everything
#' @importFrom purrr discard map2_chr pluck map_chr
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na
#' @importFrom rlang sym
#' @importFrom ready4 get_from_lup_obj
#' @importFrom youthvars add_uids_to_tbs_ls
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @keywords internal
add_new_uid <- function (data_tb, uid_vars_chr, uid_pfx_1L_chr, arrange_by_1L_chr = character(0),
    drop_old_uids_1L_lgl = FALSE, new_uid_var_1L_chr = "UID",
    imputed_uid_pfx_chr = "UNK", place_first_1L_lgl = TRUE, recode_1L_lgl = FALSE,
    recode_pfx_1L_chr = "Person_")
{
    test_1L_lgl <- assertthat::assert_that(!any(startsWith(data_tb %>%
        dplyr::pull(uid_vars_chr[2]) %>% unique() %>% purrr::discard(is.na),
        imputed_uid_pfx_chr)), msg = "Prefix for imputed identifiers must not be the same as the prefix used for the secondary identifier")
    if (length(uid_vars_chr) > 1) {
        test_1L_lgl <- assertthat::assert_that(!any(startsWith(data_tb %>%
            dplyr::pull(uid_vars_chr[2]) %>% unique() %>% purrr::discard(is.na),
            uid_pfx_1L_chr)), msg = "Secondary identifier variable must not have same prefix as used for primary identifier")
        test_1L_lgl <- assertthat::assert_that(!identical(imputed_uid_pfx_chr,
            uid_pfx_1L_chr), msg = "Prefix for imputed identifiers must be the same as the prefix used for the primary identifier")
        uid_lup <- data_tb %>% dplyr::select(tidyselect::all_of(uid_vars_chr[1:2])) %>%
            tidyr::drop_na()
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(new_uid_var_1L_chr),
            !!rlang::sym(uid_vars_chr[1]) %>% purrr::map2_chr(!!rlang::sym(uid_vars_chr[2]),
                ~ifelse(is.na(.x), ifelse(.y %in% (uid_lup %>%
                  dplyr::pull(uid_vars_chr[2])), ready4::get_from_lup_obj(uid_lup,
                  match_var_nm_1L_chr = uid_vars_chr[2], match_value_xx = .y,
                  target_var_nm_1L_chr = uid_vars_chr[1]), NA_character_),
                  .x)))) %>% dplyr::mutate(`:=`(!!rlang::sym(new_uid_var_1L_chr),
            dplyr::case_when(is.na(!!rlang::sym(new_uid_var_1L_chr)) ~
                !!rlang::sym(uid_vars_chr[1]) %>% purrr::map2_chr(!!rlang::sym(uid_vars_chr[2]),
                  ~ifelse(is.na(.x), .y, .x)), T ~ !!rlang::sym(new_uid_var_1L_chr))))
    }
    complete_ids_tb <- data_tb %>% dplyr::filter(!is.na(!!rlang::sym(new_uid_var_1L_chr)))
    imputed_ids_tb <- list(data_tb %>% dplyr::filter(is.na(!!rlang::sym(new_uid_var_1L_chr)))) %>%
        youthvars::add_uids_to_tbs_ls(prefix_1L_chr = imputed_uid_pfx_chr,
            id_var_nm_1L_chr = new_uid_var_1L_chr) %>% purrr::pluck(1)
    data_tb <- dplyr::bind_rows(complete_ids_tb, imputed_ids_tb)
    if (drop_old_uids_1L_lgl) {
        data_tb <- dplyr::select(data_tb, -tidyselect::all_of(uid_vars_chr))
    }
    if (!identical(arrange_by_1L_chr, character(0))) {
        data_tb <- dplyr::arrange(data_tb, !!rlang::sym(arrange_by_1L_chr))
    }
    if (place_first_1L_lgl) {
        data_tb <- data_tb %>% dplyr::select(!!rlang::sym(new_uid_var_1L_chr),
            dplyr::everything())
    }
    if (recode_1L_lgl) {
        unique_chr <- data_tb %>% dplyr::pull(!!rlang::sym(new_uid_var_1L_chr)) %>%
            unique()
        correspondences_r3 <- ready4show:::ready4show_correspondences() %>%
            ready4show::renew.ready4show_correspondences(old_nms_chr = unique_chr,
                new_nms_chr = paste0(recode_pfx_1L_chr, sprintf(paste0("%0",
                  length(unique_chr) %>% nchar() - 1, "d"), 1:length(unique_chr))))
        data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(new_uid_var_1L_chr),
            !!rlang::sym(new_uid_var_1L_chr) %>% purrr::map_chr(~ready4::get_from_lup_obj(correspondences_r3,
                match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x,
                target_var_nm_1L_chr = "new_nms_chr"))))
    }
    return(data_tb)
}
#' Add retainers
#' @description add_retainers() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add retainers. The function is called for its side effects and does not return a value.
#' @param data_tb Data (a tibble)
#' @param datasets_ls Datasets (a list)
#' @param cost_var_1L_chr Cost variable (a character vector of length one), Default: 'Retainer amount'
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Retainer date'
#' @return No return value, called for side effects.
#' @rdname add_retainers
#' @export
#' @importFrom dplyr full_join
#' @keywords internal
add_retainers <- function (data_tb, datasets_ls, cost_var_1L_chr = "Retainer amount",
    date_var_1L_chr = "Retainer date")
{
    dplyr::full_join(data_tb, make_retainers(datasets_ls$retainer,
        cost_var_1L_chr = cost_var_1L_chr, date_var_1L_chr = date_var_1L_chr))
}
#' Add sampled imputations
#' @description add_sampled_imputations() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add sampled imputations. The function is called for its side effects and does not return a value.
#' @param data_xx Data (an output object of multiple potential types)
#' @param groupings_chr Groupings (a character vector)
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param minimum_1L_int Minimum (an integer vector of length one), Default: 10
#' @return No return value, called for side effects.
#' @rdname add_sampled_imputations
#' @export
#' @importFrom ready4use Ready4useDyad
#' @importFrom purrr discard_at reduce pluck keep
#' @importFrom dplyr rename pull left_join group_by mutate case_when first n ungroup
#' @importFrom rlang sym syms
#' @keywords internal
add_sampled_imputations <- function (data_xx, groupings_chr, exclude_chr = character(0),
    minimum_1L_int = 10L)
{
    if (inherits(data_xx, "Ready4useDyad")) {
        X_Ready4useDyad <- data_xx
    }
    else {
        X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
    }
    roll_backs_ls <- make_roll_backs_ls(X_Ready4useDyad, groupings_chr = groupings_chr,
        exclude_chr = exclude_chr, minimum_1L_int = minimum_1L_int)
    main_lup <- roll_backs_ls$main_lup
    roll_backs_ls <- roll_backs_ls %>% purrr::discard_at("main_lup")
    if (!identical(roll_backs_ls %>% unname(), list())) {
        X_Ready4useDyad@ds_tb <- names(roll_backs_ls)[1] %>%
            purrr::reduce(.init = X_Ready4useDyad@ds_tb, ~{
                frequencies_lup <- purrr::pluck(roll_backs_ls,
                  .y)
                frequencies_lup <- frequencies_lup %>% dplyr::rename(Frequency_Tables_ls = !!rlang::sym(.y))
                missing_1L_xx <- .x %>% dplyr::pull(!!rlang::sym(.y)) %>%
                  purrr::keep(is.na) %>% purrr::pluck(1)
                .x %>% dplyr::left_join(frequencies_lup) %>%
                  dplyr::group_by(!!!rlang::syms(setdiff(names(frequencies_lup),
                    "Frequency_Tables_ls"))) %>% dplyr::mutate(`:=`(!!rlang::sym(.y),
                  dplyr::case_when(is.na(!!rlang::sym(.y)) ~
                    dplyr::first(Frequency_Tables_ls) %>% make_sampled_values(draws_int = dplyr::n(),
                      fail_with_xx = missing_1L_xx), TRUE ~ !!rlang::sym(.y)))) %>%
                  dplyr::ungroup()
            })
    }
}
#' Add sampled records
#' @description add_sampled_records() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add sampled records. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param seed_lup Seed (a lookup table)
#' @param uid_var_nm_1L_chr Unique identifier variable name (a character vector of length one), Default: 'Client ID'
#' @return Dataset (a tibble)
#' @rdname add_sampled_records
#' @export
#' @importFrom dplyr mutate pull inner_join
#' @importFrom rlang sym
#' @keywords internal
add_sampled_records <- function (ds_tb, seed_lup, uid_var_nm_1L_chr = "Client ID")
{
    ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(uid_var_nm_1L_chr),
        sample(seed_lup %>% dplyr::pull(!!rlang::sym(uid_var_nm_1L_chr)),
            size = nrow(ds_tb), replace = TRUE))) %>% dplyr::inner_join(seed_lup)
    return(ds_tb)
}
#' Add sampled variable
#' @description add_sampled_variable() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add sampled variable. The function returns Clients (a tibble).
#' @param clients_tb Clients (a tibble)
#' @param seed_ds_tb Seed dataset (a tibble)
#' @param shares_dbl Shares (a double vector)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Client_'
#' @param scale_1L_dbl Scale (a double vector of length one), Default: 100
#' @param uid_var_nm_1L_chr Unique identifier variable name (a character vector of length one), Default: 'Client ID'
#' @param var_nm_1L_chr Variable name (a character vector of length one), Default: 'Sex'
#' @return Clients (a tibble)
#' @rdname add_sampled_variable
#' @export
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
add_sampled_variable <- function (clients_tb, seed_ds_tb, shares_dbl, prefix_1L_chr = "Client_",
    scale_1L_dbl = 100, uid_var_nm_1L_chr = "Client ID", var_nm_1L_chr = "Sex")
{
    seed_lup <- make_sampling_lup(shares_dbl, values_xx = seed_ds_tb %>%
        dplyr::pull(!!rlang::sym(var_nm_1L_chr)) %>% unique() %>%
        sort(), var_nm_1L_chr = var_nm_1L_chr, prefix_1L_chr = prefix_1L_chr,
        scale_1L_dbl = scale_1L_dbl, uid_var_nm_1L_chr = uid_var_nm_1L_chr)
    clients_tb <- add_sampled_records(clients_tb, seed_lup = seed_lup,
        uid_var_nm_1L_chr = uid_var_nm_1L_chr)
    return(clients_tb)
}
#' Add shorthand to caption
#' @description add_shorthand_to_caption() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add shorthand to caption. The function returns Caption (a character vector of length one).
#' @param caption_1L_chr Caption (a character vector of length one), Default: ''
#' @param data_tsb Data (a tsibble), Default: NULL
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param original_xx Original (an output object of multiple potential types), Default: character(0)
#' @param shorten_1L_chr Shorten (a character vector of length one), Default: character(0)
#' @return Caption (a character vector of length one)
#' @rdname add_shorthand_to_caption
#' @export
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_trim str_wrap
#' @keywords internal
add_shorthand_to_caption <- function (caption_1L_chr = "", data_tsb = NULL, min_1L_int = 3L,
    original_xx = character(0), shorten_1L_chr = character(0))
{
    x_ready4show_correspondences <- make_correspondences(data_tsb,
        key_1L_chr = shorten_1L_chr, min_1L_int = min_1L_int,
        original_xx = original_xx)
    if (sum(x_ready4show_correspondences$old_nms_chr == x_ready4show_correspondences$new_nms_chr) <
        nrow(x_ready4show_correspondences)) {
        caption_1L_chr <- c(caption_1L_chr, purrr::pmap_chr(x_ready4show_correspondences,
            ~paste0(..2, "-", stringr::str_trim(..1)))) %>% paste0(collapse = "  ") %>%
            stringr::str_wrap()
    }
    return(caption_1L_chr)
}
#' Add temporal variables
#' @description add_temporal_vars() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add temporal variables. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @param temporal_vars_chr Temporal variables (a character vector), Default: make_temporal_vars()
#' @return Data (a tibble)
#' @rdname add_temporal_vars
#' @export
#' @importFrom purrr reduce
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_temporal_vars <- function (data_tb, date_var_1L_chr = "Date", fiscal_start_1L_int = 7L,
    temporal_vars_chr = make_temporal_vars())
{
    data_tb <- temporal_vars_chr %>% purrr::reduce(.init = data_tb,
        ~{
            date_tfmn_fn <- get_temporal_fn(.y)
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), date_tfmn_fn(!!rlang::sym(date_var_1L_chr))))
        })
    return(data_tb)
}
#' Add tenure
#' @description add_tenure() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add tenure. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param tenure_var_1L_chr Tenure variable (a character vector of length one), Default: 'Tenure'
#' @param uid_var_1L_chr Unique identifier variable (a character vector of length one), Default: 'UID'
#' @param unit_1L_chr Unit (a character vector of length one), Default: 'year'
#' @return Data (a tibble)
#' @rdname add_tenure
#' @export
#' @importFrom dplyr group_by mutate first ungroup select everything
#' @importFrom rlang sym
#' @importFrom lubridate time_length
#' @keywords internal
add_tenure <- function (data_tb, date_var_1L_chr = "Date", tenure_var_1L_chr = "Tenure",
    uid_var_1L_chr = "UID", unit_1L_chr = "year")
{
    data_tb <- data_tb %>% dplyr::group_by(!!rlang::sym(uid_var_1L_chr)) %>%
        dplyr::mutate(`:=`(!!rlang::sym(tenure_var_1L_chr), (!!rlang::sym(date_var_1L_chr) -
            dplyr::first(!!rlang::sym(date_var_1L_chr))) %>%
            lubridate::time_length(unit = unit_1L_chr))) %>%
        dplyr::ungroup() %>% dplyr::select(!!rlang::sym(uid_var_1L_chr),
        !!rlang::sym(date_var_1L_chr), !!rlang::sym(tenure_var_1L_chr),
        dplyr::everything())
    return(data_tb)
}
