#' Transform age groups
#' @description transform_age_groups() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform age groups. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param age_bands_lup Age bands (a lookup table)
#' @param index_1L_chr Index (a character vector of length one)
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param names_from_1L_chr Names from (a character vector of length one)
#' @param age_group_var_1L_chr Age group variable (a character vector of length one), Default: 'AgeGroup'
#' @param do_not_group_chr Do not group (a character vector), Default: character(0)
#' @param rename_to_1L_chr Rename to (a character vector of length one), Default: character(0)
#' @param strict_1L_lgl Strict (a logical vector of length one), Default: TRUE
#' @param values_to_1L_chr Values to (a character vector of length one), Default: 'Value'
#' @return Data (a tibble)
#' @rdname transform_age_groups
#' @export 
#' @importFrom dplyr filter mutate case_when across select rename
#' @importFrom rlang sym
#' @importFrom purrr flatten_chr map_dfr reduce
#' @importFrom tidyr pivot_longer pivot_wider
#' @keywords internal
transform_age_groups <- function (data_tb, age_bands_lup, index_1L_chr, key_vars_chr = character(0), 
    names_from_1L_chr, age_group_var_1L_chr = "AgeGroup", do_not_group_chr = character(0), 
    rename_to_1L_chr = character(0), strict_1L_lgl = TRUE, values_to_1L_chr = "Value") 
{
    if (strict_1L_lgl) {
        data_tb <- data_tb %>% dplyr::filter((!!rlang::sym(age_group_var_1L_chr) %in% 
            (age_bands_lup$Source %>% purrr::flatten_chr())))
    }
    if (identical(rename_to_1L_chr, character(0))) {
        rename_1L_lgl <- FALSE
        rename_to_1L_chr <- "TransformedAgeGroup"
    }
    else {
        rename_1L_lgl <- TRUE
    }
    data_tb <- 1:nrow(age_bands_lup) %>% purrr::map_dfr(~{
        band_1L_chr <- age_bands_lup$Name[.x]
        match_chr <- age_bands_lup$Source[[.x]]
        fraction_dbl <- age_bands_lup$Fraction[[.x]]
        reshaped_tb <- purrr::reduce(1:length(match_chr), .init = data_tb %>% 
            dplyr::filter(!!rlang::sym(age_group_var_1L_chr) %in% 
                match_chr), ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(band_1L_chr, 
            "_", match_chr[.y])), dplyr::case_when(!!rlang::sym(age_group_var_1L_chr) == 
            match_chr[.y] ~ !!rlang::sym(values_to_1L_chr) * 
            fraction_dbl[.y], TRUE ~ 0)))) %>% dplyr::mutate(`:=`(!!rlang::sym(band_1L_chr), 
            rowSums(dplyr::across(paste0(band_1L_chr, "_", match_chr))))) %>% 
            dplyr::select(-c(paste0(band_1L_chr, "_", match_chr), 
                values_to_1L_chr)) %>% tidyr::pivot_longer(band_1L_chr, 
            names_to = rename_to_1L_chr, values_to = values_to_1L_chr)
        reshaped_tb <- reshaped_tb %>% make_metrics_summary(index_1L_chr = index_1L_chr, 
            key_vars_chr = c(setdiff(key_vars_chr, c(rename_to_1L_chr, 
                age_group_var_1L_chr)), rename_to_1L_chr, names_from_1L_chr), 
            metrics_chr = values_to_1L_chr)
        reshaped_tb <- reshaped_tb %>% tidyr::pivot_wider(names_from = names_from_1L_chr, 
            values_from = values_to_1L_chr)
        if (!rename_1L_lgl) {
            reshaped_tb <- dplyr::rename(reshaped_tb, `:=`(!!rlang::sym(age_group_var_1L_chr), 
                !!rlang::sym(rename_to_1L_chr)))
        }
        reshaped_tb
    })
    return(data_tb)
}
#' Transform data format
#' @description transform_data_fmt() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform data format. The function returns Data (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param type_1L_chr Type (a character vector of length one), Default: c("output", "input")
#' @return Data (an output object of multiple potential types)
#' @rdname transform_data_fmt
#' @export 
#' @importFrom ready4use Ready4useDyad add_dictionary
#' @importFrom dplyr filter
#' @keywords internal
transform_data_fmt <- function (data_xx, X_Ready4useDyad = ready4use::Ready4useDyad(), 
    type_1L_chr = c("output", "input")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "input") {
        if (!inherits(data_xx, "Ready4useDyad")) {
            data_xx <- ready4use::Ready4useDyad(ds_tb = data_xx) %>% 
                ready4use::add_dictionary()
        }
    }
    else {
        if (inherits(data_xx, "Ready4useDyad")) {
            X_Ready4useDyad@dictionary_r3 <- dplyr::filter(X_Ready4useDyad@dictionary_r3, 
                var_nm_chr %in% names(X_Ready4useDyad@ds_tb))
            X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>% 
                dplyr::filter(!duplicated(var_nm_chr))
            data_xx <- X_Ready4useDyad
        }
        else {
            data_xx <- X_Ready4useDyad@ds_tb
        }
    }
    return(data_xx)
}
#' Transform output
#' @description transform_output() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform output. The function returns Output (a list).
#' @param output_ls Output (a list)
#' @return Output (a list)
#' @rdname transform_output
#' @export 
#' @importFrom purrr map flatten_dbl
#' @keywords internal
transform_output <- function (output_ls) 
{
    output_ls <- output_ls %>% purrr::map(~ifelse(is.null(.x), 
        0, .x)) %>% purrr::flatten_dbl()
    return(output_ls)
}
#' Transform to model input
#' @description transform_to_mdl_input() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to model input. The function returns Data (a tsibble).
#' @param data_xx Data (an output object of multiple potential types)
#' @param ts_models_ls Time series models (a list), Default: make_ts_models_ls()
#' @return Data (a tsibble)
#' @rdname transform_to_mdl_input
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom rlang exec
#' @importFrom dplyr left_join
#' @keywords internal
transform_to_mdl_input <- function (data_xx, ts_models_ls = make_ts_models_ls()) 
{
    assertthat::assert_that(!identical(ts_models_ls, make_ts_models_ls()))
    args_ls <- ts_models_ls$args_ls
    cumulatives_args_ls <- ts_models_ls$cumulatives_args_ls
    predictor_args_ls <- ts_models_ls$predictor_args_ls
    join_to_args_ls <- ts_models_ls$join_to_args_ls
    data_tsb <- rlang::exec(get_tsibble, data_xx = data_xx, !!!args_ls)
    if (!identical(predictor_args_ls, make_tfmn_args_ls())) {
        predictors_tsb <- rlang::exec(get_tsibble, data_xx = data_xx, 
            !!!predictor_args_ls)
        data_tsb <- dplyr::left_join(data_tsb, predictors_tsb)
    }
    if (!identical(cumulatives_args_ls, make_tfmn_args_ls())) {
        cumulatives_tsb <- rlang::exec(get_tsibble, data_xx = data_xx, 
            !!!cumulatives_args_ls)
        data_tsb <- dplyr::left_join(data_tsb, cumulatives_tsb)
    }
    if (!identical(join_to_args_ls, make_tfmn_args_ls())) {
        join_to_xx <- join_to_args_ls$join_to_xx
        join_to_args_ls$join_to_xx <- NULL
        join_to_tsb <- rlang::exec(get_tsibble, data_xx = join_to_xx, 
            !!!join_to_args_ls)
        data_tsb <- dplyr::left_join(data_tsb, join_to_tsb)
    }
    return(data_tsb)
}
#' Transform to shorthand
#' @description transform_to_shorthand() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to shorthand. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param key_1L_chr Key (a character vector of length one), Default: character(0)
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param original_xx Original (an output object of multiple potential types), Default: character(0)
#' @param x_ready4show_correspondences PARAM_DESCRIPTION, Default: ready4show::ready4show_correspondences()
#' @return Data (a tibble)
#' @rdname transform_to_shorthand
#' @export 
#' @importFrom ready4show ready4show_correspondences manufacture.ready4show_correspondences
#' @importFrom tsibble is_tsibble index key_vars as_tibble tsibble
#' @importFrom dplyr mutate pull
#' @importFrom rlang sym
#' @keywords internal
transform_to_shorthand <- function (data_tb, key_1L_chr = character(0), min_1L_int = 3L, 
    original_xx = character(0), x_ready4show_correspondences = ready4show::ready4show_correspondences()) 
{
    index_1L_chr <- character(0)
    if (tsibble::is_tsibble(data_tb)) {
        index_1L_chr <- tsibble::index(data_tb) %>% as.character()
        key_vars_chr <- tsibble::key_vars(data_tb)
        data_tb <- tsibble::as_tibble(data_tb)
    }
    if (nrow(x_ready4show_correspondences) == 0) {
        x_ready4show_correspondences <- make_new_correspondences(data_tb, 
            key_1L_chr = key_1L_chr, min_1L_int = min_1L_int, 
            original_xx = original_xx)
    }
    if (!identical(original_xx, character(0))) {
        rename_chr <- match(names(data_tb), x_ready4show_correspondences$old_nms_chr)
        names(data_tb)[na.omit(rename_chr)] <- x_ready4show_correspondences$new_nms_chr[!is.na(rename_chr)]
    }
    else {
        data_tb <- dplyr::mutate(data_tb, `:=`(!!rlang::sym(key_1L_chr), 
            ready4show::manufacture.ready4show_correspondences(x_ready4show_correspondences, 
                data_ls = list(data_tb %>% dplyr::pull(!!rlang::sym(key_1L_chr))), 
                flatten_1L_lgl = T)))
    }
    if (!tsibble::is_tsibble(data_tb) && !identical(index_1L_chr, 
        character(0))) {
        data_tb <- tsibble::tsibble(data_tb, index = index_1L_chr, 
            key = key_vars_chr)
    }
    return(data_tb)
}
#' Transform to temporal
#' @description transform_to_temporal() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to temporal. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()
#' @param metrics_chr Metrics (a character vector)
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: c("category", "name")
#' @param dictionary_r3 Dictionary (a ready4 submodule), Default: ready4use::ready4use_dictionary()
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param temporal_vars_chr Temporal variables (a character vector), Default: make_temporal_vars()
#' @return No return value, called for side effects.
#' @rdname transform_to_temporal
#' @export 
#' @importFrom ready4use Ready4useDyad ready4use_dictionary renew.ready4use_dictionary update_dyad
#' @importFrom purrr map_chr
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
transform_to_temporal <- function (X_Ready4useDyad = ready4use::Ready4useDyad(), metrics_chr, 
    arrange_by_1L_chr = c("category", "name"), dictionary_r3 = ready4use::ready4use_dictionary(), 
    index_1L_chr = "Date", key_vars_chr = character(0), temporal_vars_chr = make_temporal_vars()) 
{
    arrange_by_1L_chr <- match.arg(arrange_by_1L_chr)
    X_Ready4useDyad@ds_tb <- transform_to_tsibble(X_Ready4useDyad@ds_tb, 
        index_1L_chr = index_1L_chr, key_vars_chr = key_vars_chr, 
        metrics_chr = metrics_chr)
    if (identical(dictionary_r3, ready4use::ready4use_dictionary())) {
        dictionary_r3 <- ready4use::renew.ready4use_dictionary(ready4use::ready4use_dictionary(), 
            var_nm_chr = setdiff(names(X_Ready4useDyad@ds_tb), 
                X_Ready4useDyad@dictionary_r3$var_nm_chr), var_ctg_chr = "Temporal", 
            var_desc_chr = setdiff(names(X_Ready4useDyad@ds_tb), 
                X_Ready4useDyad@dictionary_r3$var_nm_chr), var_type_chr = setdiff(names(X_Ready4useDyad@ds_tb), 
                X_Ready4useDyad@dictionary_r3$var_nm_chr) %>% 
                purrr::map_chr(~class(X_Ready4useDyad@ds_tb %>% 
                  dplyr::pull(!!rlang::sym(.x)))[1]))
    }
    X_Ready4useDyad <- ready4use::update_dyad(X_Ready4useDyad, 
        arrange_1L_chr = arrange_by_1L_chr, dictionary_r3 = dictionary_r3, 
        what_1L_chr = "dictionary")
}
#' Transform to tsibble
#' @description transform_to_tsibble() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to tsibble. The function returns Data (a tsibble).
#' @param data_tb Data (a tibble)
#' @param date_tfmn_fn Date transformation (a function), Default: identity
#' @param focused_args_ls Focused arguments (a list), Default: NULL
#' @param focused_fn Focused (a function), Default: NULL
#' @param index_1L_chr Index (a character vector of length one), Default: 'Date'
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param temporal_vars_chr Temporal variables (a character vector), Default: make_temporal_vars()
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "focused")
#' @return Data (a tsibble)
#' @rdname transform_to_tsibble
#' @export 
#' @importFrom tsibble as_tsibble
#' @importFrom tidyselect all_of
#' @importFrom rlang exec
transform_to_tsibble <- function (data_tb, date_tfmn_fn = identity, focused_args_ls = NULL, 
    focused_fn = NULL, index_1L_chr = "Date", key_vars_chr = character(0), 
    metrics_chr = make_metric_vars(), temporal_vars_chr = make_temporal_vars(), 
    type_1L_chr = c("main", "focused")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "main") {
        data_tb <- make_metrics_summary(data_tb, index_1L_chr = index_1L_chr, 
            key_vars_chr = key_vars_chr, metrics_chr = metrics_chr)
        if (identical(key_vars_chr, character(0))) {
            cdl_key_xx <- NULL
        }
        else {
            cdl_key_xx <- key_vars_chr
        }
        data_tsb <- data_tb %>% tsibble::as_tsibble(index = index_1L_chr, 
            key = tidyselect::all_of(cdl_key_xx))
        if (!identical(temporal_vars_chr, character(0))) {
            data_tsb <- add_temporal_vars(data_tsb, date_var_1L_chr = index_1L_chr, 
                temporal_vars_chr = temporal_vars_chr, fiscal_start_1L_int = fiscal_start_1L_int) %>% 
                tsibble::as_tsibble(index = index_1L_chr, key = key_vars_chr)
        }
    }
    else {
        data_tsb <- rlang::exec(focused_fn, data_tb, !!!focused_args_ls)
    }
    return(data_tsb)
}
