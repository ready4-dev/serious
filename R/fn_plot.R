#' Plot autocorrelations
#' @description plot_autocorrelations() is a Plot function that plots data. Specifically, this function implements an algorithm to plot autocorrelations. The function returns Plot (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param max_1L_int Maximum (an integer vector of length one), Default: NULL
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @return Plot (an output object of multiple potential types)
#' @rdname plot_autocorrelations
#' @export 
#' @importFrom purrr map2 pluck
#' @importFrom feasts autoplot
#' @importFrom ggplot2 labs
#' @importFrom Hmisc capitalize
#' @keywords internal
plot_autocorrelations <- function (data_xx, frequency_1L_chr = c("daily", "weekly", "monthly", 
    "quarterly", "yearly"), max_1L_int = NULL, metrics_chr = make_metric_vars()) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    autocorrelations_ls <- calculate_autocorrelations(data_xx = data_xx, 
        frequency_1L_chr = frequency_1L_chr, max_1L_int = max_1L_int, 
        metrics_chr = metrics_chr)
    plot_ls <- purrr::map2(autocorrelations_ls, names(autocorrelations_ls), 
        ~{
            .x %>% feasts::autoplot() + ggplot2::labs(title = paste0(Hmisc::capitalize(frequency_1L_chr), 
                " Autocorrelations For ", .y))
        })
    if (length(plot_ls) > 1) {
        plot_xx <- plot_ls
    }
    else {
        plot_xx <- plot_ls %>% purrr::pluck(1)
    }
    return(plot_xx)
}
#' Plot decomposition
#' @description plot_decomposition() is a Plot function that plots data. Specifically, this function implements an algorithm to plot decomposition. The function returns Plot (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param colours_chr Colours (a character vector), Default: c("gray", "#D55E00")
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "adjusted", "trend")
#' @param x_label_1L_chr X label (a character vector of length one), Default: ''
#' @return Plot (an output object of multiple potential types)
#' @rdname plot_decomposition
#' @export 
#' @importFrom purrr map pluck
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom fabletools model components
#' @importFrom feasts STL autoplot
#' @importFrom tsibble as_tsibble
#' @importFrom ggplot2 geom_line aes labs
#' @keywords internal
plot_decomposition <- function (data_xx, colours_chr = c("gray", "#D55E00"), frequency_1L_chr = c("daily", 
    "weekly", "monthly", "quarterly", "yearly"), key_totals_ls = NULL, 
    key_vars_chr = character(0), metrics_chr = make_metric_vars(), 
    what_1L_chr = c("all", "adjusted", "trend"), x_label_1L_chr = "") 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    plot_ls <- metrics_chr %>% purrr::map(~{
        slim_tsb <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr, 
            key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr, 
            metrics_chr = .x) %>% dplyr::select(!!rlang::sym(.x))
        STL_mdl <- eval(parse(text = paste0("tsibble::fill_gaps(slim_tsb, ", 
            .x, "=0)"))) %>% fabletools::model(stl = feasts::STL())
        components_xx <- fabletools::components(STL_mdl)
        if (what_1L_chr == "trend") {
            components_xx %>% tsibble::as_tsibble() %>% feasts::autoplot(!!rlang::sym(.x), 
                colour = colours_chr[1]) + ggplot2::geom_line(ggplot2::aes(y = trend), 
                colour = colours_chr[2]) + ggplot2::labs(y = .x, 
                x = x_label_1L_chr, title = paste0(.x, " trend"))
        }
        else {
            if (what_1L_chr == "adjusted") {
                components_xx %>% tsibble::as_tsibble() %>% feasts::autoplot(!!rlang::sym(.x), 
                  colour = colours_chr[1]) + ggplot2::geom_line(ggplot2::aes(y = season_adjust), 
                  colour = colours_chr[2]) + ggplot2::labs(y = .x, 
                  x = x_label_1L_chr, title = paste0(.x, " (seasonally adjusted)"))
            }
            else {
                if (what_1L_chr == "all") {
                  feasts::autoplot(components_xx) + ggplot2::labs(x = x_label_1L_chr, 
                    title = paste0(.x, " STL decomposition"))
                }
            }
        }
    })
    if (length(plot_ls) > 1) {
        plot_xx <- plot_ls
    }
    else {
        plot_xx <- plot_ls %>% purrr::pluck(1)
    }
    return(plot_xx)
}
#' Plot forecast
#' @description plot_forecast() is a Plot function that plots data. Specifically, this function implements an algorithm to plot forecast. The function returns Plot (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param ts_models_ls Time series models (a list)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param x_label_1L_chr X label (a character vector of length one), Default: ''
#' @return Plot (an output object of multiple potential types)
#' @rdname plot_forecast
#' @export 
#' @importFrom tsibble index filter_index
#' @importFrom dplyr pull select
#' @importFrom rlang sym
#' @importFrom purrr map pluck
#' @importFrom feasts autoplot autolayer
#' @importFrom ggplot2 labs guides guide_legend
#' @importFrom Hmisc capitalize
#' @keywords internal
plot_forecast <- function (data_xx, ts_models_ls, metrics_chr = make_metric_vars(), 
    x_label_1L_chr = "") 
{
    data_tsb <- transform_to_mdl_input(data_xx, ts_models_ls = ts_models_ls)
    index_1L_chr <- tsibble::index(data_tsb) %>% as.character()
    dates_chr <- data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% 
        as.character()
    plot_ls <- intersect(names(ts_models_ls$fabels_ls), metrics_chr) %>% 
        purrr::map(~{
            measure_1L_chr <- .x
            training_tsb <- make_training_ds(data_tsb, index_1L_chr = index_1L_chr, 
                test_1L_int = ts_models_ls$test_1L_int)
            fable_fbl <- ts_models_ls$fabels_ls %>% purrr::pluck(.x)
            if (identical(ts_models_ls$test_1L_int, integer(0))) {
                fable_fbl %>% feasts::autoplot(training_tsb) + 
                  ggplot2::labs(x = x_label_1L_chr, title = paste0(Hmisc::capitalize(ts_models_ls$args_ls$frequency_1L_chr), 
                    " Forecasts For ", .x))
            }
            else {
                fable_fbl %>% feasts::autoplot(training_tsb, 
                  level = NULL) + feasts::autolayer(tsibble::filter_index(data_tsb %>% 
                  dplyr::select(!!rlang::sym(index_1L_chr), !!rlang::sym(.x)), 
                  dates_chr[(length(dates_chr) - ts_models_ls$test_1L_int)] ~ 
                    .), colour = "black") + ggplot2::labs(y = measure_1L_chr, 
                  x = x_label_1L_chr, title = paste0("Forecasts For ", 
                    Hmisc::capitalize(ts_models_ls$args_ls$frequency_1L_chr), 
                    " ", .x)) + ggplot2::guides(colour = ggplot2::guide_legend(title = "Forecast"))
            }
        })
    if (length(plot_ls) > 1) {
        plot_xx <- plot_ls
    }
    else {
        plot_xx <- plot_ls %>% purrr::pluck(1)
    }
    return(plot_xx)
}
#' Plot lags
#' @description plot_lags() is a Plot function that plots data. Specifically, this function implements an algorithm to plot lags. The function returns Plot (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param arrow_1L_lgl Arrow (a logical vector of length one), Default: F
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param lags_int Lags (an integer vector), Default: integer(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param period_1L_chr Period (a character vector of length one), Default: NULL
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Lagged '
#' @param type_1L_chr Type (a character vector of length one), Default: c("path", "point")
#' @param ... Additional arguments
#' @return Plot (an output object of multiple potential types)
#' @rdname plot_lags
#' @export 
#' @importFrom purrr map pluck
#' @importFrom rlang exec sym
#' @importFrom feasts gg_lag
#' @importFrom dplyr select
#' @importFrom ggplot2 labs
#' @keywords internal
plot_lags <- function (data_xx, arrow_1L_lgl = F, frequency_1L_chr = c("daily", 
    "weekly", "monthly", "quarterly", "yearly"), key_totals_ls = NULL, 
    key_vars_chr = character(0), lags_int = integer(0), metrics_chr = make_metric_vars(), 
    period_1L_chr = NULL, prefix_1L_chr = "Lagged ", type_1L_chr = c("path", 
        "point"), ...) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    extras_ls <- list(...)
    if (identical(lags_int, integer(0))) {
        lags_int <- switch(frequency_1L_chr, daily = 1:7, weekly = 1:12, 
            monthly = 1:24, quarterly = 1:8, yearly = 1:2)
    }
    legend_1L_chr <- switch(frequency_1L_chr, daily = "Day", 
        weekly = "Week", monthly = "Month", quarterly = "Quarter", 
        yearly = "Year")
    data_tsb <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr, 
        key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr, 
        metrics_chr = metrics_chr)
    plot_ls <- metrics_chr %>% purrr::map(~{
        args_ls <- list(geom = type_1L_chr, lags = lags_int, 
            arrow = arrow_1L_lgl, period = period_1L_chr) %>% 
            append(extras_ls)
        plt <- rlang::exec(feasts::gg_lag, data_tsb %>% dplyr::select(!!rlang::sym(.x)), 
            !!!args_ls)
        plt + ggplot2::labs(x = paste0(prefix_1L_chr, .x), color = legend_1L_chr)
    })
    if (length(plot_ls) > 1) {
        plot_xx <- plot_ls
    }
    else {
        plot_xx <- plot_ls %>% purrr::pluck(1)
    }
    return(plot_xx)
}
#' Plot multiple
#' @description plot_multiple() is a Plot function that plots data. Specifically, this function implements an algorithm to plot multiple. The function is called for its side effects and does not return a value.
#' @param tsibbles_xx Tsibbles (an output object of multiple potential types)
#' @param by_value_1L_lgl By value (a logical vector of length one), Default: F
#' @param caption_1L_chr Caption (a character vector of length one), Default: ''
#' @param clinical_vars_chr Clinical variables (a character vector), Default: make_clinical_vars()
#' @param keep_cdn_1L_chr Keep condition (a character vector of length one), Default: c("All", "Personal", "Provider", "Severity", "Sports")
#' @param label_x_1L_chr Label x (a character vector of length one), Default: ''
#' @param label_y_1L_chr Label y (a character vector of length one), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param metrics_int Metrics (an integer vector), Default: 1:4
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: ''
#' @param severity_1L_int Severity (an integer vector of length one), Default: 8
#' @param shorten_1L_chr Shorten (a character vector of length one), Default: character(0)
#' @param shorten_1L_lgl Shorten (a logical vector of length one), Default: FALSE
#' @param sports_vars_chr Sports variables (a character vector), Default: get_sports_vars()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "seasonal")
#' @param y_suffix_1L_chr Y suffix (a character vector of length one), Default: character(0)
#' @return No return value, called for side effects.
#' @rdname plot_multiple
#' @export 
#' @importFrom tsibble is_tsibble is_duplicated index
#' @importFrom purrr keep_at map2 map
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @keywords internal
plot_multiple <- function (tsibbles_xx, by_value_1L_lgl = F, caption_1L_chr = "", 
    clinical_vars_chr = make_clinical_vars(), keep_cdn_1L_chr = c("All", 
        "Personal", "Provider", "Severity", "Sports"), label_x_1L_chr = "", 
    label_y_1L_chr = character(0), metrics_chr = make_metric_vars(), 
    metrics_int = 1:4, min_1L_int = 3L, prefix_1L_chr = "", severity_1L_int = 8L, 
    shorten_1L_chr = character(0), shorten_1L_lgl = FALSE, sports_vars_chr = get_sports_vars(), 
    suffix_1L_chr = character(0), type_1L_chr = c("main", "seasonal"), 
    y_suffix_1L_chr = character(0)) 
{
    keep_cdn_1L_chr <- match.arg(keep_cdn_1L_chr)
    if (!tsibble::is_tsibble(tsibbles_xx)) {
        keep_chr <- make_keepers(names(tsibbles_xx), clinical_vars_chr = clinical_vars_chr, 
            keep_cdn_1L_chr = keep_cdn_1L_chr, severity_1L_int = severity_1L_int, 
            sports_vars_chr = sports_vars_chr)
        tsibbles_xx <- tsibbles_xx %>% purrr::keep_at(keep_chr)
    }
    if (by_value_1L_lgl) {
        tsibbles_xx %>% purrr::map2(names(tsibbles_xx), ~{
            ds_tb <- .x
            var_1L_chr <- .y
            values_xx <- dplyr::pull(ds_tb, !!rlang::sym(var_1L_chr)) %>% 
                unique() %>% sort()
            metrics_chr %>% purrr::map(~{
                metric_1L_chr <- .x
                values_xx %>% purrr::map(~{
                  ds_tb %>% dplyr::filter(!!rlang::sym(var_1L_chr) == 
                    .x) %>% plot_sngl_series(caption_1L_chr = caption_1L_chr, 
                    label_x_1L_chr = label_x_1L_chr, label_y_1L_chr = label_y_1L_chr, 
                    metrics_chr = metrics_chr, min_1L_int = min_1L_int, 
                    prefix_1L_chr = prefix_1L_chr, shorten_1L_chr = shorten_1L_chr, 
                    shorten_1L_lgl = shorten_1L_lgl, suffix_1L_chr = paste0(" (", 
                      var_1L_chr, " - ", .x, ")", suffix_1L_chr), 
                    type_1L_chr = type_1L_chr, y_suffix_1L_chr = y_suffix_1L_chr, 
                    what_1L_chr = metric_1L_chr)
                })
            })
        })
    }
    else {
        if (tsibble::is_tsibble(tsibbles_xx)) {
            tsibbles_xx <- list(ds_tb = tsibbles_xx)
        }
        tsibbles_xx %>% purrr::map2(names(tsibbles_xx), ~{
            ds_tb <- .x
            grid_1L_lgl <- (ds_tb %>% tsibble::is_duplicated(index = tsibble::index(ds_tb)))
            var_1L_chr <- ifelse(.y == "ds_tb", "", paste0(" (", 
                ifelse(grid_1L_lgl, "By ", ""), .y, ")"))
            metrics_chr[metrics_int] %>% purrr::map(~plot_sngl_series(ds_tb, 
                caption_1L_chr = caption_1L_chr, grid_1L_lgl = grid_1L_lgl, 
                label_x_1L_chr = label_x_1L_chr, label_y_1L_chr = label_y_1L_chr, 
                metrics_chr = metrics_chr, min_1L_int = min_1L_int, 
                prefix_1L_chr = prefix_1L_chr, shorten_1L_chr = shorten_1L_chr, 
                shorten_1L_lgl = shorten_1L_lgl, suffix_1L_chr = paste0(var_1L_chr, 
                  suffix_1L_chr), type_1L_chr = type_1L_chr, 
                y_suffix_1L_chr = y_suffix_1L_chr, what_1L_chr = .x))
        })
    }
}
#' Plot residuals
#' @description plot_residuals() is a Plot function that plots data. Specifically, this function implements an algorithm to plot residuals. The function returns Residuals (a plot).
#' @param ts_models_ls Time series models (a list), Default: make_ts_models_ls()
#' @param var_1L_chr Variable (a character vector of length one)
#' @param model_1L_chr Model (a character vector of length one), Default: 'ARIMA'
#' @param type_1L_chr Type (a character vector of length one), Default: 'innovation'
#' @param ... Additional arguments
#' @return Residuals (a plot)
#' @rdname plot_residuals
#' @export 
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom feasts gg_tsresiduals
#' @keywords internal
plot_residuals <- function (ts_models_ls = make_ts_models_ls(), var_1L_chr, model_1L_chr = "ARIMA", 
    type_1L_chr = "innovation", ...) 
{
    residuals_plt <- ts_models_ls$mabels_ls[[var_1L_chr]] %>% 
        dplyr::select(!!rlang::sym(model_1L_chr)) %>% feasts::gg_tsresiduals(type = type_1L_chr, 
        ...)
    return(residuals_plt)
}
#' Plot scatter
#' @description plot_scatter() is a Plot function that plots data. Specifically, this function implements an algorithm to plot scatter. The function returns Plot (an output object of multiple potential types).
#' @param data_xx Data (an output object of multiple potential types)
#' @param axis_labels_1L_chr Axis labels (a character vector of length one), Default: c("none", "show", "inner")
#' @param caption_1L_chr Caption (a character vector of length one), Default: ''
#' @param clinical_vars_chr Clinical variables (a character vector), Default: make_clinical_vars()
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly")
#' @param grid_1L_lgl Grid (a logical vector of length one), Default: F
#' @param keep_cdn_1L_chr Keep condition (a character vector of length one), Default: c("All", "Personal", "Provider", "Severity", "Sports")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param severity_1L_int Severity (an integer vector of length one), Default: 8
#' @param shorten_1L_chr Shorten (a character vector of length one), Default: character(0)
#' @param shorten_1L_lgl Shorten (a logical vector of length one), Default: F
#' @param sports_vars_chr Sports variables (a character vector), Default: get_sports_vars()
#' @param type_1L_chr Type (a character vector of length one), Default: c("totals", "key", "wide")
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @return Plot (an output object of multiple potential types)
#' @rdname plot_scatter
#' @export 
#' @importFrom tsibble index
#' @importFrom dplyr select starts_with rename_with
#' @importFrom rlang sym
#' @importFrom purrr map map2
#' @importFrom stats setNames
#' @importFrom stringr str_split str_replace
#' @importFrom Hmisc capitalize
#' @importFrom ready4 make_list_phrase
#' @importFrom GGally ggpairs
#' @importFrom ggplot2 labs ggplot aes geom_point
#' @keywords internal
plot_scatter <- function (data_xx, axis_labels_1L_chr = c("none", "show", "inner"), 
    caption_1L_chr = "", clinical_vars_chr = make_clinical_vars(), 
    frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly", 
        "yearly"), grid_1L_lgl = F, keep_cdn_1L_chr = c("All", 
        "Personal", "Provider", "Severity", "Sports"), key_totals_ls = NULL, 
    key_vars_chr = character(0), metrics_chr = make_metric_vars(), 
    min_1L_int = 3L, severity_1L_int = 8L, shorten_1L_chr = character(0), 
    shorten_1L_lgl = F, sports_vars_chr = get_sports_vars(), 
    type_1L_chr = c("totals", "key", "wide"), what_1L_chr = character(0)) 
{
    axis_labels_1L_chr <- match.arg(axis_labels_1L_chr)
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    keep_cdn_1L_chr <- match.arg(keep_cdn_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    data_tsb <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr, 
        key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr, 
        metrics_chr = metrics_chr, type_1L_chr = type_1L_chr)
    if (grid_1L_lgl) {
        index_1L_chr <- data_tsb %>% tsibble::index() %>% as.character()
        if (type_1L_chr == "wide") {
            data_tsb <- data_tsb %>% dplyr::select(!!rlang::sym(index_1L_chr), 
                dplyr::starts_with(metrics_chr))
            combinations_chr <- sub("__[^__]+$", "", setdiff(data_tsb %>% 
                names(), c(index_1L_chr, metrics_chr))) %>% unique() %>% 
                sort()
            combinations_ls <- metrics_chr %>% purrr::map(~combinations_chr[combinations_chr %>% 
                startsWith(.x)]) %>% stats::setNames(metrics_chr)
            combinations_ls <- combinations_ls %>% purrr::map2(names(combinations_ls), 
                ~{
                  paste0(.y, "__", stringr::str_split(.x, "__", 
                    simplify = TRUE)[, 2] %>% make_keepers(clinical_vars_chr = clinical_vars_chr, 
                    keep_cdn_1L_chr = keep_cdn_1L_chr, severity_1L_int = severity_1L_int, 
                    sports_vars_chr = sports_vars_chr))
                })
        }
        else {
            data_tsb <- data_tsb %>% dplyr::select(!!rlang::sym(index_1L_chr), 
                metrics_chr)
            combinations_ls <- list(metrics_ls = list(metrics_chr))
        }
        plot_ls <- combinations_ls %>% purrr::map2(names(combinations_ls), 
            ~{
                variables_chr <- .x
                name_1L_chr <- .y
                variables_chr %>% purrr::map(~{
                  variable_1L_chr <- .x
                  df <- data_tsb %>% dplyr::select(dplyr::starts_with(variable_1L_chr)) %>% 
                    as.data.frame() %>% dplyr::select(-!!rlang::sym(index_1L_chr)) %>% 
                    dplyr::rename_with(.fn = function(x) {
                      stringr::str_replace(x, paste0(name_1L_chr, 
                        "__"), "")
                    })
                  if (shorten_1L_lgl) {
                    caption_1L_chr <- add_shorthand_to_caption(caption_1L_chr = caption_1L_chr, 
                      original_xx = names(df), min_1L_int = min_1L_int)
                    df <- df %>% transform_to_shorthand(original_xx = names(df))
                  }
                  if (type_1L_chr == "wide") {
                    title_1L_chr <- paste0("Correlations And Scatter Plots For ", 
                      Hmisc::capitalize(frequency_1L_chr), " ", 
                      name_1L_chr, " By ", variable_1L_chr %>% 
                        stringr::str_replace(paste0(name_1L_chr, 
                          "__"), ""))
                  }
                  else {
                    if (is.list(variables_chr)) {
                      phrase_1L_chr <- ready4::make_list_phrase(variables_chr[[1]])
                    }
                    else {
                      phrase_1L_chr <- ready4::make_list_phrase(variables_chr)
                    }
                    title_1L_chr <- paste0("Correlations and Scatter Plots for ", 
                      Hmisc::capitalize(frequency_1L_chr), " ", 
                      phrase_1L_chr)
                  }
                  df %>% GGally::ggpairs(axisLabels = axis_labels_1L_chr) + 
                    ggplot2::labs(caption = caption_1L_chr, title = title_1L_chr)
                })
            })
    }
    else {
        combinations_mat <- combn(unique(make_metric_vars()), 
            2)
        plot_ls <- 1:ncol(combinations_mat) %>% purrr::map(~{
            variables_chr <- combinations_mat[, .x]
            data_tsb %>% ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(variables_chr[1]), 
                y = !!rlang::sym(variables_chr[2]))) + ggplot2::geom_point() + 
                ggplot2::labs(x = variables_chr[1], y = variables_chr[2])
        })
    }
    plot_xx <- plot_ls
    if (length(plot_ls) == 1) {
        if (length(plot_ls[[1]]) == 1) {
            plot_xx <- plot_ls[[1]][[1]]
        }
    }
    return(plot_xx)
}
#' Plot series
#' @description plot_series() is a Plot function that plots data. Specifically, this function implements an algorithm to plot series. The function is called for its side effects and does not return a value.
#' @param data_xx Data (an output object of multiple potential types)
#' @param by_value_1L_lgl By value (a logical vector of length one), Default: FALSE
#' @param caption_1L_chr Caption (a character vector of length one), Default: ''
#' @param clinical_vars_chr Clinical variables (a character vector), Default: make_clinical_vars()
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal", 
#'    "sub")
#' @param keep_cdn_1L_chr Keep condition (a character vector of length one), Default: c("All", "Personal", "Provider", "Severity", "Sports")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param label_x_1L_chr Label x (a character vector of length one), Default: ''
#' @param label_y_1L_chr Label y (a character vector of length one), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param metrics_int Metrics (an integer vector), Default: 1:4
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: ''
#' @param severity_1L_int Severity (an integer vector of length one), Default: 8
#' @param shorten_1L_chr Shorten (a character vector of length one), Default: character(0)
#' @param shorten_1L_lgl Shorten (a logical vector of length one), Default: FALSE
#' @param sports_vars_chr Sports variables (a character vector), Default: get_sports_vars()
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "seasonal")
#' @param what_1L_chr What (a character vector of length one), Default: c("key", "totals", "wide")
#' @param y_suffix_1L_chr Y suffix (a character vector of length one), Default: character(0)
#' @return No return value, called for side effects.
#' @rdname plot_series
#' @export 
#' @importFrom tsibble is_tsibble
#' @importFrom assertthat assert_that
#' @importFrom purrr pluck
#' @keywords internal
plot_series <- function (data_xx, by_value_1L_lgl = FALSE, caption_1L_chr = "", 
    clinical_vars_chr = make_clinical_vars(), frequency_1L_chr = c("daily", 
        "weekly", "monthly", "quarterly", "yearly", "fiscal", 
        "sub"), keep_cdn_1L_chr = c("All", "Personal", "Provider", 
        "Severity", "Sports"), key_totals_ls = NULL, key_vars_chr = character(0), 
    label_x_1L_chr = "", label_y_1L_chr = character(0), metrics_chr = make_metric_vars(), 
    metrics_int = 1:4, min_1L_int = 3L, prefix_1L_chr = "", severity_1L_int = 8L, 
    shorten_1L_chr = character(0), shorten_1L_lgl = FALSE, sports_vars_chr = get_sports_vars(), 
    suffix_1L_chr = character(0), type_1L_chr = c("main", "seasonal"), 
    what_1L_chr = c("key", "totals", "wide"), y_suffix_1L_chr = character(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    if (tsibble::is_tsibble(data_xx)) {
        if (what_1L_chr %in% c("totals", "key", "wide")) {
            cndl_type_1L_chr <- what_1L_chr
            cndl_what_1L_chr <- character(0)
        }
        else {
            cndl_type_1L_chr <- "totals"
            cndl_what_1L_chr <- what_1L_chr
        }
        tsibbles_xx <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr, 
            key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr, 
            type_1L_chr = cndl_type_1L_chr, what_1L_chr = cndl_what_1L_chr)
    }
    else {
        assertthat::assert_that(is.list(data_xx))
        dss_ls <- data_xx
        tsibbles_xx <- dss_ls %>% purrr::pluck(which(c("key", 
            "totals", "wide") == what_1L_chr)) %>% purrr::pluck(frequency_1L_chr)
    }
    plot_multiple(tsibbles_xx, by_value_1L_lgl = by_value_1L_lgl, 
        caption_1L_chr = caption_1L_chr, clinical_vars_chr = clinical_vars_chr, 
        keep_cdn_1L_chr = keep_cdn_1L_chr, label_x_1L_chr = label_x_1L_chr, 
        label_y_1L_chr = label_y_1L_chr, metrics_chr = metrics_chr, 
        metrics_int = metrics_int, min_1L_int = min_1L_int, prefix_1L_chr = prefix_1L_chr, 
        severity_1L_int = severity_1L_int, shorten_1L_chr = shorten_1L_chr, 
        shorten_1L_lgl = shorten_1L_lgl, sports_vars_chr = sports_vars_chr, 
        suffix_1L_chr = suffix_1L_chr, type_1L_chr = type_1L_chr, 
        y_suffix_1L_chr = y_suffix_1L_chr)
}
#' Plot single series
#' @description plot_sngl_series() is a Plot function that plots data. Specifically, this function implements an algorithm to plot single series. The function is called for its side effects and does not return a value.
#' @param data_tsb Data (a tsibble)
#' @param caption_1L_chr Caption (a character vector of length one), Default: ''
#' @param grid_1L_lgl Grid (a logical vector of length one), Default: FALSE
#' @param label_x_1L_chr Label x (a character vector of length one), Default: ''
#' @param label_y_1L_chr Label y (a character vector of length one), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param min_1L_int Minimum (an integer vector of length one), Default: 3
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: ''
#' @param shorten_1L_chr Shorten (a character vector of length one), Default: character(0)
#' @param shorten_1L_lgl Shorten (a logical vector of length one), Default: FALSE
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "seasonal")
#' @param what_1L_chr What (a character vector of length one), Default: make_metric_vars()
#' @param y_suffix_1L_chr Y suffix (a character vector of length one), Default: character(0)
#' @return No return value, called for side effects.
#' @rdname plot_sngl_series
#' @export 
#' @importFrom dplyr pull
#' @importFrom tsibble index key_vars is_yearweek is_yearmonth is_yearquarter key fill_gaps
#' @importFrom ggplot2 ggplot aes geom_line facet_grid vars autoplot labs
#' @importFrom rlang sym
#' @importFrom purrr pluck
#' @importFrom feasts gg_season
#' @keywords internal
plot_sngl_series <- function (data_tsb, caption_1L_chr = "", grid_1L_lgl = FALSE, 
    label_x_1L_chr = "", label_y_1L_chr = character(0), metrics_chr = make_metric_vars(), 
    min_1L_int = 3L, prefix_1L_chr = "", shorten_1L_chr = character(0), 
    shorten_1L_lgl = FALSE, suffix_1L_chr = character(0), type_1L_chr = c("main", 
        "seasonal"), what_1L_chr = make_metric_vars(), y_suffix_1L_chr = character(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    index_dtm <- data_tsb %>% dplyr::pull(data_tsb %>% tsibble::index())
    if (!identical(shorten_1L_chr, character(0)) | shorten_1L_lgl) {
        if (shorten_1L_lgl) {
            key_vars_chr <- tsibble::key_vars(data_tsb)
            shorten_1L_chr <- key_vars_chr[1]
        }
        caption_1L_chr <- add_shorthand_to_caption(caption_1L_chr, 
            data_tsb = data_tsb, min_1L_int = min_1L_int, shorten_1L_chr = shorten_1L_chr)
        data_tsb <- data_tsb %>% transform_to_shorthand(key_1L_chr = shorten_1L_chr)
    }
    temporal_1L_chr <- "Daily"
    if (tsibble::is_yearweek(index_dtm)) {
        temporal_1L_chr <- "Weekly"
    }
    if (tsibble::is_yearmonth(index_dtm)) {
        temporal_1L_chr <- "Monthly"
    }
    if (tsibble::is_yearquarter(index_dtm)) {
        temporal_1L_chr <- "Quarterly"
    }
    if (identical(y_suffix_1L_chr, character(0))) {
        if (what_1L_chr == make_metric_vars()[4]) 
            y_suffix_1L_chr <- " ($)"
    }
    if (identical(label_y_1L_chr, character(0))) {
        label_y_1L_chr <- paste0(what_1L_chr, y_suffix_1L_chr)
    }
    if (type_1L_chr == "main") {
        if (grid_1L_lgl) {
            plot_fn <- function(ds_tb, y_1L_chr) {
                ds_tb %>% ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(ds_tb %>% 
                  tsibble::index() %>% as.character()), y = !!rlang::sym(y_1L_chr))) + 
                  ggplot2::geom_line() + ggplot2::facet_grid(ggplot2::vars(!!rlang::sym(ds_tb %>% 
                  tsibble::key() %>% purrr::pluck(1) %>% as.character())), 
                  scales = "free_y")
            }
        }
        else {
            plot_fn <- ggplot2::autoplot
        }
        tfmn_fn = identity
    }
    if (type_1L_chr == "seasonal") {
        plot_fn <- feasts::gg_season
        tfmn_fn <- tsibble::fill_gaps
    }
    if (grid_1L_lgl && type_1L_chr == "main") {
        plot_plt <- plot_fn(data_tsb %>% tfmn_fn, what_1L_chr)
    }
    else {
        plot_plt <- plot_fn(data_tsb %>% tfmn_fn, !!rlang::sym(what_1L_chr))
    }
    plot_plt + ggplot2::labs(y = label_y_1L_chr, x = label_x_1L_chr, 
        caption = caption_1L_chr, title = paste0(prefix_1L_chr, 
            temporal_1L_chr, " ", what_1L_chr, suffix_1L_chr))
}
#' Plot tsibble
#' @description plot_tsibble() is a Plot function that plots data. Specifically, this function implements an algorithm to plot tsibble. The function returns Plot (an output object of multiple potential types).
#' @param series_tsb Series (a tsibble)
#' @param auto_1L_lgl Automatic (a logical vector of length one), Default: TRUE
#' @param date_tfmn_fn Date transformation (a function), Default: NULL
#' @param facet_1L_lgl Facet (a logical vector of length one), Default: F
#' @param fiscal_start_1L_int Fiscal start (an integer vector of length one), Default: 7
#' @param frequency_1L_chr Frequency (a character vector of length one), Default: c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal", 
#'    "sub")
#' @param key_totals_ls Key totals (a list), Default: NULL
#' @param key_vars_chr Key variables (a character vector), Default: character(0)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Cumulative'
#' @param transform_1L_lgl Transform (a logical vector of length one), Default: TRUE
#' @param type_1L_chr Type (a character vector of length one), Default: c("main", "cumulative")
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @return Plot (an output object of multiple potential types)
#' @rdname plot_tsibble
#' @export 
#' @importFrom lubridate ym
#' @importFrom feasts autoplot
#' @importFrom fabletools vars
#' @importFrom rlang syms sym
#' @importFrom purrr map pluck
#' @importFrom tsibble index as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom dygraphs dygraph
#' @importFrom xts xts
#' @importFrom dplyr pull
#' @keywords internal
plot_tsibble <- function (series_tsb, auto_1L_lgl = TRUE, date_tfmn_fn = NULL, 
    facet_1L_lgl = F, fiscal_start_1L_int = 7L, frequency_1L_chr = c("daily", 
        "weekly", "monthly", "quarterly", "yearly", "fiscal", 
        "sub"), key_totals_ls = NULL, key_vars_chr = character(0), 
    metrics_chr = make_metric_vars(), prefix_1L_chr = "Cumulative", 
    transform_1L_lgl = TRUE, type_1L_chr = c("main", "cumulative"), 
    what_1L_chr = character(0)) 
{
    frequency_1L_chr <- match.arg(frequency_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (is.null(date_tfmn_fn)) {
        date_tfmn_fn <- get_temporal_fn(get_new_index(frequency_1L_chr), 
            monthly_fn = lubridate::ym)
    }
    if (transform_1L_lgl) {
        focused_tsb <- series_tsb %>% get_tsibble(frequency_1L_chr = frequency_1L_chr, 
            key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr, 
            metrics_chr = metrics_chr, prefix_1L_chr = prefix_1L_chr, 
            type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)
    }
    else {
        focused_tsb <- series_tsb
    }
    if (type_1L_chr == "cumulative") {
        metrics_chr <- paste0(prefix_1L_chr, metrics_chr)
    }
    if (auto_1L_lgl) {
        if (facet_1L_lgl) {
            plt_xx <- focused_tsb %>% feasts::autoplot(fabletools::vars(!!!rlang::syms(metrics_chr)))
        }
        else {
            plt_xx <- metrics_chr %>% purrr::map(~focused_tsb %>% 
                feasts::autoplot(!!rlang::sym(.x)))
            if (length(metrics_chr) == 1) {
                plt_xx <- plt_xx %>% purrr::pluck(1)
            }
        }
    }
    else {
        if (length(metrics_chr) > 1) {
            plt_xx <- metrics_chr %>% purrr::map(~plot_tsibble(focused_tsb, 
                auto_1L_lgl = FALSE, date_tfmn_fn = date_tfmn_fn, 
                fiscal_start_1L_int = fiscal_start_1L_int, frequency_1L_chr = frequency_1L_chr, 
                metrics_chr = .x, transform_1L_lgl = FALSE, what_1L_chr = what_1L_chr))
        }
        else {
            index_1L_chr <- tsibble::index(focused_tsb)
            ds_tb <- focused_tsb %>% tsibble::as_tibble()
            if (!identical(what_1L_chr, character(0))) 
                ds_tb <- ds_tb %>% tidyr::pivot_wider(names_from = what_1L_chr, 
                  values_from = metrics_chr)
            plt_xx <- dygraphs::dygraph(xts::xts(x = ds_tb, order.by = date_tfmn_fn(ds_tb %>% 
                dplyr::pull(!!rlang::sym(index_1L_chr)))))
        }
    }
    return(plt_xx)
}
#' Plot weekdays
#' @description plot_weekdays() is a Plot function that plots data. Specifically, this function implements an algorithm to plot weekdays. The function is called for its side effects and does not return a value.
#' @param dss_ls Datasets (a list)
#' @param metrics_chr Metrics (a character vector), Default: make_metric_vars()
#' @param what_1L_chr What (a character vector of length one), Default: 'Winter'
#' @return No return value, called for side effects.
#' @rdname plot_weekdays
#' @export 
#' @importFrom purrr map pluck
#' @importFrom dplyr summarise across where
#' @importFrom tsibble fill_gaps
#' @importFrom feasts gg_season
#' @importFrom rlang sym
#' @importFrom ggplot2 labs
#' @keywords internal
plot_weekdays <- function (dss_ls, metrics_chr = make_metric_vars(), what_1L_chr = "Winter") 
{
    metrics_chr %>% purrr::map(~{
        var_1L_chr <- .x
        dss_ls$key_dss_ls$daily %>% purrr::pluck(what_1L_chr) %>% 
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), 
                sum)) %>% tsibble::fill_gaps() %>% feasts::gg_season(!!rlang::sym(var_1L_chr), 
            period = "week") + ggplot2::labs(y = var_1L_chr, 
            x = "", title = paste0(var_1L_chr, " by day of week over time"))
    })
}
