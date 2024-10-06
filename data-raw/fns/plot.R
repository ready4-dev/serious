plot_autocorrelations <- function(data_xx,
                                  frequency_1L_chr = c("daily","weekly",
                                                       "monthly", "quarterly", "yearly"),
                                  max_1L_int = NULL,
                                  metrics_chr = make_metric_vars()){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  autocorrelations_ls <- calculate_autocorrelations(data_xx = data_xx,
                                                    frequency_1L_chr = frequency_1L_chr,
                                                    max_1L_int = max_1L_int,
                                                    metrics_chr = metrics_chr)
  purrr::map2(autocorrelations_ls, names(autocorrelations_ls),
              ~{
                .x %>%
                  feasts::autoplot() +
                  ggplot2::labs(title=paste0(Hmisc::capitalize(frequency_1L_chr)," Autocorrelations For ",.y))
              })

}
plot_decomposition <- function(data_xx,
                               colours_chr = c("gray","#D55E00"),
                               frequency_1L_chr = c("daily","weekly",
                                                    "monthly", "quarterly", "yearly"),
                               key_totals_ls = NULL,
                               key_vars_chr = character(0),
                               metrics_chr = make_metric_vars(),
                               what_1L_chr = c("all","adjusted","trend"),
                               x_label_1L_chr = ""){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  metrics_chr %>%
    purrr::map(~{
      slim_tsb <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr,
                              key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                              metrics_chr = .x)  %>% dplyr::select(!!rlang::sym(.x))
      STL_mdl <- eval(parse(text = paste0("tsibble::fill_gaps(slim_tsb, ",.x,"=0)") )) %>%
        #tsibble::fill_gaps(Active=0) %>%
        fabletools::model(stl = feasts::STL())
      components_xx <- fabletools::components(STL_mdl)
      if(what_1L_chr=="trend"){
        components_xx %>% tsibble::as_tsibble() %>%
          feasts:: autoplot(!!rlang::sym(.x), colour=colours_chr[1]) +
          ggplot2::geom_line(ggplot2::aes(y=trend), colour = colours_chr[2]) +
          ggplot2::labs(
            y = .x,
            x = x_label_1L_chr,
            title = paste0(.x, " trend"))
      }else{
        if(what_1L_chr=="adjusted"){
          components_xx %>% tsibble::as_tsibble() %>%
            feasts:: autoplot(!!rlang::sym(.x), colour=colours_chr[1]) +
            ggplot2::geom_line(ggplot2::aes(y=season_adjust), colour = colours_chr[2]) +
            ggplot2::labs(
              y = .x,
              x = x_label_1L_chr,
              title = paste0(.x, " (seasonally adjusted)"))
        }else{
          if(what_1L_chr=="all"){
            feasts::autoplot(components_xx) +
              ggplot2::labs(x = x_label_1L_chr,    title = paste0(.x, " STL decomposition"))
          }
        }
      }


    })
}
plot_forecast <- function(data_xx,
                          ts_models_ls,
                          metrics_chr = make_metric_vars(),
                          x_label_1L_chr = ""){
  data_tsb <- rlang::exec(get_tsibble, data_xx = data_xx, !!!ts_models_ls$args_ls)
  index_1L_chr <- tsibble::index(data_tsb) %>% as.character()
  dates_chr <- data_tsb %>% dplyr::pull(!!rlang::sym(index_1L_chr)) %>% as.character()
  intersect(names(ts_models_ls$fabels_ls), metrics_chr) %>%
    purrr::map(~{
      measure_1L_chr <- .x
      training_tsb <- make_training_ds(data_tsb,
                                       index_1L_chr =index_1L_chr,
                                       test_1L_int = ts_models_ls$test_1L_int)
      fable_fbl <- ts_models_ls$fabels_ls %>% purrr::pluck(.x)
      if(identical(ts_models_ls$test_1L_int, integer(0))){
        fable_fbl %>%
          feasts::autoplot(training_tsb) +
          ggplot2::labs(x = x_label_1L_chr,
                        title=paste0(Hmisc::capitalize(ts_models_ls$args_ls$frequency_1L_chr)," Forecasts For ",.x))


      }else{
        fable_fbl %>%
          feasts::autoplot(training_tsb, level = NULL) +
          feasts::autolayer( # or fabletools::
            tsibble::filter_index(data_tsb %>% dplyr::select(!!rlang::sym(index_1L_chr), !!rlang::sym(.x)), dates_chr[(length(dates_chr)-ts_models_ls$test_1L_int)] ~ .),
            colour = "black") +
          ggplot2::labs(
            y = measure_1L_chr,
            x = x_label_1L_chr,
            title = paste0("Forecasts For ",Hmisc::capitalize(ts_models_ls$args_ls$frequency_1L_chr)," ",.x)
          ) +
          ggplot2::guides(colour = ggplot2::guide_legend(title = "Forecast"))
      }
    })
}
plot_lags <- function(data_xx,
                      frequency_1L_chr = c("daily","weekly",
                                           "monthly", "quarterly", "yearly"),
                      key_totals_ls = NULL,
                      key_vars_chr = character(0),
                      metrics_chr = make_metric_vars(),
                      prefix_1L_chr = "Lagged "){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  metrics_chr %>%
    purrr::map(~{
      get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr,
                  key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                  metrics_chr = metrics_chr)  %>%
        feasts::gg_lag(!!rlang::sym(.x), geom = "point") +
        ggplot2::labs(x = paste0(prefix_1L_chr, .x))
    })

}
plot_multiple <- function(tsibbles_xx,
                          by_value_1L_lgl = F,
                          caption_1L_chr = "",
                          clinical_vars_chr = make_clinical_vars(),
                          keep_cdn_1L_chr = c("All", "Personal", "Provider", "Severity", "Sports"),
                          label_x_1L_chr = "",
                          label_y_1L_chr = character(0),
                          metrics_chr = make_metric_vars(),
                          metrics_int = 1:4,
                          min_1L_int = 3L,
                          prefix_1L_chr = "",
                          severity_1L_int = 8L,
                          shorten_1L_chr = character(0),
                          shorten_1L_lgl = FALSE,
                          sports_vars_chr = get_sports_vars(),
                          suffix_1L_chr = character(0),
                          type_1L_chr = c("main", "seasonal"),
                          y_suffix_1L_chr = character(0)){
  keep_cdn_1L_chr <- match.arg(keep_cdn_1L_chr)
  if(!tsibble::is_tsibble(tsibbles_xx)){
    keep_chr <- make_keepers(names(tsibbles_xx),
                             clinical_vars_chr = clinical_vars_chr,
                             keep_cdn_1L_chr = keep_cdn_1L_chr,
                             severity_1L_int = severity_1L_int,
                             sports_vars_chr = sports_vars_chr)
    # keep_chr <- names(tsibbles_xx)
    # if(keep_cdn_1L_chr != "All"){
    #   if(keep_cdn_1L_chr == "Personal")
    #     keep_chr <-setdiff(keep_chr, c(clinical_vars_chr, sports_vars_chr))
    #   if(keep_cdn_1L_chr == "Provider")
    #     keep_chr <- intersect(keep_chr, clinical_vars_chr[-8])
    #   if(keep_cdn_1L_chr == "Severity")
    #     keep_chr <- intersect(keep_chr, clinical_vars_chr[8])
    #   if(keep_cdn_1L_chr == "Sports")
    #     keep_chr <- intersect(keep_chr, sports_vars_chr)
    # }
    tsibbles_xx <- tsibbles_xx %>% purrr::keep_at(keep_chr)
  }
  if(by_value_1L_lgl){
    tsibbles_xx %>%
      purrr::map2(names(tsibbles_xx),
                  ~{
                    ds_tb <- .x
                    var_1L_chr <- .y
                    values_xx <- dplyr::pull(ds_tb, !!rlang::sym(var_1L_chr)) %>% unique() %>% sort()
                    metrics_chr %>%
                      purrr::map(~{
                        metric_1L_chr <- .x
                        values_xx %>%
                          purrr::map(~{
                            ds_tb %>%
                              dplyr::filter(!!rlang::sym(var_1L_chr) == .x) %>%
                              plot_sngl_series(caption_1L_chr = caption_1L_chr,
                                               label_x_1L_chr = label_x_1L_chr,
                                               label_y_1L_chr = label_y_1L_chr,
                                               metrics_chr = metrics_chr,
                                               min_1L_int = min_1L_int,
                                               prefix_1L_chr = prefix_1L_chr,
                                               shorten_1L_chr = shorten_1L_chr,
                                               shorten_1L_lgl = shorten_1L_lgl,
                                               suffix_1L_chr = paste0(" (",var_1L_chr," - ",.x,")", suffix_1L_chr),
                                               type_1L_chr = type_1L_chr,
                                               y_suffix_1L_chr = y_suffix_1L_chr,
                                               what_1L_chr = metric_1L_chr)
                          })
                      })

                  })
  }else{
    if(tsibble::is_tsibble(tsibbles_xx)){
      tsibbles_xx <- list(ds_tb = tsibbles_xx)
    }
    tsibbles_xx %>%
      purrr::map2(names(tsibbles_xx),
                  ~{
                    ds_tb <- .x
                    grid_1L_lgl <- (ds_tb %>% tsibble::is_duplicated(index = tsibble::index(ds_tb)))
                    var_1L_chr <- ifelse(.y == "ds_tb","",paste0(" (",
                                                                 ifelse(grid_1L_lgl,"By ",""),
                                                                 .y,")"))
                    metrics_chr[metrics_int] %>%
                      purrr::map(~plot_sngl_series(ds_tb,
                                                   caption_1L_chr = caption_1L_chr,
                                                   grid_1L_lgl = grid_1L_lgl,
                                                   label_x_1L_chr = label_x_1L_chr,
                                                   label_y_1L_chr = label_y_1L_chr,
                                                   metrics_chr = metrics_chr,
                                                   min_1L_int = min_1L_int,
                                                   prefix_1L_chr = prefix_1L_chr,
                                                   shorten_1L_chr = shorten_1L_chr,
                                                   shorten_1L_lgl = shorten_1L_lgl,
                                                   suffix_1L_chr = paste0(var_1L_chr,suffix_1L_chr),
                                                   type_1L_chr = type_1L_chr,
                                                   y_suffix_1L_chr = y_suffix_1L_chr,
                                                   what_1L_chr = .x))
                  })

  }
}
plot_scatter <- function(data_xx,
                         axis_labels_1L_chr = c("none","show","inner"),
                         caption_1L_chr = "",
                         clinical_vars_chr = make_clinical_vars(),
                         frequency_1L_chr = c("daily","weekly",
                                              "monthly", "quarterly", "yearly"),
                         grid_1L_lgl = F,
                         keep_cdn_1L_chr = c("All", "Personal", "Provider", "Severity", "Sports"),
                         key_totals_ls = NULL,
                         key_vars_chr = character(0),
                         metrics_chr = make_metric_vars(),
                         min_1L_int = 3L,
                         severity_1L_int = 8L,
                         shorten_1L_chr = character(0),
                         shorten_1L_lgl = F,
                         sports_vars_chr = get_sports_vars(),
                         type_1L_chr = c("totals","key", "wide"),
                         what_1L_chr = character(0)){
  axis_labels_1L_chr <- match.arg(axis_labels_1L_chr)
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  keep_cdn_1L_chr <- match.arg(keep_cdn_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  data_tsb <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr,
                          key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                          metrics_chr = metrics_chr, type_1L_chr = type_1L_chr)
  # if(!identical(shorten_1L_chr, character(0)) | shorten_1L_lgl){
  #   if(shorten_1L_lgl){
  #     key_vars_chr <- tsibble::key_vars(data_tsb)
  #     shorten_1L_chr <- key_vars_chr[1]
  #   }
  #   caption_1L_chr <- add_shorthand_to_caption(caption_1L_chr, data_tsb = data_tsb, min_1L_int = min_1L_int, shorten_1L_chr = shorten_1L_chr)
  #   data_tsb <- data_tsb %>% transform_to_shorthand(key_1L_chr = shorten_1L_chr)
  # }
  if(grid_1L_lgl){
    index_1L_chr <-  data_tsb %>% tsibble::index() %>% as.character()
    # keep_cdn_1L_chr = c("All", "Personal", "Provider", "Severity", "Sports")
    # metrics_chr = make_metric_vars()
    # sports_vars_chr = sports_vars_chr
    # clinical_vars_chr = clinical_vars_chr
    # severity_1L_int = 8L

    if(type_1L_chr == "wide"){
      data_tsb <- data_tsb %>%
        dplyr::select(!!rlang::sym(index_1L_chr), dplyr::starts_with(metrics_chr))
      combinations_chr <- sub("__[^__]+$", "", setdiff(data_tsb %>% names(),c(index_1L_chr, metrics_chr))) %>% unique() %>% sort()
      combinations_ls <- metrics_chr %>% purrr::map(~combinations_chr[combinations_chr %>% startsWith(.x)]) %>% stats::setNames(metrics_chr)
      combinations_ls <- combinations_ls %>% purrr::map2(names(combinations_ls),~{
        paste0(.y,"__",stringr::str_split(.x, '__', simplify = TRUE)[,2] %>%
                 make_keepers(clinical_vars_chr = clinical_vars_chr,
                              keep_cdn_1L_chr = keep_cdn_1L_chr,
                              severity_1L_int = severity_1L_int,
                              sports_vars_chr = sports_vars_chr))
      })
    }else{
      data_tsb <- data_tsb %>%
        dplyr::select(!!rlang::sym(index_1L_chr), metrics_chr)
      combinations_ls <- list(metrics_ls = list(metrics_chr))
    }
    combinations_ls %>%
      purrr::map2(names(combinations_ls),
                  ~{
                    variables_chr <- .x
                    # if(is.list(variables_chr)){
                    #   variables_chr <- variables_chr[[1]]
                    # }
                    name_1L_chr <- .y
                    variables_chr %>% purrr::map(
                      ~{
                        variable_1L_chr <- .x
                        df <- data_tsb  %>%
                          dplyr::select(dplyr::starts_with(variable_1L_chr)) %>%
                          as.data.frame() %>%
                          dplyr::select(-!!rlang::sym(index_1L_chr)) %>%
                          dplyr::rename_with(.fn = function(x){stringr::str_replace(x,paste0(name_1L_chr,"__"),"")})
                        if(shorten_1L_lgl){
                          caption_1L_chr <- add_shorthand_to_caption(caption_1L_chr = caption_1L_chr, original_xx = names(df), min_1L_int = min_1L_int)
                          df <- df %>% transform_to_shorthand(original_xx = names(df)) ## add args
                        }
                        if(type_1L_chr == "wide"){
                          title_1L_chr <- paste0("Correlations And Scatter Plots For ", Hmisc::capitalize(frequency_1L_chr)," ", name_1L_chr," By ", variable_1L_chr %>% stringr::str_replace(paste0(name_1L_chr,"__"),""))
                        }else{
                          if(is.list(variables_chr)){
                            phrase_1L_chr <- ready4::make_list_phrase(variables_chr[[1]])
                          }else{
                            phrase_1L_chr <- ready4::make_list_phrase(variables_chr)
                          }
                          title_1L_chr <- paste0("Correlations and Scatter Plots for ", Hmisc::capitalize(frequency_1L_chr)," ", phrase_1L_chr)
                        }

                        df %>% GGally::ggpairs(axisLabels = axis_labels_1L_chr) +
                          ggplot2::labs(caption = caption_1L_chr,
                                        title = title_1L_chr)
                      })
                  })


  }else{
    combinations_mat <- combn(unique(make_metric_vars()),2)
    1:ncol(combinations_mat) %>% purrr::map(~{
      variables_chr <- combinations_mat[,.x]
      data_tsb %>%
        ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(variables_chr[1]), y = !!rlang::sym(variables_chr[2]))) +
        ggplot2::geom_point() +
        ggplot2::labs(x = variables_chr[1],
                      y = variables_chr[2])
    })
  }

}
plot_series <- function(data_xx,
                        by_value_1L_lgl = FALSE,
                        caption_1L_chr = "",
                        clinical_vars_chr = make_clinical_vars(),
                        frequency_1L_chr = c("daily", "weekly", "monthly", "quarterly", "yearly", "fiscal","sub"),
                        keep_cdn_1L_chr = c("All", "Personal", "Provider", "Severity", "Sports"),
                        key_totals_ls = NULL,
                        key_vars_chr = character(0),
                        label_x_1L_chr = "",
                        label_y_1L_chr = character(0),
                        metrics_chr = make_metric_vars(),
                        metrics_int = 1:4,
                        min_1L_int = 3L,
                        prefix_1L_chr = "",
                        severity_1L_int = 8L,
                        shorten_1L_chr = character(0),
                        shorten_1L_lgl = FALSE,
                        sports_vars_chr = get_sports_vars(),
                        suffix_1L_chr = character(0),
                        type_1L_chr = c("main", "seasonal"),
                        what_1L_chr = c("key", "totals", "wide"),
                        y_suffix_1L_chr = character(0)){
  type_1L_chr <- match.arg(type_1L_chr)
  # what_1L_chr <- match.arg(what_1L_chr)
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  if(tsibble::is_tsibble(data_xx)){
    if(what_1L_chr %in% c("totals","key", "wide")){
      cndl_type_1L_chr <- what_1L_chr
      cndl_what_1L_chr <- character(0)
    }else{
      cndl_type_1L_chr <- "totals"
      cndl_what_1L_chr <- what_1L_chr
    }
    tsibbles_xx <- get_tsibble(data_xx, frequency_1L_chr = frequency_1L_chr,
                               key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                               type_1L_chr = cndl_type_1L_chr,  what_1L_chr = cndl_what_1L_chr)
  }else{
    assertthat::assert_that(is.list(data_xx))
    dss_ls <- data_xx
    tsibbles_xx <- dss_ls %>% purrr::pluck(which(c("key", "totals", "wide") == what_1L_chr)) %>% purrr::pluck(frequency_1L_chr)
  }
  plot_multiple(tsibbles_xx,
                by_value_1L_lgl = by_value_1L_lgl,
                caption_1L_chr = caption_1L_chr,
                clinical_vars_chr = clinical_vars_chr,
                keep_cdn_1L_chr = keep_cdn_1L_chr,
                label_x_1L_chr = label_x_1L_chr,
                label_y_1L_chr = label_y_1L_chr,
                metrics_chr = metrics_chr,
                metrics_int = metrics_int,
                min_1L_int = min_1L_int,
                prefix_1L_chr = prefix_1L_chr,
                severity_1L_int = severity_1L_int,
                shorten_1L_chr = shorten_1L_chr,
                shorten_1L_lgl = shorten_1L_lgl,
                sports_vars_chr = sports_vars_chr,
                suffix_1L_chr = suffix_1L_chr,
                type_1L_chr = type_1L_chr,
                y_suffix_1L_chr = y_suffix_1L_chr)

}
plot_tsibble <- function(series_tsb,
                         auto_1L_lgl = TRUE,
                         date_tfmn_fn = NULL,
                         fiscal_start_1L_int = 7L,
                         frequency_1L_chr = c("daily","weekly",
                                              "monthly", "quarterly", "yearly", "fiscal", "sub"),
                         key_totals_ls = NULL,
                         key_vars_chr = character(0),
                         metrics_chr = make_metric_vars(),
                         prefix_1L_chr = "Cumulative",
                         transform_1L_lgl = TRUE,
                         type_1L_chr = c("main","cumulative"),
                         what_1L_chr = character(0)
){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if(is.null(date_tfmn_fn)){
    date_tfmn_fn <- get_temporal_fn(get_new_index(frequency_1L_chr), monthly_fn = lubridate::ym)
    # list(sub = lubridate::ymd_hms, daily = function(x){format(x,"%d-%b-%y") %>% as.Date("%d-%b-%y")}, weekly = tsibble::yearweek,
    #              monthly = lubridate::ym, quarterly = tsibble::yearquarter, yearly = lubridate::year, fiscal = function(x, y = fiscal_start_1L_int){tsibble::yearquarter(x, fiscal_start = y)}) %>%
    # purrr::pluck(frequency_1L_chr)
  }
  if(transform_1L_lgl){
    focused_tsb <- series_tsb %>% get_tsibble(frequency_1L_chr = frequency_1L_chr,
                                              key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                                              metrics_chr = metrics_chr, prefix_1L_chr = prefix_1L_chr, type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr)
  }else{
    focused_tsb <- series_tsb
  }
  if(auto_1L_lgl){
    plt <- metrics_chr %>% purrr::map(~focused_tsb %>% feasts::autoplot(!!rlang::sym(.x)))
  }else{
    if(length(metrics_chr)>1){
      plt <- metrics_chr %>% purrr::map(~plot_tsibble(focused_tsb, auto_1L_lgl = FALSE, date_tfmn_fn = date_tfmn_fn, fiscal_start_1L_int = fiscal_start_1L_int,
                                                      frequency_1L_chr = frequency_1L_chr, metrics_chr = .x, transform_1L_lgl = FALSE, what_1L_chr = what_1L_chr))
    }else{
      index_1L_chr <- tsibble::index(focused_tsb)
      ds_tb <- focused_tsb %>% tsibble::as_tibble()
      if(!identical(what_1L_chr, character(0)))
        ds_tb <- ds_tb %>% tidyr::pivot_wider(names_from = what_1L_chr, values_from = metrics_chr)
      plt <- dygraphs::dygraph(xts::xts(x=ds_tb, order.by = date_tfmn_fn(ds_tb %>% dplyr::pull(!!rlang::sym(index_1L_chr)))))
    }
  }
  return(plt)
}
plot_sngl_series <- function(data_tsb,
                             caption_1L_chr = "",
                             grid_1L_lgl = FALSE,
                             label_x_1L_chr = "",
                             label_y_1L_chr = character(0),
                             metrics_chr = make_metric_vars(),
                             min_1L_int = 3L,
                             prefix_1L_chr = "",
                             shorten_1L_chr = character(0),
                             shorten_1L_lgl = FALSE,
                             suffix_1L_chr = character(0),
                             type_1L_chr = c("main", "seasonal"),
                             what_1L_chr = make_metric_vars(),
                             y_suffix_1L_chr = character(0)){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  index_dtm <- data_tsb %>% dplyr::pull(data_tsb %>% tsibble::index())
  if(!identical(shorten_1L_chr, character(0)) | shorten_1L_lgl){
    if(shorten_1L_lgl){
      key_vars_chr <- tsibble::key_vars(data_tsb)
      shorten_1L_chr <- key_vars_chr[1]
    }
    caption_1L_chr <- add_shorthand_to_caption(caption_1L_chr, data_tsb = data_tsb, min_1L_int = min_1L_int, shorten_1L_chr = shorten_1L_chr)
    data_tsb <- data_tsb %>% transform_to_shorthand(key_1L_chr = shorten_1L_chr)
  }
  temporal_1L_chr <- "Daily"
  if(tsibble::is_yearweek(index_dtm)){
    temporal_1L_chr <- "Weekly"
  }
  if(tsibble::is_yearmonth(index_dtm)){
    temporal_1L_chr <- "Monthly"
  }
  if(tsibble::is_yearquarter(index_dtm)){
    temporal_1L_chr <- "Quarterly"
  }
  if(identical(y_suffix_1L_chr, character(0))){
    if(what_1L_chr== make_metric_vars()[4])
      y_suffix_1L_chr <- " ($)"
  }
  if(identical(label_y_1L_chr, character(0))){
    label_y_1L_chr <- paste0(what_1L_chr, y_suffix_1L_chr)
  }
  if(type_1L_chr == "main"){
    if(grid_1L_lgl){
      plot_fn <- function(ds_tb,
                          y_1L_chr){
        ds_tb %>%
          ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(ds_tb %>% tsibble::index() %>% as.character()),
                                       y = !!rlang::sym(y_1L_chr)
          )) +
          ggplot2::geom_line() +
          ggplot2::facet_grid(ggplot2::vars(!!rlang::sym(ds_tb %>% tsibble::key() %>% purrr::pluck(1) %>% as.character())
          ), scales = "free_y")
      }
    }else{
      plot_fn <- ggplot2::autoplot
    }

    tfmn_fn = identity
  }
  if(type_1L_chr == "seasonal"){
    plot_fn <- feasts::gg_season
    tfmn_fn <- tsibble::fill_gaps
  }
  if(grid_1L_lgl && type_1L_chr=="main"){
    plot_plt <- plot_fn(data_tsb %>% tfmn_fn,
                        what_1L_chr)

  }else{
    plot_plt <- plot_fn(data_tsb %>% tfmn_fn,!!rlang::sym(what_1L_chr))
  }

  plot_plt +
    ggplot2::labs(y = label_y_1L_chr, x = label_x_1L_chr, caption =  caption_1L_chr,
                  title = paste0(prefix_1L_chr, temporal_1L_chr, " ", what_1L_chr, suffix_1L_chr))
}
plot_weekdays <- function(dss_ls, ## TO UPDATE
                          metrics_chr = make_metric_vars(),
                          what_1L_chr = "Winter"){
  metrics_chr %>%
    purrr::map(~ {
      var_1L_chr <- .x
      dss_ls$key_dss_ls$daily %>% purrr::pluck(what_1L_chr) %>% dplyr::summarise(dplyr::across(dplyr::where(is.numeric),sum)) %>% tsibble::fill_gaps() %>% feasts::gg_season(!!rlang::sym(var_1L_chr), period = "week") +
        ggplot2::labs(y=var_1L_chr,x="", title=paste0(var_1L_chr, " by day of week over time"))
    }
    )
}
