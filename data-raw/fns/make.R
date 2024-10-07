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
make_correspondences <- function(data_tb = NULL,
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
make_erp_ds <- function(erp_raw_tb,
                        age_range_int,
                        age_bands_lup = NULL,
                        age_tfmn_fn = as.integer,
                        as_dyad_1L_lgl = TRUE,
                        frequency_1L_chr = "quarterly",
                        measure_1L_chr = "count",
                        select_chr = c("time_period", "age", "sex", "obs_value"),
                        sex_chr = c("female", "male", "person"),
                        summarise_1L_lgl = FALSE,
                        var_ctg_chr = c("Temporal", "Key", "Key", "Metric")){
  measure_1L_int <- switch(measure_1L_chr, "count" = 1, "change" = 2, "%change" = 3)
  period_1L_chr <-  get_new_index(frequency_1L_chr)
  temporal_fn <- get_temporal_fn(period_1L_chr)
  value_1L_chr <- ifelse(measure_1L_int < 3, "Persons","Percentage")
  # temporal_fns_ls <- make_temporal_fns(rename_1L_lgl = TRUE) %>% purrr::keep_at(periods_chr)
  erp_tb <- erp_raw_tb %>%
    dplyr::filter(measure == measure_1L_int) %>%
    dplyr::select(tidyselect::all_of(select_chr)) %>%
    # -c(measure, region, freq, unit_measure, obs_status, obs_comment))
    haven::zap_labels() %>%
    dplyr::mutate(sex = dplyr::case_when(sex == 1 ~ sex_chr[2],
                                         sex == 2 ~ sex_chr[1],
                                         sex == 3 ~ sex_chr[3])) %>%
    dplyr::mutate(age = age_tfmn_fn(age)) %>%
    dplyr::arrange(time_period, age, sex) %>%
    #dplyr::select(time_period, age, sex, dplyr::everything()) %>%
    dplyr::rename(!!rlang::sym(period_1L_chr) := time_period,
                  !!rlang::sym(value_1L_chr) := obs_value,
                  Age = age,
                  Sex = sex) %>%
    dplyr::mutate(!!rlang::sym(period_1L_chr) := temporal_fn(Quarter)) %>%
    dplyr::filter(Age %in% age_range_int)
  if(!is.null(age_bands_lup)){
    erp_tb <- erp_tb %>%
      dplyr::mutate(Age = Age %>% purrr::map_chr(~{
        age_1L_int <- .x
        age_bands_lup$Name[which(age_bands_lup$Range %>% purrr::map_lgl(~age_1L_int %in% .x[1]:.x[2]))]
      }))
  }
  if(summarise_1L_lgl){
    erp_tb <- erp_tb %>%
      dplyr::group_by(dplyr::across(setdiff(names(erp_tb), value_1L_chr))) %>%
      dplyr::summarise(!!rlang::sym(value_1L_chr) := sum(!!rlang::sym(value_1L_chr))) %>%
      dplyr::ungroup()
  }
  if(as_dyad_1L_lgl){
    erp_xx <- ready4use::Ready4useDyad(ds_tb = erp_tb) %>% ready4use::add_dictionary(var_ctg_chr = var_ctg_chr)
  } else{
    erp_xx <- erp_tb
  }
  return(erp_xx)
}
make_medicare_ds <- function(mbs_raw_tb,
                             as_dyad_1L_lgl = TRUE,
                             erp_tb = NULL,
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
make_retainers <- function(retainers_tb,
                           as_tsibble_1L_lgl = FALSE,
                           censor_1L_lgl = TRUE,
                           cumulatives_1L_lgl = FALSE,
                           cost_var_1L_chr = "Retainer amount",
                           data_tb = NULL,
                           date_var_1L_chr = "Retainer date",
                           default_1L_dbl = numeric(0),
                           dyad_1L_lgl = FALSE,
                           end_date_dtm = NULL,
                           fill_gaps_1L_lgl = FALSE,
                           fiscal_start_1L_int = 7L,
                           offset_1L_int = -1,
                           provider_id_1L_chr = "ProviderID",
                           reset_new_1L_lgl = TRUE,
                           unit_1L_chr = "days"){
  retainers_tsb <- update_retainers_ds(retainers_tb,
                                       cost_var_1L_chr = cost_var_1L_chr,
                                       date_var_1L_chr = date_var_1L_chr,
                                       end_date_dtm = end_date_dtm) %>%
    transform_to_tsibble(index_1L_chr = "Date", metrics_chr = c("Clinicians","Retainer")) %>%
    dplyr::select(Date, Clinicians, Retainer)
  if(cumulatives_1L_lgl){
    retainers_tsb <- retainers_tsb %>% dplyr::mutate(CumulativeRetainer = cumsum(`Retainer`),
                                                     CumulativeClinicians = cumsum(Clinicians))
  }
  if(fill_gaps_1L_lgl){
    retainers_tsb <- retainers_tsb  %>%
      tsibble::fill_gaps(Clinicians = 0, Retainer = 0)
    if(cumulatives_1L_lgl){
      retainers_tsb <- retainers_tsb %>% tidyr::fill(CumulativeRetainer, CumulativeClinicians)
    }
  }
  retainers_tsb <- add_temporal_vars(retainers_tsb, date_var_1L_chr = "Date", fiscal_start_1L_int = fiscal_start_1L_int)
  if(!is.null(data_tb)){
    preexisting_chr <- setdiff(data_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>% unique(), retainers_tb %>% dplyr::pull(!!rlang::sym(provider_id_1L_chr)) %>% unique()) %>% purrr::discard(is.na)
    if(!identical(preexisting_chr, character(0))){
      if(identical(default_1L_dbl, numeric(0))){
        default_1L_dbl <- mean(retainers_tb %>% dplyr::pull(!!rlang::sym(cost_var_1L_chr)), na.rm=TRUE)
      }
      due_date_dtm <- min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))) + lubridate::duration(offset_1L_int, units = unit_1L_chr)
      if(is.null(end_date_dtm)){
        end_date_dtm <- max(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr)))
      }
      preexisting_tb <- retainers_tb %>% dplyr::filter(F) %>%
        dplyr::bind_rows(tibble::tibble(!!rlang::sym(date_var_1L_chr) :=  due_date_dtm,
                                        !!rlang::sym(provider_id_1L_chr) := preexisting_chr,
                                        !!rlang::sym(cost_var_1L_chr) :=  default_1L_dbl))
      preexisting_tsb <- preexisting_tb %>% make_retainers(as_tsibble_1L_lgl = T, cumulatives_1L_lgl = cumulatives_1L_lgl, cost_var_1L_chr = cost_var_1L_chr, data_tb = NULL,
                                                           date_var_1L_chr = date_var_1L_chr, dyad_1L_lgl = FALSE, end_date_dtm = end_date_dtm, fill_gaps_1L_lgl = fill_gaps_1L_lgl, fiscal_start_1L_int = fiscal_start_1L_int)
      if(reset_new_1L_lgl){
        preexisting_tsb <- preexisting_tsb %>% dplyr::mutate(Clinicians = 0)
      }
      retainers_tsb <- retainers_tsb %>% tsibble::as_tibble() %>%
        dplyr::bind_rows(preexisting_tsb %>% tsibble::as_tibble()) %>%
        dplyr::arrange(Date) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(dplyr::across(setdiff(names(retainers_tsb), c("Date",make_temporal_vars())), sum),
                         dplyr::across(intersect(names(retainers_tsb), make_temporal_vars()), dplyr::first)
        ) %>%
        tsibble::as_tsibble(index = Date)
      if(censor_1L_lgl)
        retainers_tsb <- retainers_tsb %>%
        dplyr::filter(Date>= min(retainers_tb %>% dplyr::pull(!!rlang::sym(date_var_1L_chr))))
    }
  }
  if(!as_tsibble_1L_lgl){
    data_xx <- retainers_tsb %>% tsibble::as_tibble()
  }else{
    data_xx <- retainers_tsb
  }
  if(dyad_1L_lgl){
    data_xx <- Ready4useDyad(ds_tb = data_xx) %>%
      ready4use::add_dictionary(var_ctg_chr = "Temporal")
    #c("Temporal",rep("Metrics",ncol(data_xx)-1))
    data_xx@dictionary_r3 <- data_xx@dictionary_r3 %>%
      dplyr::mutate(var_ctg_chr = dplyr::case_when(var_nm_chr %in% c("Clinicians", "Retainer", "CumulativeRetainer", "CumulativeClinicians") ~ "Metrics",
                                                   TRUE ~ var_ctg_chr))
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
make_summary_ds <- function(data_xx,
                            join_before_dtm = NULL,
                            max_tenure_1L_dbl = numeric(0),
                            prefix_1L_chr = "Cumulative",
                            separation_after_dbl = numeric(0)){
  if(inherits(data_xx,"Ready4useDyad")){
    X_Ready4useDyad <- data_xx
  }else{
    X_Ready4useDyad <- ready4use::Ready4useDyad(ds_tb = data_xx)
  }
  active_vars_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl) %>% purrr::keep(~startsWith(.,"Active"))
  # episodes_vars_chr <- make_metric_vars("eoc", separation_after_dbl = separation_after_dbl) %>% sort()
  # active_vars_chr <- episodes_vars_chr[startsWith(episodes_vars_chr,"Active")]
  # cumulatives_chr <- paste0(prefix_1L_chr, c(episodes_vars_chr[startsWith(episodes_vars_chr,"Episodes")],
  #                      c("Appointments", "Cancellations", "Referrals", "Cost"),
  #                      episodes_vars_chr[startsWith(episodes_vars_chr,"Separations")]))
  cumulatives_chr <- make_cumulatives(prefix_1L_chr = prefix_1L_chr,
                                      separation_after_dbl = separation_after_dbl)
  if(!identical(max_tenure_1L_dbl, numeric(0))){
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 X_Ready4useDyad@ds_tb %>%
                                   dplyr::filter(!is.na(UID)) %>%
                                   dplyr::filter(Tenure <= max_tenure_1L_dbl) %>%
                                   add_cumulatives(metrics_chr = make_metric_vars(), arrange_by_1L_chr = "Date", prefix_1L_chr = "Cumulative", group_by_1L_chr = "UID") %>%
                                   dplyr::group_by(UID) %>%
                                   dplyr::summarise(dplyr::across(c(Date, Referrer, Role, Sex, Age, Categorisation, Para, Aesthetic, Individual, Winter, Severity), dplyr::first),
                                                    dplyr::across(c("Tenure", make_cumulatives(separation_after_dbl = separation_after_dbl)), max),
                                                    dplyr::across(make_metric_vars("eoc", separation_after_dbl = separation_after_dbl)[startsWith(make_metric_vars("eoc", separation_after_dbl = separation_after_dbl),"Active")], sum)) %>%
                                   dplyr::mutate(dplyr::across(make_metric_vars("eoc", separation_after_dbl = separation_after_dbl)[startsWith(make_metric_vars("eoc", separation_after_dbl = separation_after_dbl),"Active")], as.logical)) %>%
                                   dplyr::ungroup() %>% dplyr::filter(Date <= join_before_dtm))
  }else{
    X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "ds_tb",
                                 X_Ready4useDyad@ds_tb %>%
                                   dplyr::filter(!is.na(UID)) %>%
                                   dplyr::group_by(UID) %>%
                                   dplyr::summarise(dplyr::across(c(Date, Referrer, Role, Sex, Age, Categorisation, Para, Aesthetic, Individual, Winter, Severity, #Disengaged
                                                                    ProviderID, ProviderState), dplyr::first),
                                                    dplyr::across(c(Tenure, !!!rlang::syms(cumulatives_chr)), max),
                                                    dplyr::across(c(!!!rlang::syms(active_vars_chr)), sum)) %>%
                                   dplyr::mutate(dplyr::across(c(!!!rlang::syms(active_vars_chr)), as.logical)) %>%
                                   dplyr::ungroup())
  }
  if(inherits(data_xx,"Ready4useDyad")){
    X_Ready4useDyad@dictionary_r3 <- dplyr::filter(X_Ready4useDyad@dictionary_r3, var_nm_chr %in% names(X_Ready4useDyad@ds_tb))
    data_xx <- X_Ready4useDyad
  }else{
    data_xx <- X_Ready4useDyad@ds_tb
  }
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
make_ts_models <- function(data_xx,
                           approximation_xx = NULL,
                           fill_gaps_1L_lgl = FALSE,
                           frequency_1L_chr = c("daily","weekly",
                                                "monthly", "quarterly", "yearly"),
                           index_1L_chr = character(0),
                           key_vars_chr = character(0),
                           key_totals_ls = NULL,
                           #lag_1L_chr = "year",
                           metrics_chr = make_metric_vars(),
                           models_chr = c("Mean","Naïve","Seasonal naïve", "Drift", "Trend", "LMTS", "ETS", "ARIMA",# "ANN"
                                          "NNTEAR", "Prophet", "Reg_ARIMA", "Reg_TSLM"
                           ),
                           model_type_1L_chr = "multiplicative",
                           order_1L_int = 2,
                           period_1L_int = 12,
                           stepwise_1L_lgl = TRUE,
                           terms_1L_chr = character(0),
                           test_1L_int = integer(0),
                           type_1L_chr = c("totals","key"),
                           what_1L_chr = character(0)){
  frequency_1L_chr <- match.arg(frequency_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  args_ls <- list(fill_gaps_1L_lgl = fill_gaps_1L_lgl,  frequency_1L_chr = frequency_1L_chr,
                  key_totals_ls = key_totals_ls, key_vars_chr = key_vars_chr,
                  metrics_chr = metrics_chr,
                  type_1L_chr = type_1L_chr,what_1L_chr = what_1L_chr)
  data_tsb <- rlang::exec(get_tsibble, data_xx = data_xx, !!!args_ls)
  if(identical(index_1L_chr, character(0))){
    index_1L_chr <- get_new_index(frequency_1L_chr)
  }
  training_tsb <- make_training_ds(data_tsb,
                                   index_1L_chr = index_1L_chr,
                                   test_1L_int = test_1L_int)
  if(identical(terms_1L_chr, character(0))){
    models_chr <- setdiff(models_chr, c("Reg_TSLM", "Reg_ARIMA"))
  }
  mabels_ls <- metrics_chr %>% purrr::map(~{
    mdl_1L_chr <- paste0(.x, " ~ ",terms_1L_chr) # Not evaluated prior to environment transfer
    model_args_ls <- list()
    model_args_ls <- list(Mean = fable::MEAN(!!rlang::sym(.x)),
                          `Naïve` = fable::NAIVE(!!rlang::sym(.x)),
                          `Seasonal naïve` = fable::SNAIVE(!!rlang::sym(.x) #~ lag(lag_1L_chr)
                          ),
                          Drift = fable::NAIVE(!!rlang::sym(.x) ~ drift()),
                          Trend = fable::TSLM(!!rlang::sym(.x) ~ trend()),
                          LMTS = fable::TSLM(!!rlang::sym(.x) ~ trend() + season()),
                          ETS = fable::ETS(!!rlang::sym(.x)),
                          ARIMA = fable::ARIMA(!!rlang::sym(.x), approximation = approximation_xx, stepwise = stepwise_1L_lgl), #ANN = fable::ETS(!!rlang::sym(.x) ~ error("A") + trend("N") + season("N"))
                          NNTEAR = fable::NNETAR(!!rlang::sym(.x)),
                          Prophet = eval(parse(text = paste0("fable.prophet::prophet(",
                                                             .x,
                                                             "~season(period=",
                                                             period_1L_int,
                                                             ", type='",
                                                             model_type_1L_chr,
                                                             "', order=",
                                                             order_1L_int,
                                                             "))")))
    )
    if(!identical(terms_1L_chr, character(0))){
      model_args_ls <- append(model_args_ls,
                              list(Reg_ARIMA = eval(parse(text = paste0("fable::ARIMA(",
                                                                        mdl_1L_chr,
                                                                        ", approximation = approximation_xx, stepwise = stepwise_1L_lgl)"))),
                                   Reg_TSLM = eval(parse(text = paste0("fable::TSLM(",mdl_1L_chr,")")))))
    }
    model_args_ls <- model_args_ls  %>%
      purrr::keep_at(models_chr)
    rlang::exec(fabletools::model, .data = training_tsb, !!!model_args_ls)
  }) %>%
    stats::setNames(metrics_chr)
  ts_models_ls <-  list(mabels_ls = mabels_ls,
                        args_ls = args_ls,
                        models_chr = models_chr,
                        test_1L_int = test_1L_int)
  return(ts_models_ls)

}
