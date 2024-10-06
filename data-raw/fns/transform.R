transform_age_groups <- function(data_tb,
                                 age_bands_lup,
                                 index_1L_chr,
                                 key_vars_chr = character(0),
                                 #fractions_ls,
                                 names_from_1L_chr,
                                 #new_age_groups_chr,
                                 #source_ctgs_ls,
                                 age_group_var_1L_chr = "AgeGroup",
                                 do_not_group_chr = character(0),

                                 #exclude_chr = character(0),
                                 rename_to_1L_chr = character(0),
                                 strict_1L_lgl = TRUE,
                                 values_to_1L_chr = "Value"){
  if(strict_1L_lgl){
    data_tb <- data_tb %>%
      dplyr::filter((!!rlang::sym(age_group_var_1L_chr) %in% (age_bands_lup$Source %>% purrr::flatten_chr())))
  }
  if(identical(rename_to_1L_chr, character(0))){
    rename_1L_lgl <- FALSE
    rename_to_1L_chr <- "TransformedAgeGroup"
  }else{
    rename_1L_lgl <- TRUE
  }
  data_tb <- 1:nrow(age_bands_lup) %>%
    purrr::map_dfr(~{
      band_1L_chr <- age_bands_lup$Name[.x]
      match_chr <- age_bands_lup$Source[[.x]]
      fraction_dbl <- age_bands_lup$Fraction[[.x]]
      reshaped_tb <- purrr::reduce(1:length(match_chr),
                                   .init = data_tb %>% dplyr::filter(!!rlang::sym(age_group_var_1L_chr) %in% match_chr),
                                   ~ .x %>% dplyr::mutate(!!rlang::sym(paste0(band_1L_chr,"_", match_chr[.y])) := dplyr::case_when(!!rlang::sym(age_group_var_1L_chr) == match_chr[.y] ~ !!rlang::sym(values_to_1L_chr)*fraction_dbl[.y],
                                                                                                                                   TRUE ~ 0))) %>%
        dplyr::mutate(!!rlang::sym(band_1L_chr) := rowSums(dplyr::across(paste0(band_1L_chr,"_",match_chr)))) %>%
        dplyr::select(-c(paste0(band_1L_chr,"_",match_chr), values_to_1L_chr)) %>%
        tidyr::pivot_longer(band_1L_chr, names_to = rename_to_1L_chr, values_to = values_to_1L_chr)
      reshaped_tb <- reshaped_tb %>%
        make_metrics_summary(index_1L_chr = index_1L_chr,
                             key_vars_chr = c(setdiff(key_vars_chr,c(rename_to_1L_chr, age_group_var_1L_chr)),rename_to_1L_chr, names_from_1L_chr),
                             metrics_chr = values_to_1L_chr)
      # reshaped_tb %>%
      #   dplyr::group_by(dplyr::across(setdiff(names(reshaped_tb), c(values_to_1L_chr, do_not_group_chr)))) %>%
      #   dplyr::summarise(!!rlang::sym(values_to_1L_chr) := sum(!!rlang::sym(values_to_1L_chr))) %>%
      #   dplyr::ungroup()
      reshaped_tb <- reshaped_tb %>%
        tidyr::pivot_wider(names_from = names_from_1L_chr,
                           values_from = values_to_1L_chr)
      if(!rename_1L_lgl){
        reshaped_tb <- dplyr::rename(reshaped_tb, !!rlang::sym(age_group_var_1L_chr) := !!rlang::sym(rename_to_1L_chr))
      }
      # %>%
      #   dplyr::select(-!!rlang::sym(age_group_var_1L_chr))
      reshaped_tb
    })
  return(data_tb)
}
transform_output <- function(output_ls){
  output_ls <- output_ls %>%
    purrr::map(~ifelse(is.null(.x),0,.x)) %>%
    purrr::flatten_dbl()
  return(output_ls)
}
transform_to_shorthand <- function(data_tb,
                                   key_1L_chr = character(0),
                                   min_1L_int = 3L,
                                   original_xx = character(0),
                                   x_ready4show_correspondences = ready4show::ready4show_correspondences()){
  index_1L_chr <- character(0)
  if(tsibble::is_tsibble(data_tb)){
    index_1L_chr <- tsibble::index(data_tb) %>% as.character()
    key_vars_chr <- tsibble::key_vars(data_tb)
    data_tb <- tsibble::as_tibble(data_tb)
  }
  if(nrow(x_ready4show_correspondences)==0){
    x_ready4show_correspondences <- make_correspondences(data_tb, key_1L_chr = key_1L_chr, min_1L_int = min_1L_int, original_xx = original_xx)
  }
  if(!identical(original_xx, character(0))){
    rename_chr <- match(names(data_tb), x_ready4show_correspondences$old_nms_chr)
    names(data_tb)[na.omit(rename_chr)] <- x_ready4show_correspondences$new_nms_chr[!is.na(rename_chr)]
    # data_tb <- dplyr::mutate(data_tb,
    #                          !!rlang::sym(key_1L_chr) :=  manufacture_temp(x_ready4show_correspondences,
    #                                                                        data_ls = list(data_tb %>%
    #                                                                                         dplyr::pull(!!rlang::sym(key_1L_chr))), flatten_1L_lgl = T))
  }else{
    data_tb <- dplyr::mutate(data_tb,
                             !!rlang::sym(key_1L_chr) :=  ready4show::manufacture.ready4show_correspondences(x_ready4show_correspondences,
                                                                                                             data_ls = list(data_tb %>%
                                                                                                                              dplyr::pull(!!rlang::sym(key_1L_chr))), flatten_1L_lgl = T))
  }

  if(!tsibble::is_tsibble(data_tb) && !identical(index_1L_chr, character(0))){
    data_tb <- tsibble::tsibble(data_tb, index = index_1L_chr, key = key_vars_chr)
  }
  return(data_tb)
}
transform_to_temporal <- function(X_Ready4useDyad = ready4use::Ready4useDyad(),
                                  metrics_chr,
                                  arrange_by_1L_chr = c("category", "name"),
                                  dictionary_r3 = ready4use::ready4use_dictionary(),
                                  index_1L_chr = "Date",
                                  key_vars_chr = character(0),
                                  temporal_vars_chr = make_temporal_vars()){
  arrange_by_1L_chr <- match.arg(arrange_by_1L_chr)
  X_Ready4useDyad@ds_tb <- transform_to_tsibble(X_Ready4useDyad@ds_tb,
                                                index_1L_chr = index_1L_chr,
                                                key_vars_chr = key_vars_chr,
                                                metrics_chr = metrics_chr)
  if(identical(dictionary_r3, ready4use::ready4use_dictionary())){
    dictionary_r3 <- ready4use::renew.ready4use_dictionary(ready4use::ready4use_dictionary(),
                                                           var_nm_chr = setdiff(names(X_Ready4useDyad@ds_tb), X_Ready4useDyad@dictionary_r3$var_nm_chr),
                                                           var_ctg_chr = "Temporal",
                                                           var_desc_chr = setdiff(names(X_Ready4useDyad@ds_tb), X_Ready4useDyad@dictionary_r3$var_nm_chr),
                                                           var_type_chr = setdiff(names(X_Ready4useDyad@ds_tb), X_Ready4useDyad@dictionary_r3$var_nm_chr) %>%
                                                             purrr::map_chr(~class(X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(.x)))[1]))

  }
  X_Ready4useDyad <- ready4use::update_dyad(X_Ready4useDyad, arrange_1L_chr = arrange_by_1L_chr, dictionary_r3 = dictionary_r3,  what_1L_chr = "dictionary")
}
transform_to_tsibble <- function(data_tb,
                                 activity_1L_chr = "Activity",
                                 athlete_roles_chr = c("Athlete", "AlumniAthlete"),
                                 appointments_var_1L_chr = "Appointments",
                                 cancellations_var_1L_chr = "Cancellations",
                                 clinical_team_1L_chr = "Clinical Team",
                                 clinician_1L_chr = "Clinician",
                                 clinician_discipline_1L_chr = "Service",
                                 components_chr = c("Year","Quarter", "Week"),
                                 cost_var_1L_chr = "Cost",
                                 date_tfmn_fn = identity,
                                 days_1L_chr = "Weekday",
                                 duration_1L_chr = "Duration",
                                 exclude_chr = "Group",#character(0),
                                 fiscal_start_1L_int = 7L,
                                 group_1L_chr = character(0),
                                 index_1L_chr = "Date",
                                 is_wide_1L_lgl = F,
                                 key_vars_chr = character(0),
                                 metrics_chr = make_metric_vars(),
                                 referrals_var_1L_chr = "Referrals",
                                 referrers_1L_chr = "Referrer Role",
                                 severity_1L_chr = "Severity",
                                 team_disciplines_1L_chr = "Disciplines",
                                 temporal_vars_chr = make_temporal_vars(),
                                 uid_var_1L_chr = "UID",
                                 type_1L_chr = c("main", "focused"),
                                 what_1L_chr = c("all", "totals")){
  what_1L_chr <- match.arg(what_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if(type_1L_chr == "main"){
    # key_vars_chr <- c("Role", "Age",  "Sex",   "ProviderState", "Severity", "Categorisation", "Para", "Aesthetic Sports", "Individual Sports", "Winter Sports",  "Service", "Referrer Role")
    data_tb <- make_metrics_summary(data_tb,
                                    index_1L_chr = index_1L_chr,
                                    key_vars_chr = key_vars_chr,
                                    metrics_chr = metrics_chr)
    # data_tb <- dplyr::arrange(data_tb, !!rlang::sym(index_1L_chr))
    # data_tb <- dplyr::select(data_tb, tidyselect::all_of(c(index_1L_chr,
    #                                                         metrics_chr,#c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr),
    #                                                         key_vars_chr))) %>%
    #                             dplyr::group_by(dplyr::across(tidyselect::all_of(c(index_1L_chr,
    #                                                                                key_vars_chr)))) %>%
    #                             dplyr::summarise(dplyr::across(tidyselect::all_of(metrics_chr#c(referrals_var_1L_chr, appointments_var_1L_chr, cancellations_var_1L_chr, cost_var_1L_chr)
    #                                                                               ), ~sum(.x, na.rm = T)), .groups = 'drop')
    if(identical(key_vars_chr, character(0))){
      cdl_key_xx <- NULL
    }else{
      cdl_key_xx <- key_vars_chr
    }
    data_tsb <- data_tb %>% tsibble::as_tsibble(index = index_1L_chr, key = tidyselect::all_of(cdl_key_xx))
    if(!identical(temporal_vars_chr, character(0))){
      data_tsb <- add_temporal_vars(data_tsb, date_var_1L_chr = index_1L_chr, temporal_vars_chr = temporal_vars_chr, fiscal_start_1L_int = fiscal_start_1L_int) %>%
        tsibble::as_tsibble(index = index_1L_chr,
                            key = key_vars_chr)
    }
  }else{
    if(!is_wide_1L_lgl){
      data_tb <- transform_to_prep(data_tb,
                                   activity_1L_chr = activity_1L_chr,
                                   athlete_roles_chr = athlete_roles_chr,
                                   appointments_var_1L_chr = appointments_var_1L_chr,
                                   cancellations_var_1L_chr = cancellations_var_1L_chr,
                                   clinical_team_1L_chr = clinical_team_1L_chr,
                                   clinician_1L_chr = clinician_1L_chr,
                                   clinician_discipline_1L_chr =clinician_discipline_1L_chr,
                                   components_chr = components_chr,
                                   cost_var_1L_chr = cost_var_1L_chr,
                                   days_1L_chr = days_1L_chr,
                                   duration_1L_chr = duration_1L_chr,
                                   exclude_chr = exclude_chr,#character(0),
                                   group_1L_chr = group_1L_chr,
                                   index_1L_chr = index_1L_chr,
                                   referrals_var_1L_chr = referrals_var_1L_chr,
                                   referrers_1L_chr = referrers_1L_chr,
                                   severity_1L_chr = severity_1L_chr,
                                   team_disciplines_1L_chr = team_disciplines_1L_chr,
                                   uid_var_1L_chr = uid_var_1L_chr,
                                   what_1L_chr = "prep")
      #}
      metrics_chr <- make_metric_vars(appointments_var_1L_chr = appointments_var_1L_chr,
                                      cancellations_var_1L_chr = cancellations_var_1L_chr,
                                      cost_var_1L_chr = cost_var_1L_chr,
                                      referrals_var_1L_chr = referrals_var_1L_chr)
      if(identical(key_vars_chr, character(0))){
        key_vars_chr <- get_key_vars(data_tb,
                                     activity_1L_chr = activity_1L_chr,
                                     athlete_roles_chr = athlete_roles_chr,
                                     appointments_var_1L_chr = appointments_var_1L_chr,
                                     cancellations_var_1L_chr = cancellations_var_1L_chr,
                                     clinical_team_1L_chr = clinical_team_1L_chr,
                                     clinician_1L_chr = clinician_1L_chr,
                                     clinician_discipline_1L_chr =clinician_discipline_1L_chr,
                                     components_chr = components_chr,
                                     cost_var_1L_chr = cost_var_1L_chr,
                                     days_1L_chr = days_1L_chr,
                                     duration_1L_chr = duration_1L_chr,
                                     exclude_chr = exclude_chr,#character(0),
                                     group_1L_chr = group_1L_chr,
                                     index_1L_chr = index_1L_chr,
                                     referrals_var_1L_chr = referrals_var_1L_chr,
                                     referrers_1L_chr = referrers_1L_chr,
                                     severity_1L_chr = severity_1L_chr,
                                     team_disciplines_1L_chr = team_disciplines_1L_chr,
                                     uid_var_1L_chr = uid_var_1L_chr)#names(data_tb) %>% setdiff(c(index_1L_chr, metrics_chr))
      }
      #if(!is_wide_1L_lgl){
      #metrics_chr <- make_metric_vars()
      data_tb <- transform_to_prep(data_tb,
                                   appointments_var_1L_chr = metrics_chr[2],
                                   cancellations_var_1L_chr = metrics_chr[3],
                                   cost_var_1L_chr = metrics_chr[4],
                                   index_1L_chr = index_1L_chr,
                                   referrals_var_1L_chr = metrics_chr[1],
                                   what_1L_chr = "prep")
      data_tb <- dplyr::select(data_tb, tidyr::all_of(c(index_1L_chr,metrics_chr,key_vars_chr)))
      data_tb <- data_tb %>%
        dplyr::mutate(!!rlang::sym(index_1L_chr) := date_tfmn_fn(!!rlang::sym(index_1L_chr))) %>%
        dplyr::group_by(dplyr::across(tidyr::all_of(c(index_1L_chr, key_vars_chr)))) %>%
        #dplyr::summarise(dplyr::across(.cols = dplyr::everything(),.fns = ~sum(.x, na.rm = TRUE))) %>% ## DOES NOT WORK - NEED TO CHECK WHY
        ## INSTEAD USING BELOW CALL
        dplyr::summarise(!!rlang::sym(metrics_chr[1]) := sum(!!rlang::sym(metrics_chr[1]) , na.rm = TRUE),
                         !!rlang::sym(metrics_chr[2])  := sum(!!rlang::sym(metrics_chr[2]) , na.rm = TRUE),
                         !!rlang::sym(metrics_chr[3])  := sum(!!rlang::sym(metrics_chr[3]) , na.rm = TRUE),
                         !!rlang::sym(metrics_chr[4])  := sum(!!rlang::sym(metrics_chr[4]) , na.rm = TRUE)) %>%
        dplyr::ungroup()
      data_tsb <- data_tb %>% tsibble::as_tsibble(key = key_vars_chr, index = index_1L_chr)
      if(what_1L_chr == "totals"){
        data_tsb <- data_tsb %>%  dplyr::select(tidyr::all_of(c(index_1L_chr, metrics_chr))) %>%
          # dplyr::summarise(!!rlang::sym(metrics_chr[4])  := sum(!!rlang::sym(metrics_chr[4]) , na.rm = TRUE))
          dplyr::summarise(!!rlang::sym(metrics_chr[1]) := sum(!!rlang::sym(metrics_chr[1]) , na.rm = TRUE),
                           !!rlang::sym(metrics_chr[2])  := sum(!!rlang::sym(metrics_chr[2]) , na.rm = TRUE),
                           !!rlang::sym(metrics_chr[3])  := sum(!!rlang::sym(metrics_chr[3]) , na.rm = TRUE),
                           !!rlang::sym(metrics_chr[4])  := sum(!!rlang::sym(metrics_chr[4]) , na.rm = TRUE))
      }
    }else{
      data_tsb <- data_tb %>%
        dplyr::mutate(!!rlang::sym(index_1L_chr) := date_tfmn_fn(!!rlang::sym(index_1L_chr))) %>%
        dplyr::group_by(!!rlang::sym(index_1L_chr)) %>%
        dplyr::summarise(dplyr::across(.cols = dplyr::everything(),.fns = sum)) %>%
        dplyr::ungroup() %>%
        tsibble::as_tsibble(index = index_1L_chr)
    }
  }

  return(data_tsb)
}
