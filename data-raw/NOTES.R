library(ready4)
library(ready4use)
library(ready4fun)
# mbs,
#
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
abbreviations_lup <- Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>%
  ready4fun::renew.ready4fun_abbreviations(short_name_chr = c("mbs"),
                                                              long_name_chr = c("Medicare Benefits Schedule"),
                                                              plural_lgl = c(F)) %>%
  dplyr::arrange(short_name_chr)
treat_as_words_chr <- c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr,
                        "autocorrelation", "autocorrelations", "cumulatives", "disengaged", "fabels",  "summarise", "tsibble", "tsibbles") %>%
  sort()

# abbreviations_lup <- Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>%
#   ready4fun::renew.ready4fun_abbreviations(short_name_chr = c("tsb","tsb_ls"),
#                                                               long_name_chr = c("tsibble","list of tsibbles"),
#                                                               plural_lgl = c(F,F)) %>%
#   dplyr::arrange(short_name_chr)
# object_type_lup <- Y@b_Ready4useIngest@objects_ls$object_type_lup %>%
#   ready4fun::renew.ready4fun_abbreviations(short_name_chr = c("tsb","tsb_ls"),
#                                            long_name_chr = c("tsibble","list of tsibbles"),
#                                            plural_lgl = c(F,F)) %>%
#   dplyr::arrange(short_name_chr)
# classes_bup_lup <- Y@b_Ready4useIngest@objects_ls$classes_bup_lup %>%
#   tibble::add_case(type_chr = "tbl_ts",
#                    val_chr = "tsibble::tsibble(Date = numeric(0), index = Date)",
#                    pt_ns_chr = "tsibble",
#                    fn_to_call_chr = "tsibble",
#                    default_val_chr = "",
#                    old_class_lgl = FALSE) %>%
#   dplyr::arrange(type_chr)
# classes_lup <- Y@b_Ready4useIngest@objects_ls$classes_lup %>%
#   tibble::add_case(type_chr = "tbl_ts",
#                    val_chr = "tsibble::tsibble(Date = numeric(0), index = Date)",
#                    pt_ns_chr = "tsibble",
#                    fn_to_call_chr = "tsibble",
#                    default_val_chr = "",
#                    old_class_lgl = FALSE) %>%
#   dplyr::arrange(type_chr)
# prototype_lup <- Y@b_Ready4useIngest@objects_ls$prototype_lup %>%
#   tibble::add_case(type_chr = "tbl_ts",
#                    val_chr = "tsibble::tsibble(Date = numeric(0), index = Date)",
#                    pt_ns_chr = "tsibble",
#                    fn_to_call_chr = "tsibble",
#                    default_val_chr = "",
#                    old_class_lgl = FALSE) %>%
#   dplyr::arrange(type_chr)
# seed_obj_lup_tb <- Y@b_Ready4useIngest@objects_ls$seed_obj_lup_tb %>%
#   tibble::add_case(short_name_chr = "tsb",
#                    long_name_chr = "tsibble",
#                    atomic_element_lgl = FALSE,
#                    r3_element_lgl = TRUE) %>%
#   dplyr::arrange(short_name_chr)
#
# seed_obj_type_lup <- Y@b_Ready4useIngest@objects_ls$seed_obj_type_lup %>%
#   tibble::add_case(short_name_chr = "tsb",
#                    long_name_chr = "tsibble",
#                    atomic_element_lgl = FALSE,
#                    r3_can_extend_lgl = TRUE) %>%
#   dplyr::arrange(short_name_chr)
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 abbreviations_lup = abbreviations_lup,
                 # classes_bup_lup =  classes_bup_lup,
                 # classes_lup = classes_lup,
                 # object_type_lup = object_type_lup,
                 # prototype_lup = prototype_lup,
                 # seed_obj_lup_tb = seed_obj_lup_tb,
                 # seed_obj_type_lup = seed_obj_type_lup
                 treat_as_words_chr = treat_as_words_chr
               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")
