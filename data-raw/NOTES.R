library(ready4)
library(ready4use)
library(ready4fun)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
abbreviations_lup <- Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>%
  ready4fun::renew.ready4fun_abbreviations(short_name_chr = c("tsb","tsb_ls"),
                                                              long_name_chr = c("tsibble","list of tsibbles"),
                                                              plural_lgl = c(F,F)) %>%
  dplyr::arrange(short_name_chr)
classes_bup_lup <- Y@b_Ready4useIngest@objects_ls$classes_bup_lup %>%
  tibble::add_case(type_chr = "tbl_ts",
                   val_chr = "tsibble::tsibble(Date = numeric(0), index = Date)",
                   pt_ns_chr = "tsibble",
                   fn_to_call_chr = "tsibble",
                   default_val_chr = "",
                   old_class_lgl = FALSE) %>%
  dplyr::arrange(type_chr)
classes_lup <- Y@b_Ready4useIngest@objects_ls$classes_lup %>%
  tibble::add_case(type_chr = "tbl_ts",
                   val_chr = "tsibble::tsibble(Date = numeric(0), index = Date)",
                   pt_ns_chr = "tsibble",
                   fn_to_call_chr = "tsibble",
                   default_val_chr = "",
                   old_class_lgl = FALSE) %>%
  dplyr::arrange(type_chr)
object_type_lup <- Y@b_Ready4useIngest@objects_ls$object_type_lup
prototype_lup <- Y@b_Ready4useIngest@objects_ls$prototype_lup
seed_obj_lup_tb <- Y@b_Ready4useIngest@objects_ls$seed_obj_lup_tb
seed_obj_type_lup <- Y@b_Ready4useIngest@objects_ls$seed_obj_type_lup

Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 abbreviations_lup = abbreviations_lup,
                 classes_bup_lup =  classes_bup_lup,
                 classes_lup = classes_lup,
                 object_type_lup = object_type_lup,
                 prototype_lup = prototype_lup,
                 seed_obj_lup_tb = seed_obj_lup_tb,
                 seed_obj_type_lup = seed_obj_type_lup

               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")
