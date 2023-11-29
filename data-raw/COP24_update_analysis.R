library(magrittr)
library(datimutils)
library(datapackcommons)
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                "datim.json"))


# DP-1161 (based off cop23 DP-895) ----

# Get a comparison of the FY23 and FY24 target data entry forms
# FY23 TARGETS DATA SETS - MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2023 - iADcaCD5YXh
# MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) - dA9C5bL44NX
comparison <- datapackcommons::diffDataEntryForm("iADcaCD5YXh",
                                                 "dA9C5bL44NX")

# dataelement/catCombos in the newer but not older data entry form
added <- dplyr::select(comparison$b_not_a, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("added" = "data")

# dataelement/catCombos in the older but not newer data entry form
removed <- dplyr::select(comparison$a_not_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("removed" = "data")

# unchanged dataelement/catCombos
stable <- dplyr::select(comparison$a_and_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("stable" = "data")

# combined
joined <- dplyr::full_join(added, removed) %>%
  dplyr::full_join(stable) %>%
  dplyr::filter(purrr::map2_lgl(added, removed,
                                ~ !(is.null(.x) && is.null(.y))))

View(joined)

joined_template_to_fill <-
  joined %>%
  mutate(
    reason_for_diff = NA,
    accounted_for_in_model = NA,
    second_check = NA
    )

# write out template to analyze
# write.csv(joined_template_to_fill[,c(
#   "dataelement", "reason_for_diff", "accounted_for_in_model", "second_check"
# )], "dp-1161_dp_model_QA.csv", row.names = F)



# DP-1162 (based off cop23 DP-896) ----

# Get a comparison of the FY23 and FY22 results data to compare drift
# MER R: Community Based FY2022Q4 - HfhTPdnRWES
# MER R: Community Based - H4N0I02dn1L

comparison_community <- datapackcommons::diffDataEntryForm("HfhTPdnRWES",
                                                           "H4N0I02dn1L")

# dataelement/catCombos in the newer but not older data entry form
added <- dplyr::select(comparison_community$b_not_a, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("added" = "data")

# dataelement/catCombos in the older but not newer data entry form
removed <- dplyr::select(comparison_community$a_not_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("removed" = "data")

# unchanged dataelement/catCombos
stable <- dplyr::select(comparison_community$a_and_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("stable" = "data")

# combined
joined_community <- dplyr::full_join(added, removed) %>%
  dplyr::full_join(stable) %>%
  dplyr::filter(purrr::map2_lgl(added, removed,
                                ~ !(is.null(.x) && is.null(.y)))) %>%
  dplyr::mutate(type = "community")

# MER R: Facility Based FY2022Q4 - BHlhyPmRTUY
# MER R: Facility Based - iU1zca2XkDh

comparison_facility <- datapackcommons::diffDataEntryForm("BHlhyPmRTUY",
                                                          "iU1zca2XkDh")

# dataelement/catCombos in the newer but not older data entry form
added <- dplyr::select(comparison_facility$b_not_a, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("added" = "data")

# dataelement/catCombos in the older but not newer data entry form
removed <- dplyr::select(comparison_facility$a_not_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("removed" = "data")

# unchanged dataelement/catCombos
stable <- dplyr::select(comparison_facility$a_and_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("stable" = "data")

# combined
joined_facility <- dplyr::full_join(added, removed) %>%
  dplyr::full_join(stable) %>%
  dplyr::filter(purrr::map2_lgl(added, removed,
                                ~ !(is.null(.x) && is.null(.y)))) %>%
  dplyr::mutate(type = "facility")


# bind pull unique list
joined_all <- rbind(
  joined_community,
  joined_facility
)

# overlapped items
overlap <- joined_all[duplicated(joined_all$dataelement), ]$dataelement
joined_all_overlap <- joined_all[joined_all$dataelement %in% overlap, ] %>%
  dplyr::arrange(dataelement)

# unique records
joined_all_uniq <- joined_all[!duplicated(joined_all$dataelement), ]


# write out if needed
# write.csv(joined_all_uniq[,c("dataelement","type")], "dp-1162-unique-all-dataelements-diff.csv", row.names = F)



#### start DP-929 (NEED COP24 EQUIVALENT TICKET)

library(magrittr)
library(datimutils)
library(datapackcommons)
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                "datim.json"))


### Begin DP-895

# Get a comparison of the FY22 and FY23 target data entry forms
# MER TARGETS 2023 MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2023
# MER TARGETS 2024
comparison <- diffDataEntryForm("iADcaCD5YXh", "dA9C5bL44NX")

# dataelement/catCombos in the newer but not older data entry form
added <- dplyr::select(comparison$b_not_a, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("added" = "data")

# dataelement/catCombos in the older but not newer data entry form
removed <- dplyr::select(comparison$a_not_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("removed" = "data")

# unchanged dataelement/catCombos
stable <- dplyr::select(comparison$a_and_b, dataelement, categoryoptioncombo) %>%
  dplyr::group_by(dataelement) %>%
  tidyr::nest() %>%
  dplyr::rename("stable" = "data")

# combined
joined <- dplyr::full_join(added, removed) %>%
  dplyr::full_join(stable) %>%
  dplyr::filter(purrr::map2_lgl(added, removed,
                                ~ !(is.null(.x) && is.null(.y))))
