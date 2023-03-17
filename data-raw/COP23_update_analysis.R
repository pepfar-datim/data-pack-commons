library(magrittr)
library(datimutils)
library(datapackcommons)
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"),
                                "datim.json"))


### Begin DP-895

# Get a comparison of the FY22 and FY23 target data entry forms
# First is MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2022
# Second is MER Target Setting: PSNU (Facility and Community Combined) (TARGETS) FY2023
comparison <- datapackcommons::diffDataEntryForm("YfZot37BbTm",
                                                 "iADcaCD5YXh")

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


### End DP-895


### Begin DP-896

# Get a comparison of the FY22 and FY21 results data to compare drift
# MER R: Community Based FY2022Q4 - HfhTPdnRWES
# MER R: Community Based FY2021Q4 - TBcmmtoaCBC

comparison_community <- datapackcommons::diffDataEntryForm("TBcmmtoaCBC",
                                                           "HfhTPdnRWES")

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
# MER R: Facility Based FY2021Q4 - zL8TlPVzEBZ

comparison_facility <- datapackcommons::diffDataEntryForm("zL8TlPVzEBZ",
                                                          "BHlhyPmRTUY")

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

# review priority indicators (currently nonhighlighted indicators)
# PMTCT is in facility
unique(comparison_facility$a_and_b$dataelement)[grep("PMTCT_", unique(comparison_facility$a_and_b$dataelement))]
unique(comparison_community$a_and_b$dataelement)[grep("PMTCT_", unique(comparison_community$a_and_b$dataelement))]

#VMMC
unique(comparison_facility$a_and_b$dataelement)[grep("VMMC", unique(comparison_facility$a_and_b$dataelement))]
unique(comparison_community$a_and_b$dataelement)[grep("VMMC", unique(comparison_community$a_and_b$dataelement))]

#TX_TB
unique(comparison_facility$a_and_b$dataelement)[grep("TX_TB", unique(comparison_facility$a_and_b$dataelement))]
unique(comparison_community$a_and_b$dataelement)[grep("TX_TB", unique(comparison_community$a_and_b$dataelement))]

#PREP_NEW
unique(comparison_facility$a_and_b$dataelement)[grep("PrEP_NEW", unique(comparison_facility$a_and_b$dataelement))]
unique(comparison_community$a_and_b$dataelement)[grep("PrEP_NEW", unique(comparison_community$a_and_b$dataelement))]

#TX_CURR
unique(comparison_facility$a_and_b$dataelement)[grep("TX_CURR", unique(comparison_facility$a_and_b$dataelement))]
unique(comparison_community$a_and_b$dataelement)[grep("TX_CURR", unique(comparison_community$a_and_b$dataelement))]


# write out if needed
# write.csv(joined_all_uniq[,c("dataelement","type")], "dp-896-unique-all-dataelements-diff.csv", row.names = F)

### End DP-896


#### start DP-929

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



#### end DP-929
