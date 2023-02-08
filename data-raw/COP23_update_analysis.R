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
