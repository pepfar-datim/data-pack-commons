# TBD COPY QA TICKETS PROCESS ----



# begin DP-1147 ----

library(dplyr)

# try get diff ----

# try datim
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"), "datim.json"))

baseline <- readRDS("/Users/faustolopezbao/Desktop/datapack_testing/model_data_pack_input_24_2023-10-24_d98a330_flat.rds")
new <- readRDS("/Users/faustolopezbao/Desktop/datapack_testing/latestmodel.rds")

deltas <- diffDataPackModels(baseline, new, full_diff = TRUE)

# <error/dplyr:::mutate_error>
#   Error in `dplyr::mutate()`:
#   ℹ In argument: `ou = purrr::map_chr(ancestors, purrr::pluck, 1, 3)`.
# Caused by error in `purrr::map_chr()`:
#   ℹ In index: 9693.
# Caused by error:
#   ! Result must be length 1, not 0.
# ---
#   Backtrace:
#   1. global diffDataPackModels(baseline, new, full_diff = TRUE)
# 9. purrr::map_chr(ancestors, purrr::pluck, 1, 3)
# 10. purrr:::map_("character", .x, .f, ..., .progress = .progress)
# 13. purrr:::call_with_cleanup(...)

# nulls are breaking the function
# grabbed deltas inside diffDataPackModels to test response
# if the response is null from datim return the psnuid
res <-
  lapply(unique(deltas_rec$psnu_uid), function(y) {
    if(is.null(datimutils::getOrgUnits(y))) {
      data.frame(y)
    }
  }) %>% bind_rows()

unique(res$y)
#[1] "fLUGXJpcbBG" "c5vciSlu23e" "dych0G4cSKX" "uKySbK9FbCs" "cvUxg9nGhyT" "fxRKUvibHKO" "P4Jy185ZPvd" "fS7IEqWT2nt" "cRFfhS2VNIx" "VBuhjzBhFbO" "b6YghAVB7z3"
#[12] "QqwsdoP7fCj" "MaAYvJxPzx8" "S2OIBRCLpKW" "SY4mkHZ9v6Q" "jnRxemaD4Ho" "OnS7zv4sovx" "ssnTVZrwRTw" "mNVEj1DdEMV" "gzzURYR2joc" "TRSvGymYp6f" "NRqNrURtame"
#[23] "VB2yM4524e2" "NTHFO92QWFd" "uTLfZeYvEy7" "OzeDEb1NGx8" "rO9Ezo9ZBQ2" "WitpKwqvSx5" "JizHkcQs67Z" "KLtCjjVJH1l" "kqENDKBOnAR" "y3BAMl7ya5g" "axeGa2g5J8K"
#[34] "R3kINuTnXKu" "a1pbWIff3gO" "liBgJGTTwku" "MvS2br5FWRN" "tsUYbG4USw7" "lWlJCv6bQAA"


# check for psnus
psnus_exist <-
  datapackr::getDataPackOrgUnits(cop_year = 2024) %>%
  filter(uid %in% res$y)

length(psnus_exist$uid) == length(unique(res$y))
# TRUE

# try coptest
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"), "coptest.json"))

baseline <- readRDS("/Users/faustolopezbao/Desktop/datapack_testing/model_data_pack_input_24_2023-10-24_d98a330_flat.rds")
new <- readRDS("/Users/faustolopezbao/Desktop/datapack_testing/latestmodel.rds")

deltas <- diffDataPackModels(baseline, new, full_diff = TRUE)

res <-
  lapply(unique(deltas_rec$psnu_uid), function(y) {
    if(is.null(datimutils::getOrgUnits(y))) {
      data.frame(y)
    }
  }) %>% bind_rows()

unique(res$y)

# [1] "QMahblY1xEd" "cV7VYPgsAH9" "Y8tOboornd1"


# can't get diff, check model against schema  ----
baseline <-
  readRDS("/Users/faustolopezbao/Desktop/datapack_testing/model_data_pack_input_24_2023-10-24_d98a330_flat.rds") %>%
  bind_rows() %>%
  mutate(
    has_data = ifelse(!is.na(value), TRUE, FALSE)
  ) %>%
  mutate(has_data = replace(has_data, value == 0, FALSE))

valid_schema_indicators <-
  filter(datapackr::cop24_data_pack_schema,
         (dataset == "mer" & col_type == "past") |
           (dataset == "datapack" & col_type == "calculation")) %>%
  select(indicator_code, valid_ages, valid_sexes, valid_kps) %>%
  distinct()

valid_schema_combos <-
  lapply(unique(valid_schema_indicators$indicator_code), function(y){

    q <- valid_schema_indicators %>%
      filter(
        indicator_code == y
      )

    combos <-
      tidyr::crossing(
        q$valid_ages[[1]] %>% rename(valid_ages = name, age_option_uid = id) %>% select(valid_ages, age_option_uid),
        q$valid_sexes[[1]] %>% rename(valid_sexes = name, sex_option_uid = id) %>% select(valid_sexes, sex_option_uid),
        q$valid_kps[[1]] %>% rename(valid_kps = name, kp_option_uid = id) %>% select(valid_kps, kp_option_uid)
      ) %>%
      mutate(indicator_code = q$indicator_code[1]) %>%
      select(indicator_code, valid_ages, valid_sexes, valid_kps, age_option_uid, sex_option_uid, kp_option_uid)

  }) %>% dplyr::bind_rows()
#View(valid_schema_combos)

# validate model data against schema combos ----
# check against model data with values
valid_schema_combos_merged <-
  inner_join(
    valid_schema_combos,
    baseline %>%
      filter(has_data == TRUE) %>%
      select(-value, -psnu_uid, -period) %>%
      distinct()
  )

# combos in schema not in the data?
anti_join(
  valid_schema_combos,
  baseline %>%
    filter(has_data == TRUE) %>%
    select(-value, -psnu_uid, -period) %>%
    distinct()
)


# new model check against schema ----
new <-
  readRDS("/Users/faustolopezbao/Desktop/datapack_testing/latestmodel.rds") %>%
  bind_rows() %>%
  mutate(
    has_data = ifelse(!is.na(value), TRUE, FALSE)
  ) %>%
  mutate(has_data = replace(has_data, value == 0, FALSE))

valid_schema_indicators <-
  filter(datapackr::cop24_data_pack_schema,
         (dataset == "mer" & col_type == "past") |
           (dataset == "datapack" & col_type == "calculation")) %>%
  select(indicator_code, valid_ages, valid_sexes, valid_kps) %>%
  distinct()

valid_schema_combos <-
  lapply(unique(valid_schema_indicators$indicator_code), function(y){

    q <- valid_schema_indicators %>%
      filter(
        indicator_code == y
      )

    combos <-
      tidyr::crossing(
        q$valid_ages[[1]] %>% rename(valid_ages = name, age_option_uid = id) %>% select(valid_ages, age_option_uid),
        q$valid_sexes[[1]] %>% rename(valid_sexes = name, sex_option_uid = id) %>% select(valid_sexes, sex_option_uid),
        q$valid_kps[[1]] %>% rename(valid_kps = name, kp_option_uid = id) %>% select(valid_kps, kp_option_uid)
      ) %>%
      mutate(indicator_code = q$indicator_code[1]) %>%
      select(indicator_code, valid_ages, valid_sexes, valid_kps, age_option_uid, sex_option_uid, kp_option_uid)

  }) %>% dplyr::bind_rows()
#View(valid_schema_combos)

# validate model data against schema combos ----
# check against model data with values
valid_schema_combos_merged <-
  inner_join(
    valid_schema_combos,
    new %>%
      filter(has_data == TRUE) %>%
      select(-value, -psnu_uid, -period) %>%
      distinct()
  )

# combos in schema not in the data?
View(
  anti_join(
    valid_schema_combos,
    new %>%
      filter(has_data == TRUE) %>%
      select(-value, -psnu_uid, -period) %>%
      distinct()
  )
)

