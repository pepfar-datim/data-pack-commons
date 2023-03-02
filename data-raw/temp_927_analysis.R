# the following analysis is to help in ascertaining whether
# addition of indicators in the model are in line with the data from
# datim. Users can change the indicators as needed

library(datapackcommons)
library(datimutils)
library(dplyr)
datimutils::loginToDATIM(paste0(Sys.getenv("SECRETS_FOLDER"), "datim.json"))

# the models to work with
# initial comparison between PSNUxIM_DP746_2023-02-23_prod and
# PSNUxIM_COP23_dp746_dd7fd8_prod_2023-03-02.rds
baseline_model <- readr::read_rds(file.choose())
latest_model <- readr::read_rds(file.choose())

# deltas to look at
deltas <- diffSnuximModels(
  baseline_model,
  latest_model,
  full_diff = TRUE
)

# summation of all new values
temp <- dplyr::group_by(deltas, indicator_code) %>%
  dplyr::summarise(total = sum(value.new, na.rm = TRUE))


# hts index ----
temp_hts_index <- temp %>% filter(
  indicator_code %in% c(
    "HTS.Index.Neg.T",
    "HTS.Index.Pos.T"
  )
)
sum(temp_hts_index$total)
# HTS_INDEX
# 6,573,598 current total see pivot table in ticket

# hts tst ----
temp_hts_tst <- temp %>% filter(
  indicator_code %in% c(
    "HTS_TST.ActiveOther.Neg.T",
    "HTS_TST.ActiveOther.Pos.T",
    "HTS_TST.Other.Pos.T",
    "HTS_TST.Other.Neg.T"
  )
)
sum(temp_hts_tst$total)
#HTS_TST
# 47,165,147 current total see pivot table in ticket


# hts recent ----
temp_hts_recent <- temp %>% filter(
  indicator_code %in% c(
    "HTS_RECENT.T"
  )
)
sum(temp_hts_recent$total)
# HTS_RECENT
# 977,560 current total

# prep ct ----

temp_prep_ct <- temp %>% filter(
  indicator_code %in% c(
    "PrEP_CT.KP.T",
    "PrEP_CT.T"
  )
)
sum(temp_prep_ct$total)




temp_hts_recent <- temp[grepl("HTS_RECENT",temp$indicator_code),]
sum(temp_hts_recent$total)
