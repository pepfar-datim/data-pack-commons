require(tidyverse)

wd <- getwd()
setwd("/Users/sam/Documents/GitHub/data-pack-commons")

dim_item_sets <- readr::read_csv("./data-raw/model_calculations/dimension_item_sets.csv",
                                 na = c("NA")) %>%
  dplyr::select(dim_uid, dim_name, dim_item_uid, dim_cop_type,
         option_uid, sort_order, weight, model_sets) %>%
  dplyr::mutate(model_sets = stringr::str_split(model_sets,";")) %>%
  tidyr::unnest(model_sets)
#TODO explicitly define column types
usethis::use_data(dim_item_sets, overwrite = TRUE, compress = "gzip")

dim_item_sets_test <- readr::read_csv("./data-raw/model_calculations_test/dimension_item_sets.csv",
                                 na = c("NA")) %>%
  dplyr::select(dim_uid, dim_name, dim_item_uid, dim_cop_type,
         option_uid, sort_order, weight, model_sets) %>%
  dplyr::mutate(model_sets = stringr::str_split(model_sets,";")) %>%
  tidyr::unnest(model_sets)
 usethis::use_data(dim_item_sets_test, overwrite = TRUE, compress = "gzip")

data_required <- readr::read_csv("./data-raw/model_calculations/data_required.csv", 
                                 na = c("NA"))
usethis::use_data(data_required, overwrite = TRUE, compress = "gzip")

data_required_test <- readr::read_csv("./data-raw/model_calculations_test/data_required.csv", 
                                      na = c("NA"))
usethis::use_data(data_required_test, overwrite = TRUE, compress = "gzip")


setwd(wd)

