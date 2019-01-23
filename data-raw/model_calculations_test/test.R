# operating_units <- tibble::tribble(
#   ~ country_name, ~ id, ~ prioritization,
#   "Global", "ybg3MO3hcf4", "2")
# 
# replace data-raw/model_calculations with data-raw/model_calculations_test


test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_1"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB")
assert_that(test_data$value == .1 * 5066273)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_2"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB")
assert_that(test_data$value == .5)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_3"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[1,"value"]]
assert_that(test_data == 1099712)
test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_3"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[2,"value"]]
assert_that(test_data == 1099712)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_4"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[1,"value"]]
assert_that(test_data == 642894)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_4"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[4,"value"]]
assert_that(test_data == 642894)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_5"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[1,"value"]]
assert_that(test_data == .5 * 11558658)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_5"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[2,"value"]]
assert_that(test_data == .5 * 11558658)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_6"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[1,"value"]]
assert_that(test_data == 11558658)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_6"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[2,"value"]]
assert_that(test_data == 11558658)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_7"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[1,"value"]]
assert_that(test_data == 11558658)

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_8"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB", sex_option_uid == "Qn0I5FbKQOA", age_option_uid == "GaScV37Kk29") %>% .[["B"]]
assert_that(is.na(test_data))

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_8"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB", sex_option_uid == "Qn0I5FbKQOA", age_option_uid == "GaScV37Kk29") %>% .[["value"]]
assert_that(is.na(test_data))

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_8"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB", sex_option_uid == "Z1EnpTPaUfq", age_option_uid == "ttf9eZCHsTU") %>% .[["A"]]
assert_that(is.na(test_data))

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_8"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB", sex_option_uid == "Z1EnpTPaUfq", age_option_uid == "ttf9eZCHsTU") %>% .[["value"]]
assert_that(is.na(test_data))

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_8"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB", sex_option_uid == "Z1EnpTPaUfq", age_option_uid == "ttf9eZCHsTU") %>% .[["value"]]
assert_that(is.na(test_data))

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_8"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB", sex_option_uid == "Qn0I5FbKQOA", age_option_uid == "ttf9eZCHsTU") %>% .[["value"]]
assert_that(test_data == (.1 * 642894) / (.1 * 5066273) )

test_data <- cop_data[["ybg3MO3hcf4"]][["hts_tst"]][["hts_tst_9"]][["results"]] %>% 
  filter(org_unit_uid == "KSkooYTy8FB") %>% .[[1,"value"]]
assert_that(test_data == 6492385)