library(dplyr)

opts <- datapackcommons::dim_item_sets %>%
  dplyr::select(model_sets, option_name)

indicators <- datapackcommons::data_required %>%
  dplyr::select(data_pack_code, full_formula,
                A.age_set, B.age_set,
                A.sex_set, B.sex_set,
                A.kp_set, B.kp_set)

ages_A <- indicators %>%
  dplyr::left_join(opts, by = c("A.age_set" = "model_sets")) %>%
  dplyr::filter(!is.na(option_name)) %>%
  unique()

ages_B <- indicators %>%
  dplyr::left_join(opts, by = c("B.age_set" = "model_sets")) %>%
  dplyr::filter(!is.na(option_name)) %>%
  unique()

ages <- dplyr::bind_rows(ages_A, ages_B) %>%
  unique() %>%
  dplyr::rename(ages = "option_name")


sexes_A <- indicators %>%
  dplyr::left_join(opts, by = c("A.sex_set" = "model_sets")) %>%
  dplyr::filter(!is.na(option_name)) %>%
  unique()

sexes_B <- indicators %>%
  dplyr::left_join(opts, by = c("B.sex_set" = "model_sets")) %>%
  dplyr::filter(!is.na(option_name)) %>%
  unique()

sexes <- dplyr::bind_rows(sexes_A, sexes_B) %>%
  unique() %>%
  dplyr::rename(sexes = "option_name")

kps_A <- indicators %>%
  dplyr::left_join(opts, by = c("A.kp_set" = "model_sets")) %>%
  dplyr::filter(!is.na(option_name)) %>%
  unique()

kps_B <- indicators %>%
  dplyr::left_join(opts, by = c("B.kp_set" = "model_sets")) %>%
  dplyr::filter(!is.na(option_name)) %>%
  unique()

kps <- dplyr::bind_rows(kps_A, kps_B) %>%
  unique() %>%
  dplyr::rename(kps = "option_name")

disaggs_by_indicators <- dplyr::left_join(indicators, ages) %>%
  dplyr::left_join(sexes) %>%
  dplyr::left_join(kps) %>%
  dplyr::select(-A.age_set, -B.age_set,
                -A.sex_set, -B.sex_set,
                -A.kp_set, -B.kp_set)
