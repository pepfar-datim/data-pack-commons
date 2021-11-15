require(magrittr)

require(datapackcommons)
require(assertthat)
require(datimutils)
require(tidyr)
require(dplyr)

ValidateDimItems <-
  function(data, dim_uid_colname, item_name_colname, item_uid_colname, base_url) {
    # validate dimension item names and ids
    # 1 - Get items for all dimensions listed
    api_names_ids <- data[[dim_uid_colname]] %>% 
      unique() %>% na.omit() %>%
      paste0(collapse = ",") %>% {paste0("id:in:[", ., "]")} %>%
      {datapackcommons::getMetadata("dimensions", filters = .,
                                     fields = "items[name,id]")} %>%
      tidyr::unnest(c("items")) 
    
    # 2 - compare api items with items in data
    unmatched <- data %>% dplyr::select(!!item_uid_colname, !!item_name_colname) %>%
      setNames(c("id", "name")) %>% 
      na.omit() %>% unique() %>%
      dplyr::anti_join(api_names_ids) 
    
    if(NROW(unmatched) > 0){
      stop("Dimension items that cannot be matched", unmatched)
    } else{
      TRUE
    }
  }  

ValidateDimItemSets <- function(dim_item_sets, base_url){
  
# validate dimension names and ids
  dim_item_sets %>% dplyr::filter(!is.na(dim_uid), dim_uid != "co") %>% 
  {datapackcommons::ValidateNameIdPairs(.$dim_name, .$dim_uid, "dimensions")} %>% 
    assertthat::assert_that()

# validate dimension item names and ids
  dim_item_sets %>% dplyr::filter(dim_uid != "co") %>%
  ValidateDimItems("dim_uid", "dim_item_name", "dim_item_uid", base_url)
  
# validate category option names and ids
  dim_item_sets %>% dplyr::filter(!is.na(.$option_uid)) %>%  
  {datapackcommons::ValidateNameIdPairs(.$option_name, .$option_uid, "categoryOptions")} %>% 
    assertthat::assert_that()  

  # validate category option combination rows
  dim_item_sets %>% dplyr::filter(dim_uid == "co") %>%
    .$dim_name %>% unique() %>% assertthat::are_equal("Category option combo") %>% 
    assertthat::assert_that()  
  # category option combination names and ids
  dim_item_sets %>%  dplyr::filter(dim_uid == "co") %>%
  {datapackcommons::ValidateNameIdPairs(.$dim_item_name, .$dim_item_uid, "categoryOptionCombos")} %>% 
    assertthat::assert_that()  
  
  }

ValidateDataRequired <- function(data_required, base_url){
  # data_required %>% dplyr::filter(!is.na(A.dx_code)) %>%
  # {ValidateCodeIdPairs(base_url, .[["A.dx_code"]], .[["A.dx_id"]], "indicators")}
  # data_required %>% dplyr::filter(!is.na(B.dx_code)) %>%
  # {ValidateCodeIdPairs(base_url, .[["B.dx_code"]], .[["B.dx_id"]], "indicators")}
  # 
  data_required %>% dplyr::filter(!is.na(A.dx_name)) %>%
  {datapackcommons::ValidateNameIdPairs(.[["A.dx_name"]], 
                                        .[["A.dx_id"]], 
                                        "indicators", 
                                        base_url = base_url)} %>% 
    assertthat::assert_that()
  
  data_required %>% dplyr::filter(!is.na(B.dx_name)) %>%
  {datapackcommons::ValidateNameIdPairs(.[["B.dx_name"]], 
                                        .[["B.dx_id"]], 
                                        "indicators", 
                                        base_url = base_url)} %>% 
    assertthat::assert_that()
  
  
  # data_required %>% dplyr::filter(!is.na(A.add_dim_1_uid)) %>%
  # {ValidateNameIdPairs(.[["A.add_dim_1"]], .[["A.add_dim_1_uid"]], "dimensions")}
  # data_required %>% dplyr::filter(!is.na(B.add_dim_1_uid)) %>%
  # {ValidateNameIdPairs(.[["B.add_dim_1"]], .[["B.add_dim_1_uid"]], "dimensions")}
  # 
  # data_required %>% ValidateDimItems("A.add_dim_1_uid", "A.add_dim_1_items", "A.add_dim_1_items_uid", base_url)
  # data_required %>% ValidateDimItems("B.add_dim_1_uid", "B.add_dim_1_items", "B.add_dim_1_items_uid", base_url)
}

ValidateMapT_1toT <- function(t_1_to_t, dim_item_sets, base_url){
  
  t_1_to_t %>% mutate(dim_uid_colname = "LxhLO68FcXm") %>% 
  ValidateDimItems("dim_uid_colname", "technical_area", "technical_area_uid", base_url) 
  
  t_1_to_t %>% mutate(dim_uid_colname = "lD2x0c8kywj") %>% 
    ValidateDimItems("dim_uid_colname", "num_or_den", "num_or_den_uid", base_url) 
      
  t_1_to_t %>% mutate(dim_uid_colname = "HWPJnUTMjEq") %>% 
    ValidateDimItems("dim_uid_colname", "disagg_type", "disagg_type_uid", base_url) 
  
# chack for matching model sets in Dimension item sets
  c(t_1_to_t$age_set, t_1_to_t$sex_set, t_1_to_t$kp_set, t_1_to_t$other_disagg) %>% 
    na.omit() %>% 
    {. %in% dim_item_sets$model_sets} %>% 
    all() %>% 
    assertthat::assert_that()

  }

datapackr::loginToDATIM("~/.secrets/cop.json")
base_url <- getOption("baseurl")
wd <- getwd()
setwd("~/Documents/GitHub/data-pack-commons")

dim_item_sets <- readr::read_csv("./data-raw/model_calculations/dimension_item_sets.csv",
                                 col_types = readr::cols(.default = "c", sort_order = "d", weight = "d"),
                                 na = c("NA")) %>%
  dplyr::select(dim_uid, dim_name, dim_item_uid, dim_cop_type,
                dim_item_name, option_name, option_uid, sort_order, weight, model_sets) %>%
  dplyr::mutate(model_sets = stringr::str_split(model_sets,";")) %>%
  tidyr::unnest(model_sets)

ValidateDimItemSets(dim_item_sets, base_url)

data_required <- 
  readr::read_csv("./data-raw/model_calculations/data_required.csv", 
                  col_types = readr::cols(.default = "c", A.value_na = "d", B.value_na = "d"),
                  na = c("NA")) %>% select(-data_pack_type)

ValidateDataRequired(data_required, base_url)

Map21Tto22T <- 
  readr::read_csv("./data-raw/snu_x_im_distribution_configuration/21Tto22TMap.csv", 
                  col_types = readr::cols(.default = "c"),
                  na = c("NA")) 

ValidateMapT_1toT(Map21Tto22T, dim_item_sets, base_url)

dplyr::all_equal(datapackcommons::data_required, data_required)
dplyr::all_equal(datapackcommons::Map21Tto22T, Map21Tto22T)
dplyr::all_equal(datapackcommons::dim_item_sets, dim_item_sets)
dr_dif_removed <- dplyr::anti_join(datapackcommons::data_required, data_required)
map_dif_removed <- dplyr::anti_join(datapackcommons::Map21Tto22T, Map21Tto22T)
dim_dif_removed <- dplyr::anti_join(datapackcommons::dim_item_sets, dim_item_sets)
dr_dif_added <- dplyr::anti_join(data_required, 
                                 datapackcommons::data_required)
map_dif_added <- dplyr::anti_join(Map21Tto22T, 
                                  datapackcommons::Map21Tto22T)
dim_dif_added <- dplyr::anti_join(dim_item_sets,
                                  datapackcommons::dim_item_sets)

usethis::use_data(dim_item_sets, overwrite = TRUE, compress = "gzip")
usethis::use_data(data_required, overwrite = TRUE, compress = "gzip")
usethis::use_data(Map21Tto22T, overwrite = TRUE, compress = "gzip")

setwd(wd)


# SQL query for mechanisms

# select
# mechs.name mechanism_name,
# mechs.uid mechanism_co_uid,
# mechs.code mechanism_code,
# country_name country,
# country_uid
# from
# (
#   select
#   distinct dv.attributeoptioncomboid,
#   dv.sourceid,
#   co.name,
#   co.uid,
#   co.code
#   from
#   datavalue dv
#   inner join _periodstructure pe on
#   dv.periodid = pe.periodid
#   inner join categoryoptioncombos_categoryoptions coc_co on
#   dv.attributeoptioncomboid = coc_co.categoryoptioncomboid
#   inner join dataelementcategoryoption co on
#   co.categoryoptionid = coc_co.categoryoptionid
#   where
#   dv.deleted = false
#   and (pe.iso = '${period}')
#   and co.uid != 'xYerKDKCefk') mechs
# inner join (
#   select
#   ou.organisationunitid,
#   ou.uid,
#   country_uid,
#   country_name
#   from
#   organisationunit ou
#   inner join (
#     select
#     organisationunit.uid country_uid,
#     organisationunit.name country_name
#     from
#     orgunitgroup
#     inner join orgunitgroupmembers on
#     orgunitgroup.orgunitgroupid = orgunitgroupmembers.orgunitgroupid
#     inner join organisationunit on
#     organisationunit.organisationunitid = orgunitgroupmembers.organisationunitid
#     where
#     orgunitgroup.uid = 'cNzfcPWEGSH') countries on
#   ou."path" like concat('%', countries.country_uid, '%')) orgunits on
# mechs.sourceid = orgunits.organisationunitid
# group by
# mechs.name,
# mechs.uid,
# mechs.code,
# country_uid,
# country_name