require(tidyverse)
require(devtools)


devtools::install_github("pepfar-datim/data-pack-commons",
                         build = TRUE,
                         upgrade = FALSE,force=TRUE)
#
# devtools::install_github("pepfar-datim/datapackr", 
#                          ref = "regional-fix", 
#                          build = TRUE,
#                          upgrade = FALSE, 
#                          force = TRUE)

config_file <-            "/users/sam/.secrets/prod.json"
datapackcommons::DHISLogin(config_file)
base_url <- getOption("baseurl")



site_to_datim <- datapackr::SiteToDATIM

# changing label on ages to match what is in DATIM
site_to_datim$valid_ages[site_to_datim$valid_ages == "<01"] <- "<1"
site_to_datim$valid_ages[site_to_datim$valid_ages == "01-04"] <- "1-4"
site_to_datim$valid_ages[site_to_datim$valid_ages == "05-09"] <- "5-9"
site_to_datim$valid_ages[site_to_datim$valid_ages == "<= 02 months"] <- "<= 2 months"
site_to_datim$valid_ages[site_to_datim$valid_ages == "02 - 12 months"] <- "2 - 12 months"

# check sexes are correct

cat("      \n-  Check for mismatches due to sex in SiteToDatim")
Sys.sleep(3)

site_to_datim %>% 
  dplyr::filter(!is.na(valid_sexes), !is.na(categoryoptioncombouid)) %>% 
  {datapackcommons::ValidateNameIdPairs(.$valid_sexes, 
                                        .$categoryoptioncombouid, 
                                        "categoryOptionCombos",
                                        exact = FALSE)} %>% 
  dplyr::inner_join(site_to_datim, 
                    by = c("id" = "categoryoptioncombouid",
                           "name" = "valid_sexes")
                    ) %>% 
  dplyr::select(indicator_code, name, name_datim) %>% unique() %>% print()


cat("      \nWe observe two unique cases where sex in the config file do not match the sex the the related category option combination")
Sys.sleep(3)
cat("\n\nThese are fine because hts_self unassisted does not disaggegate on sex or age (unlike directly assisted)")
Sys.sleep(3)
cat("\n\nCONCLUSION: No evidence of miscoding based on sex dissagregates.")
Sys.sleep(3)

# check kps are correct
cat("\n\n-  Check for mismatches due to key population in SiteToDatim.\n\n")
Sys.sleep(3)
site_to_datim %>% 
  dplyr::filter(!is.na(valid_kps), !is.na(categoryoptioncombouid)) %>% 
  {datapackcommons::ValidateNameIdPairs(.$valid_kps, 
                                        .$categoryoptioncombouid, 
                                        "categoryOptionCombos",
                                        exact = FALSE)} %>% 
  dplyr::inner_join(site_to_datim, 
                    by = c("id" = "categoryoptioncombouid",
                           "name" = "valid_kps")
  ) %>% 
  dplyr::select(indicator_code, name, name_datim) %>% unique() %>% print()


cat("\n\nWe observe four cases where key population in the config file does not match the key pop in the related category option combination")
Sys.sleep(5)
cat("\n\nThese are fine because PWID is only implied in the case of KP_MAT")
Sys.sleep(5)
cat("\n\nCONCLUSION: No evidence of miscoding based on kp dissagregates.")
Sys.sleep(5)

# check kps are correct
cat("\n\n\n\n-  NOW, check for mismatches due to ages in SiteToDatim.\n\n")
Sys.sleep(3)

age_check <- site_to_datim %>% 
  dplyr::filter(!is.na(valid_ages), !is.na(categoryoptioncombouid)) %>% 
  {datapackcommons::ValidateNameIdPairs(.$valid_ages, 
                                        .$categoryoptioncombouid, 
                                        "categoryOptionCombos",
                                        exact = FALSE)} %>% 
  dplyr::inner_join(site_to_datim, 
                    by = c("id" = "categoryoptioncombouid",
                           "name" = "valid_ages")
  ) %>%  unique() 

cat("\n\nget list of all the different category option combos in site to datim with an age mismatch\n\n")
Sys.sleep(3)

unique(age_check$name_datim) %>% print()
Sys.sleep(3)

cat("\n\nWe need to check on usage of inclusive categories <15, 15+, Unknown Age, 30+, and 15-29. These may have a perfectly valid mismatch between the site tool age and what goes in to datim")
Sys.sleep(3)
cat("\n\nSite tool ages associated with <15 cat combos\n\n") 
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "<15" )) %>% .$name %>% unique() %>% print()
cat("\n\nSite tool indicators associated with <15\n\n") 
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "<15" )) %>% .$indicator_code %>% unique() %>% print()
Sys.sleep(5)
cat("\n\nCONCLUSION: Those are all expected")
Sys.sleep(3)

cat("\n\n\nSite tool ages associated with 15+\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "15\\+" )) %>% .$name %>% unique() %>% print()
cat("\n\nSite tool indicators associated with 15+\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "15\\+" )) %>% .$indicator_code %>% unique()%>% print()
Sys.sleep(5)
cat("\n\nCONCLUSION: Those look fine\n\n")

cat("\n\nSite tool ages associated with 30+\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "30\\+" )) %>% .$name %>% unique()%>% print()
cat("\n\nSite tool indicators associated with 30+\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "30\\+" )) %>% .$indicator_code %>% unique()%>% print()

Sys.sleep(5)
cat("\n\nCONCLUSION: Those look fine\n\n")

cat("\n\nSite tool ages associated with 15-29\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "15-29" )) %>% .$name %>% unique()%>% print()
cat("\n\nSite tool indicators associated with 15-29\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "15-29" )) %>% .$indicator_code %>% unique()%>% print()

Sys.sleep(5)
cat("\n\nCONCLUSION: Those look fine\n\n")


cat("\n\nSite tool indicators associated with unknown age\n\n")
age_check %>% dplyr::filter(stringr::str_detect(name_datim, "Unknown Age" )) %>% .$indicator_code %>% unique()%>% print()

Sys.sleep(5)
cat("\n\nCONCLUSION: Those look fine\n\n")

cat("\n\n Filter out these inclusive categories and see what mismatches remain\n\n")
age_check <- age_check %>% 
  dplyr::filter(!stringr::str_detect(name_datim, "15\\+" )) %>%
  dplyr::filter(!stringr::str_detect(name_datim, "<15" )) %>%
  dplyr::filter(!stringr::str_detect(name_datim, "30\\+" )) %>% 
  dplyr::filter(!stringr::str_detect(name_datim, "Unknown Age" )) %>% 
  dplyr::filter(!stringr::str_detect(name_datim, "15-29" ))

Sys.sleep(3)
print(dplyr::select(age_check,name,name_datim) %>% unique())

cat("\n\nThese all appear to be genuine miscoding, check which indicators are affected by these\n\n")

cat(unique(age_check$indicator_code) %>% paste(collapse = "\n"))

cat("\n\nCONCLUSION: TB_ART is the isolated case of mistranslating site tool dissagregates to category option combinations\n\n")

Sys.sleep(5)

cat("\n\n- Can I check for miscoding of he 'extra' disagg. I will parse the indicator code and see if the is a mismatch of the text after 20T. and the category option combo name")

extra_check <- site_to_datim %>% dplyr::filter(stringr::str_detect(indicator_code, "20T\\."), 
                                !is.na(categoryoptioncombouid)) %>% 
  dplyr::mutate(additional_disagg = stringr::str_replace(indicator_code, ".*20T\\.", ""))

extra_check <-  extra_check %>%  {datapackcommons::ValidateNameIdPairs(.$additional_disagg, 
                                        .$categoryoptioncombouid, 
                                        "categoryOptionCombos",
                                        exact = FALSE)} %>% 
  dplyr::inner_join(extra_check, 
                    by = c("id" = "categoryoptioncombouid",
                           "name" = "additional_disagg")) %>%  
  dplyr::select(name, name_datim, indicator_code) %>%  unique() %>% 
  dplyr::arrange(name,name_datim,indicator_code)  %>%  print(n=1000) 

cat("\n\nMost of the indicator code implied extra dissagg 'mismatches' can be igonored based on a visaul inspection.")

cat("\n\nThe exception is 'already' and 'new' for TB_ART. In the site tool this is seperated into Already and New, but based on a review of the forms these get combined and the new already dissag gets dropped.")
cat("\n\nCONCLUSION: there are no mismatches on the Extra disagg implied in the indicator code.")