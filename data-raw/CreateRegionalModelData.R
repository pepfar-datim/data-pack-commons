require(tidyverse)
require(datapackcommons)

datapackcommons::DHISLogin("/users/sam/.secrets/prod.json")
cop_data <-
  readRDS("/Users/sam/COP data/model_data_pack_input_20190118.rds")

regional_cop_data <- list()

CreateRegionalDP <- function(cop_data, countries) {
  country_cop_data = list()
  operating_units <-
    datapackcommons::GetCountryLevels(countries)
  
  # start off by adding the data for the first country
  country_cop_data  <-  cop_data[[operating_units[[1, "id"]]]]
  country_cop_data[["ou_name"]] = operating_units$country_name
  country_cop_data[["ou_psnu_level"]] = operating_units$planning_level
  operating_units <-
    operating_units %>% filter(id != operating_units[[1, "id"]])
  
  for (i in 1:NROW(datapackcommons::data_required)) {
    data_spec = slice(datapackcommons::data_required, i)
    
    for (id in operating_units$id) {
      country_cop_data[[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]] <-
        rbind(country_cop_data[[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]],
              cop_data[[id]][[data_spec$data_pack_sheet]][[data_spec$data_pack_code]][["results"]])
    }
  }
  
  for (id in operating_units$id) {
    if (NROW(cop_data[[id]][["KP"]][["KP_PREV.N.KeyPop.19T"]][["results"]]) == 0) {
      next
    }
    country_cop_data[["KP"]][["KP_PREV.N.KeyPop.19T"]][["results"]] <-
      dplyr::bind_rows(country_cop_data[["KP"]][["KP_PREV.N.KeyPop.19T"]][["results"]],
                       cop_data[[id]][["KP"]][["KP_PREV.N.KeyPop.19T"]][["results"]])
  }
  
  for (id in operating_units$id) {
    if (NROW(cop_data[[id]][["KP"]][["PrEP_NEW.N.KeyPopAbr.19T"]][["results"]]) == 0) {
      next
    }
    country_cop_data[["KP"]][["PrEP_NEW.N.KeyPopAbr.19T"]][["results"]] <-
      dplyr::bind_rows(country_cop_data[["KP"]][["PrEP_NEW.N.KeyPopAbr.19T"]][["results"]],
                       cop_data[[id]][["KP"]][["PrEP_NEW.N.KeyPopAbr.19T"]][["results"]])
  }
  
  for (id in operating_units$id) {
    if (NROW(cop_data[[id]][["Prioritization"]][["IMPATT.PRIORITY_SNU.19T"]][["results"]]) == 0) {
      next
    }
    country_cop_data[["Prioritization"]][["IMPATT.PRIORITY_SNU.19T"]][["results"]] <-
      dplyr::bind_rows(country_cop_data[["Prioritization"]][["IMPATT.PRIORITY_SNU.19T"]][["results"]],
                       cop_data[[id]][["Prioritization"]][["IMPATT.PRIORITY_SNU.19T"]][["results"]])
  }
  return(country_cop_data)
}

regional_cop_data[["Asia_Regional_Data_Pack"]] <-
  CreateRegionalDP(
    cop_data,c("Burma",
               "Cambodia",
               "India",
               "Indonesia",
               "Kazakhstan",
               "Kyrgyzstan",
               "Laos",
               "Papua New Guinea",
               #"Nepal"
               "Tajikistan",
               "Thailand"
    )
  )

regional_cop_data[["Caribbean_Data_Pack"]] <-
  CreateRegionalDP(cop_data,
                   c("Barbados",
                     # Caribbean Region_Military
                     "Guyana",
                     "Jamaica",
                     "Suriname",
                     "Trinidad & Tobago"))

regional_cop_data[["Central_America_Data_Pack"]] <-
  CreateRegionalDP(
    cop_data,
    c(#"Brazil", Central American Region_Military, "Costa Rica",
      "El Salvador",
      "Guatemala",
      "Honduras",
      "Nicaragua",
      "Panama"
    )
  )


regional_cop_data[["Western_Africa_Data_Pack"]] <-
  CreateRegionalDP(cop_data, c("Ghana")) 
                              #, #"Burkina Faso",
                               #"Liberia",
                               #"Mali",
                               #"Senegal", 
                               #"Sierra Leone",
                               #"Togo",
                               #"Western Africa Region_Military"))
    
    saveRDS(regional_cop_data, file = "/Users/sam/COP data/regional_model_data_pack_input_20190118.rds")
