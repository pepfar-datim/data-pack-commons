# Script that can be used to generate and store a site tool density object for 
# a particular country.
# 
# 

## Edit as necessary #####################

country_name <-  "Botswana"
use_stored_mechanisms19T <-  TRUE
support_dir_path <- "/Users/sam/Documents/cop_19_data/"
config_file <- "/users/sam/.secrets/prod.json"


# install local version of datapackcommons if needed

devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                  build = TRUE,
                  upgrade = FALSE)

## Stop Edits #############################


require(datapackcommons)
require(tidyverse)

datapackcommons::DHISLogin(config_file)
base_url <- getOption("baseurl")

# mechanisms with data for FY19. calling this places a lock on data value table in datim, 
# so run as little as possible. Run once here and pass as argument toDistributeToSites 
if(use_stored_mechanisms19T == TRUE){
  mechanisms_19T <- readr::read_rds(paste0(support_dir_path, "support/mechanisms_19T.rds"))
} else{
  if(!exists("mechanisms_19T")){
    mechanisms_19T <<- datapackcommons::Get19TMechanisms(base_url)
  }
}

# create density for specified country
density_out_file_path <-  paste0(support_dir_path, "density_data/", 
                                 country_name, "_site_density_v2_",
                                 format(lubridate::now(tz="UTC"), "%Y%m%d_%H%M%S"), 
                                 ".rds")

density <- 
  datapackcommons::DistributeToSites(d = NULL
                                     , mechanisms_historic_global = mechanisms_19T
                                     , country_name = country_name)
readr::write_rds(density, 
                 density_out_file_path, 
                 compress = c("gz"))