# Script to run data pack commons DistributeToSite function
# pulling the data pack export and the density for the specified country
# from the support files flder
# assumes the following files exist under the specificed local support
# file directory: 
# - datapack_data
# - density_data 
# - distributed_data
# - sites_data
# - mechanism_data
#
# The script will look for files including the country name and
# use the MOST RECENT copy of the relevant support files based on the descending sort order 
# which should be controlled by having a consistent file name including a time stamp yyyymmdd 
# 

## Edit as necessary #####################

country_name <-  "Tanzania"
support_dir_path <- "/Users/sam/Documents/cop_19_support_files/"
config_file <- "/users/sam/.secrets/triage.json"


# install local version of datapackcommons if needed

devtools::install(pkg = "/Users/sam/Documents/GitHub/data-pack-commons",
                  build = TRUE,
                  upgrade = FALSE)

## Stop Edits #############################


require(datapackcommons)
require(tidyverse)

datapack_data_files <- 
  list.files(paste0(support_dir_path, "datapack_data/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
density_data_files <-
  list.files(paste0(support_dir_path, "density_data/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
distributed_data_files <-
  list.files(paste0(support_dir_path, "distributed_data/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
mechanism_data_files <-
  list.files(paste0(support_dir_path, "mechanism_data/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
sites_data_files <-
  list.files(paste0(support_dir_path, "sites_data/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
datapackcommons::DHISLogin(config_file)
base_url <- getOption("baseurl")

# mechanisms with data for FY19. calling this places a lock on data value table in datim, 
# so run as little as possible. Run once here and pass as argument toDistributeToSites 
if(!exists("mechanisms_19T")){
  mechanisms_19T <<- datapackcommons::Get19TMechanisms(base_url)
}

d <- readr::read_rds(paste0(support_dir_path, "datapack_data/", datapack_data_files[1]))

# check for mechanism map and load
if(length(mechanism_data_files) > 0){
  mechanism_map <- readr::read_rds(paste0(support_dir_path, "mechanism_data/", mechanism_data_files[1]))
} else {
  mechanism_map = NULL
}

# check for site list and load
if(length(sites_data_files) > 0){
  sites <- readr::read_csv(paste0(support_dir_path, "sites_data/", sites_data_files[1])) %>%
    .[["orgunituid"]]
} else {
  sites = NULL
}

density <- readr::read_rds(paste0(support_dir_path, "density_data/", density_data_files[1]))
d <- datapackcommons::DistributeToSites(d, 
                                        mechanisms = mechanisms_19T, 
                                        site_densities = density, verbose = FALSE,
                                        mech_to_mech_map = mechanism_map,
                                        sites = sites)

d_out_file_path <-  paste0(support_dir_path, "distributed_data/", 
                                 country_name, "_distributed_",
                                 format(lubridate::now(tz="UTC"), "%Y%m%d_%H%M%S"), 
                                 ".rds")

readr::write_rds(d, 
                 d_out_file_path, 
                 compress = c("gz"))
