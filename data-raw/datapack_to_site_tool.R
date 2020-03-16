# Script to run data pack commons DistributeToSite function
# building/pulling the data pack and the density for the specified country
# from the support files folder.
#
# Assumes the following files exist under the specificed local support
# file directory e.g. (/Users/me/Documents/cop_19_data/): 
# - datapacks/
#   - xlsx data packs with country name in the file name
# - datapack_exports/
#   - output folder if store_datapack_export = TRUE
#   - previously generated datapack exports (rds) with country name in the file name
# - density_data/ 
#   - pre-generated site density object (rds) with country name in the file name
#   - Should be available: https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Data%20Pack%202019%20Staging%20Area/Support%20Files/density_data
# - site_lists/
#   - csv file with site list filter (sites NOT in this list will be dropped from density) 
# - mechanism_maps/
#   - excel version of mechanism map 
# - support/ 
#   - must contain mechanisms_19T.rds 
#   - https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Data%20Pack%202019%20Staging%20Area/Support%20Files/support
# - distributed_data/
#   - output location of the d object including distributed data used to pack site tool
# - site_tools/
#   - output location of packed site tool
# The script will look for files including the country name and
# will issue an error if multiple versions for the same country are found

## EDIT THESE - no defaults #####################

country_name <-  # country name as a string - should be spelled the same way in all file names
support_dir_path <-   # e.g.  "/Users/sam/Documents/cop_19_data/"
config_file <-        # e.g.    "/users/sam/.secrets/prod.json"
reinstall_datapackcommons_local <- # local repo path e.g. "/Users/sam/Documents/GitHub/data-pack-commons"
                                   # your local data-pack-commons branch should be PROD
                                   # if NULL nothing is installed  
  
reinstall_datapackr_remote <-  # datapackr branch e.g. current required branch "packFrame-fix-names(wb)"
                               # if NULL does not reinstall 

  
## Edit these as necessary #####################
store_datapack_export <- FALSE
use_stored_mechanisms19T <- TRUE
store_distributed_data <- TRUE
unpack_datapack <- TRUE      # {TRUE, FALSE} if true use xlsx if false use a previoiusly generated data pack export
  
## Stop Edits #############################


require(devtools)
require(tidyverse)

## install local version of datapackcommons if needed
if(!is.null(reinstall_datapackcommons_local)){  
  devtools::install(pkg = reinstall_datapackcommons_local,
                    build = TRUE,
                    upgrade = FALSE)
}
require(datapackcommons)

if(!is.null(reinstall_datapackr_remote)){  
  devtools::install_github("pepfar-datim/datapackr", 
                           ref = reinstall_datapackr_remote, 
                           build = TRUE,
                           upgrade = FALSE, 
                           force = TRUE)
}
require(datapackr)


# look for the differnt input file types (based on directory) with the country name in the file name
datapack_exports <- 
  list.files(paste0(support_dir_path, "datapack_exports/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
datapack_excels <- 
  list.files(paste0(support_dir_path, "datapacks/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
density_data_files <-
  list.files(paste0(support_dir_path, "density_data/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
mechanism_maps <-
  list.files(paste0(support_dir_path, "mechanism_maps"), pattern = country_name) %>% 
  sort(decreasing = TRUE)
site_lists <-
  list.files(paste0(support_dir_path, "site_lists/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)

# Check Files identified

file_issues = NULL
if (length(datapack_excels) == 0 &&  unpack_datapack == TRUE){
  file_issues = c(file_issues, "\nNo datapack with the country name found")
}
if (length(datapack_excels) > 1 &&  unpack_datapack == TRUE){
  file_issues = c(file_issues, "\nMultiple datapacks with the country name found")
}
if (length(density_data_files) == 0){
  file_issues = c(file_issues, "\nNo density file with the country name found")
} 
if (length(density_data_files) > 1){
  file_issues = c(file_issues, "\nMultiple density files with the country name found")
}
if (length(mechanism_maps) > 1){
  file_issues = c(file_issues, "\nMultiple mechanism map files with the country name found")
}
if (length(site_lists) > 1){
  file_issues = c(file_issues, "\nMultiple site filter list files with the country name found")
} 



try({
  if(!is.null(file_issues)){
    stop(paste(file_issues))
  }
  
  cat(paste("Files to be used:",
              "\n- datapack_export: ", datapack_exports,
              "\n- datapack_excel: ", datapack_excels,
              "\n- density: ", density_data_files,
              "\n- mechanism map: ", mechanism_maps,
              "\n- site list: ", site_lists
            )
      )
  
  user_input <- utils::askYesNo("\n\nContinue using these files?", default = FALSE, prompts = "y/n/cancel")
  
  if(user_input == FALSE || is.na(user_input)){
    stop("Stopped by you.")
    }
  
  datapackcommons::DHISLogin(config_file)
  base_url <- getOption("baseurl")
  options_string <- NULL
  # mechanisms with data for FY19. calling this places a lock on data value table in datim, 
  # so run as little as possible. Run once here and pass as argument toDistributeToSites 

  use_mechanism_map <- dplyr::if_else(length(mechanism_maps) == 1, TRUE, FALSE)
  use_site_list <- dplyr::if_else(length(site_lists) == 1, TRUE, FALSE)
  
    if(use_stored_mechanisms19T == TRUE){
    mechanisms_19T <- readr::read_rds(paste0(support_dir_path, "support/mechanisms_19T.rds"))
  } else{
    if(!exists("mechanisms_19T")){
      mechanisms_19T <<- datapackcommons::Get19TMechanisms(base_url)
    }
  }
  
  # check for mechanism map and load
  
  mechanism_map = NULL
  if(use_mechanism_map == TRUE){
    mechanism_map <- 
      datapackr::unPackMechanismMap(paste0(mechMap_path = support_dir_path, 
                                           "mechanism_maps/", 
                                           mechanism_maps[1])) %>% dplyr::filter(weight > 0)
    # add _mechMap to distributed data filename
    options_string <- paste0(options_string, "_mechMap")
    
  }
  
  # check for site list and load
  sites = NULL
  if(use_site_list == TRUE){
    sites <- readr::read_csv(paste0(support_dir_path, "site_lists/", site_lists[1])) %>%
      .[["orgunituid"]]
    # add _siteList to distributed data filename
    options_string <- paste0(options_string, "_siteList")
  }
  
  if(unpack_datapack == TRUE){
    if(length(datapack_excels) == 0){
      stop("Datapack not found")
    }
    d <-  datapackr::unPackData(paste0(support_dir_path, "datapacks/", datapack_excels[1]),
                                paste0(support_dir_path, "datapack_exports/"), 
                                archive_results = store_datapack_export)
  } else{
    if(length(datapack_exports) == 0){
      stop("Datapack export not found")
    }
    d <- readr::read_rds(paste0(support_dir_path, "datapack_exports/", datapack_exports[1]))
  }
  
  density <- readr::read_rds(paste0(support_dir_path, "density_data/", density_data_files[1]))
  
  d <- datapackcommons::DistributeToSites(d, country_name = country_name,
                                          mechanisms = mechanisms_19T, 
                                          site_densities = density, verbose = TRUE,
                                          mech_to_mech_map = mechanism_map,
                                          sites = sites)
  
  check_results = datapackcommons::CheckSiteToolData(d)
  if(abs(sum(check_results$reconciled_totals$difference)) > .001){
    stop("Data pack values do not reconcile with site tool values. Something is wrong! There should only be very very small floating point error.")
  }
  
  
  if(store_distributed_data == TRUE){
    d_out_file_path <-  paste0(support_dir_path, "distributed_data/", 
                               country_name, "_distributed_",
                               format(lubridate::now(tz="UTC"), "%Y%m%d_%H%M%S"), 
                               options_string,
                               ".rds")
    
    readr::write_rds(d, 
                     d_out_file_path, 
                     compress = c("gz"))
  }
  
  datapackr::packSiteTool(d,
                          output_path = paste0(support_dir_path, "site_tools/"))
  
  cat(paste("Generated with:",
            "\n  -", datapack_excels,
            "\n  -", density_data_files,
            "\n  -", mechanism_maps,
            "\n  -", site_lists,
            "\n\nsite tool should be filed: https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Data%20Pack%202019%20Staging%20Area/Site%20Tools",
            "\nDistributed data should be filed: https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Data%20Pack%202019%20Staging%20Area/Support%20Files/distributed_data",
            "\nSource files (datapack excel, mechanism map, and site list) should be filed in the appropriate folder here: https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Data%20Pack%202019%20Staging%20Area/Support%20Files\n"
            
  ))
  
  }
  )


