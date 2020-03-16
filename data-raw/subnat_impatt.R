country_name <- "Nepal" # country name as a string - should be spelled the same way in all file names
support_dir_path <-   "/Users/sam/Documents/cop_19_data/"
config_file <-          "/users/sam/.secrets/prod.json"

reinstall_datapackr_remote <- "packFrame-fix-names(wb)"
                               # if NULL does not reinstall 


require(devtools)
require(tidyverse)


if(!is.null(reinstall_datapackr_remote)){  
  devtools::install_github("pepfar-datim/datapackr", 
                           ref = reinstall_datapackr_remote, 
                           build = TRUE,
                           upgrade = FALSE, 
                           force = TRUE)
}
require(datapackr)


datapack_excels <- 
  list.files(paste0(support_dir_path, "datapacks_impatt/"), pattern = country_name) %>% 
  sort(decreasing = TRUE)

  datapackcommons::DHISLogin(config_file)
  base_url <- getOption("baseurl")

d <-  datapackr::unPackData(paste0(support_dir_path, "datapacks_impatt/", datapack_excels[1]),
                                paste0(support_dir_path, "datapack_impatt/"), 
                                archive_results = FALSE)

write.table(d$datim$SUBNAT_IMPATT,
            file=paste0("/Users/sam/Documents/cop_19_data/", country_name ,"_IMPATT.csv"),
            col.names = TRUE,
            row.names = FALSE,
            sep=",",
            quote=TRUE)
