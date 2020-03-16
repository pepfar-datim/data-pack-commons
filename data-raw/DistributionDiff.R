country_name <-  "Cameroon"
support_dir_path <- "/Users/sam/Documents/cop_19_data/"


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

diff_dist <- function(old_name, new_name, support_dir_path){
  new <- readr::read_rds(paste0(support_dir_path, 
                                "distributed_data/", 
                                new_name))  %>% 
    .[["data"]] %>% 
    .[["site"]] %>% 
    .[["distributed"]] %>% dplyr::select(-dplyr::one_of("siteValueH", "psnuValueH"))
  
  old <- readr::read_rds(paste0(support_dir_path, 
                                "distributed_data/", 
                                old_name)) %>% 
    .[["data"]] %>% 
    .[["site"]] %>% 
    .[["distributed"]]  %>% dplyr::select(-dplyr::one_of("siteValueH", "psnuValueH"))
  
  dplyr::all_equal(new, old)
}

diff_dens <- function(old_name, new_name, support_dir_path){
  new <- readr::read_rds(paste0(support_dir_path, 
                                "density_data/", 
                                new_name))
  
  old <- readr::read_rds(paste0(support_dir_path, 
                                "density_data/", 
                                old_name)) 

 # return(list(old=old, new=new))
purrr::map2(new,old,dplyr::all_equal)
  
}


diff_dist = purrr::map(distributed_data_files[-1], safely(diff_dist), distributed_data_files[1], support_dir_path)
diff_dens = purrr::map(density_data_files[-1], safely(diff_dens), density_data_files[1], support_dir_path)
