devtools::install_github("pepfar-datim/datapackr", 
                         ref = "prod", 
                         build = TRUE,
                         upgrade = FALSE, 
                         force = TRUE)

require(magrittr)
require(datapackr)

in_file<-file.choose()
d<-unPackSiteToolData(in_file)

foo <- d$datim$site_data %>% 
  dplyr::filter(attributeOptionCombo != "00000") %>% 
  dplyr::group_by(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo)%>% 
  dplyr::summarise(value=round(sum(as.numeric(value)))) %>%
  dplyr::mutate(value = as.character(value))

filename = paste0( "cop19_import_",tolower(d$info$datapack_name),Sys.Date(),".csv")

write.table(
  foo,
  sep = ",",
  quote = TRUE,
  row.names = FALSE,
  col.names = TRUE,
  file = filename
)
