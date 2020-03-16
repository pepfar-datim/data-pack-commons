# This script contains the code to recode the TB_ART, VL_SUPPRESSION, and DIAGNOSED_SUBNAT data 

# note I have hard coded some file references as this is an ad hoc one-off cleaning
require(dplyr)
config_file <-  "/users/sam/.secrets/triage.json"
datapackcommons::DHISLogin(config_file)


# Load the sql views 

xml2::read_xml("/Users/sam/Documents/GitHub/COP-19-Target-Setting/data-raw/DataCleanseSQLViews.xml") %>% 
  xml2::as_list() %>% 
  print()

r = httr::POST("https://triage.datim.org/api/metadata", 
               body = httr::upload_file("/Users/sam/Documents/GitHub/COP-19-Target-Setting/data-raw/DataCleanseSQLViews.xml", 
                                        "application/xml"))

httr::content(r, as="text") %>% xml2::read_xml() %>% xml2::as_list() %>% 
.$importReport %>% .$stats %>% print()


## check that 6 view were created

## check if anyone else has entered data by running COP 19 Check Data Value Creation sql view
temp = httr::GET("https://triage.datim.org/api/sqlViews/GZaBRVIhpGV/data.csv") %>% 
  httr::content(as="text") %>% readr::read_csv() %>% NROW() 

## check if anyone else has entered data by running COP 19 Check Data Value Audits sql view
temp = httr::GET("https://triage.datim.org/api/sqlViews/qADMzH1Hcuq/data.csv") %>% 
  httr::content(as="text") %>% readr::read_csv() %>% NROW() 

tb_art_before <- 
"https://triage.datim.org/api/29/analytics.csv?dimension=e485zBiR7vG&filter=ou:Z6HA94OLdeg;NTRKjfU2iaF;qM7yIwbxM76;XOivy2uDpMF;l1KFEXKI4Dg;Qh4XMQJhbk8;bQQJe0cC1eD;ds0ADyc9UCU;ANN4YCOufcP;V0qMZH29CtN;IH1kchw86uA;y3zhsvdXlhN;HfVjCurKxh2;qllxzIjjurr;lZsCb6y0KDX;h11OyvlPxpJ;FFVkaV9Zk1S;PqlFzhuPcF1;XtxUYCsDWrR;cDGPF739ZZr;WLG0z5NxQs8;mdXu6iCbn2G;G0BT4KrJouu;f5RoebaDLMx;a71G4Gtcttv&filter=dx:FLq9skcuN0k;VNjZAOFstOs&filter=pe:2019Oct&filter=sdoDQv2EDjp:UwIZeT7Ciz3&displayProperty=SHORTNAME&outputIdScheme=NAME" %>% 
  httr::GET() %>% httr::content("text") %>% readr::read_csv()


## assign host country targets and planning attribure data sets to military org units through GUI




r = httr::GET("https://triage.datim.org/api/dataValueSets?dataSet=N4X89PgW01w&dataSet=pTuDWXzkAkJ&period=2019Oct&orgUnitGroup=nwQbMeALRjL&includeDeleted=false")

r$dataValues = purrr::map(r$dataValues, function(x) {x$storedBy <- NULL
return(x)})

r$dataValues = purrr::map(r$dataValues, function(x) {x$created <- NULL
return(x)})

r$dataValues = purrr::map(r$dataValues, function(x) {x$lastUpdated <- NULL
return(x)})

r$dataValues = purrr::map(r$dataValues, function(x) {x$followup <- NULL
return(x)})

print(xml2::as_xml_document(r))
xml2::write_xml(r %>% xml2::as_xml_document(),"test.xml")

readr::read_csv(col_names = TRUE, col_types = col_types)


r = httr::POST("https://triage.datim.org/api/dataValueSets?dryRun=true&importStrategy=DELETE&force=TRUE&preheatCache=true", 
           body = httr::upload_file("/Users/sam/Desktop/COP 19 data cleaning test/20190623/cop 19 mil purge.csv", "application/csv"))


r = httr::POST("https://triage.datim.org/api/dataValueSets?dryRun=true&importStrategy=DELETE&force=TRUE&preheatCache=true", 
               body = httr::upload_file("/Users/sam/Desktop/COP 19 data cleaning test/20190623/delete small.csv", "application/csv"), timeout=6000)

