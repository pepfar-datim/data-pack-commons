#' Input to model calculation gives the mapping between dimension items and category options or the
#' raw category option combination or other analytics dimension
#'
#'  - Each row corresponds to an individual category option required in the output, 
#'    there are multiple entries per dimension and potentially even dimension item in some cases.
#'  - Includes weight to apply if raw values are being distributed so even a category 
#'    option can be repeated (<5 and 1-9 data required for 1-4)
#'  - Defines a key for the different sets of mappings
#'
#' @format A data frame with these variables:
#' \describe{
#'   \item{dim_uid}{Dimension uid used when running indicators, co in the case of a category option combination,
#'   NA if it is a datapack dimension that is missing from DATIM's historical data}
#'   \item{dim_name}{Name for dimension as it appears in DATIM - for documentation not used directly in model calculations}
#'   \item{dim_item_uid}{Dimension item or category option combination used when running indicator}
#'   \item{dim_cop_type}{This column is purely organizational - not functional. The dimension type from data pack perspective.
#'   These are categories that can appear in the rows of the data pack. In theory different indicators could use different 
#'   dimensions to produce these disaggregations (e.g. Cascade age bands or Semi fine age). 
#'   Currently we use the same dimensions respectively for age, sex, kp.}
#'   \item{dim_item_name}{Name of the dimension item as it appears in DATIM - for documentation not used directly in 
#'   model calculations}
#'   \item{option_name}{Name of the category option the dimension item will map to as it appears in DATIM - for 
#'   documentation not used directly in model calculations. In the case of data element group set dimensions, this will be NA}
#'   \item{option_uid}{UID of the category option the dimension item will map to. Only used by datapackr in the case of age, sex and KP; 
#'   otherwise this can be NA if it doesn't map to a single category oprtion}
#'   \item{sort_order}{Sort order for the category option, not strictly necessary for any 
#'   processing but can be useful in debugging and config work}
#'   \item{weight}{Value from 0-1 indicating the percent of the value for the dimension item that 
#'   gets distributed the category option when the raw values are distributed 
#'   (column "allocate" = "distribute" in data_required.csv) E.g. Dimension item 40-49 may 
#'   appear twice once for category option 40-44 and once for 45-49. 
#'   Each of these rows would have weight .5}
#'   \item{model_sets}{Semicolon seperated list of keys used for grouping the dimension item to 
#'   category option mappings into sets for calling the analytics api and calculating the model 
#'   output.}
#' }
#' @source COP19 systems team 
"dim_item_sets"

#' Input to model calculation. One row per required data pack column (from DATIM).
#' Includes parameters (dimensions, items) for calling the indicators, and the key to 
#' dimension_items_sets for mapping to category options for output.
#'
#' Each data pack output can be the result of combining up to two DATIM indicators into a 
#' combining calculation. The indicators are designated A and B and the calculation combining 
#' them is in the calculation column.
#' 
#' @format A data frame with these variables:
#' \describe{
#'   \item{data_pack_sheet}{Datapack Excel sheet label - from datapack schems}
#'   \item{data_pack_code}{given identifier to map output to a specific column in datapack - from
#'   datapack schema}
#'   \item{type}{numeric or percent - if percent then values recieved from DATIM will be 
#'   divided by 100 - from datapack schema}
#'   \item{full_formula}{specification for required calculation/output - from datapack schema}
#'   \item{allocate}{determines how dimension that splits to multiple category options will be 
#'   handled. If distribute the value returned by analytics will be multiplied by the weight from 
#'   corresponding entry in dim_item_sets. Replicate means the value is copied and NA means 
#'   there are no dimensions in the indicator call.
#'   
#'   Note: when data are distributed it is sometimes the case that we receive results for a 
#'   category option from more than one dimension (e.g. age 1-4 data can result from analytics 
#'   dimension <5 and 1-9): These “duplicates” are added together in the code. This should not 
#'   happen in the case of replicate and the program will throw an error if duplicate category 
#'   options result from the indicator call.}
#'   \item{A.dx_code}{indicator code}
#'   \item{A.dx_id}{indicator uid}
#'   \item{A.pe_iso}{period (ISO/DHIS2 format) - currently only one supported}
#'   \item{A.age_set}{age set from dim_item_sets}
#'   \item{A.sex_set}{sex set from dim_item_sets}
#'   \item{A.kp_set}{key population set from dim_item_sets}
#'   \item{A.add_dim_1}{Name of additional dimension - details on one nonstandard (age, sex, kp) 
#'   dimension used when calling the datim indicator. Currently on one dimension item is 
#'   supported. These do not get mapped to category options as they act more as a filter than an 
#'   explicit dimension.}
#'   \item{A.add_dim_1_uid}{additonal dimensions uid}
#'   \item{A.add_dim_1_items}{name of additional dimension item to filter on - only one supported}
#'   \item{A.add_dim_1_items_uid}{name of additional dimension item to filter on - only one supported}
#'   \item{B.dx_code}{see A.dx_code}
#'   \item{B.dx_id}{see A.dx_id}
#'   \item{B.pe_iso}{see A.pe_iso}
#'   \item{B.age_set}{see A.age_set}
#'   \item{B.sex_set}{see A.sex_set}
#'   \item{B.kp_set}{see A.kp_set}
#'   \item{B.add_dim_1}{see A.add_dim_1}
#'   \item{B.add_dim_1_uid}{see A.add_dim_1_uid}
#'   \item{B.add_dim_1_items}{see A.add_dim_1_items}
#'   \item{B.add_dim_1_items_uid}{see A.add_dim_1_items_uid}
#'   \item{calculation}{the formula for combiingthe A and B indicator values}
#' }
#' @source COP19 systems team
"data_required"