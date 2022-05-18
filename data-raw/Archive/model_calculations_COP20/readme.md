## dimension_item_sets.csv 
- Input to model calculation gives the mapping between dimension items and category options
- Each row corresponds to an individual category option required in the output, there are multiple entries per dimensions and even dimension item in some cases
- Includes weight to apply if raw values are being distributed so even a category option can be repeated (<5 and 1-9 data required for 1-4)
- defines a key for the different sets of mappings 

### columns

- dim_uid
  Dimension uid used when running indicators  
- dim_name
  Name for dimension - for documentation not used directly in model calculations
- dim_item_uid
  Dimension item used when running indicator
- dim_cop_type
  {age, sex, kp}
  The dimension type from data pack perspective. These are categories that can appear in the rows of the data pack. In theory different indicators could use different dimensions to produce these disaggregations (e.g. Cascade age bands or Semi fine age). Currently we use the same dimensions respectively for age, sex, kp.
- dim_item_name
  Name of the dimension item - for documentation not used directly in model calculations
- option_name
  Name of the category option the dimension item will map to - for documentation not used directly in model calculations
- option_uid
  UID of the category option the dimension item will map to
- sort_order
  Sort order for the category option, not strictly necessary for any processing but can be useful in debugging and config work
- weight
  Value from 0-1 indicating the percent of the value for the dimension item that gets distributed the category option when the raw values are distributed (column "allocate" = "distribute" in data_required.csv)
  E.g. Dimension item 40-49 may appear twice once for category optiuon 40-44 and once for 45-49. Each of these rows would have weight .5 
- model_sets
  Semicolon seperated list of keys used for grouping the dimension item to category option mappings into sets for calling the analytics api and calculating the model output.  
- <1-50+ through kp
  The models sets to which the dimension item to category option mapping belongs - this is concatenated into the model sets column in the excel file. Only the model sets column is used in the R processing.

## data_required.csv 
- Maps required output for datapack to the relevant parameterized indicator
- Allows for up to two base parameterized indicators (labeled A and B) which can be combined via an additional calculation using addition, subtraction, and multiplication.

### columns



## data_required.csv 
- Maps required output for datapack to the relevant parameterized indicator
- Allows for up to two base parameterized indicators (labeled A and B) which can be combined via an additional calculation using addition, subtraction, and multiplication.

### columns

## model_calculations.xlsx
- for convenience in editing the config files above
- must save as csv from the relevant tabs to create the files above.
- ths file is not used directly for the r program that creates the datapack output file.