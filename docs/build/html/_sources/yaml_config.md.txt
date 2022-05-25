# YAML Configuration

This section is continually being expanded as the configuration feature set is
modified. For the time being, see existing dataset configuration files in 
[this directory](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes/-/tree/default/yaml-configuration)
for examples.

## Top-level YAML sections

- `tag`: dataset tag name (e.g. "CV"); used minimally but not quite deprecated to continue to support comparison between two datasets
- `globals`: contains two global settings that are applied across all variables
  - `min_age_for_inclusion`: (required) subjects will be excluded from all histograms etc. in the report and from the cleaned output if their age falls below this threshold
  - `max_invalid_datatypes_per_subject`: subjects will be excluded from  the cleaned output if they have more than this number of variables that can not be converted to the expected datatypes
  - `consent_inclusion_file`: name of file containing subject IDs with confirmed consent approval. format is: plaintext file, no header, one ID per line
  - `consent_exclusion_file`: name of file containing subject IDs without valid consent. format is: plaintext file, no header, one ID per line
    - the above two files are each optional. the files can be specified but empty. if neither is specified, no subjects will be included or excluded for consent. if one or the other but not both is specified, the included file's action will be applied alone. if both are specified, both actions will be performed, and additionally any subject ID found in the dataset but in neither list will be excluded and listed in a table in the final report for inspection. IDs cannot be present on both lists at once; if any such IDs are found, the program will complain.
- `variables`: this section contains one block for each variable in the dataset, with a variety of other configuration settings described in the next section
- `derived`: this section defines variables to be derived from existing variables

## `variables` YAML section

Each variable in the dataset is assigned a normalized encoded value (e.g. CV00001, CV00002, etc.).  Under each variable block, there are a variety of other possible configuration settings:
- `name`: this is the header of the variable in the input dataset
- `type`: (either this or shared_model are required) expected variable type; one of: string, numeric, ordinal, categorical; the variable type dictates what kinds of cleaning are applied to that variable
- `shared_model`: (either this or type are required) expected variable type as defined in `yaml-configuration/shared-models.yaml`
- `canonical_name`: if desired, a string with a more descriptive variable name than what's present in `name`
- `bounds`: min, max, and/or sd (standard deviation) bounds for a numeric variable
- `suppress_reporting`: a boolean to turn off printing a table of unique values and counts in the html report; useful for variables with PII or with many expected unique values like phone numbers
- `linked_date`: for `age` variables, this optionally points to a corresponding date variable for cross-comparison
  - `reported_year`: standardized name of variable containing corresponding year variable
  - `reference_year`: which year the age was collected
- `subject_age`: boolean flag to mark which variable is the accepted age of the subjects
- `subject_id`: boolean flag to mark which variable is the accepted unique subject ID
- `na-values`: any non-canonical values to be treated as NA (e.g. nil, not specified, etc.)
- `multimodal`: used to define another variable for plotting overlayed histograms, e.g. overlayed plots of BMI by sex
- `dependencies`: test for expected relationships between variables; can also include contingency tables to compare two variables and instructions for setting values to NA if certain dependency tests fail

## `derived` YAML section

Derived variables are calculated from existing data, e.g. calculating BMI from reported waist and height measurements.  This section allows the user to define arbitrary new variables to derive.
- Most sections here have been previously described, but `code` is where the logic is injected to create the derived variable, written in `R` syntax with access to the normalized variable names

## YAML validation

Prior to running this tool, you should validate the yaml configurations you've set up as follows:

`./yaml_validator.py dataset_file.yaml shared_model_file.yaml`

If you are using pre-commit as described below, the yaml configurations will be validated automatically when you commit changes.

