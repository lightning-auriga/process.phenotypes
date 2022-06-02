# YAML Configuration

This section is continually being expanded as the configuration feature set is
modified. For the time being, see existing dataset configuration files in 
[this directory](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes/-/tree/default/yaml-configuration)
for examples.

## Top-level YAML Sections

|Section|Description|
|---|---|
|`tag`|Dataset tag name (e.g. "HW"); used minimally but not quite <br>deprecated to continue to support comparison between two <br>datasets|
|`globals`|Contains two global settings that are applied across all variables|
|`min_age_for_inclusion`|**Required** Subjects will be excluded from all histograms etc. in<br>the report and from the cleaned output if their age falls below<br>this threshold|
|`max_invalid_datatypes_per_subject`|Subjects will be excluded from the cleaned output if they have<br>more than this number of variables that can not be converted to<br>the expected datatypes|
|`consent_inclusion_file`|Name of file containing subject IDs [with confirmed consent approval](description.md).<br>Format is: plaintext file, no header, one ID per line|
|`consent_exclusion_file`|Name of file containing subject IDs [without valid consent](description.md). Format<br>is: plaintext file, no header, one ID per line|
|`variables`|This section contains one block for each variable in the dataset,<br>with a variety of other configuration settings described in the next section|
|`derived`|This section defines variables to be derived from existing variables|

## Variables YAML Section

Each variable in the dataset is assigned a normalized encoded value (e.g. HW00001, HW00002, etc.).  Under each variable block, there are a variety of other possible configuration settings:

|Section|Description|
|---|---|
|`name`|This is the header of the variable in the input dataset|
|`type`|**Either this or shared_model are required** Expected variable type; one of:<br>string, numeric, ordinal, categorical; the variable type dictates what kinds of<br>cleaning are applied to that variable|
|`shared_model`|**Either this or type are required** Expected variable type as defined<br>in `yaml-configuration/shared-models.yaml`|
|`canonical_name`|If desired, a string with a more descriptive variable name than what's present in `name`|
|`bounds`|Min, max, and/or sd (standard deviation) bounds for a numeric variable|
|`suppress_reporting`|A boolean to turn off printing a table of unique values and counts in the<br>html report; useful for variables with PII or with many expected unique<br>values like phone numbers|
|`linked_date`|For `age` variables, this optionally points to a corresponding date variable for<br>cross-comparison; should also include flags indicating whether the variable is<br>the `reported_year` (standardized name of variable containing corresponding<br>year variable) or the `reference_year` (which year the age was collected)|
|`subject_age`|Boolean flag to mark which variable is the accepted age of the subjects|
|`subject_id`|Boolean flag to mark which variable is the accepted unique subject ID|
|`na-values`|Any non-canonical values to be treated as NA (e.g. nil, not specified, etc.)|
|`multimodal`|Used to define another variable for plotting overlayed histograms,<br>e.g. overlayed plots of BMI by sex|
|`dependencies`|Test for expected relationships between variables; can also include<br>contingency tables to compare two variables and instructions for setting values<br>to NA if certain dependency tests fail|

## Derived YAML Section

Derived variables are calculated from existing data, e.g. calculating BMI from reported waist and height measurements.  This section allows the user to define arbitrary new variables to derive.
- Most sections here have been previously described, but `code` is where the logic is injected to create the derived variable, written in `R` syntax with access to the normalized variable names

## YAML Validation

Prior to running this tool, you should validate the YAML configurations you've set up as follows:

`./yaml_validator.py dataset_file.yaml shared_model_file.yaml`

If you are using pre-commit as described below, the YAML configurations will be validated automatically when you commit changes.

