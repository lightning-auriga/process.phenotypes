# Description of Data Cleaning

Cleaning is orchestrated predominantly through subroutines called by `create.phenotype.report`.

Cleaning steps are generally performed in the order of the documentation here; this is important to consider for various cascading behaviors.

## Load Raw Input Phenotype Data

- The package receives comma- or tab-delimited input data, with or without compression
- Input data is expected to have a header
- Comments are not respected
- Quote characters around fields and delimiters between columns are both configurable in the library call (see `?process.phenotypes::create.phenotype.report` for details)

## Read in Configuration YAML

- The package expects a dataset-specific configuration file and a shared model configuration file
- The shared model config can be empty if you do not need variables defined across multiple questions
- Shared models are useful in cases where several questions have identically-structured answers (e.g. yes/no, high/medium/low, etc.), whereas dataset-specific configuration is done per-column of input phenotype data
- SurveyCTO form definitions can be automatically converted into shared model configurations, which can then be passed into the package

## Drop Invalid Columns

- Removes columns that are missing column headers
- Removes columns for which the header is `NA` or equivalent in R

## Sanitize Header Content in the Input Phenotype Data

- All remaining column headers are mapped to alphanumeric values based on the normalized encoded value provided in the dataset-specific config (e.g. `NN0001` in the example dataset-specific config)

## String Cleanup

### Convert to Lowercase

- Uniformly converts all alphabetical characters to lowercase
- This is to facilitate comparisons between values and accommodate data entry inconsistencies

### Remove Whitespace

- Collapses any repeated whitespace into a single instance of that whitespace
- Takes any amount of space surrounding a dash between characters and collapses it down to just the dash (e.g. `ABC  - 123` is converted to `ABC-123`)

### Collapse Repeats

- Specifically for back slashes, forward slashes, and periods, collapses repeating adjacent values to a single value
- Forward and back slashes are harmonized to forward slashes only

### Process Unicode Characters

- Unicode characters are replaced with the most-likely matching non-Unicode intended character
- Currently hard-coded; flagged for moving into a user-configurable file

### Remove Excel Error Codes

- Takes a series of recognized Excel error codes and converts them into NAs
- Includes the following Excel error codes, in any case combination, and optionally prepended by an equals sign
  - `#DIV/0!`
  - `#ERROR!`
  - `#VALUE!`
- Counts the detected instances of these errors and emits this information into the downstream report

### Detect Remaining Unicode Characters

- Searches for anything remaining in the Unicode range and adds it to the downstream report for manual follow-up

### Remove Non-word Characters

- For variables that are not designated `string` in the dataset-specific config, do the following:
  - Prefix with a `0` entries denoted as `numeric` type, starting with a period, and followed by numbers
  - For entries starting with `>` or `<`, replace with `greater than` or `less than`, respectively
  - For entries that begin with `-` and are followed by at least one numeric character, the value is set to NA
  - Non-word characters are removed from the beginning and end of entries (non-word corresponds to `\W`, aka `[^A-Za-z0-9_]`)

### Normalize Missing Values

- Attempt to capture a wide array of missing value indications, inclusive of common mispellings, and set them to NA
- This will catch some but not all potential intended NA values; additional NA values can be configured in the relevant section in the dataset-specific config

## Apply Consent Exclusions

- Two optional consent lists can be provided:
  - Consent inclusion list: subjects on this list will be included in the output
  - Consent exclusion list: subjects on this list will be excluded from the output
- If both lists are provided, subjects not present on either will be recorded as missing in the HTML report and excluded from the output
- If both lists are provided and a subject is present on both lists, exclusion takes precedence
- If only one list is provided, subjects not on the list are either included or excluded, depending on whether an exclusion or inclusion list is used

## Apply Variable-specific NA Values

- Takes NA values enumerated under `na-values` in the dataset-specific config file and substitutes them with NA for a given variable
- If `suppress_output` is true in the dataset-specific config for a given variable, all entries for that variable are subtituted with NA in the output

## Apply Type Conversions

- Each variable has a "type" specified either in the dataset-specific config, or in the shared models config.  This step converts the values within a given variable to the type specified
- If by this point in the cleaning process there are still entries that cannot be successfully converted to the specified type, those values are set to NA, and a count of these instances is emitted in the report for each variable
- See [this page](variable_types.md) for details on available variable types

## Exclude Subjects by Age

- In the dataset-specific config, there is a mandatory tag `min_age_for_inclusion`; there is also a tag `subject_age` used to define which variable contains age information
- The minimum permissible age, as defined above, is applied to the subject age values, and any subject with an age below the minimum is excluded from the output.  The number of subjects excluded here is emitted in the report

## Exclude Subjects Missing Subject IDs

- Excludes subjects from output if they have no value present in the variable marked in the config with the `subject_id` tag

(bounds)=
## Apply Bounds on Numeric Data

- For numeric or date type variables, there can optionally be bounds applied (min, max, or standard deviation) per variable in the dataset-specific config
- If any bounds are present, they are applied here.  Values outside the bounds are excluded from the output, and the count of excluded values is emitted in the report
  - For standard deviation, the bounds applied are the indicated number of deviations above and below the mean

## Attempt to Harmonize Self-reported Ancestry Labels

- Placeholder

## Create Derived Variables

- In the dataset-specific config, there can optionally be a section `derived`, separate from the `variables` section of the config
- This section defines custom variables that can be constructed from the post-cleaned variables defined in the remainder of the config
- Derived variables can also be defined based on other derived variables
  - Dependencies between derived variables must be resolvable; for example, if a derived variable A is created based on derived variable B, and derived variable B is created based on derived variable A, the system will error
- For more details on creating derived variables, see [this page](derived_vars.md)

## Re-apply Bounds on Derived Numeric Variables

- The cleaning described in the [Applies bounds section](bounds) is applied to any derived variables

(dependencies)=
## Check Cross-variable Dependencies

- In the dataset-specific config, there can optionally be dependencies derived per-variable.  These dependencies define expected relationships between it and other variables (e.g. to assert that year of birth as derived from age is within a certain window of self-reported year of birth, or that self-reported non-smokers do not also report number of packs per week smoked)
- Input from SurveyCTO (or other candidate survey systems) may have already had dependencies like this enforced

## Handle Dependency Failures

- In the dataset-specific config, each dependency (described [here](dependencies)) allows the option `exclude_on_failure` to set a value to NA if it has failed the dependency check (e.g. for subjects where they are self-reported non-smokers but they do list packs per week, you can set the packs per week value to NA for those subjects)
- The number of values set to NA due to this option is emitted in the report

## Compute Distribution Data for Numeric Variables

- For numeric variable types and dates, computes mean, min, max, deciles, and number of NA values, and adds to a table in the report

## Remove Subjects with Excess Invalid Entries Across All Variables

- In the dataset-specific config, there is a threshold `max_invalid_datatypes_per_subjects`.  For any subjects who have more NA values than this threshold, they are excluded from the output

## Emit an HTML Report

- Based on the configurations and the data cleaning performed, a report is emitted that records relevant information
- The contents of the report are somewhat configurable via the dataset-specific config, and vary by variable type
- The report is described in more detail [here](report.md)

## Write Out Cleaned Phenotype Data in TSV and/or Various Other Formats

- Primarily intended to output cleaned results in TSV format, but does support optional additional output formats, including:
  - SAS (`.sas7bdat` and supplemental code)
  - SPSS (`.zsav`)
  - STATA (`.dta`)

