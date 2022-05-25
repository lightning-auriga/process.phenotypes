# Description of data cleaning

Cleaning is orchestrated predominantly through subroutines called by `create.phenotype.report`.

Cleaning steps are generally performed in the order of the documentation here; this is important to consider for various cascading behaviors.

## Loads raw input phenotype data

- The package receives comma- or tab-delimited input data, with or without compression
- Input data is expected to have a header
- Comments are not respected
- Quote characters around fields and delimiters between columns are both configurable in the library call (see `?process.phenotypes::create.phenotype.report` for details)

## Reads in configuration yaml

- The package expects a dataset-specific configuration file and a shared model configuration file
- The shared model config can be empty if you do not need variables defined across multiple questions
- Shared models are useful in cases where several questions have identically-structured answers (e.g. yes/no, high/medium/low, etc.), whereas dataset-specific configuration is done per-column of input phenotype data
- SurveyCTO form definitions can be automatically converted into shared model configurations, which can then be passed into the package

## Drops invalid columns

- Removes columns that are missing column headers
- Removes columns for which the header is `NA` or equivalent in R

## Sanitizes header content in the input phenotype data

- All remaining column headers are mapped to alphanumeric values based on the normalized encoded value provided in the dataset-specific config (e.g. `NN0001` in the example dataset-specific config)

## String cleanup

### Converts to lowercase

- Uniformly converts all alphabetical characters to lowercase
- This is to facilitate comparisons between values and accommodate data entry inconsistencies

### Remove whitespace

- Collapses any repeated whitespace into a single instance of that whitespace
- Takes any amount of space surrounding a dash between characters and collapses it down to just the dash (e.g. `ABC  - 123` is converted to `ABC-123`)

### Collapse repeats

- Specifically for back slashes, forward slashes, and periods, collapses repeating adjacent values to a single value
- Forward and back slashes are harmonized to forward slashes only

### Process Unicode characters

- Unicode characters are replaced with the most-likely matching non-Unicode intended character
- Currently hard-coded; flagged for moving into a user-configurable file

### Remove Excel error codes

- Takes a series of recognized Excel error codes and converts them into NAs
- Includes the following Excel error codes, in any case combination, and optionally prepended by an equals sign
  - `#DIV/0!`
  - `#ERROR!`
  - `#VALUE!`
- Counts the detected instances of these errors and emits this information into the downstream report

### Detects remaining Unicode characters

- Searches for anything remaining in the Unicode range and adds it to the downstream report for manual follow-up

### Removes non-word characters

- For variables that are not designated `string` in the dataset-specific config, do the following:
  - Prefix entries starting with a period and followed by numbers, prepend with a `0`
  - For entries starting with `>` or `<`, replace with `greater than` or `less than`, respectively
  - For entries that begin with `-` and are followed by at least one numeric character, the value is set to NA
  - Non-word characters are removed from the beginning and end of entries (non-word corresponds to `\W`, aka `[^A-Za-z0-9_]`)

### Normalizes missing values

- Attempt to capture a wide array of missing value indications, inclusive of common mispellings, and set them to NA
- This will catch some but not all potential intended NA values; additional NA values can be configured in the relevant section in the dataset-specific config

applies consent exclusions
applies variable-specific NA values
applies type conversions
excludes subjects missing subject IDs
applies bounds on numeric data
attempts to harmonize self-reported ancestry labels
creates derived variables
re-applies bounds on derived numeric variables
checks cross-variable dependencies
handles dependency failures
computes distribution data for numeric variables
removes subjects with excess invalid entries across all variables
emits an html report
writes out cleaned phenotype data in tsv and/or various other formats
