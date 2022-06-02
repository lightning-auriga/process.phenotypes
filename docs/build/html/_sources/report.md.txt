# HTML Report

This software generates both a cleaned output matrix as well as a report indicating various cleaning behaviors and displaying, where relevant, distribution information and other summary data.

## Summary and overall metrics

The first part of the report describes overall counts (e.g. subjects in the input, subjects removed for consent status, subjects below the minimum permissible age, subjects removed for containing too many mismatching variable types, variables set to NA due to failed dependency checks, and final number of subjects remaining).  Note that most of these thresholds and inclusion/exclusion criteria are user-configurable in the YAML configuration; the report simply states the results of applying that config to the dataset.

NA counts by subject and by variable are reported as distributions and as tables of the top ten highest NA counts.  This allows quick troubleshooting, e.g. identifying a variable that was misconfigured resulting in ~all NAs, or a subject that was a placeholder or test subject who does not have the expected data type entries.

## Linker table

A table is emitted into the report that links the uniform code assigned by the process.phenotypes software with both the original variable name and the question description.  The use of uniform assigned codes permits processing of input datasets that may have inadvertently duplicated headers; being able to read in both apparent duplicate variables is important and can subsequently allow analysis via, for example, asserting a dependency between the two variables that values for each subject are equivalent.

## Variable summaries

The remainder of the report provides summaries of each of the variables.  Report output changes based on variable [type](variable_types.md)

### Numeric

Report contains a histogram of the data, with vertical dashed lines indicating +/- 3 standard deviations from the mean.  It also contains a table with the mean, min, max, deciles, and number of NAs.  If any values are not in numeric format after parsing, those are reported as well.

### Date

Same as numeric.

### String

Report contains a contingency table of listing the number of occurrences of each unique string, unless the number of unique strings are greater than the value set in `uniq.var.inclusion.prop`, which is a parameter of `create.phenotype.report`.

### Categorical

Report contains a table of the expected categories and the number of observations of each.  It also enumerates any values encountered that were not mapped to expected categories.

### Ordinal

Same as categorical.

### Blood Pressure

Same as string, except it will report any entry that does not match expected blood pressure format.  This variable type is intended to be further parsed into derived numeric variables representing SBP and DBP.

### Binary

Same as categorical.

### Categorical to Numeric

*Experimental*

Same as numeric.

### Derived Variables

Derived variables are reported in accordance to their assigned type, except that they also include the code snippet used to define the variable in the configuration.

## Configuration of Report

Configuration flags can be used to add or suppress report content, as described [here](yaml_config.md).
