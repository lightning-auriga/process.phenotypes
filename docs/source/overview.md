# Overview

This is an `R` package designed to help the process of phenotype
dataset cleaning be automated, rigorous, and transparent. The overall
cleaning process is simplified as follows:

- A phenotype spreadsheet is exported to any of several supported formats:
  - .tsv (plaintext, tab-delimited)
  - .dta (STATA format)
  - .sas7bdat (SAS format, with accompanying .sas code for category labels)
  - .zsav (SPSS format)
- The phenotype dataset is configured in 
[YAML](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
format. this allows the user to specify the [expected data type](variable_types.md)
(binary, categorical, ordinal, numeric, date, blood pressure, string), boundary conditions
for numeric values, levels and alias for binary/categorical/ordinal variables,
special values to be encoded as NA (missing) entries, and other restrictions.
- The entire cleaning process for the file is run with a single R command.
- After cleaning is complete, an HTML format report is emitted, reporting 
summary statistics and data cleaning observations (e.g. invalid values detected
for categorical variables); this file is both for recordkeeping and for helping
the user improve configuration for more refined cleaning.

