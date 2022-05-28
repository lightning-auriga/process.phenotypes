# Variable types available

## About variable types

Variable types are assigned in the dataset-specific config or the shared models config.  The type of a variable will influence how it is handled in terms of cleaning and reporting.  For example, a numeric type variable will have leading non-numeric characters stripped, will be displayed in the html report with min/max/mean/deciles, number of NA values, and a histogram.

## Enumerated variable types

### Numeric

Suitable for numeric values, both float and integer.  Configured by entering `type: numeric` in the config.

### Date

Suitable for date representations in a variety of formats.  Configured by entering `type: date` in the config.  Date formats are matched in hierarchical order as follows:

- `(\d{4})-\d{2}-\d{2}`
- `.*[/ -](\d{2}|\d{4})`
- `\d{4}`
- `january|february|march|april|may|june|july|august|september|october|november|december,?(\\d{4})`
- Note the capture groups defined above.  We are subsetting out the four-digit year and using that as the entirety of the date entry.  This is done to accommodate the wide variety of possible date formats in a single free-text entry field, and to address the preponderance of rounded dates (e.g. many entries are reported as January 1, YYYY)
- The resulting captured year values are further processed as follows:
  - If the captured year is two digits and less than or equal to 21, add 2000
  - If the captured year is two digits and less than 100, add 1900
  - If the captured year is less than 1800, set to NA

### String

Suitable for any value that doesn't easily fit into the other variables, e.g. free text responses, doctor's notes, etc.  Configured by entering `type: string` in the config.  The least modified of the data types, with the least summary data in the html report.  If there are fewer than a configurable number of unique values, a table will be included in the report that lists the number of occurrences of each value.

(categorical)=
### Categorical

Suitable for responses that fit in a small number of expected unordered categories.  Configured by entering `type: categorical` in the config, and then by defining levels for the categories.  E.g.:

```yaml
variables:
   HW001:
     name: "example variable"
     type: categorical
     levels:
       lvl1:
         name: "male"
         alternate_patterns:
           - "m"
       lvl2:
         name: "female"
       lvl3:
         name: "prefer not to answer"
```

Entries in the variable will be matched to the levels defined in the config (see above) based on exact match to name or match to any provided alternate pattern as a regular expression.  Alternate pattern matches are re-mapped to the canonical name in the output.  The first values under `levels`, e.g. `lvl1` above, are arbitrary strings - you can call them whatever you like as long as it's a unique string within the variable.  This was originally intended to hold the numeric encoding to be indicated for STATA, but this is not currently implemented.  The report will show a table of counts of observations of the harmonized level names.

### Ordinal

Suitable for responses that fit in a small number of expected categories with intrinsic ordering.  Configured by entering `type: ordinal` in the config, and then by defining levels as described [here](categorical).

### Blood pressure

Suitable for blood pressure entries in the format of SBP/DBP (specifically, `\d+ */ *\d+.*`, to accommodate potential units being included in the entry, e.g. "110/70 mmhg").  Configured by entering `type: blood_pressure` or `type: bp` in the config.  This is intended to then be used as input into two derived variables, one for systolic, the other for diastolic.

### Binary

This is a convenience alias for [categorical](categorical) variables with only two levels.

### Categorical to numeric

_Experimental_

Intended for use when something is received as a categorical encoding, but the levels are numeric values.  For example, if the input has levels 1 week, 2 weeks, and 3 weeks, the use of this type will enable automatic conversion to numerics 1, 2, and 3.  Configured by entering `type: categorical_to_numeric` in the config.`
