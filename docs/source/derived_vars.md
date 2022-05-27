# Derived variables

Users can define variables that are derived from existing questionnaire responses in the dataset-specific config file, under a `derived` yaml block.  See the following example, and the explanation below:

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
derived:
  HW001_derived:
    name: "sex binary encoding"
    type: numeric
    code: |
      binary.sex <- rep(NA, length(HW001))
      is.male <- HW001 == "male" & !is.na(HW001)
      is.female <- HW001 == "female" & !is.na(HW001)
      binary.sex[is.male] <- 0
      binary.sex[is.female] <- 1
      binary.sex
```

Note that there are two main blocks in the config - `variables`, which contains the questionnaire response data, and `derived`, which contains new variables created from the questionnaire response data.  The `variables` section is explained in more detail [here](yaml_config.md).  In the `derived` section, the one section that isn't present in the `variables` block is the `code` section.
- Within the `code` block, you can write arbitrary `R` syntax.  You have access to the variable names (e.g. `HW001`) from both the `variables` and the `derived` sections.  The variable name gives you access to the entire vector of data, and you can use that and modify it, but this will not change the underlying original data
- The code block should return the value that should be stored as the derived variable (the vector `binary.sex` in the example above)
- The return value should be of length n.subjects or a single value, in which case it will be extended to be assigned to all subjects

