# Execution

This section assumes you have already installed the package.  If not, please visit the [installation](installation.md) section.

First, load the library in an R instance:

`library(process.phenotypes)`

The entry point for the software is `process.phenotypes::create.phenotype.report`. 
You can get useful help documentation for this function
in the usual R manner: `?process.phenotypes::create.phenotype.report`. An example
run command might be:

```r
process.phenotypes::create.phenotype.report("/path/to/HW_input_phenotypes.tsv",
                                            "yaml-configuration/dataset-specific.yaml",
                                            "yaml-configuration/shared-models.yaml",
                                            "/path/to/HW_output.html")
```

