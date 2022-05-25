# Execution

First, load the library in the current R instance:

`library(process.phenotypes)`

The entry point for the software is `process.phenotypes::create.phenotype.report`. 
You can get useful help documentation for this function
in the usual R manner: `?process.phenotypes::create.phenotype.report`. An example
run command might be:

```r
process.phenotypes::create.phenotype.report("/path/to/CV.export.tsv",
                                            "yaml-configuration/CV.yaml",
                                            "yaml-configuration/shared-models.yaml",
                                            "/path/to/CV-output.html")
```

