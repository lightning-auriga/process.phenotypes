# Execution

This section assumes you have already installed the package.  If not, please visit the [installation](installation.md) section.

## Environment Setup

An environment specification is provided in the repository (`environment.yaml`).  We recommend using [miniconda](https://docs.conda.io/en/latest/) to create an environment containing all dependencies needed for this package to run.  If you need to install miniconda, you can find instructions [here](https://docs.conda.io/en/latest/miniconda.html).  Once miniconda is configured, create the environment:

```bash
conda install mamba
mamba env create -f environment.yaml
```

Activate the environment and launch an R terminal:

```bash
conda activate process.phenotypes
R
```

## Run process.phenotypes

First, load the library in an R instance:

`library(process.phenotypes)`

The entry point for the software is `process.phenotypes::create.phenotype.report`. 
You can get useful help documentation for this function
in the usual R manner: `?process.phenotypes::create.phenotype.report`. An example
run command might be:

```r
process.phenotypes::create.phenotype.report("/path/to/input-phenotypes.tsv",
                                            "yaml-configuration/dataset-specific.yaml",
                                            "yaml-configuration/shared-models.yaml",
                                            "/path/to/output.html")
```

