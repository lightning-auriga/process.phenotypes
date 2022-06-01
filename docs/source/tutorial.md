# Tutorial

## Installation

This is a brief example of one method of package installation.  Please see the [installation](installation.md) section for more details.


Acquire the library source code, then navigate into the directory, and open an R terminal.

```bash
git clone git@gitlab.com:data-analysis5/phenotypes/process.phenotypes.git
cd process.phenotypes/
R
```
## Configuration

Configuration files have been provided for you to run a small test dataset.  You can review them at `example/config/`; please see the [YAML Configuration](yaml_config.md) section for details on configuration options.  Run the following to validate the configuration files:

```bash
./yaml-configuration/yaml_validator.py example/config/HW.dataset-specific.yaml example/config/HW.shared_models.yaml
```

## Execution

Within R, load `devtools`, then use it to install `process.phenotypes`.  The example shown here will also pull in any required R dependencies (enumerated in the `DESCRIPTION` file in the source).

```r
library(devtools)
devtools::install(".", dependencies=TRUE)
```

Still within R, load the library, then run the command shown to process the test dataset provided in the repository.

```r
library(process.phenotypes)
process.phenoytpes("example/data/HW_phenotypes.tsv", "example/config/HW.dataset-specific.yaml", "example/config/HW.shared-models.yaml", "example/HW_example_output.html")
```

Look at your output.  You should see both `example/HW_example_output.html` and `example/HW_example_output.tsv`

## Modifying the Configuration

Now, try adding some features to the configuration files. ...
