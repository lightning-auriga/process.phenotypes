# Tutorial

## Installation

This is a brief example of one method of package installation.  Please see the [installation](installation.md) section for more details.


Acquire the library source code, then navigate into the directory.

```bash
git clone git@gitlab.com:data-analysis5/phenotypes/process.phenotypes.git
cd process.phenotypes/
```
## Configuration

Configuration files have been provided for you to run a small test dataset.  You can review them at `inst/examples/`; please see the [YAML Configuration](yaml_config.md) section for details on configuration options.  Run the following to validate the configuration files:

```bash
./yaml-configuration/yaml_validator.py inst/examples/example.dataset.yaml inst/examples/example.shared_models.yaml yaml-configuration/schema.datasets.yaml yaml-configuration/schema.shared-models.yaml
```

Note that this requires access to Python 3.x and the jsonschema package.

## Execution

Within R, load `devtools`, then use it to install `process.phenotypes`.  The example shown here will also pull in any required R dependencies (enumerated in the `DESCRIPTION` file in the source).

```r
library(devtools)
devtools::install(".", dependencies=TRUE)
```

Still within R, load the library, then run the command shown to process the test dataset provided in the repository.

```r
library(process.phenotypes)
if (!dir.exists("example_output")) {
    dir.create("example_output")
}
process.phenotypes::create.phenotype.report(system.file("examples/example.data.tsv", package = "process.phenotypes"), system.file("examples/example.dataset.yaml", package = "process.phenotypes"), system.file("examples/example.shared_models.yaml", package = "process.phenotypes"), "example_output/example.output.html")
```

Look at your output.  You should see both `example_output/example.output.html` and `example_output/example.output.tsv`

## Modifying the Configuration

Now, try adding some features to the configuration files. ...
