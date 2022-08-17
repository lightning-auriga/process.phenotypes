# process.phenotypes: automated phenotype standardization and reporting

## Overview

This is an `R` package designed to help the process of phenotype
dataset cleaning be automated, rigorous, and transparent. The overall
cleaning process is simplified as follows:

- a phenotype spreadsheet is exported to any of several supported formats:
  - .tsv (plaintext, tab-delimited)
  - .dta (STATA format)
  - .sas7bdat (SAS format, with accompanying .sas code for category labels)
  - .zsav (SPSS format)
- the phenotype dataset is configured in 
[YAML](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
format. this allows the user to specify the expected data format
(binary, categorical, ordinal, numeric, date, blood pressure, string), boundary conditions
for numeric values, levels and alias for binary/categorical/ordinal variables,
special values to be encoded as NA (missing) entries, and other restrictions.
- the entire cleaning process for the file is run with a single R command.
- after cleaning is complete, an html format report is emitted, reporting 
summary statistics and data cleaning observations (e.g. invalid values detected
for categorical variables); this file is both for recordkeeping and for helping
the user improve configuration for more refined cleaning.

## Documentation

Please see any of the following documentation:

- This package's [Read the Docs](http://54gene-processphenotypes-docs.s3-website.us-east-2.amazonaws.com/index.html)
  contains extended documentation about various aspects of the package, including installation, configuration, and use.
- Once installed, the standard R man pages (accessed, for example, with `?create.phenotype.report`) contain extensive
  function interface documentation and examples.
- The package has several useful vignettes, specifically covering [manual dataset configuration](doc/manual-configuration.html),
  [configuration from SurveyCTO form definitions](configuration-from-SurveyCTO-form.Rmd),
  and the [creation of derived variables](doc/derived-variables.html), among other topics; see the `doc` directory of the GitLab
  repo or `r vignette(package = "process.phenotypes")` for more.


## Version History
See [changelog](CHANGELOG.md) for more information.
 * 19 Aug 2022: public release; tagged version 1.4.0
 * 01 Aug 2022: complete unit test coverage; refactor report and assorted minor fixes
 * 24 May 2022: merge CTO dataset configuration files and corresponding added functionality
 * 21 Sep 2021: initial release v1.0.0
 * 27 Aug 2021: derived variables, transformations, many assorted improvements, and better readme
 * 12 Jul 2021: string_cleanup branch merged into default; v0.1.0

## How to contribute to development

### Step 1: Set up a development environment (OSX and Linux only)

- If needed, install miniconda by following the steps [here](https://docs.conda.io/en/latest/miniconda.html).
- If needed, install [mamba](https://github.com/mamba-org/mamba): `conda install mamba`
- Clone a copy of this repo: 

```
git clone https://gitlab.com/data-analysis5/phenotypes/process.phenotypes.git
# or 
git clone git@gitlab.com:data-analysis5/phenotypes/process.phenotypes.git
```

- Navigate into the repo directory: `cd process.phenotypes`
- Create a conda environment with, minimally, the dependencies defined in `r-dev.yaml`.  Make sure to activate your dev environment whenever you are writing/committing code!

```
# create the env
mamba env create -f r-dev.yaml

# activate the env
conda activate r-dev
```

- Install [commitizen](https://github.com/commitizen/cz-cli) as follows

```
npm install -g commitizen cz-conventional-changelog
commitizen init cz-conventional-changelog --save-dev --save-exact
```

- Set up pre-commit hook scripts.  This will apply linting and check for some common code formatting errors every time you commit.  See https://pre-commit.com/ for more details.  

```
pre-commit install
```

- Install pre-commit in R (either in an R terminal or in Rstudio):

```{r}
install.packages("precommit")
```

### Step 2: Select an issue to work on, or submit one that you'd like to address

See the current [issues](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes/-/issues) for this project.

### Step 3: Contribute code

- All development work should branch off of the `dev` branch.  Make sure you're on the right branch: `git checkout dev`
- Make sure your repository is up-to-date with the remote: `git pull origin dev`
- Create a new branch named for the feature you're going to work on: `git checkout <feature_branch>`
- Write code and commit often!
    - Stage changes with `git add .`
    - Commit code with `git cz`; make sure to cite the issue number you're working on
    - Push your changes to the remote repository with `git push origin <feature_branch>`
- When you're all done, submit a merge request [here](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes/-/merge_requests).  Other developers will review your code, make comments, and merge in your changes when ready!
