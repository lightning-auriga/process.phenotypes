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

## Installation

### Direct Installation from GitLab

R has the capacity to install packages directly from GitLab.

Run the following in [R](https://www.r-project.org/) or [RStudio](https://www.rstudio.com/):

```
# the following step is only required if you don't have the 'devtools' package installed yet
install.packages("devtools")
# the following steps are always required when launching R
library(devtools)
devtools::install_gitlab("data-analysis5/phenotypes/process.phenotypes@default", auth_token = devtools::github_pat())
```

#### **Note: Secured Access to GitLab**

For security reasons, R must be permitted access to GitLab
in order to allow remote installation. Please follow the instructions
[here](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html)
to generate an access token with at least `read_api` access.

The convention in R is to set the access token to the `GITHUB_PAT` environment
variable (note that this is **GITHUB_PAT** even though the access is via GitLab;
this is a strange R quirk). You can do this by including the line

`GITHUB_PAT = YOURACCESSTOKEN`

in your R environment file `~/.Renviron`. Note that if you already have
an `R` or `RStudio` instance running, you need to restart `R` for the
`.Renviron` changes to be recognized.

### Alternative: Installation from Local Copy

There are various ways to install an R package from a local copy of the project.

#### **With a tarball**

The easiest way to get a tarball (`.tar.gz`) compressed version of the package is
to go to the [project GitLab page](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes),
click the download button (to the left of the `Clone` button), select `tar.gz` as
output format, and save it somewhere on the local drive.

Then, choose one of the following methods:
- on the command line: `R CMD INSTALL /path/to/process.phenotypes-default.tar.gz`; or
- from RStudio: `Tools -> Install Packages -> Install from: Package Archive File`,
and select the tarball from your local drive.

#### **From a git clone**

With `git` from the command line, depending on whether ssh or https connection is configured
for your system:

`git clone git@gitlab.com:data-analysis5/phenotypes/process.phenotypes.git`

`git clone https://gitlab.com/data-analysis5/phenotypes/process.phenotypes.git`

Then, launch `R`. Install and launch `devtools` as needed (see section 
"Direct Installation from GitLab" as necessary), and run the following commands:

```
library(devtools)
devtools::install("/path/to/process.phenotypes")
```

### Alternative: Installation from Conda (OSX and Linux only)

Note: this option will only be available slightly after this README goes live, and at that
time this message will be removed.

This package has been added to the 54gene [Conda](https://docs.conda.io/en/latest/) channel.
To install, first install and configure [miniconda](https://docs.conda.io/en/latest/miniconda.html).
Then add the following to your `~/.condarc` (creating the file if it does not already exist):

```
channels:
  - https://gitlab.com/data-analysis5/conda-54gene/-/raw/default/conda-54gene
```

From the command line, execute the following command:

`conda install r-process.phenotypes`

## Execution

First, load the library in the current R instance:

`library(process.phenotypes)`

The entry point for the software is `process.phenotypes::create.phenotype.report`. 
You can get useful help documentation for this function
in the usual R manner: `?process.phenotypes::create.phenotype.report`. An example
run command might be:

```{r}
process.phenotypes::create.phenotype.report("/path/to/CV.export.tsv",
                                            "yaml-configuration/CV.yaml",
                                            "yaml-configuration/shared-models.yaml",
                                            "/path/to/CV-output.html")
```

## YAML Configuration

This section is continually being expanded as the configuration feature set is
modified. For the time being, see existing dataset configuration files in 
[this directory](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes/-/tree/default/yaml-configuration)
for examples.

### Top-level YAML sections

- `tag`: dataset tag name (e.g. "CV"); used minimally but not quite deprecated to continue to support comparison between two datasets
- `globals`: contains two global settings that are applied across all variables
  - `min_age_for_inclusion`: (required) subjects will be excluded from all histograms etc. in the report and from the cleaned output if their age falls below this threshold
  - `max_invalid_datatypes_per_subject`: subjects will be excluded from  the cleaned output if they have more than this number of variables that can not be converted to the expected datatypes
  - `consent_inclusion_file`: name of file containing subject IDs with confirmed consent approval. format is: plaintext file, no header, one ID per line
  - `consent_exclusion_file`: name of file containing subject IDs without valid consent. format is: plaintext file, no header, one ID per line
    - the above two files are each optional. the files can be specified but empty. if neither is specified, no subjects will be included or excluded for consent. if one or the other but not both is specified, the included file's action will be applied alone. if both are specified, both actions will be performed, and additionally any subject ID found in the dataset but in neither list will be excluded and listed in a table in the final report for inspection. IDs cannot be present on both lists at once; if any such IDs are found, the program will complain.
- `variables`: this section contains one block for each variable in the dataset, with a variety of other configuration settings described in the next section
- `derived`: this section defines variables to be derived from existing variables

### `variables` YAML section

Each variable in the dataset is assigned a normalized encoded value (e.g. CV00001, CV00002, etc.).  Under each variable block, there are a variety of other possible configuration settings:
- `name`: this is the header of the variable in the input dataset
- `type`: (either this or shared_model are required) expected variable type; one of: string, numeric, ordinal, categorical; the variable type dictates what kinds of cleaning are applied to that variable
- `shared_model`: (either this or type are required) expected variable type as defined in `yaml-configuration/shared-models.yaml`
- `canonical_name`: if desired, a string with a more descriptive variable name than what's present in `name`
- `bounds`: min, max, and/or sd (standard deviation) bounds for a numeric variable
- `suppress_reporting`: a boolean to turn off printing a table of unique values and counts in the html report; useful for variables with PII or with many expected unique values like phone numbers
- `linked_date`: for `age` variables, this optionally points to a corresponding date variable for cross-comparison
  - `reported_year`: standardized name of variable containing corresponding year variable
  - `reference_year`: which year the age was collected
- `subject_age`: boolean flag to mark which variable is the accepted age of the subjects
- `subject_id`: boolean flag to mark which variable is the accepted unique subject ID
- `na-values`: any non-canonical values to be treated as NA (e.g. nil, not specified, etc.)
- `multimodal`: used to define another variable for plotting overlayed histograms, e.g. overlayed plots of BMI by sex
- `allow_undelimited_bp`: only for variables of type `bp` (blood pressure): enable recognition of systolic and diastolic blood pressure specified exactly as: `^\d{4}\d?\d?$`, where systolic will use three digits preferentially if 5 or 6 digits are specified. this behavior is imperfect given the lack of delimiter, and is not recommended in most circumstances.
- `dependencies`: test for expected relationships between variables; can also include contingency tables to compare two variables and instructions for setting values to NA if certain dependency tests fail

### `derived` YAML section

Derived variables are calculated from existing data, e.g. calculating BMI from reported waist and height measurements.  This section allows the user to define arbitrary new variables to derive.
- Most sections here have been previously described, but `code` is where the logic is injected to create the derived variable, written in `R` syntax with access to the normalized variable names

### YAML validation

Prior to running this tool, you should validate the yaml configurations you've set up as follows:

`./yaml_validator.py dataset_file.yaml shared_model_file.yaml`

If you are using pre-commit as described below, the yaml configurations will be validated automatically when you commit changes.

## Future Development Targets

### Imminent
- [x] input YAML format checker
- [x] derived variables, using format similar to dependency specification
- [x] expanded README documentation
- [x] improved report format, because whoa

### Longer Term
- [x] action to take upon dependency failure
- [x] data export formats
  - [x] plaintext/tsv
  - [x] STATA (dta)
  - [x] SAS (sas7bdat and auxiliary source file)
  - [x] SPSS (zsav)

### Open Proposals
- [x] aliased variable transformations
  - [x] alternatively, can use derived variables explicitly

## Version History
See changelog for more information.
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
