# Installation

## Method 1: Direct Installation from GitLab

R has the capacity to install packages directly from GitLab.

Run the following in [R](https://www.r-project.org/) or [RStudio](https://www.rstudio.com/):

```
# the following step is only required if you don't have the 'devtools' package installed yet
install.packages("devtools")
# the following steps are always required when launching R
library(devtools)
devtools::install_gitlab("data-analysis5/phenotypes/process.phenotypes@default", auth_token = devtools::github_pat())
```

**Note: Secured Access to GitLab**

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

------------------

## Method 2: Installation from Local Copy

There are various ways to install an R package from a local copy of the project.

### With a Tarball

The easiest way to get a tarball (`.tar.gz`) compressed version of the package is
to go to the [project GitLab page](https://gitlab.com/data-analysis5/phenotypes/process.phenotypes),
click the download button (to the left of the `Clone` button), select `tar.gz` as
output format, and save it somewhere on the local drive.

Then, choose one of the following methods:
- on the command line: `R CMD INSTALL /path/to/process.phenotypes-default.tar.gz`; or
- from RStudio: `Tools -> Install Packages -> Install from: Package Archive File`,
and select the tarball from your local drive.

### From a Git Clone

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

------------------

## Method 3: Installation from Conda (OSX and Linux only)

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

