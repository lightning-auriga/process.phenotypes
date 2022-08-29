# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

- nothing for the moment


## [1.4.0] - 2022-08-29

### Added
- yaml validation as built-in, exported package function
- SurveyCTO repeat expansion utility function
### Changed
- report Rmd is largely refactored into R helper functions
- unit test coverage now 100%
- roxygen documentation revised for all functions
- roxygen documentation CRAN style compliant
- readthedocs and vignettes improved for style
### Removed
- references to python-based yaml validation
  - this was long ago deprecated, but further references to it are removed
### Fixed
- `parse.surveycto` respects capitalization in csv header names for capitalized factor levels
  - this issue was correctly caught by final sanity check, and should not impact anyone
- `parse.surveycto` no longer emits multiple redundant `alternate_patterns` for most shared models
  - this was asthetic but still frustrating
  - fixed by specifying that input yaml sequences should be maintained as lists 
  - expect there are some hidden functionalities in the package that may yet break due to correction
- `create.phenotype.report` optional output yaml should now be compatible with reloading output tsv into the package
- added missing CI dependency


## [1.3.0] - 2022-07-12

### Added
- new configuration option:
  - for `dependencies` block, in addition to `exclude_on_failure`, there is `exclude_all_on_failure: yes` in user config space. this will cause subjects failing a dependency to be set to NA for _all_ variable responses in the dataset, with the exception of the variable tagged `subject_id: yes`. this is an extreme option, and should be saved for the most toxic dependency failures (for example, repeated response to the same question varies incompatibly, indicating possible file merge failure or something like that).
### Fixed
- output tsv from `create.phenotype.report` now reflects NA settings from dependency error handlers `exclude_on_failure` and `exclude_all_on_failure`

## [1.2.3] - 2022-07-11

### Added
- new configuration option:
  - for `type: "bp"` variables, a boolean `allow_undelimited_bp` flag. if set to yes, blood pressure entries without a canonical delimiter between SBP and DBP will be tolerated and reformatted under the following conditions:
	- if the string representation is `^\d{6}$`, the parsed value is `\d\d\d/\d\d\d`
    - if the string representation is `^\d{5}$`, the parsed value is `\d\d\d/\d\d`
	- if the string representation is `^\d{4}$`. the parsed value is `\d\d/\d\d`
  - this option is more error-prone than most of the package's heuristics, and is thus disabled by default. it should only be used if a vast number of blood pressure responses lack delimiters, and even then, the user should evaluate whether the data recapture from the method is worth the introduced noise.

## [1.2.2] - 2022-07-08

### Added
- minor Unicode mapping addition: endash
### Changed
- `type: "bp"` variables now accept either `-` or `,` as a delimiter between SBP and DBP measurements

## [1.2.1] - 2022-07-05

### Fixed
- in `create.phenotype.report`, `rmarkdown::render` now correctly uses run-specific directories for intermediate generation. this fixes a sporadic race condition when multiple reports created in the same conda environment led to missing temporary files, due to promiscuous intermediate deletion under the hood by `rmarkdown`

## [1.2.0] - 2022-06-24

### Changed
- `parse.surveycto` output formatting change:
  - the function uses conventional `[A-Z]+\d\d\d\d\d` variable tags in its output. those variable tags are now incremented after a repeat or multiple response block by exactly the number of configured variables, _not_ the _number of observed repeats/multiples_. as an example, the desired behavior is:
```{yaml}
      - HW00001_1
      - HW00002_1
      - HW00001_2
      - HW00002_2
      - HW00003   # this was previously unhelpfully being tagged `HW00005`
```
  - this is mostly aesthetic, but has beneficial behaviors in some contexts

## [1.1.3] - 2022-06-24

### Changed
- Unicode character conversions are no longer hard-coded. mapping table is available in plaintext in

```{r}
system.file("unicode_pattern_replacements.tsv", package = "process.phenotypes"`)
```

- mapping table _may_ cause issues with Windows runs, according to docs; but we've yet to observe it ourselves

## [1.1.2] - 2022-06-09

### Added
- entire code base? see [readthedocs](https://54geneprocessphenotypes.readthedocs.io/en/latest/) for elaborated feature set

## [0.1.0] - 2021-07-12
### Added
- Initial release!




[//]: # (- Added)
[//]: # (- Changed)
[//]: # (- Deprecated)
[//]: # (- Removed)
[//]: # (- Fixed)
[//]: # (- Security)
