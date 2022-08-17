# Helper Functions

This package includes a variety of exposed helper functions that can be called from the configuration files, which are generally used either in [cross-variable dependencies](dependencies) or creation of [derived variables](derived_vars.md).  All of these helper functions can be elaborated in the usual way in an R terminal (e.g. `?process.phenotypes::derive.rank.normal.transform`), but this page provides a brief listing of the functions available.

```R
process.phenotypes::response.depends.on.not.na
process.phenotypes::response.depends.on.yes
process.phenotypes::response.is.duplicate.of
process.phenotypes::response.is.computed.bmi
process.phenotypes::response.is.less.than
process.phenotypes::response.is.greater.than
process.phenotypes::year.is.consistent.with.age
process.phenotypes::derive.rank.normal.transform
process.phenotypes::derive.first.degree
```
