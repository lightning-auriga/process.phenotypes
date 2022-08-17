#!/usr/bin/env Rscript
library(jsonvalidate)
library(process.phenotypes)
library(rjson)
library(yaml)

cargs <- commandArgs(trailingOnly = TRUE)
if (length(cargs) != 4) {
  stop(
    "usage: yaml_validator.R dataset-yaml shared-models-yaml",
    "schema.datasets.yaml schema.shared-models.yaml"
  )
}

dataset.yaml <- cargs[1]
shared.models.yaml <- cargs[2]
dataset.schema <- cargs[3]
shared.models.schema <- cargs[4]

process.phenotypes::config.validation(
  dataset.yaml,
  shared.models.yaml,
  dataset.schema,
  shared.models.schema
)
