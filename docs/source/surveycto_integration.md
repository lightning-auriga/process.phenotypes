# Integration with SurveyCTO

If you are using SurveyCTO to collect phenotype data, you may be able to use functionality in process.phenotypes for automatically converting a SurveyCTO [form](https://docs.surveycto.com/02-designing-forms/01-core-concepts/02.starting-a-new-form.html) to a set of configurations (dataset-specific and shared models) for use with this package.  We can not guarantee compatibility with all SurveyCTO features.

## Retrieve data from SurveyCTO

First, you'll need to export some data from SurveyCTO.  Both the form definition spreadsheet and the resulting collected phenotypes are required because in the case of repeat variables, you don't know how many repeats occur for any given block until you've looked in the questionnaire responses.

### Export response data from SurveyCTO

#### API call

```bash
curl -u "${EMAIL}:${PASSWORD}" https://${ORGANIZATION}.surveycto.com/api/v1/forms/data/wide/csv/${DATASET} -o "${OUTFILE}.csv"
```

#### Web interface

- Log in to SurveyCTO and navigate to the "Export" tab
- Select "Download form data" (or just "Download" depending on browser)
- In the dropdown labeled "Export repeat groups as:", select "Wide format (default)"
- In the dropdown labeled "Submissions to include:", select "All submissions (default)" (this may be slow)
- In the dropdown labeled "File format:", select ".csv (default)"
- Click "Export files"

### Export the form defition spreadsheet from SurveyCTO

- Navigate to the "Design" tab
- Select "Download"
- Select "Form files"
- Click on "Spreadsheet form definition", which should be an xlsx file

## Convert forms to process.phenotypes configurations

Assuming you've loaded the package in R (see [this page](execution.md) for details), you can use the function `parse.surveycto` to convert a SurveyCTO form spreadsheet into a set of yaml config files.  For example:

```R
parse.surveycto("surveycto_form.xlsx", "responses_in_wide_form.csv", "HW", "dataset-specific-config.yaml", "shared-model-config.yaml")
```

Inspect the output to confirm that it matches expectations and to make any required edits.  You will need to update fields for consent lists, minimum age threshold, etc.
