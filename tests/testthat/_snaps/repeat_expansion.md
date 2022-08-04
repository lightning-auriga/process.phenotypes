# expand_surveycto_config correctly errors when it finds a variable it cannot handle

    Code
      expand.surveycto.config(current.form, dataset.yaml, current.data.pull,
        current.form.definition, current.data.merge, intermediate.dataset,
        intermediate.shared.models)
    Output
      [1] "computed result variables missing real output variables"
      [1] "var460"
    Error <simpleError>
      output variable prediction has failed

