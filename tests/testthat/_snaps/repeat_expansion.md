# expand_surveycto_config correctly errors when it finds a variable it cannot handle, at the end

    Code
      expand.surveycto.config(current.form, dataset.yaml, current.data.pull,
        current.form.definition, current.data.merge, intermediate.dataset,
        intermediate.shared.models)
    Output
      [1] "computed result variables missing real output variables"
      [1] "var460"
    Error <simpleError>
      output variable prediction has failed

# expand_surveycto_config correctly errors when it finds a variable it cannot handle, in the middle

    Code
      expand.surveycto.config(current.form, dataset.yaml, current.data.pull,
        current.form.definition, current.data.merge, intermediate.dataset,
        intermediate.shared.models)
    Output
      [1] "computed result variables missing real output variables"
      [1] "var460"
    Error <simpleError>
      output variable prediction has failed

