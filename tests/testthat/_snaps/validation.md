# validator recognizes unacceptable dataset yaml

    Code
      config.validation(dataset.yaml.invalid, shared.models.yaml.valid,
        dataset.schema, shared.models.schema)
    Output
      File files/validation/shared-models-valid.yaml passes schema validation!
    Error <validation_error>
      4 errors validating json:
      	- /variables/HW00003 (#/properties/variables/patternProperties/%5E.*%24/oneOf/0/additionalProperties): must NOT have additional properties
      	- /variables/HW00003 (#/properties/variables/patternProperties/%5E.*%24/oneOf/1/required): must have required property 'shared_model'
      	- /variables/HW00003 (#/properties/variables/patternProperties/%5E.*%24/oneOf/1/additionalProperties): must NOT have additional properties
      	- /variables/HW00003 (#/properties/variables/patternProperties/%5E.*%24/oneOf): must match exactly one schema in oneOf

# validator recognizes unacceptable shared models yaml

    Code
      config.validation(dataset.yaml.valid, shared.models.yaml.invalid,
        dataset.schema, shared.models.schema)
    Error <validation_error>
      1 error validating json:
      	- /models/yesno (#/properties/models/patternProperties/%5E.*%24/required): must have required property 'type'

