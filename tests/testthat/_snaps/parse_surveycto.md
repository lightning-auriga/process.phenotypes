# parse.surveycto correctly detects header prediction failure: extra expected variables

    Code
      parse.surveycto(in.xlsx, in.csv, in.tag, out.dataset.yaml.fname,
        out.shared.models.fname, subject.id.name = "subjectid_1", age.name = "subjectage")
    Output
      [1] "computed result variables are not present in real data"
      [1] "starttime"
    Error <simpleError>
      output variable prediction has failed

