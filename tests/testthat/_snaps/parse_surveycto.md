# parse.surveycto correctly detects header prediction failure: extra expected variables

    Code
      parse.surveycto(in.xlsx, in.csv, in.tag, out.dataset.yaml.fname,
        out.shared.models.fname, subject.id.name = "subjectid_1", age.name = "subjectage")
    Output
      [1] "computed result variables are not present in real data"
      [1] "starttime"
    Error <simpleError>
      output variable prediction has failed

# parse.surveycto correctly detects header prediction failure: extra observed variables

    Code
      parse.surveycto(in.xlsx, in.csv, in.tag, out.dataset.yaml.fname,
        out.shared.models.fname, subject.id.name = "subjectid_1", age.name = "subjectage")
    Warning <simpleWarning>
      unrecognized CTO type flag detected: "alientype", for name "alienname" and label "this shouldn’t exist"
    Output
      [1] "computed result variables missing real output variables"
      [1] "deviceid"
    Error <simpleError>
      output variable prediction has failed

# parse.surveycto correctly detects header prediction failure: correct variables but wrong order

    Code
      parse.surveycto(in.xlsx, in.csv, in.tag, out.dataset.yaml.fname,
        out.shared.models.fname, subject.id.name = "subjectid_1", age.name = "subjectage")
    Output
      [1] "output variables are correct but in the wrong order"
           responses   output.predicted.headers
      [1,] "endtime"   "starttime"             
      [2,] "starttime" "endtime"               
    Error <simpleError>
      output variable prediction has failed

