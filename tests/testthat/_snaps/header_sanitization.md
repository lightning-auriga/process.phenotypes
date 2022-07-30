# map.header reports an informative message and errors when column names mismatch

    Code
      map.header(in.phenotype.data, "HW", in.variable.summary)
    Output
           [,1]   [,2]  
      [1,] "var1" "VAR1"
      [2,] "var2" "VAR2"
    Error <simpleError>
      for dataset tag HW, column names in phenotypes do not match yaml config values

