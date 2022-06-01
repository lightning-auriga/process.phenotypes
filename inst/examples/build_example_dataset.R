out.data <- data.frame(
  "subject.ids" = c("A", "B", "C", "D", "E"),
  "age" = c("13", "18", "18", "20", "22"),
  "date of birth" = c("december2009", "05/2004", "04", "01/02", "05/1/2000"),
  "height" = c("1.5m", ".9m", "1.1", "1.2", "1.5m"),
  "sex" = c("male", "female", "female", "male", "male"),
  "blood pressure" = c("180/90", "220", "223/100", "231 / 44 mmhg", NA),
  "satisfaction rating" = c("satisfied", "neutral", "satisfied", "dissatisfied", "neutral"),
  "weight" = c("60kg", "70kg", "65.5", "70.4", "85"),
  check.names = FALSE
)
write.table(out.data, "example.data.tsv", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
