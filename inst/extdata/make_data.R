library(stringr)

reformat.dob <- function(years) {
  res <- rep("", length(years))
  ##  yyyy-mm-dd
  ##  .*[ -/]yy(yy)?
  ##  yyyy
  ##  (full name of month),?yyyy
  ## other bad formats?
  mode.selection <- sample(1:5, length(years), replace = TRUE)
  res[mode.selection == 1] <- paste(
    years[mode.selection == 1],
    stringr::str_pad(as.character(sample(1:12,
      length(which(mode.selection == 1)),
      replace = TRUE
    )),
    2,
    pad = "0"
    ),
    stringr::str_pad(as.character(sample(1:28,
      length(which(mode.selection == 1)),
      replace = TRUE
    )),
    2,
    pad = "0"
    ),
    sep = "-"
  )
  res[mode.selection == 2] <- paste(sample(c(
    "jan", "feb", "mar", "apr", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec",
    "01", "02", "03", "04", "05", "06",
    "07", "08", "09", "10", "11", "12"
  ),
  length(which(mode.selection == 2)),
  replace = TRUE
  ),
  sample(c(" ", "-", "/"),
    length(which(mode.selection == 2)),
    replace = TRUE
  ),
  years[mode.selection == 2] %% sample(c(10000, 100),
    length(which(mode.selection == 2)),
    replace = TRUE
  ),
  sep = ""
  )
  res[mode.selection == 3] <- years[mode.selection == 3]
  res[mode.selection == 4] <- paste(c(
    "January", "February", "March",
    "April", "May", "June", "July",
    "August", "September", "October",
    "November", "December"
  ),
  sample(c(" ", ","),
    length(which(mode.selection == 4)),
    replace = TRUE
  ),
  years[mode.selection == 4],
  sep = ""
  )
  res[mode.selection == 5] <- years[mode.selection == 5] %% 100
  res
}

n.rows <- 100
## create subject IDs
subject.ids <- paste("ID", seq_len(n.rows), sep = "")
## intersperse NA values
subject.ids[sample(seq_len(n.rows), round(n.rows * 0.05), replace = FALSE)] <- NA

## compute subject age
subject.age <- sample(1:100, n.rows, replace = TRUE)

## compute date of birth
subject.year.of.birth <- 2022 - subject.age
## make it noisy
subject.year.of.birth <- subject.year.of.birth + sample(seq(-20, 20),
  n.rows,
  replace = TRUE,
  prob = c(
    rep(0.0175, 10),
    rep(0.03, 10),
    0.05,
    rep(0.03, 10),
    rep(0.0175, 10)
  )
)
## create chaos date formats
subject.dob <- reformat.dob(subject.year.of.birth)

## create unimodal subject height
subject.height <- rnorm(n.rows, 1.5, 0.25)
## for chaos reasons, apply units sometimes
subject.height <- paste(round(subject.height, 3),
  sample(c(" cm", "cm", " m", "m"),
    n.rows,
    replace = TRUE
  ),
  sep = ""
)

## create bimodal subject waist circumference
subject.wc.highmode <- rnorm(n.rows, 85, 5)
subject.wc.lowmode <- rnorm(n.rows, 35, 2)
subject.wc.selector <- sample(c(TRUE, FALSE), n.rows, replace = TRUE, prob = c(0.8, 0.2))
subject.wc <- subject.wc.lowmode
subject.wc[subject.wc.selector] <- subject.wc.highmode[subject.wc.selector]
subject.wc <- round(subject.wc, 2)

## create sex annotation with periodic chaos
subject.sex <- sample(c(
  "female", "Female", "male", "Male",
  "f", "F", "m", "M",
  "alive", "panda"
),
n.rows,
replace = TRUE
)

## create first question in a dependency statement
subject.eats.fruit <- sample(c("yes", "no", "not answered"),
  n.rows,
  replace = TRUE,
  prob = c(0.6, 0.3, 0.1)
)

## create second question in a dependency statement
subject.preferred.fruit <- sample(c("apple", "pear", "strawberry", NA),
  n.rows,
  replace = TRUE,
  prob = c(0.3, 0.3, 0.3, 0.1)
)

## make some chaos text
subject.letter.pair <- paste(sample(letters, n.rows, replace = TRUE),
  sample(letters, n.rows, replace = TRUE),
  sep = ""
)

## categorical to numeric
subject.rannum <- sample(1:4, n.rows, replace = TRUE)

## blood pressure
subject.bp <- paste(sample(100:250, n.rows, replace = TRUE),
  sample(c("", " ", "  "), n.rows, replace = TRUE),
  sample(c("-", "/", ","), n.rows, replace = TRUE),
  sample(c("", " ", "  "), n.rows, replace = TRUE),
  sample(50:150, n.rows, replace = TRUE),
  sep = ""
)

## ordinal
subject.awesomeness <- sample(c(
  "Not very awesome",
  "Kinda awesome",
  "Awesome",
  "Uncertain awesomeness",
  "Very Awesome",
  "UBER AWESOME"
),
n.rows,
replace = TRUE
)

## format the output data frame
out.df <- data.frame(
  subjid = subject.ids,
  age = subject.age,
  dob = subject.dob,
  height = subject.height,
  waist_circumference = subject.wc,
  sex = subject.sex,
  fruit = subject.eats.fruit,
  "preferred fruit" = subject.preferred.fruit,
  letters = subject.letter.pair,
  measure = subject.rannum,
  bloodpressure = subject.bp,
  awesomeness = subject.awesomeness,
  check.names = FALSE
)

out.df <- out.df[sample(seq_len(nrow(out.df)), nrow(out.df), replace = FALSE), ]
write.table(out.df, "raw_phenotypes.tsv",
  row.names = FALSE, col.names = TRUE,
  quote = FALSE, sep = "\t"
)
