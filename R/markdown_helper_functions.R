#' Helper function to create highest/lowest ten tables in markdown report.
#'
#' @description
#' Takes a named vector containing values (e.g. subjects) and counts,
#' sorts by number of counts, takes the top ten, and returns tabular
#' information about the data in a data frame for potential reporting.
#'
#' @details
#' This is a helper function used to grab the highest or lowest ten values
#' to print as a table in the markdown report. Though it refers to grabbing
#' the top ten values, it will return less information if there are fewer
#' than ten observations in the input.
#'
#' @param decreasing Logical controlling sort order on data to be reported.
#' @param vec Named vector containing values, generally subject IDs or
#' phenotype names, and counts of observations related to those identifiers.
#' @param column.label Character vector name of first column (e.g. Subjects,
#' Variables).
#' @return Data frame with (at most) ten rows containing values
#' and counts.
#' @examples
#' data <- sample(1:20, 20, replace = FALSE)
#' names(data) <- letters[1:20]
#' result <- process.phenotypes:::get.top.ten(TRUE, data, "Letters")
get.top.ten <- function(decreasing, vec, column.label) {
  ten.df <- data.frame(c(), c())
  if (length(vec) > 0) {
    vec.sorted <- sort(vec, decreasing = decreasing)[seq_len(min(10, length(vec)))]
    ten.df <- data.frame(names(vec.sorted), vec.sorted)
    colnames(ten.df) <- c(column.label, "Counts")
    rownames(ten.df) <- NULL
  }
  ten.df
}


#' @title
#' Try to ensure that the number of bins in a histogram isn't too close to the
#' number of unique values.
#'
#' @description
#' The phenotype report features general purpose histograms that are
#' created from a wide array of unstructured numeric information. In
#' some contexts, the bin value selected makes a huge difference for
#' the utility of such plots. This entirely heuristically attempts
#' to generate a bin setting that works _pretty well_ for a wide
#' array of data configurations. As always, ymmv.
#'
#' @param vec Numeric vector of input data to histogram.
#' @return Numeric setting for geom_histogram bin count.
#' @examples
#' data <- rnorm(100)
#' bin.count <- process.phenotypes:::get.bins(data)
get.bins <- function(vec) {
  n.unique.values <- length(unique(vec))
  if (n.unique.values > 50) {
    50
  } else {
    n.unique.values
  }
}


#' @title
#' Helper function to conditionally print output content.
#'
#' @description
#' If the passed value is null, no action is taken; otherwise,
#' the thing is emitted wrapped in a print() or cat() statement.
#'
#' @details
#' Other data types could presumably benefit from conditional
#' behavior here, but this distinction is the only one that
#' has had any meaning for the cleaning report as of this writing.
#'
#' This function exists almost exclusively to avoid cyclomatic
#' complexity errors in linting. If that sentence makes no sense
#' to you, don't worry about it.
#'
#' @param val thing to be conditionally printed
#' @return NULL
#' @examples
#' my.data <- data.frame(x = rnorm(1000))
#' my.plot <- ggplot2::ggplot(ggplot2::aes(x), data = my.data) +
#'   ggplot2::geom_histogram()
#' process.phenotypes:::print.conditionally(my.plot)
print.conditionally <- function(val) {
  if (!is.null(val)) {
    if (inherits(val, "knitr_kable")) {
      cat(val)
    } else {
      print(val)
    }
  }
}

#' @title
#' Helper function to emit formatted markdown header summarizing
#' data cleaning process.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' for the top of the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param dataset.yaml Character vector name of input dataset yaml file.
#' @param total.initial.sample.size Numeric number of lines in initial input data file.
#' @param consent.inclusion.file Character or NULL. Name of input file containing
#' consent approved subject list, or NULL if none provided.
#' @param subjects.consent.yes Numeric; if consent.inclusion.file is not
#' NULL, the number of subjects in the input dataset that were present in the
#' input consent approved subject list.
#' @param consent.exclusion.file Character or NULL. Name of input file containing
#' consent rejected subject list, or NULL if none provided.
#' @param subjects.consent.no Numeric; if consent.exclusion.file is not
#' NULL, the number of subjects in the input dataset that were present in the
#' input consent rejected subject list.
#' @param subjects.ambiguous.consent Numeric; if consent.inclusion.file and
#' consent.exclusion.file are both not NULL, the number of subjects in the
#' input dataset that were absent from both consent lists.
#' @param subjects.excluded.for.age Numeric number of subjects with reported
#' age less than the minimum value specified in dataset yaml/globals/min_age_for_inclusion.
#' @param min.age.for.inclusion Numeric entry from dataset yaml/globals/min_age_for_inclusion.
#' @param na.subject.id.count Numeric number of subjects with invalid subject ID entries.
#' @return Character formatted header content.
#' @usage NULL
emit.markdown.header <- function(dataset.yaml,
                                 total.initial.sample.size,
                                 consent.inclusion.file,
                                 subjects.consent.yes,
                                 consent.exclusion.file,
                                 subjects.consent.no,
                                 subjects.ambiguous.consent,
                                 subjects.excluded.for.age,
                                 min.age.for.inclusion,
                                 na.subject.id.count) {
  out.msg <- paste("This report and the accompanying cleaned data is generated by the code available ",
    "at <a href=\"https://gitlab.com/data-analysis5/process.phenotypes\">this</a> repository, ",
    "run using the configuration outlined in ", dataset.yaml, ".\n<br><br>\n",
    "The report is a summary of the phenotype data and includes subject-level summary metrics, ",
    "variable-level summary metrics, a linker between original variable names and standardized encoded names, ",
    "and per-variable summaries.  The per-variable summaries vary based on data type.  Numeric types will ",
    "display a histogram and min/max/mean/median/deciles.  Categorical and ordinal types display counts of each ",
    "factor level.  All types other than string include a summary of data entries that are not consistent with ",
    "the indicated data type.  Dependencies, bounds, data types, etc. are defined in the accompanying configuration ",
    "file.\n\n",
    "Please note that with few exceptions, the original variable names and descriptions in this report are read ",
    "verbatim from the input data, and have not been edited, to maintain harmonization with the original ",
    "questionnaire definitions.\n\n",
    total.initial.sample.size, ifelse(total.initial.sample.size == 1, " subject was", " subjects were"),
    " originally present in this dataset.",
    ifelse(!is.null(consent.inclusion.file),
      paste(" ", subjects.consent.yes,
        ifelse(subjects.consent.yes == 1,
          " subject was",
          " subjects were"
        ),
        " included with verified consent.",
        sep = ""
      ),
      ""
    ),
    ifelse(!is.null(consent.exclusion.file),
      paste(" ",
        subjects.consent.no,
        ifelse(subjects.consent.no == 1,
          " subject was",
          " subjects were"
        ),
        " excluded for lacking consent.",
        sep = ""
      ),
      ""
    ),
    ifelse(!is.null(consent.inclusion.file) & !is.null(consent.exclusion.file),
      paste(" ",
        length(subjects.ambiguous.consent),
        ifelse(length(subjects.ambiguous.consent) == 1,
          " subject was",
          " subjects were"
        ),
        " additionally excluded for not being explicitly listed as either having or lacking consent.",
        sep = ""
      ),
      ""
    ),
    " ", subjects.excluded.for.age,
    ifelse(subjects.excluded.for.age == 1, " subject was ", " subjects were "),
    "removed for being below the minimum permissible age of ", min.age.for.inclusion, ".",
    "\n\n",
    na.subject.id.count,
    ifelse(na.subject.id.count == 1, " subject was ", " subjects were "),
    "removed for lacking a subject ID.\n\n",
    ifelse(length(subjects.ambiguous.consent) > 0, "***\n<br>", ""),
    sep = ""
  )
  out.msg
}


#' @title
#' Helper function to report tracking information about variable
#' name and, for derived variables, code creating the variable.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @return NULL
#' @usage NULL
report.name.and.code <- function(variable.entry) {
  if (!is.null(variable.entry$params$canonical_name)) {
    cat(
      "Official variable identity: \"",
      variable.entry$params$canonical_name, "\"", "\n\n",
      sep = ""
    )
  }
  if (!is.null(variable.entry$params$code)) {
    cat(
      "\n\nThe logic to create this derived variable is as follows:\n\n```\n",
      variable.entry$params$code, "\n```\n\n",
      sep = ""
    )
  }
}

#' @title
#' Helper function to report detected Excel problems in variable contents.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return NULL
#' @usage NULL
report.excel.problems <- function(variable.entry, suppress.reporting) {
  if (!is.null(variable.entry$excel.problem.count) && !suppress.reporting) {
    if (variable.entry$excel.problem.count == 1) {
      cat("\nWARNING: 1 Excel error code detected in this variable.\n")
    } else {
      cat("\nWARNING: ", variable.entry$excel.problem.count,
        " Excel error codes detected in this variable.\n",
        sep = ""
      )
    }
  }
}

#' @title
#' Helper function to report summary information about numeric
#' variables' distributions.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param data.vec Numeric vector of column from phenotype dataframe
#' for this variable.
#' @param phenotype.data Data frame of full phenotype data for selection
#' of linked variable contents.
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param variable.pretty.name Character vector of human-legible name of
#' variable with more helpful description.
#' @param my.theme ggplot2 accumulated theme settings.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return List; first entry 'hist.plot' is the variable distribution plot,
#' second entry 'tab.summary' distribution table. Either entry can be null
#' based on conditional .logic
#' @importFrom graphics hist
#' @usage NULL
report.numeric.summary <- function(data.vec,
                                   phenotype.data,
                                   variable.entry,
                                   name,
                                   variable.pretty.name,
                                   my.theme,
                                   suppress.reporting) {
  hist.plot <- NULL
  tab.summary <- NULL
  if (is.vector(data.vec, mode = "numeric") && !suppress.reporting) {
    if (length(which(!is.na(data.vec)))) {
      ## create data histogram for numeric data
      plot.data <- data.frame(x = data.vec[!is.na(data.vec)])
      if (length(unique(plot.data$x)) > 0) {
        hist.plot <- ggplot2::ggplot(data = plot.data) + my.theme
        nbins <- get.bins(data.vec[!is.na(data.vec)])
        binwidth <- (diff(range(data.vec[!is.na(data.vec)])) + 1) / nbins
        if (!is.null(variable.entry$params$multimodal)) {
          multimodal.varname <- variable.entry$params$multimodal
          stopifnot(multimodal.varname %in% colnames(phenotype.data))
          multimodal.values <- unique(phenotype.data[!is.na(phenotype.data[, multimodal.varname]), multimodal.varname])
          hist.plot.colours <- RColorBrewer::brewer.pal(max(3, length(multimodal.values)), "Dark2")
          hist.plot.colours <- hist.plot.colours[seq_len(length(multimodal.values))]
          annotate.y <- max(hist(data.vec, breaks = nbins, plot = FALSE)$counts) / nrow(phenotype.data)
          for (i in seq_len(length(multimodal.values))) {
            if (length(which(phenotype.data[, multimodal.varname] == multimodal.values[i] &
              !is.na(data.vec))) == 0) {
              next
            }
            plot.subset <- phenotype.data[phenotype.data[, multimodal.varname] == multimodal.values[i] &
              !is.na(data.vec) & !is.na(phenotype.data[, multimodal.varname]), ]
            hist.plot <- hist.plot + ggplot2::geom_histogram(ggplot2::aes_string(
              x = name,
              y = "..count../sum(..count..)"
            ),
            data = plot.subset,
            bins = nbins,
            binwidth = binwidth,
            fill = hist.plot.colours[i],
            alpha = 0.2
            )
            mode.mean <- mean(plot.subset[, name],
              na.rm = TRUE
            )
            hist.plot <- hist.plot + ggplot2::geom_vline(
              xintercept = mode.mean,
              colour = hist.plot.colours[i]
            )
            hist.plot <- hist.plot + ggplot2::annotate("text",
              label = multimodal.values[i],
              x = mode.mean * 1.15,
              y = annotate.y * (1 - (i - 1) * 0.05),
              size = 3,
              hjust = 0,
              colour = hist.plot.colours[i]
            )
          }
        } else {
          hist.plot <- hist.plot + ggplot2::geom_histogram(
            ggplot2::aes_string(x = "x", y = "..count.. / sum(..count..)"),
            bins = nbins,
            binwidth = binwidth
          )
        }
        hist.plot <- hist.plot + ggplot2::xlab(name) + ggplot2::ylab("proportion of data")
        if (is.null(variable.entry$params$bounds$sd)) {
          var.mean <- mean(data.vec, na.rm = TRUE)
          var.sd <- sd(data.vec, na.rm = TRUE)
          if (!is.na(var.sd)) {
            hist.plot <- hist.plot + ggplot2::geom_vline(
              xintercept = var.mean + 3 * var.sd,
              linetype = "dashed"
            )
            hist.plot <- hist.plot + ggplot2::geom_vline(
              xintercept = var.mean - 3 * var.sd,
              linetype = "dashed"
            )
          }
        }
        cat(
          "\n\n#### Histogram of ", name, " (", variable.pretty.name,
          ") Distribution\n\n",
          sep = ""
        )
      }
    }
    if (!is.null(variable.entry$params$bounds)) {
      bound.type <- c()
      bound.value <- c()
      bound.count <- c()
      if (!is.null(variable.entry$params$bounds$min)) {
        bound.type <- c(bound.type, "minimum")
        bound.value <- c(bound.value, variable.entry$params$bounds$min)
        bound.count <- c(bound.count, variable.entry$num.below.min)
      }
      if (!is.null(variable.entry$params$bounds$max)) {
        bound.type <- c(bound.type, "maximum")
        bound.value <- c(bound.value, variable.entry$params$bounds$max)
        bound.count <- c(bound.count, variable.entry$num.above.max)
      }
      if (!is.null(variable.entry$params$bounds$sd)) {
        bound.type <- c(bound.type, "standard deviation")
        bound.value <- c(bound.value, variable.entry$params$bounds$sd)
        bound.count <- c(bound.count, variable.entry$num.beyond.sd)
      }
      bound.df <- data.frame(
        Type = bound.type,
        Value = bound.value,
        Count = bound.count
      )
      rownames(bound.df) <- NULL
      table.caption <- paste("Numeric bounds on ", name, " (", variable.pretty.name, ")", sep = "")
      tab.summary <- knitr::kable(bound.df, caption = table.caption) %>%
        kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
    }
  }
  list(
    hist.plot = hist.plot,
    tab.summary = tab.summary
  )
}


#' @title
#' Helper function to report relationship between an age variable
#' and its corresponding date.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param data.vec Numeric vector of column from phenotype data frame
#' for this age variable.
#' @param phenotype.data Data frame of full phenotype data for selection
#' of linked variable contents.
#' @param variable.entry List entry in dataset yaml for this age variable.
#' @param name Character vector harmonized name of variable in yaml.
#' @param my.theme ggplot2 accumulated theme settings.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted ggplot for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.linked.date <- function(data.vec,
                               phenotype.data,
                               variable.entry,
                               name,
                               my.theme,
                               suppress.reporting) {
  if (!is.null(variable.entry$params$linked_date) && !suppress.reporting) {
    reported.year.varname <- variable.entry$params$linked_date$reported_year
    reference.year <- variable.entry$params$linked_date$reference_year
    stopifnot(
      reported.year.varname %in% colnames(phenotype.data),
      is.numeric(reference.year) |
        (is.character(reference.year) & reference.year %in% colnames(phenotype.data))
    )
    reported.year <- phenotype.data[, reported.year.varname]
    if (is.character(reference.year)) {
      reference.year <- phenotype.data[, reference.year]
    }
    reported.age <- data.vec
    derived.age <- reference.year - reported.year
    plot.data <- data.frame(
      x = reported.age,
      y = derived.age
    )
    plot.data <- plot.data[!is.na(plot.data$x) & !is.na(plot.data$y), ]
    age.plot <- ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y"), data = plot.data) + my.theme
    age.plot <- age.plot + ggplot2::geom_point() + ggplot2::geom_abline(slope = 1, intercept = 0)
    age.plot <- age.plot + ggplot2::xlab("Reported Age") + ggplot2::ylab("Age Computed from Date")
    cat("\n\n#### Comparison between reported age ", name, " and age derived from date ",
      reported.year.varname, "\n\n",
      sep = ""
    )
    age.plot
  }
}

#' Helper function to report summary information about reported BMI
#' and computed BMI directly from weight and height data.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param phenotype.data Data frame of full phenotype data for variable
#' selection.
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector harmonized name of variable in yaml.
#' @param my.theme ggplot2 accumulated theme settings.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted ggplot for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.bmi.comparison <- function(phenotype.data,
                                  variable.entry,
                                  name,
                                  my.theme,
                                  suppress.reporting) {
  if (!is.null(variable.entry$params$computed_bmi) && !suppress.reporting) {
    height.varname <- variable.entry$params$computed_bmi$height
    weight.varname <- variable.entry$params$computed_bmi$weight
    stopifnot(
      height.varname %in% colnames(phenotype.data),
      weight.varname %in% colnames(phenotype.data)
    )
    reported.bmi <- phenotype.data[, name]
    height.var <- phenotype.data[, height.varname]
    weight.var <- phenotype.data[, weight.varname]
    computed.bmi <- weight.var / height.var^2
    plot.data <- data.frame(
      x = reported.bmi,
      y = computed.bmi
    )
    plot.data <- plot.data[!is.na(plot.data$x) & !is.na(plot.data$y), ]
    bmi.plot <- ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y"), data = plot.data) + my.theme
    bmi.plot <- bmi.plot + ggplot2::geom_point() + ggplot2::geom_abline(slope = 1, intercept = 0)
    bmi.plot <- bmi.plot + ggplot2::xlab("Reported BMI") + ggplot2::ylab("BMI Computed from Height/Weight")
    cat("\n\n#### Comparison between reported BMI ", name,
      " and BMI derived from height ", height.varname,
      " and weight ", weight.varname, "\n\n",
      sep = ""
    )
    bmi.plot
  }
}


#' @title
#' Helper function to report plot showing relationship between
#' self-reported systolic and diastolic blood pressure
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param phenotype.data Data frame of full phenotype data for variable
#' selection.
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector harmonized name of variable in yaml.
#' @param my.theme ggplot2 accumulated theme settings.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted ggplot for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.bp.ratio <- function(phenotype.data,
                            variable.entry,
                            name,
                            my.theme,
                            suppress.reporting) {
  if (!is.null(variable.entry$params$computed_bp_ratio) && !suppress.reporting) {
    systolic.bp <- phenotype.data[, name]
    diastolic.bp.varname <- variable.entry$params$computed_bp_ratio$diastolic
    stopifnot(diastolic.bp.varname %in% colnames(phenotype.data))
    diastolic.bp <- phenotype.data[, diastolic.bp.varname]
    plot.data <- data.frame(x = systolic.bp, y = diastolic.bp)
    bp.plot <- ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y"), data = plot.data) + my.theme
    bp.plot <- bp.plot + ggplot2::geom_point() + ggplot2::geom_abline(slope = 1, intercept = 0)
    bp.plot <- bp.plot + ggplot2::xlab("Reported systolic BP") + ggplot2::ylab("Reported diastolic BP")
    cat("\n\n#### Comparison between reported systolic ", name,
      " and diastolic blood pressure ", diastolic.bp.varname, "\n\n",
      sep = ""
    )
    bp.plot
  }
}


#' @title
#' Helper function to report information about observed entries
#' in text-like variables.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @param phenotype.data Data frame of full phenotype data for variable
#' selection.
#' @param var.summary Table contingency data about observations
#' of values in variable.
#' @param unique.var.value.inc.prop Numeric representing the proportion threshold
#' above which variables with too many unique values will not have
#' their value contingency data reported.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param variable.pretty.name Character vector of human-legible name of
#' variable with more helpful description.
#' @param suppress.reporting Logical of whether variable report data
#' should be suppressed.
#' @return Formatted kable for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.content.summary <- function(phenotype.data,
                                   var.summary,
                                   unique.var.value.inc.prop,
                                   name,
                                   variable.pretty.name,
                                   suppress.reporting) {
  if ((is.vector(phenotype.data[, name], mode = "numeric") && !suppress.reporting) ||
    ((is.factor(phenotype.data[, name]) ||
      length(var.summary) < nrow(phenotype.data) * unique.var.value.inc.prop) && !suppress.reporting)) {
    variable.summary.df <- data.frame(
      names(var.summary), as.vector(var.summary)
    )
    variable.summary.df[, 1] <- stringr::str_replace(variable.summary.df[, 1], "^( *)([\\+\\*])", "\\1\\\\\\2")
    variable.summary.df[, 1] <- stringr::str_replace(variable.summary.df[, 1], "^( *)([0-9]+)\\. ", "\\1\\2\\\\. ")
    variable.summary.df[, 1] <- stringr::str_replace(variable.summary.df[, 1], "^-$", "--")
    rownames(variable.summary.df) <- NULL
    colnames(variable.summary.df) <- c("Value", "Summary statistics")
    table.caption <- paste("Summary of ", name, " (", variable.pretty.name, ")", sep = "")
    knitr::kable(variable.summary.df, caption = table.caption) %>%
      kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
  } else if (length(var.summary) >= nrow(phenotype.data) * unique.var.value.inc.prop && !suppress.reporting) {
    cat("\n\nVariable ", name, " (", variable.pretty.name, ") has ", length(var.summary), " unique values, and thus",
      " report output of individual value counts is suppressed.\n\n",
      sep = ""
    )
  } else if (suppress.reporting) {
    cat("\n\nReporting for variable", name, "was suppressed in the configuration.\n\n")
  }
}


#' @title
#' Helper function to report information about blood pressure
#' variable entries that do not match expected blood pressure
#' systolic/diastolic reporting format.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @details Many instances of non-compliant blood pressure reporting
#' formats have been observed in various test datasets. This reporting
#' information is intended both to record non-compliant data and to
#' suggest new, "creative" formats that might be supported in patches
#' to this package.
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted kable for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.noncompliant.bp <- function(variable.entry,
                                   name,
                                   suppress.reporting) {
  if (!is.null(variable.entry$invalid.blood.pressure.entries) && !suppress.reporting) {
    if (length(variable.entry$invalid.blood.pressure.entries) > 0) {
      df <- data.frame(table(variable.entry$invalid.blood.pressure.entries, useNA = "ifany"))
      rownames(df) <- NULL
      colnames(df) <- c(name, "Count")
      df[, 1] <- stringr::str_replace(df[, 1], "^( *)([\\+\\*])", "\\1\\\\\\2")
      knitr::kable(df, caption = "Values that were not consistent with blood pressure measurement format.") %>%
        kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
    } else {
      cat("\n\nAll values consistent with blood pressure format or missing data.\n\n")
    }
  }
}

#' @title
#' Helper function to report information about entries in
#' expected-numeric variables that do not match supported
#' numeric format.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @details
#' Substantial processing is applied to string representations
#' of input data before numeric conversion is attempted. This report
#' covers entries that fail conversion after such string cleaning
#' is attempted.
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted kable for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.noncompliant.numerics <- function(variable.entry,
                                         name,
                                         suppress.reporting) {
  if (!is.null(variable.entry$invalid.numeric.entries) && !suppress.reporting) {
    if (length(variable.entry$invalid.numeric.entries) > 0) {
      df <- data.frame(table(variable.entry$invalid.numeric.entries, useNA = "ifany"))
      rownames(df) <- NULL
      colnames(df) <- c(name, "Count")
      df[, 1] <- stringr::str_replace(df[, 1], "^( *)([\\+\\*])", "\\1\\\\\\2")
      knitr::kable(df, caption = "Values that were not consistent with numeric format.") %>%
        kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
    } else {
      cat("\n\nAll values consistent with numeric format or missing data.\n\n")
    }
  }
}


#' @title
#' Helper function to report summary information about uncertain
#' values in expected categorical variables.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @details
#' This function has hybrid functionality. It was originally
#' intended to report entries of factor variables that do not match
#' defined levels/shared model data. Later on, functionality was added
#' to try to _ad hoc_ harmonize self-reported ancestry labels, and at
#' that point this function was expanded to provide a detailed summary
#' of resolution status for such ancestry variables as well.
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted kable for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.factor.summary <- function(variable.entry,
                                  name,
                                  suppress.reporting) {
  ## give ancestry factor variable special treatment
  if (!is.null(variable.entry$params$subject_ancestry) &&
    length(variable.entry$ancestry.reasoning) > 0 &&
    !suppress.reporting) {
    ## instead of reporting failed conversions specifically,
    ## report a table of all imperfect matches to the reference ancestry labels,
    ## and track the outcomes for all of them, even the ones that were eventually
    ## called. this behavior may be updated later once this method has been
    ## improved
    df <- data.frame(
      variable.entry$ancestry.conversion.before,
      variable.entry$ancestry.conversion.after,
      variable.entry$ancestry.reasoning,
      variable.entry$ancestry.best.match,
      round(variable.entry$ancestry.best.value, 2),
      variable.entry$ancestry.second.match,
      round(variable.entry$ancestry.second.value, 2)
    )
    colnames(df) <- c(
      "Input",
      "Output",
      "Reasoning",
      "Best Match",
      "Best Score",
      "Second Match",
      "Second Score"
    )
    df$Count <- rep(0, nrow(df))
    for (query in unique(df[, 1])) {
      df$Count[df[, 1] == query] <- length(which(df[, 1] == query))
    }

    ## clean up table: for large datasets, this gets unwieldy
    ## first remove duplicate queries, as the resolution is always the same
    df <- df[!duplicated(df[, 1]), ]
    ## then sort first by reasoning, then by output, then by query
    df <- df[order(df[, 3], df[, 2], df[, 1]), ]
    rownames(df) <- NULL
    knitr::kable(df, caption = "Handling of all partial match self-reported ancestries.") %>%
      kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
  } else if (!is.null(variable.entry$invalid.factor.entries) && !suppress.reporting) {
    ## other things that are factors
    if (length(variable.entry$invalid.factor.entries) > 0) {
      df <- data.frame(table(variable.entry$invalid.factor.entries, useNA = "ifany"))
      rownames(df) <- NULL
      colnames(df) <- c(name, "Count")
      df[, 1] <- stringr::str_replace(df[, 1], "^( *)([\\+\\*])", "\\1\\\\\\2")
      knitr::kable(df, caption = "Values that were not consistent with categorical format.") %>%
        kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
    } else {
      cat("\n\nAll values consistent with categorical format or missing data.\n\n")
    }
  }
}

#' @title
#' Helper function to report summary information about values
#' in date variables that do not match expected date formats.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @details
#' Free-text date variables typically contain a wide variety
#' of input formats. The package heuristically supports a variety of
#' date formats, but still many observed values are ultimately not
#' reasonably convertable to year specifications. This function reports
#' such values, both for reporting purposes and to suggest possible
#' extensions to the supported date format regex (or not).
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted kable for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.noncompliant.dates <- function(variable.entry,
                                      name,
                                      suppress.reporting) {
  if (!is.null(variable.entry$invalid.date.entries) && !suppress.reporting) {
    if (length(variable.entry$invalid.date.entries) > 0) {
      df <- data.frame(table(variable.entry$invalid.date.entries, useNA = "ifany"))
      rownames(df) <- NULL
      colnames(df) <- c(name, "Count")
      df[, 1] <- stringr::str_replace(df[, 1], "^( *)([\\+\\*])", "\\1\\\\\\2")
      knitr::kable(df, caption = "Values that were not consistent with date (year only) format.") %>%
        kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
    } else {
      cat("\n\nAll values consistent with date format (year only) or missing data.\n\n")
    }
  }
}

#' @title
#' Helper function to report summary information about Unicode
#' characters that are not removed by upstream cleaning.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @details
#' Unicode characters often sneak by the _ad hoc_ conversion
#' logic used in this package. The linking between Unicode character
#' and desired ASCII representation has been exposed to config space.
#' This reporting function includes tracking information about the
#' observed Unicode character, in the hopes that the user can expand
#' the mapping table to convert such characters into compliant values.
#'
#' @param variable.entry List entry in dataset yaml for this variable.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return Formatted kable for rendering, or NULL if reporting suppressed.
#' @usage NULL
report.unicode.entries <- function(variable.entry,
                                   suppress.reporting) {
  if (!is.null(variable.entry$unicode.entries) && !suppress.reporting) {
    unicode.df <- data.frame(
      names(variable.entry$unicode.entries),
      as.vector(variable.entry$unicode.entries)
    )
    rownames(unicode.df) <- NULL
    colnames(unicode.df) <- c("Unicode String", "Observations")
    knitr::kable(unicode.df, caption = "String entries containing unresolved Unicode characters.") %>%
      kableExtra::kable_styling("condensed", position = "left", full_width = FALSE)
  }
}

#' @title
#' Helper function to report summary information about encoded
#' dependency relationships between variables.
#'
#' @description
#' This function exclusively functions to emit formatted information
#' in the phenotype Rmarkdown report. It has been
#' parametrized out in order to:
#'
#'  - clean up the report proper;
#'  - sanitize some of the required hybrid syntax from working with
#'    R content in markdown; and
#'  - expose the results of this function to testthat
#'
#' You should never need to call this function for any reason.
#'
#' @details
#' This function handles all reporting of variable dependencies
#' from the input config yaml dependency blocks. Currently, the report
#' does not contain direct information about the downstream effects of
#' `exclude_on_error` or `exclude_all_on_error` directives. This may
#' be added in future iterations of the report.
#'
#' @param phenotype.data Data frame with full phenotype data for selection
#' of linked variable contents.
#' @param variable.summary List input dataset config yaml.
#' @param name Character vector of harmonized name of variable in yaml.
#' @param suppress.reporting Logical controlling whether variable report data
#' should be suppressed.
#' @return List, with entries 'contingency' and 'cross', for kables describing
#' contingency table of results and cross-variable comparisons; or NULL,
#' if reporting suppressed or the relevant features not configured.
#' @usage NULL
report.dependencies <- function(phenotype.data,
                                variable.summary,
                                name,
                                suppress.reporting) {
  result.tables <- list()
  if (!is.null(variable.summary$variables[[name]]$params$dependencies) && !suppress.reporting) {
    cat("\n\n#### Dependency tracking\n\n")
    dependency.names <- c()
    dependency.conditions <- c()
    dependency.results <- c()
    dependencies <- variable.summary$variables[[name]]$params$dependencies
    for (dependency.index in names(dependencies)) {
      dependency.names <- c(dependency.names, dependencies[[dependency.index]]$name)
      dependency.conditions <- c(dependency.conditions, dependencies[[dependency.index]]$condition)
      dependency.result <- variable.summary$variables[[name]]$dependency.results[[dependency.index]]
      if (length(dependency.result) == 0) {
        dependency.result <- "all subjects pass"
      } else if (length(dependency.result) > 1) {
        dependency.result <- paste(sort(dependency.result), collapse = ", ")
      }
      dependency.results <- c(dependency.results, dependency.result)


      ## allow table_comparisons for simple contingency tables between this and other variables
      if (!is.null(dependencies[[dependency.index]]$table_comparisons)) {
        for (table.target in dependencies[[dependency.index]]$table_comparisons) {
          stopifnot(table.target %in% colnames(phenotype.data))
          comp.var.pretty.name <- variable.summary$variables[[table.target]]$original.name
          if (!is.null(variable.summary$variables[[table.target]]$params$canonical_name)) {
            comp.var.pretty.name <- paste(comp.var.pretty.name,
              " (",
              variable.summary$variables[[table.target]]$params$canonical_name,
              ")",
              sep = ""
            )
          }
          table.data <- table(
            phenotype.data[, table.target],
            phenotype.data[, name],
            useNA = "ifany"
          )
          result.tables$contingency <- knitr::kable(table.data,
            caption = paste("Contingency table for ",
              table.target,
              " (",
              comp.var.pretty.name,
              ") [rows] versus ",
              name,
              " [columns]",
              sep = ""
            )
          ) %>% kableExtra::kable_styling("condensed")
        }
      }
    }
    dependency.df <- data.frame(
      Name = dependency.names,
      Condition = dependency.conditions,
      Count = sapply(strsplit(dependency.results, ","), function(i) {
        ifelse(identical(i, "all subjects pass"), 0, length(i))
      }),
      Result = dependency.results
    )
    rownames(dependency.df) <- NULL
    result.tables$cross <- knitr::kable(dependency.df,
      caption = "Results of cross-variable dependency tests."
    ) %>% kableExtra::kable_styling("condensed")
  }
  result.tables
}
