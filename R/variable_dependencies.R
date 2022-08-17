#' @title
#' Apply config-specified R commands as variable sanity checks
#'
#' @description
#' Certain variables in an input dataset should have defined relationships
#' between one another. This function will evaluate, in a hopefully safe manner,
#' R syntax expressions from the config yaml files and report their results
#' to the per-variable summary data, for reporting in the output html.
#'
#' @details
#' Dependencies are a very convenient way to put guardrails on your dataset,
#' such that as the dataset expands or provenance is called into question,
#' you can have some degree of surety (or not) that the dataset has maintained
#' some minimum amount of integrity.
#'
#' Questionnaire management systems like SurveyCTO encode certain types
#' of dependencies internally, leading to situations where variables have enforced
#' relationships before they ever reach this package. However, the SurveyCTO
#' form configuration still needs to actually have the dependencies encoded in it
#' for that to be so, and that depends on how thorough the questionnaire designer
#' was being. Furthermore, we have observed instances in which the deployed
#' form for a questionnaire changed mid-deployment, leading to SurveyCTO collapsing
#' responses to fundamentally different questions into the same variable.
#' All this is to say that: even when upstream seems to be handling dependencies,
#' it's good to keep an eye on things in the actual dataset you're working with,
#' just in case.
#'
#' The package's behavior when a subject fails a dependency relationship
#' is also user configurable, and generally involves setting either a specific
#' list of variable responses for the subject, or potentially _all_ variable
#' responses for the subject if the dependency is fundamental enough, to NA.
#'
#' @param phenotype.data Data frame containing input phenotype data.
#' @param variable.summary List containing input dataset configuration.
#' @return List containing modified version of input dataset configuration,
#' with some reporting information about dependency check success/failure that
#' is intended for use with the cleaning report Rmd.
#' @seealso dependency.failure.handling
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 1:4,
#'   HW00003 = 4:1
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "count1",
#'     type = "numeric"
#'   )),
#'   HW00003 = list(params = list(
#'     name = "count2",
#'     type = "numeric",
#'     dependencies = list("1" = list(
#'       name = "dep1",
#'       condition = "HW00003 < HW00002"
#'     ))
#'   ))
#' ))
#' result <- process.phenotypes:::check.variable.dependencies(phenotype.data, variable.summary)
check.variable.dependencies <- function(phenotype.data, variable.summary) {
  ## first step: find the subject ID variable. this will enable prettier error
  ## reporting for some styles of dependencies
  subject.id.column.index <- find.subject.id.index(variable.summary)
  for (i in seq_len(ncol(phenotype.data))) {
    dependencies <- variable.summary$variables[[i]]$params$dependencies
    if (!is.null(dependencies)) {
      variable.summary$variables[[i]]$dependency.results <- list()
      for (j in names(dependencies)) {
        dependency.name <- dependencies[[j]]$name
        dependency.condition <- dependencies[[j]]$condition
        stopifnot(
          !is.null(dependency.name),
          !is.null(dependency.condition)
        )
        ## in theory, this creates an unevaluated expr structure from the
        ## input string and then evaluates it in a quarantined environment.
        ## the point of that is that this should prevent this unknown code
        ## from overwriting anything in the current environment.
        dependency.result <- rlang::eval_tidy(rlang::parse_expr(dependency.condition),
          data = phenotype.data
        )
        ## postconditions: result must be logical
        stopifnot(is.logical(dependency.result))
        ## if it's a single logical, it's just an overall test
        if (length(dependency.result) == 1) {
          variable.summary$variables[[i]]$dependency.results[[j]] <- dependency.result
          next
        } else if (length(dependency.result) == nrow(phenotype.data)) {
          ## if it's a logical as long as the data frame has rows,
          ## it's a per-subject test that should be true for everyone
          variable.summary$variables[[i]]$dependency.results[[j]] <-
            as.character(phenotype.data[!dependency.result, subject.id.column.index])
          next
        }
        ## it's something bizarre that this very tiny brain tester
        ## cannot understand
        stop("for variable ", names(variable.summary$variables)[i],
          ", dependency \"", dependency.name, "\", evaluated result",
          " is not in a recognized format (should either be a logical",
          " of length 1 or length nrow(phenotype.data))",
          sep = ""
        )
      }
    }
  }
  variable.summary
}

#' @title
#' Take action to ameliorate subject observations failing encoded dependency relationships
#'
#' @description
#' Certain variables in an input dataset should have defined relationships
#' between one another. `check.variable.dependencies` handles the process of
#' evaluating user-specified dependency relationships. Once that evaluation
#' is complete, and the results are recorded in the variable summary data,
#' this function responds to user-configured requests for action on failure,
#' which involves setting either some or all variables for the offending subject to NA.
#'
#' @details
#' Ascribing severity to a dependency failure is very much situational. To some extent,
#' it depends on the nature of the data as well. Perhaps one measure is supposed to be
#' greater than another, but is that generally supposed to be so, or always? Dependency
#' relationships are a reasonable way to evaluate either case, but the action on failure
#' might vary. In general, one should hesitate to blank out data unless a preponderance
#' of evidence suggests the data are actually toxic: they don't represent natural or technical
#' variation, but in fact indicate that the respondent wasn't answering the apparent
#' question, or that dataset integrity has been violated in some way.
#'
#' The most severe dependencies are generally when duplicate variables crop up in various
#' places. This happens with surprising frequency. This is particularly noteworthy
#' if the dataset in question is the byproduct of any sort of file join operation
#' (e.g. join operations, Excel vlookup, etc.). Dependency violations around such
#' joins may be your only way of retroactively determining that the upstream
#' (and often manual) join process failed, and should be treated with appropriate seriousness.
#'
#' @param phenotype.data Data frame containing input phenotype data.
#' @param variable.summary List containing input dataset configuration.
#' @return Data frame containing modified version of input phenotype data
#' with requested subject/variable entries replaced with NA on
#' dependency failure.
#' @seealso check.variable.dependencies
#' @examples
#' phenotype.data <- data.frame(
#'   HW00001 = c("A", "B", "C", "D"),
#'   HW00002 = 1:4,
#'   HW00003 = 4:1
#' )
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "count1",
#'     type = "numeric"
#'   )),
#'   HW00003 = list(params = list(
#'     name = "count2",
#'     type = "numeric",
#'     dependencies = list("1" = list(
#'       name = "dep1",
#'       condition = "HW00003 < HW00002",
#'       exclude_on_failure = c("HW00002")
#'     ))
#'   ))
#' ))
#' ## first, evaluate the dependencies and record any violations
#' result.config <- process.phenotypes:::check.variable.dependencies(phenotype.data, variable.summary)
#' ## now, based on those results, take appropriate action
#' result.data <- process.phenotypes:::dependency.failure.handling(phenotype.data, result.config)
dependency.failure.handling <- function(phenotype.data, variable.summary) {
  ## need to map reported failed subject ID back to phenotype.data row index
  subject.id.column.index <- find.subject.id.index(variable.summary)
  ## tracking variables, to determine number of dependency failures out of possible max
  all.targeted.variables <- c()
  na.applied.mask <- matrix(FALSE, nrow = nrow(phenotype.data), ncol = ncol(phenotype.data))
  colnames(na.applied.mask) <- colnames(phenotype.data)
  for (i in seq_len(ncol(phenotype.data))) {
    dependencies <- variable.summary$variables[[i]]$params$dependencies
    results <- variable.summary$variables[[i]]$dependency.results
    stopifnot(length(dependencies) == length(results))
    for (j in seq_len(length(dependencies))) {
      exclude.on.failure <- unlist(dependencies[[j]]$exclude_on_failure)
      exclude.all.on.failure <- dependencies[[j]]$exclude_all_on_failure
      if (!is.null(exclude.all.on.failure)) {
        if (exclude.all.on.failure) {
          exclude.on.failure <- colnames(phenotype.data)[-subject.id.column.index]
        }
      }
      all.targeted.variables <- c(
        all.targeted.variables,
        exclude.on.failure
      )
      exclude.subjects <- results[[j]]
      if (is.null(exclude.on.failure) ||
        is.null(exclude.subjects)) {
        next
      }
      exclude.indices <- phenotype.data[, subject.id.column.index] %in% exclude.subjects
      stopifnot(identical(
        exclude.on.failure %in% colnames(phenotype.data),
        rep(TRUE, length(exclude.on.failure))
      ))
      was.na.before <- is.na(phenotype.data[, exclude.on.failure])
      phenotype.data[exclude.indices, exclude.on.failure] <- NA
      is.na.now <- is.na(phenotype.data[, exclude.on.failure])
      na.applied.mask[, exclude.on.failure] <- na.applied.mask[, exclude.on.failure] | (is.na.now & !was.na.before)
    }
  }
  variable.summary$actual.nas.from.deps <- sum(na.applied.mask[, unique(all.targeted.variables)])
  variable.summary$possible.nas.from.deps <- nrow(na.applied.mask) * length(unique(all.targeted.variables))
  list(
    phenotype.data = phenotype.data,
    variable.summary = variable.summary
  )
}

#' @title
#' Locate a variable in the dataset flagged as subject ID
#'
#' @description
#' The input yaml specification allows a single variable
#' to be flagged as "subject_id: true" for context-specific
#' behaviors. This function scans the configuration data and
#' returns the location of that variable.
#'
#' @details
#' If this column is not located, stop() is called. The handling
#' of this tag, and the `subject_age` tag, could be improved.
#'
#' @param variable.summary List of input configuration data.
#' @return Integer index of subject ID column in input data.
#' @examples
#' variable.summary <- list(variables = list(
#'   HW00001 = list(params = list(
#'     name = "subjid",
#'     type = "string",
#'     subject_id = TRUE
#'   )),
#'   HW00002 = list(params = list(
#'     name = "subjage",
#'     type = "string"
#'   ))
#' ))
#' subj.id.index <- process.phenotypes:::find.subject.id.index(variable.summary)
find.subject.id.index <- function(variable.summary) {
  for (i in seq_len(length(variable.summary$variables))) {
    if (!is.null(variable.summary$variables[[i]]$params$subject_id)) {
      if (variable.summary$variables[[i]]$params$subject_id) {
        return(i)
      }
    }
  }
  stop("no variable in input config yaml was flagged as the subject ID ",
    "variable; this is required exactly once per file and can be added ",
    "with the line \"subject_id: true\" under the corresponding variable",
    sep = ""
  )
}
