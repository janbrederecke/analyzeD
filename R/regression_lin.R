#' @title regression_lin
#'
#' @description Calculates linear regression models using lm(). Designed to make
#' analysis with multiple predictors / outcomes easy and quick.
#'
#' @param .data A data.frame or .mids object.
#' @param .outcomes A character vector containing the outcomes.
#' @param .predictors A character vector containing the predictors.
#' @param .covariates A character vector containing covariates.
#' @param .annotation A matrix or data.frame of format (name, pname, unit,
#' short_pname, comment) that contains pretty names for the used variables.
#' @param .subset Can be used to internally subset the data. Use .subset =
#' "variable == 'x'" to subset data.
#' @param .cpus Input number of desired cpus to use. Useful only in case of big
#' datasets and multiple outcomes/predictors.
#' @param .sort_by A character string that indicates either to sort the analyses
#' by "outcomes" or by "predictors".
#' @param .std_prd If TRUE, predictors are standardized.
#' @param .std_cov Character vector of covariates that should be standardized.
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to specify interactions using a list of
#' character vectors containing the interaction variables, e.g.
#' list(c("variable1", "variable2"), c("variable2", "variable3")).
#' @param .imputed_predictors If TRUE, cases with imputed predictors are used.
#' @param .imputed_outcomes If TRUE, cases with imputed outcomes are used.
#' @param ... Optional input passed directly to the regression function.
#'
#'
#' @return A list containing individual results tables for each regression
#' analysis.
#' @examples -
#' @export
#'
regression_lin <- function(.data
                           , .outcomes = NULL
                           , .predictors = NULL
                           , .covariates = NULL
                           , .annotation = NULL
                           , .subset = NULL
                           , .cpus = 1
                           , .sort_by = "outcomes"
                           , .std_prd = FALSE
                           , .std_cov = NULL
                           , .summary = FALSE
                           , .interaction = NULL
                           , .imputed_predictors = FALSE
                           , .imputed_outcomes = FALSE
                           , ...

){

  # Check if .data is in one of the supported formats
  ## Check if .data has been specified at all
  if (is.null(.data)) {
    stop("No data provided.")

  ## Check if .data is a data.frame or mids object
  } else if (!is.data.frame(.data) && !mice::is.mids(.data)) {
    stop("Your data must be either a data.frame or mids object.")
  }

  # Check if .outcomes has been specified and is in the right format
  ## Check if outcomes have been specified at all
  if (is.null(.outcomes)) {
    stop("You have to specify outcomes!")

  ## Check if the outcomes vector is in the right format
  } else if (!is.null(.outcomes) && class(.outcomes) != "character") {
    stop("Outcomes have to be provided in a vector of type 'character'.")
  }

  # Check if the data of the outcomes is in the right format
  ## Check if outcomes are numeric and binary
  ## For data.frame
  num_unique <- function(x) length(unique(x))
  if (is.data.frame(.data)) {
    outcomes_numeric <- apply(.data[.outcomes], 2, is.numeric)
    temp_data <- as.data.frame(.data[stats::complete.cases(.data[.outcomes]), .outcomes])
    outcomes_unique_values <- apply(temp_data, 2, num_unique)

  ## For mids object
  } else if (mice::is.mids(.data)) {
    temp_data <- mice::complete(.data, 0)[.outcomes]
    outcomes_numeric <- apply(temp_data, 2, is.numeric)
    temp_data <- as.data.frame(temp_data[stats::complete.cases(temp_data[.outcomes]), .outcomes])
    outcomes_unique_values <- apply(temp_data, 2, num_unique)
  }

  ## Stop if not all outcomes are numeric
  if (!all(outcomes_numeric)) {
    stop("Outcomes have to be numeric.")
  }

  ## Warn if outcomes appear binary
  if (any(outcomes_unique_values <= 2)) {
    print("You seem to have binary outcomes in your data.")
  }

  # Check if .predictors has been specified and is in the right format
  ## Check if predictors have been specified at all
  if (is.null(.predictors)) {
    stop("You have to specify predictors!")

  ## Check if the predictors vector is in the right format
  } else if (!is.null(.predictors) && class(.predictors) != "character") {
    stop("Predictors have to be provided in a vector of type 'character'.")
  }

  # Check if .covariates has been specified and is in the right format
  if (!is.null(.covariates) && class(.covariates) != "character") {
    stop("Covariates have to be provided in a vector of type 'character'.")
  }
  
  # Check if .covariates has not been specified but "base_model" in .predictors
  if (is.null(.covariates) && "base_model" %in% .predictors) {
    stop("Base model can only be calculated with covariates.")
  }

  # Check if .annotation has been specified and is in the right format
  ## Check if annotation has been specified as a matrix and turn to data.frame
  if (!is.null(.annotation) && is.matrix(.annotation)) {
    .annotation <- as.data.frame(.annotation)

  ## Check if annotation has been specified as a data.frame and stop if not
  } else if (!is.null(.annotation) && !is.data.frame(.annotation)) {
    stop("Annotation has to be provided as a matrix or data.frame")

  ## Check if a specified annotation has the right column names and stop if not
  } else if (!is.null(.annotation) &&
             !all(c("name", "pname") %in% names(.annotation))) {
    stop("The annotation has to include the columns 'name' and 'pname'.")
  }
  rownames(.annotation) <- .annotation$name

  # Subset data if .subset != NULL
  ## For data.frame
  if (!is.null(.subset) && is.data.frame(.data)) {
    .data <- subset(.data, eval(parse(text = .subset)))

  ## For mids object
  } else if (!is.null(.subset) && mice::is.mids(.data)) {
    .data <- mice::filter(.data, eval(parse(text = .subset)))
  }

  # Check .cpus argument
  if (is.null(.cpus) || !.cpus %in% 1:parallel::detectCores()) {
    stop(paste0("The number of cpus has to be an integer between 1 and ",
                  parallel::detectCores(),
                  ".")
               )
  }

  # Call the sorting functions depending on the .sort_by input
  ## For outcomes
  if (.sort_by == "outcomes") {
    return(reg_lin_sort_by_outcomes(.data = .data
                                    , .outcomes = .outcomes
                                    , .predictors = .predictors
                                    , .covariates = .covariates
                                    , .annotation = .annotation
                                    , .cpus = .cpus
                                    , .std_prd = .std_prd
                                    , .std_cov = .std_cov
                                    , .summary = .summary
                                    , .interaction = .interaction
                                    , .imputed_predictors = .imputed_predictors
                                    , .imputed_outcomes = .imputed_outcomes
                                    , ...
                                   )
    )

  ## For predictors
  }  else if (.sort_by == "predictors") {
    return(reg_lin_sort_by_predictors(.data = .data
                                      , .outcomes = .outcomes
                                      , .predictors = .predictors
                                      , .covariates = .covariates
                                      , .annotation = .annotation
                                      , .cpus = .cpus
                                      , .std_prd = .std_prd
                                      , .std_cov = .std_cov
                                      , .summary = .summary
                                      , .interaction = .interaction
                                      , .imputed_predictors = .imputed_predictors
                                      , .imputed_outcomes = .imputed_outcomes
                                      , ...
                                      )
    )
  }
}
