#' @title regression_lin
#'
#' @description Calculates linear regression models using lm(). Designed to make
#' analysis with multiple predictors / outcomes easy.
#'
#' @param .data A data.frame.
#' @param .outcomes A vector containing the outcomes.
#' @param .predictors A vector containing the predictors.
#' @param .covariates A vector containing covariates for each regression.
#' @param .annotation A matrix or data.frame in the annotation format (name,
#' pname, unit, short_pname, comment) that contains pretty names for the used
#' variables and their dummy variables.
#' @param .subset Can be used to internally subset the data. Use .subset =
#' "variable == 'x'" to subset data.
#' @param .cpus Input number of desired cpus to use. Useful only in case of big
#' datasets and multiple analysis.
#' @param .sort_by A character string that indicates either to sort the analyses
#' by "outcomes" or by "predictors".
#' @param .std_prd If TRUE, predictors are standardized using std(predictor).
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to input interactions.
#' @param ... Optional input passed to the regression function.
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
                           , .summary = FALSE
                           , .interaction = NULL
                           , ...

){

  # Check if .data is in one of the supported formats
  if (!is.data.frame(.data) && !mice::is.mids(.data)) {

    stop("Your data must be either a data.frame or mids object.")

  }

  # Check if .outcomes has been specified and is in the right format
  if (is.null(.outcomes)) {

    stop("You have to specify outcomes!")

  } else if (!is.null(.outcomes) && class(.outcomes) != "character") {

    stop("Outcomes have to be provided in a vector of type 'character'.")
  }

  # Check if .predictors has been specified and is in the right format
  if (is.null(.predictors)) {

    stop("You have to specify predictors!")

  } else if (!is.null(.predictors) && class(.predictors) != "character") {

    stop("Predictors have to be provided in a vector of type 'character'.")
  }

  # Check if .covariates has been specified and is in the right format
  if (!is.null(.covariates) && class(.covariates) != "character") {

    stop("Covariates have to be provided in a vector of type 'character'.")
  }

  # Check if .annotation has been specified and is in the right format
  if (!is.null(.annotation) && !class(.annotation) %in% c("matrix",
                                                          "data.frame")) {

    stop("Annotation has to be provided as a matrix or data.frame")

  } else if (!is.null(.annotation) && !identical(names(.annotation),
                                                c("name",
                                                  "pname",
                                                  "unit",
                                                  "short_pname",
                                                  "comment"))) {

    stop("Names of the annotation have to be name, pname, unit, short_pname, and
         comment")
  }

  # Subset data if .subset != NULL
  if (!is.null(.subset)) {
    .data <- subset(.data, eval(parse(text = .subset)))
  }

  if (.sort_by == "outcomes") {

    return(reg_lin_sort_by_outcomes(.data = .data
                                      , .outcomes = .outcomes
                                      , .predictors = .predictors
                                      , .covariates = .covariates
                                      , .annotation = .annotation
                                      , .cpus = .cpus
                                      , .std_prd = .std_prd
                                      , .summary = .summary
                                      , .interaction = .interaction
                                      , ...
                                      )
    )

  }  else if (.sort_by == "predictors") {

    return(reg_lin_sort_by_predictors(.data = .data
                                        , .outcomes = .outcomes
                                        , .predictors = .predictors
                                        , .covariates = .covariates
                                        , .annotation = .annotation
                                        , .cpus = .cpus
                                        , .std_prd = .std_prd
                                        , .summary = .summary
                                        , .interaction = .interaction
                                        , ...
                                        )
    )
  }
}

# Exposing 'i' to global environment
globalVariables(c("i"))