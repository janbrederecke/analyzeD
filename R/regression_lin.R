#' @title regression_lin
#'
#' @description Calculates linear regression models using lm(). Designed to make
#' analysis with multiple predictors / outcomes easy.
#'
#' @param .data A data.frame
#' @param .outcomes A vector containing the outcomes
#' @param .predictors A vector containing the predictors
#' @param .covariates A vector containing covariates for each regression
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
                           , .annotation = annotation
                           , .subset = NULL
                           , .cpus = 1
                           , .sort_by = "outcomes"
                           , .std_prd = FALSE
                           , .summary = FALSE
                           , .interaction = NULL
                           , ...
  
){
  
  # Subset data if .subset != NULL
  if (!is.null(.subset)) {
    .data <- subset(.data, eval(parse(text = .subset)))
  }
  
  
  if (!class(.data)[1] %in% c("tbl_df", "data.frame", "mids")) {
    
    stop("Your data must be either of class data.frame, tbl_df, or mids")
  
  } else if (class(.data) == "data.frame" && .sort_by == "outcomes") {
  
    return(regression_lin_by_outcomes(.data = .data
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
    
  }  else if (class(.data) == "data.frame" && .sort_by == "predictors") {
    
    return(regression_lin_by_predictors(.data = .data
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
