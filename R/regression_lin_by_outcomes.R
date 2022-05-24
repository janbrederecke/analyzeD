#' @title regression_lin_by_outcomes
#'
#' @description This function calls the actual regression_lin_predictors
#' function that shuffles through the predictors
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
#' @importFrom foreach "foreach" "%dopar%"
#' @importFrom parallel "makeCluster" "stopCluster"
#' @importFrom doParallel "registerDoParallel"
#' 
regression_lin_by_outcomes <- function(.data
                                       , .outcomes
                                       , .predictors 
                                       , .covariates
                                       , .annotation
                                       , .cpus
                                       , .std_prd
                                       , .summary
                                       , .interaction
                                       , ...
){
  
  # Processing on single CPU  
  if (.cpus == 1) {
  
    fit_list <- lapply(.outcomes, function(outcome) {
      regression_lin_predictors(.data = .data
                                , .outcome = outcome
                                , .predictors = .predictors
                                , .covariates = .covariates
                                , .annotation = .annotation
                                , .std_prd = .std_prd
                                , .summary = .summary
                                , .interaction = .interaction
                                , ...
                                )
    })
    fit_list
  
  # Processing on multiple CPUS  
  } else if (.cpus >= 1) {
    
    print(paste0("Parallel processing. Using ", .cpus, " cores."))
    
    # Register cluster
    my_cluster <- parallel::makeCluster(.cpus)
    doParallel::registerDoParallel(cl = my_cluster)
    
    n <- length(.outcomes)
    
    # Calculate regressions for each outcome on a single CPU
    fit_list <- foreach::foreach(
      i = 1:n,
      .packages = c("broom", "dplyr", "stringr"),
      .export = c("regression_lin_predictors")
      
    ) %dopar% {
      
      outcome <- .outcomes[i]
      
      ## Fit regression models for specified outcome
      fit_list_predictors <- regression_lin_predictors(.data = .data
                                                       , .outcome = outcome
                                                       , .predictors = .predictors
                                                       , .covariates = .covariates
                                                       , .annotation = .annotation
                                                       , .std_prd = .std_prd
                                                       , .summary = .summary
                                                       , .interaction = .interaction
                                                       , ...
                                                       )
      return(fit_list_predictors)
    }
    parallel::stopCluster(cl = my_cluster)
    fit_list
  }
  
  names(fit_list) <- .outcomes
  fit_list
}
