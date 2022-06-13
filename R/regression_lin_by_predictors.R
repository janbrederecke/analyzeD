#' @title regression_lin_by_predictors
#'
#' @description This function calls the actual regression_lin_outcomes
#' function that shuffles through the outcomes.
#'
#' @param .data A data.frame.
#' @param .outcomes A vector containing the outcomes.
#' @param .predictors A vector containing the predictors.
#' @param .covariates A vector containing covariates for each regression.
#' @param .annotation A matrix or data.frame in the annotation format (name,
#' pname, unit, short_pname, comment) that contains pretty names for the used
#' variables and their dummy variables.
#' @param .cpus Input number of desired cpus to use. Useful only in case of big
#' datasets and multiple analysis.
#' @param .std_prd If TRUE, predictors are standardized using std(predictor).
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to input interactions.
#' @param ... Optional input passed to the regression function.
#'
#' @importFrom foreach "%dopar%"
#'
regression_lin_by_predictors <- function(.data
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
    
    fit_list <- lapply(.predictors, function(predictor) {
      regression_lin_outcomes(.data = .data
                              , .outcomes = .outcomes
                              , .predictor = predictor
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
    
    n <- length(.predictors)
    
    # Calculate regressions for each outcome on a single CPU
    fit_list <- foreach::foreach(
      i = 1:n,
      .packages = c("broom", "dplyr", "stringr"),
      .export = c("regression_lin_outcomes",
                  "regression_lin_outcomes_summary")
      
    ) %dopar% {
      
      predictor <- .predictors[i]
      
      ## Fit regression models for specified outcome
      fit_list_outcomes <- regression_lin_outcomes(.data = .data
                                                   , .outcomes = .outcomes
                                                   , .predictor = predictor
                                                   , .covariates = .covariates
                                                   , .annotation = .annotation
                                                   , .std_prd = .std_prd
                                                   , .summary = .summary
                                                   , .interaction = .interaction
                                                   , ...
                                                   )
      ## Return the list of results per predictor
      return(fit_list_outcomes)
    }
    
    # Stop the cluster
    parallel::stopCluster(cl = my_cluster)
    
    # Return the list of lists
    fit_list
  }
  
  # Name the list of lists using the respective outcomes
  if (.std_prd == TRUE) {
    names(fit_list) <- paste0("std(", .predictors, ")")
  } else {
    names(fit_list) <-  .predictors
  }
  
  # Return results                          
  fit_list
}