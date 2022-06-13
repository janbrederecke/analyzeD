#' @title reg_lin_sort_by_outcomes
#'
#' @description This function calls the actual reg_lin_predictors
#' function that shuffles through the predictors.
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
reg_lin_sort_by_outcomes <- function(.data
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
  
  # Process on single CPU  
  if (.cpus == 1) {
  
    fit_list <- lapply(.outcomes, function(outcome) {
      reg_lin_predictors(.data = .data
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
    
    # Return results
    fit_list
  
  # Process in parallel on multiple CPUS  
  } else if (.cpus >= 1) {
    
    print(paste0("Parallel processing. Using ", .cpus, " cores."))
    
    # Register cluster
    my_cluster <- parallel::makeCluster(.cpus)
    
    ## Export summary function because the foreach export did not work properly 
    parallel::clusterExport(cl = my_cluster,
                            varlist = c("reg_lin_predictors_summary"))

    ## Actual registering of cluster                        
    doParallel::registerDoParallel(cl = my_cluster)
    
    n <- length(.outcomes)
    
    # Calculate regressions for each outcome on a single CPU
    fit_list <- foreach::foreach(
      i = 1:n,
      .packages = c("broom", "dplyr", "stringr", "tidyselect"),
      .export = c("reg_lin_predictors")
      
    ) %dopar% {
      
      ## Get one outcome to distribute to a core
      outcome <- .outcomes[i]
      
      ## Fit regression models for specified outcome
      fit_list_predictors <- reg_lin_predictors(
        .data = .data
        , .outcome = outcome
        , .predictors = .predictors
        , .covariates = .covariates
        , .annotation = .annotation
        , .std_prd = .std_prd
        , .summary = .summary
        , .interaction = .interaction
        , ...
                                                       )
      
      ## Return the list of results per outcome
      return(fit_list_predictors)
    }
    
    # Stop the cluster
    parallel::stopCluster(cl = my_cluster)
    
    # Return the list of lists
    fit_list
  }
  
  # Name the list of lists using the respective outcomes
  names(fit_list) <- .outcomes
  
  # Return results
  fit_list
}
