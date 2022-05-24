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
