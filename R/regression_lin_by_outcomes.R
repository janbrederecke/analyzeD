regression_lin_by_outcomes <- function(.data
                                       , .outcomes
                                       , .predictors 
                                       , .covariates
                                       , .annotation
                                       , .cpus
                                       , ...
){
  
  # Processing on single CPU  
  if (.cpus == 1) {
  
    fit_list <- lapply(.outcomes, function(outcome) {
      regression_lin_predictors(.data = .data
                                , .outcome = outcome
                                , .predictors = .predictors
                                , .covariates = .covariates
                                , .annotation = .annotation)
    })
    
    names(fit_list) <- .annotation[.outcomes, "pname"]
    fit_list
  
  # Processing on multiple CPUS  
  } else if (.cpus >= 1) {
    
    print(paste0("Parallel processing. Using ", .cpus, " cores."))
    # Register cluster
    library(foreach)
    library(doRNG)
    library(parallel)
    library(doParallel)
    
    my_cluster <- makeCluster(.cpus)
    registerDoParallel(cl = my_cluster)
    
    n <- length(.outcomes)
    
    # Calculate regressions for each outcome on a single CPU
    fit_list <- foreach(
      i = 1:n,
      .packages = c("broom", "dplyr", "stringr"),
      .export = c("regression_lin_predictors")
      
    ) %dopar% {
      
      outcome <- .outcomes[i]
      
      ## Fit regression models for specified outcome
      fit_list_predictors <- regression_lin_predictors(.data
                                                       , .outcome = outcome
                                                       , .predictors
                                                       , .covariates
                                                       , .annotation
                                )
      
      names(fit_list_predictors) <- i
      return(fit_list_predictors)
    }
    stopCluster(cl = my_cluster)
    fit_list
  }
}
