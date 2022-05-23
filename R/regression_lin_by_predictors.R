regression_lin_by_predictors <- function(.data
                                         , .outcomes
                                         , .predictors 
                                         , .covariates
                                         , .annotation
                                         , .cpus
                                         , .std_prd
                                         , .summary
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
                              )
    })
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
    
    n <- length(.predictors)
    
    # Calculate regressions for each outcome on a single CPU
    fit_list <- foreach(
      i = 1:n,
      .packages = c("broom", "dplyr", "stringr"),
      .export = c("regression_lin_outcomes")
      
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
                                                   )
      
      return(fit_list_outcomes)
    }
    stopCluster(cl = my_cluster)
    fit_list
  }
  if (.std_prd == TRUE) {
    names(fit_list) <- paste0("std(", .predictors, ")")
  } else {
    names(fit_list) <-  .predictors
  }
                            
  fit_list
}