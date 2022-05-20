regression_lin_outcomes <- function(.data
                                    , .outcomes
                                    , .predictors
                                    , .covariates
                                    , .annotation = .annotation
                                    , ...
){
    

  
  fit_list <- lapply(.outcomes, regression_lin_predictors)
  
  names(fit_list) <- .annotation[.outcomes, "pname"]
  fit_list
  }
