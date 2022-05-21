regression_lin_outcomes <- function(.data
                                    , .outcomes
                                    , .predictors = NULL
                                    , .covariates = NULL
                                    , .annotation = annotation
                                    , ...
){
    

  
  fit_list <- lapply(.outcomes, function(x) {
    regression_lin_predictors(.data = .data
                              , .outcome = x
                              , .predictors = .predictors)
  })
  
  names(fit_list) <- .annotation[.outcomes, "pname"]
  fit_list
  }
