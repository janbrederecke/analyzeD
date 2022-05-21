regression_lin <- function(.data
                           , .outcomes
                           , .predictors = NULL
                           , .covariates = NULL
                           , .annotation = annotation
                           , .cpus = 1
                           , ...
  
){
  if (!class(.data) %in% c("tbl_df", "data.frame", "mids")) {
    stop("Your data must be either of class data.frame, tbl_df, or mids")
  
  } else if (class(.data) == "data.frame") {
  
  return(regression_lin_outcomes(.data = .data
                                 , .outcomes = .outcomes
                                 , .predictors = .predictors
                                 , .covariates = .covariates
                                 , .annotation = .annotation
                                 , .parallel = .parallel
                                 , .cpus = .cpus)
         )
  }
}