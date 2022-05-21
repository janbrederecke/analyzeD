regression_lin <- function(.data
                           , .outcomes
                           , .predictors
                           , .covariates = NULL
                           , .annotation = annotation
                           , ...
  
){
  if (!class(.data) %in% c("tbl_df", "data.frame", "mids")) {
    stop("Your data must be either of class data.frame, tbl_df, or mids")
  
  } else if (class(.data) == "data.frame") {
  
  return(regression_lin_outcomes(.data
                                 , .outcomes
                                 , .predictors
                                 , .annotation))
  }
  
  
  }