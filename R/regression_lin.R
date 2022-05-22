regression_lin <- function(.data
                           , .outcomes
                           , .predictors = NULL
                           , .covariates = NULL
                           , .annotation = annotation
                           , .subset = NULL
                           , .cpus = 1
                           , .sort_by = "outcomes"
                           , ...
  
){
  
  if (!is.null(.subset)) {
    
    .data <- .data[.subset,]
  }
  
  
  if (!class(.data) %in% c("tbl_df", "data.frame", "mids")) {
    
    stop("Your data must be either of class data.frame, tbl_df, or mids")
  
  } else if (class(.data) == "data.frame" && .sort_by == "outcomes") {
  
    return(regression_lin_by_outcomes(.data = .data
                                      , .outcomes = .outcomes
                                      , .predictors = .predictors
                                      , .covariates = .covariates
                                      , .annotation = .annotation
                                      , .parallel = .parallel
                                      , .cpus = .cpus
                                      )
    )
    
  }  else if (class(.data) == "data.frame" && .sort_by == "predictors") {
    
    return(regression_lin_by_predictors(.data = .data
                                        , .outcomes = .outcomes
                                        , .predictors = .predictors
                                        , .covariates = .covariates
                                        , .annotation = .annotation
                                        , .parallel = .parallel
                                        , .cpus = .cpus
                                        )
    )
  }
}
