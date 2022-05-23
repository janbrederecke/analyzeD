regression_lin <- function(.data
                           , .outcomes = NULL
                           , .predictors = NULL
                           , .covariates = NULL
                           , .annotation = annotation
                           , .subset = NULL
                           , .cpus = 1
                           , .sort_by = "outcomes"
                           , .std_prd = FALSE
                           , .summary = FALSE
  
){
  
  # Subset data if .subset != NULL
  if (!is.null(.subset)) {
    .data <- subset(.data, eval(parse(text = .subset)))
  }
  
  
  if (!class(.data)[1] %in% c("tbl_df", "data.frame", "mids")) {
    
    stop("Your data must be either of class data.frame, tbl_df, or mids")
  
  } else if (class(.data) == "data.frame" && .sort_by == "outcomes") {
  
    return(regression_lin_by_outcomes(.data = .data
                                      , .outcomes = .outcomes
                                      , .predictors = .predictors
                                      , .covariates = .covariates
                                      , .annotation = .annotation
                                      , .cpus = .cpus
                                      , .std_prd = .std_prd
                                      , .summary = .summary
                                      )
    )
    
  }  else if (class(.data) == "data.frame" && .sort_by == "predictors") {
    
    return(regression_lin_by_predictors(.data = .data
                                        , .outcomes = .outcomes
                                        , .predictors = .predictors
                                        , .covariates = .covariates
                                        , .annotation = .annotation
                                        , .cpus = .cpus
                                        , .std_prd = .std_prd
                                        , .summary = .summary
                                        )
    )
  }
}
