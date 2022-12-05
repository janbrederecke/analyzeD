#' @title reg_log_predictors_summary
#'
#' @description This function produces summary tables.
#'
#' @param .fit_list The list of fits produced beforehand.
#' @param .outcome A vector containing the outcome.
#' @param .annotation A matrix or data.frame in the annotation format (name,
#' pname, unit, short_pname, comment) that contains pretty names for the used
#' variables and their dummy variables.
#'
reg_log_predictors_summary <- function(.fit_list
                                       , .outcome
                                       , .annotation
){
  
  # If an annotation is provided, extract outcome pname
  if (!is.null(.annotation)) {
    pname_outcome <- vector(mode = "character", length = 1)
    pname_outcome <- .annotation[["pname"]][which(.annotation[["name"]] %in%
                                                    .outcome)]
  }
  
  # Create empty summary table with names of the .fit_list columns
  summary_table <- .fit_list[[1]][FALSE,]
  
  # Create rows of the summary table
  for (i in seq_along(.fit_list)) {
    
    # If the base_model is included
    if (names(.fit_list)[i] == "base_model") {
      summary_table[(nrow(summary_table) + 1), 1] <- "Base model"
      
      # For normal models
    } else {
      summary_table <- dplyr::bind_rows(summary_table, .fit_list[[i]][2, ])
    }
    
    print(.fit_list)
    
    
    
    
  }
  # Add the respective outcome to the summary table
  ## In case a pname for the outcome is provided
  if (!exists("pname_outcome")) {
    summary_table <- dplyr::mutate(summary_table,
                                   outcome = rep(.outcome,
                                                 nrow(summary_table)),
                                   .before = tidyselect::all_of("term")
    )
    
    ## In case no pname for the outcome is provided
  } else {
    summary_table <- dplyr::mutate(summary_table,
                                   outcome = rep(pname_outcome,
                                                 nrow(summary_table)),
                                   .before = tidyselect::all_of("term")
    )
  }
  
  # Return summary table
  summary_table
}