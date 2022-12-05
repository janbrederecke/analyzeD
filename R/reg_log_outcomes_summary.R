#' @title reg_log_outcomes_summary
#'
#' @description This function produces summary tables.
#'
#' @param .fit_list The list of fits produced beforehand.
#' @param .outcomes A vector containing the outcomes.
#' @param .predictor A vector containing the predictor.
#' @param .annotation A matrix or data.frame in the annotation format (name,
#' pname, unit, short_pname, comment) that contains pretty names for the used
#' variables and their dummy variables.
#'
reg_log_outcomes_summary <- function(.fit_list
                                     , .outcomes
                                     , .predictor
                                     , .annotation
){
  
  # If an annotation is provided, extract outcome pnames
  if (!is.null(.annotation)) {
    pnames_outcomes <- vector(mode = "character", length = length(.outcomes))
    for (i in seq_along(.outcomes)) {
      pnames_outcomes[i] <- .annotation[[2]][which(.annotation[[1]] %in%
                                                     .outcomes[i])]
    }
  }
  
  # Create empty summary table with names of the .fit_list columns
  summary_table <- .fit_list[[1]][FALSE, ]
  
  # Create rows of the summary table
  for (i in seq_along(.fit_list)) {
    
    # For the base_model if included
    if (.predictor == "base_model") {
      summary_table[(nrow(summary_table) + 1), 1] <- "Base model"
      
      # For normal models
    } else {
      summary_table <- dplyr::bind_rows(summary_table, .fit_list[[i]][2, ])
    }
    
    summary_table[i, 7] <-
      .fit_list[[i]][[7]][which(.fit_list[[i]][["term"]] == "nobs")]
  }
  
  names(summary_table)[7] <- "nobs"
  
  # Add the respective outcome to the summary table
  ## In case a pname for the outcome is provided
  if (!exists("pnames_outcomes")) {
    summary_table <- dplyr::mutate(summary_table,
                                   outcome = .outcomes,
                                   .before = tidyselect::all_of("term")
    )
    
    ## In case no pname is provided
  } else {
    summary_table <- dplyr::mutate(summary_table,
                                   outcome = pnames_outcomes,
                                   .before = tidyselect::all_of("term")
    )
  }
  
  # Return summary table
  summary_table
}
