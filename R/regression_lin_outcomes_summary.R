regression_lin_outcomes_summary <- function(.fit_list
                                            , .outcomes
                                            , .annotation
){
  if (!is.null(.annotation)) {
    pnames_outcomes <- vector(mode = "character", length = length(.outcomes))
    for (i in seq_along(.outcomes)) {
      pnames_outcomes[i] <- .annotation[[2]][which(.annotation[[1]] %in%
                                                     .outcomes[i])]
    }
  }
  
  summary_table <- .fit_list[[1]][FALSE,]

  for (i in seq_along(.fit_list)) {
    
    if (names(.fit_list)[i] == "base_model") {
      
      summary_table[(nrow(summary_table) + 1), 1] <- "Base model"
      
    } else {
      
      summary_table <- dplyr::bind_rows(summary_table, .fit_list[[i]][2,])
    }
    
    summary_table[i, 7] <-
      as.numeric(stringr::str_remove(.fit_list[[i]][nrow(.fit_list[[i]]), 1],
                                     "<i>N</i> used: "))
  }
  
  names(summary_table)[7] <- "<i>N</i> used: "
  
  if (!exists("pnames_outcomes")) {
    
    summary_table <- dplyr::mutate(summary_table,
                                   Outcome = .outcomes,
                                   .before = Predictor)
    
  } else {
    
    summary_table <- dplyr::mutate(summary_table,
                                   Outcome = pnames_outcomes,
                                   .before = Predictor)
    
  }
  
  summary_table
  
}