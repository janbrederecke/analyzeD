regression_lin_predictors_summary <- function(.fit_list
                                              , .outcome
                                              , .annotation
){
  if (!is.null(.annotation)) {
    pname_outcome <- vector(mode = "character", length = 1)
    pname_outcome <- .annotation[[2]][which(.annotation[[1]] %in%
                                              .outcome)]
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
  
  if (!exists("pname_outcome")) {
    
    summary_table <- dplyr::mutate(summary_table,
                                   Outcome = rep(.outcome,
                                                 nrow(summary_table)),
                                   .before = Predictor)
    
  } else {
    
    summary_table <- dplyr::mutate(summary_table,
                                   Outcome = rep(pname_outcome,
                                                 nrow(summary_table)),
                                   .before = Predictor)
  }  
  
  summary_table
  
}