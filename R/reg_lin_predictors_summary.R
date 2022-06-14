#' @title reg_lin_predictors_summary
#'
#' @description This function produces summary tables.
#'
#' @param .fit_list The list of fits produced beforehand.
#' @param .outcome A vector containing the outcome.
#' @param .annotation A matrix or data.frame in the annotation format (name,
#' pname, unit, short_pname, comment) that contains pretty names for the used
#' variables and their dummy variables.
#'
reg_lin_predictors_summary <- function(.fit_list
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

    summary_table[i, 7] <- .fit_list[[i]][nrow(.fit_list[[i]]), 7]
  }

  names(summary_table)[7] <- "nobs"

  if (!exists("pname_outcome")) {

    summary_table <- dplyr::mutate(summary_table,
                                   Outcome = rep(.outcome,
                                                 nrow(summary_table)),
                                   .before = tidyselect::all_of("term"))

  } else {

    summary_table <- dplyr::mutate(summary_table,
                                   outcome = rep(pname_outcome,
                                                 nrow(summary_table)),
                                   .before = tidyselect::all_of("term"))
  }

  summary_table
}