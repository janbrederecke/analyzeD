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
    pname_outcome <- .annotation[["pname"]][which(.annotation[["name"]] %in%
                                              .outcome)]
  }

  # Check if AIC should be included (only complete case analyses)
  if ("AIC" %in% .fit_list[[1]][["term"]]) {
    aic_col <- TRUE
  } else {
    aic_col <- FALSE
  }

  # Create empty summary table with names of the .fit_list columns
  summary_table <- .fit_list[[1]][FALSE,]

  # Create rows of the summary table
  for (i in seq_along(.fit_list)) {

    if (names(.fit_list)[i] == "base_model") {

      summary_table[(nrow(summary_table) + 1), 1] <- "Base model"

    } else {

      summary_table <- dplyr::bind_rows(summary_table, .fit_list[[i]][2, ])
    }

    summary_table[i, 7] <-
      .fit_list[[i]][[7]][which(.fit_list[[i]][["term"]] == "r.squared")]
    summary_table[i, 8] <-
      .fit_list[[i]][[7]][which(.fit_list[[i]][["term"]] == "adj.r.squared")]
    summary_table[i, 9] <-
      .fit_list[[i]][[7]][which(.fit_list[[i]][["term"]] == "nobs")]
    if (aic_col == TRUE) {
      summary_table[i, 10] <-
        .fit_list[[i]][[7]][which(.fit_list[[i]][["term"]] == "AIC")]
    }
  }

  names(summary_table)[7] <- "r.squared"
  names(summary_table)[8] <- "adj.r.squared"
  names(summary_table)[9] <- "nobs"
  if (aic_col == TRUE) {
    names(summary_table)[10] <- "AIC"
  }

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