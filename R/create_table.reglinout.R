#' @title create_table.reglinout
#'
#' @description Generic function to create HTML tables from lists of results
#'
#' @param .fit_list A list of fits produced using functions of the analyzeD
#' package that is sorted by the outcomes.
#' @param .annotation An annotation file.
#' @param ... Additional arguments to be passed to the function.
#' @export
#'
create_table.reglinout <- function(.fit_list
                                   , .annotation = NULL
                                   , ...
){

  # Check input object
  if (!"reglinout" %in% class(.fit_list)) {
    stop("fit_list must be a list of type 'reglinout'")
  }

  # Produce one summary table from all the reglinout objects
  if ("with_summary" %in% class(.fit_list)) {

  # Create empty summary table with names of the .fit_list$summary columns
  summary_table <- .fit_list[[1]]$summary[FALSE, ]

    # Add all summaries and remove them from the fit_list
    for (i in seq_along(.fit_list)) {
      summary <- .fit_list[[i]]$summary
      .fit_list[[i]]$summary <- NULL
      summary_table <- rbind(summary_table, summary)
    }
    rm(summary)
  }

  for (i in seq_along(.fit_list)) {
    for (j in seq_along(.fit_list[[i]])) {

      # Create table caption
      outcome_name <- names(.fit_list)[i]
      predictor_name <- names(.fit_list[[i]])[j]

      ## If an annotation is provided use pretty names
      if (!is.null(.annotation)) {
        if (predictor_name == "base_model") {
          predictor_name <- "Base model"
        } else {
          predictor_name <- .annotation[["pname"]][which(
            (.annotation[["name"]]) %in% predictor_name)]
        }
        outcome_name <- .annotation[["pname"]][which(
          (.annotation[["name"]]) %in% outcome_name)]
      }

      ## Create actual caption
      caption <- paste0("Regression of ", predictor_name, " on ", outcome_name)

    # Print HTML table
    options(knitr.kable.NA = "")
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        .fit_list[[i]][[j]],
        format = "html",
        digits = 3,
        caption = caption,
        escape = FALSE
      ),
      full_width = TRUE
    ))
    }
  }

  # Print summary_table if it exists
  if (exists("summary_table")) {
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        summary_table,
        format = "html",
        digits = 3,
        caption = "Summary",
        escape = FALSE
      ),
      full_width = TRUE
    ))
  }
}
