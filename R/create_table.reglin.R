#' @title create_table.reglin
#'
#' @description Generic function to create HTML tables from lists of results
#'
#' @param .fit_list A list of fits produced using functions of the analyzeD
#' package.
#' @param .annotation An annotation file.
#' @param ... Additional arguments to be passed to the function.
#' @export
#'
create_table.reglin <- function(.fit_list
                                   , .annotation = NULL
                                   , .only_summary = FALSE
                                   , ...
){

  # Check input object
  if (!"reglin" %in% class(.fit_list)) {
    stop("fit_list must be a list of type 'reglin'")
  }

  # Produce one summary table from all the reglin objects
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

    # Add pretty HTML names to the summary_table
    names(summary_table) <- c("Outcome",
                              "Predictor",
                              "Estimate",
                              "CI low",
                              "CI high",
                              "<i>p</i>",
                              "Significance",
                              "R<sup>2</sup>",
                              "Adjusted R<sup>2</sup>",
                              "<i>N</i>"
                             )

    # Print the summary_table
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        summary_table,
        format = "html",
        digits = 3,
        caption = "Summary",
        escape = FALSE
      ),
      full_width = TRUE,
      bootstrap_options = c("striped",
                            "hover",
                            "condensed",
                            "responsive")
    ))
  }

  if (!.only_summary == TRUE) {
    for (i in seq_along(.fit_list)) {
      for (j in seq_along(.fit_list[[i]])) {

        # Create table caption depending on the way the input is sorted
        if ("by_outcomes" %in% class(.fit_list)) {
          predictor_name <- names(.fit_list[[i]])[j]
          outcome_name <- names(.fit_list)[i]
        } else if ("by_predictors" %in% class(.fit_list)) {
          predictor_name <- names(.fit_list)[i]
          outcome_name <- names(.fit_list[[i]])[j]
        }

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
        caption <- paste0("Regression of ",
                            predictor_name,
                            " on ",
                            outcome_name)

        # Insert HTML names for each table
        names(.fit_list[[i]][[j]]) <- c("Predictor",
                                        "Estimate",
                                        "CI low",
                                        "CI high",
                                        "<i>p</i>",
                                        "Significance",
                                        "Info"
                                       )

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
          full_width = TRUE,
          bootstrap_options = c("striped",
                              "hover",
                              "condensed",
                              "responsive")
        ))
      }
    }
  }
}
