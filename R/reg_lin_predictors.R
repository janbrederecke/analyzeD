#' @title reg_lin_predictors
#'
#' @description This function calculates the regressions while shuffeling
#' through the provided predictors.
#'
#' @param .data A data.frame or .mids object.
#' @param .outcome A character vector containing the outcome.
#' @param .predictors A character vector containing the predictors.
#' @param .covariates A character vector containing covariates.
#' @param .annotation A matrix or data.frame of format (name, pname, unit,
#' short_pname, comment) that contains pretty names for the used variables.
#' @param .std_prd If TRUE, predictors are standardized.
#' @param .std_cov Character vector of covariates that should be standardized.
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to specify interactions using a list of
#' character vectors containing the interaction variables, e.g.
#' list(c("variable1", "variable2"), c("variable2", "variable3")).
#' @param .imputed_predictors If TRUE, cases with imputed predictors are used.
#' @param .imputed_outcomes If TRUE, cases with imputed outcomes are used.
#' @param ... Optional input passed directly to the regression function.
#'
reg_lin_predictors <- function(.data
                               , .predictors
                               , .covariates
                               , .outcome
                               , .annotation
                               , .std_prd
                               , .std_cov
                               , .summary
                               , .interaction
                               , .imputed_predictors
                               , .imputed_outcomes
                               , ...
){

  # Filter out cases that miss the outcome
  ## For input data.frame
  if (is.data.frame(.data)) {
    .data <- dplyr::filter(.data, !is.na(tidyselect::all_of(.outcome)))

  ## For input mids object
  } else if (mice::is.mids(.data) && .imputed_outcomes == FALSE) {
    .data <- mice::filter(.data, !is.na(.data[[.outcome]]))
  }
  
  # In case the outcome is in .predictors, remove
  .predictors <- .predictors[which(!.predictors %in% .outcome)]
  
  # Create output-list of length .predictors
  fit_list <- vector(mode = "list", length = length(.predictors))
  
  # Keep the original .predictors for later operations
  predictors_original <- .predictors

  # If wanted, standardize predictors
  ## With .annotation
  if (.std_prd == TRUE && !is.null(.annotation)) {
    for (i in seq_along(.predictors)) {
      if (.predictors[i] != "base_model") {
        name <- paste0("scale(", .predictors[i], ")")
        pname <- paste0("std(", .annotation[.predictors[i], "pname"], ")")
        .annotation <- rbind(.annotation, c(name, pname, "", "", ""))

        # Check if predictor is in interaction and standardize there as well
        if (!is.null(.interaction)) {
          for (j in seq_along(.interaction)) {
            if (.predictors[i] %in% .interaction[[j]]) {
              .interaction[[j]][which(.interaction[[j]] == .predictors[i])] <-
                paste0("scale(", .predictors[i], ")")
            }
          }
        }
        .predictors[i] <- paste0("scale(", .predictors[i], ")")
      }
    }
    rownames(.annotation) <- .annotation[[1]]

  ## Without .annotation
  } else if (.std_prd == TRUE && is.null(.annotation)) {

    # Check if predictor is in interaction and standardize as well
    if (!is.null(.interaction)) {
      for (i in seq_along(.predictors)) {
        for (j in seq_along(.interaction)) {
          if (.predictors[i] %in% .interaction[[j]]) {
            .interaction[[j]][which(.interaction[[j]] == .predictors[i])] <-
              paste0("scale(", .predictors[i], ")")
          }
        }
      }
    }
    .predictors <- paste0("scale(", .predictors, ")")
  }

  # If wanted, standardize covariates
  ## With .annotation
  if (!is.null(.std_cov) && !is.null(.annotation)) {
    for (i in seq_along(.std_cov)) {
      name <- paste0("scale(", .std_cov[i], ")")
      pname <-
        paste0("std(",
               .annotation[.covariates[.covariates == .std_cov[i]], "pname"],
               ")"
              )
      .annotation <- rbind(.annotation, c(name, pname, "", "", ""))

      # Check if covariate is in interaction and standardize as well
      if (!is.null(.interaction)) {
        for (j in seq_along(.interaction)) {
          if (.std_cov[i] %in% .interaction[[j]]) {
            .interaction[[j]][which(.interaction[[j]] == .std_cov[i])] <-
              paste0("scale(", .std_cov[i], ")")
          }
        }
      }

    .covariates[.covariates == .std_cov[i]] <-
      paste0("scale(",
             .covariates[.covariates == .std_cov[i]],
             ")"
            )
    }
    rownames(.annotation) <- .annotation[[1]]

  ## Without .annotation
  } else if (!is.null(.std_cov) && is.null(.annotation)) {
    for (i in seq_along(.std_cov)) {

      # Check if covariate is in interaction and standardize as well
      if (!is.null(.interaction)) {
        for (j in seq_along(.interaction)) {
          if (.std_cov[i] %in% .interaction[[j]]) {
            .interaction[[j]][which(.interaction[[j]] == .std_cov[i])] <-
              paste0("scale(", .std_cov[i], ")")
          }
        }
      }
      .covariates[.covariates == .std_cov[i]] <-
        paste0("scale(",
          .covariates[.covariates == .std_cov[i]],
          ")"
          )
    }
  }

  # Create annotation entries for interaction-terms, if .interaction != NULL
  ## With .annotation
  if (!is.null(.interaction) && !is.null(.annotation)) {
    for (i in seq_along(.interaction)) {
      vars <- .interaction[[i]]
      name <- paste0(vars[1], ":", vars[2])
      pname <- paste0(.annotation[[2]][which(.annotation[[1]] %in%
                                               vars[1])],
                      ":",
                      .annotation[[2]][which(.annotation[[1]] %in%
                                               vars[2])])
      .annotation <- rbind(.annotation, c(name, pname, "", "", ""))

      .interaction[[i]] <- paste0(.interaction[[i]], collapse = "*")
    }
    rownames(.annotation) <- .annotation[[1]]

  ## Without .annotation
  } else if (!is.null(.interaction) && is.null(.annotation)) {
    for (i in seq_along(.interaction)) {
      .interaction[[i]] <- paste0(.interaction[[i]], collapse = "*")
    }
  }

  # Seq along the predictors (actual analyses)
  for (i in seq_along(.predictors)) {
    if (.predictors[i] == "base_model") {
      formula <- paste0(paste(.outcome), "~", paste(.covariates,
                                                     collapse = "+"))
    } else {
      if (!is.null(.covariates)) {
        if (!is.null(.interaction)) {
          formula <- paste0(
            paste(.outcome),
            "~",
            paste(.predictors[i]),
            "+",
            paste(.covariates, collapse = "+"),
            "+",
            paste(.interaction, collapse = "+")
          )
        } else {
          formula <- paste0(
            paste(.outcome),
            "~",
            paste(.predictors[i]),
            "+",
            paste(.covariates, collapse = "+")
          )
        }
      } else {
        formula <- paste0(paste(.outcome),
                          "~",
                          paste(.predictors[i])
                          )
      }
    }

    # Select the right method for data.frame or mids
    if (is.data.frame(.data)) {
      model <- stats::lm(formula, data = .data, x = TRUE)
      model_tidy <- broom::tidy(model, conf.int = TRUE)
      model_glance <- broom::glance(model)
    } else if (mice::is.mids(.data)) {
      
      if (.imputed_predictors == FALSE) {
        ## Remove cases with the imputed .predictor 
        if (.predictors[i] != "base_model") {
          .data_pred <- mice::filter(.data, !is.na(.data[[predictors_original[i]]]))
        } else {
          .data_pred <- .data
        }
      } else {
        .data_pred <- .data
      }
      
      text2eval <- paste0("model <- with(.data_pred, exp = lm(",
                          formula,
                          ", x = TRUE))"
      )
      eval(parse(text = text2eval))
      model_tidy <- tibble::as_tibble(broom::tidy(mice::pool(model),
                                                  conf.int = TRUE)
                                      )
      model_glance <- tibble::as_tibble(broom::glance(mice::pool(model),))
    }

    # Add pretty names to the table if annotation is available
    if (!is.null(.annotation)) {
      for (j in 2:nrow(model_tidy)) {
        model_tidy$term[j] <-
          .annotation[["pname"]][which(.annotation[["name"]] %in%
                                      model_tidy$term[j])]
      }
    }
    fit_list[[i]] <- dplyr::select(model_tidy, tidyselect::all_of(c(
                                   "term",
                                   "estimate",
                                   "conf.low",
                                   "conf.high",
                                   "p.value"))
                                  )
    fit_list[[i]][ncol(fit_list[[i]]) + 1] <- NA
    fit_list[[i]][nrow(fit_list[[i]]) + 1, 7] <- model_glance$r.squared
    names(fit_list[[i]])[7] <- "info"
    fit_list[[i]][nrow(fit_list[[i]]), 1] <- "r.squared"
    fit_list[[i]][nrow(fit_list[[i]]) + 1, 7] <- ifelse(
      is.nan(model_glance$adj.r.squared) != TRUE,
      model_glance$adj.r.squared,
      0
    )
    if (is.nan(model_glance$adj.r.squared)) {
            print("Negative adj.r.squared has been set to 0")
    }
    fit_list[[i]][nrow(fit_list[[i]]), 1] <- "adj.r.squared"
    fit_list[[i]][nrow(fit_list[[i]]) + 1, 1] <- "nobs"
    fit_list[[i]][nrow(fit_list[[i]]), 7] <- model_glance$nobs
    if (is.data.frame(.data)) {
      fit_list[[i]][nrow(fit_list[[i]]) + 1, 1] <- "AIC"
      fit_list[[i]][nrow(fit_list[[i]]), 7] <- model_glance$AIC
    }
    fit_list[[i]][6] <- ifelse(fit_list[[i]]$p.value < .05, "*", NA_character_)
    names(fit_list[[i]])[6] <- "significance"
    }
  names(fit_list) <- .predictors

  # Add summary if wanted
  if (.summary == TRUE) {
    fit_list[["summary"]] <-
      reg_lin_predictors_summary(.fit_list = fit_list
                                 , .outcome = .outcome
                                 , .annotation = .annotation
                                )
  }
  fit_list
}
