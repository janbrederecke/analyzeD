#' @title reg_lin_outcomes
#'
#' @description This function calculates the regressions while shuffeling
#' through the provided outcomes.
#'
#' @param .data A data.frame or .mids object.
#' @param .outcomes A character vector containing the outcomes.
#' @param .predictor A character vector containing the predictor.
#' @param .covariates A character vector containing covariates.
#' @param .annotation A matrix or data.frame of format (name, pname, unit,
#' short_pname, comment) that contains pretty names for the used variables.
#' @param .std_prd If TRUE, predictors are standardized.
#' @param .std_cov Character vector of covariates that should be standardized.
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to specify interactions using a list of
#' character vectors containing the interaction variables, e.g.
#' list(c("variable1", "variable2"), c("variable2", "variable3")).
#' @param ... Optional input passed directly to the regression function.
#'
reg_lin_outcomes <- function(.data
                             , .outcomes
                             , .predictor
                             , .covariates
                             , .annotation
                             , .std_prd
                             , .std_cov
                             , .summary
                             , .interaction
                             , ...
){

  # Filter out cases that miss the predictor
  ## For input data.frame
  if (is.data.frame(.data)) {
    .data <- dplyr::filter(.data, !is.na(tidyselect::all_of(.predictor)))

  ## For input mids object
  } else if (mice::is.mids(.data)) {
    .data <- mice::filter(.data, !is.na(.data[[.predictor]]))
  }

  # Create output-list of length .outcomes
  fit_list <- vector(mode = "list", length = length(.outcomes))

  # In case the predictor is in .outcomes, remove
  .outcomes <- .outcomes[which(!.outcomes %in% .predictor)]

  # If wanted, standardize predictor
  ## With .annotation
  if (.std_prd == TRUE && !is.null(.annotation)) {
    if (.predictor != "base_model") {
      name <- paste0("scale(", .predictor, ")")
      pname <- paste0("std(", .annotation[.predictor, "pname"], ")")
      .annotation <- rbind(.annotation, c(name, pname, "", "", ""))

      # Check if predictor is in interaction and standardize there as well
      if (!is.null(.interaction)) {
        for (j in seq_along(.interaction)) {
          if (.predictor %in% .interaction[[j]]) {
            .interaction[[j]][which(.interaction[[j]] == .predictor)] <-
              paste0("scale(", .predictor, ")")
          }
        }
      }
      .predictor <- paste0("scale(", .predictor, ")")
    }
    rownames(.annotation) <- .annotation[[1]]

  ## Without .annotation
  } else if (.std_prd == TRUE && is.null(.annotation)) {

    # Check if predictor is in interaction and standardize as well
    if (!is.null(.interaction)) {
      for (j in seq_along(.interaction)) {
        if (.predictor %in% .interaction[[j]]) {
          .interaction[[j]][which(.interaction[[j]] == .predictor)] <-
            paste0("scale(", .predictor, ")")
        }
      }
    }
    .predictor <- paste0("scale(", .predictor, ")")
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
      pname <- paste0(
        .annotation[[2]][which(.annotation[[1]] %in%
          vars[1])],
        ":",
        .annotation[[2]][which(.annotation[[1]] %in%
          vars[2])]
      )
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

  # Seq along the outcomes (actual analyses)
  for (i in seq_along(.outcomes)) {
    if (.predictor == "base_model") {
      formula <- paste0(paste(.outcomes[i]), "~", paste(.covariates,
                                                    collapse = "+"))
    } else {
      if (!is.null(.covariates)) {
        if (!is.null(.interaction)) {
          formula <- paste0(
            paste(.outcomes[i]),
            "~",
            paste(.predictor),
            "+",
            paste(.covariates, collapse = "+"),
            "+",
            paste(.interaction, collapse = "+")
          )
        } else {
          formula <- paste0(
            paste(.outcomes[i]),
            "~",
            paste(.predictor),
            "+",
            paste(.covariates, collapse = "+")
          )
        }
      } else {
        formula <- paste0(paste(.outcomes[i]),
                                "~",
                                paste(.predictor, collapse = "+")
                          )
      }
    }

    # Select the right method for data.frame or mids
    if (is.data.frame(.data)) {
      model <- stats::lm(formula, data = .data, x = TRUE)
      model_tidy <- broom::tidy(model, conf.int = TRUE)
      model_glance <- broom::glance(model)
    } else if (mice::is.mids(.data)) {
      text2eval <- paste0("model <- with(.data, exp = lm(",
                          formula,
                          ", x = TRUE))"
      )
      eval(parse(text = text2eval))
      model_tidy <- tibble::as_tibble(broom::tidy(mice::pool(model),
                                                  conf.int = TRUE)
                                      )
      model_glance <- tibble::as_tibble(broom::glance(mice::pool(model), ))
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
  names(fit_list) <- .outcomes

  # Add summary if wanted
  if (.summary == TRUE) {
    fit_list[["summary"]] <-
      reg_lin_outcomes_summary(.fit_list = fit_list
                               , .outcomes = .outcomes
                               , .predictor = .predictor
                               , .annotation = .annotation
                              )
  }
  fit_list
}
