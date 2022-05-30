#' @title regression_lin_predictors
#'
#' @description This function calculates the regressions while shuffeling
#' through the input predictors
#'
#' @param .data A data.frame
#' @param .outcome A vector containing the outcome
#' @param .predictors A vector containing the predictors
#' @param .covariates A vector containing covariates for each regression
#' @param .annotation A matrix or data.frame in the annotation format (name,
#' pname, unit, short_pname, comment) that contains pretty names for the used
#' variables and their dummy variables.
#' @param .std_prd If TRUE, predictors are standardized using std(predictor).
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to input interactions.
#' 
regression_lin_predictors <- function(.data
                                      , .outcome
                                      , .predictors
                                      , .covariates
                                      , .annotation
                                      , .std_prd
                                      , .summary
                                      , .interaction
                                      , ...
){
  
  # Create output-list of length .predictors
  fit_list <- vector(mode = "list", length = length(.predictors))
  
  # In case the outcome is in .predictors, remove
  .predictors <- .predictors[which(!.predictors %in% .outcome)]
  
  # If wanted, standardize predictors  
  if (.std_prd == TRUE && !is.null(.annotation)) {
    
    for (i in seq_along(.predictors)) {
      
      if (.predictors[i] != "base_model") {
        name <- paste0("scale(", .predictors[i], ")")
        pname <- paste0("std(", .annotation[.predictors[i], "pname"], ")")
        .annotation <- rbind(.annotation, c(name, pname, "", "", ""))
        .predictors[i] <- paste0("scale(", .predictors[i], ")")
      }
    }
      rownames(.annotation) <- .annotation[[1]]
  }
  
  # Create annotation entries for interaction-terms, if .interaction != NULL
  if (!is.null(.interaction) && !is.null(.annotation)) {
    
    for (i in seq_along(.interaction)) {
      
      vars <- unlist(stringr::str_split(string = .interaction[i],
                                        pattern = "\\*",
                                        n = 2))
      vars <- stringr::str_remove_all(vars, " ")
      
      name <- paste0(vars[1], ":", vars[2])
      pname <- paste0(.annotation[[2]][which(.annotation[[1]] %in%
                                               vars[1])],
                      ":",
                      .annotation[[2]][which(.annotation[[1]] %in%
                                               vars[2])])
      .annotation <- rbind(.annotation, c(name, pname, "", "", ""))
    }
    rownames(.annotation) <- .annotation[[1]]
  }
  
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
      
    model <- lm(formula, data = .data, x = TRUE)
    tbl <- broom::tidy(model, conf.int = TRUE)
    
    #
    if (!is.null(.annotation)) {
      
      
      
      for (j in 2:nrow(tbl)) {
          
        tbl$term[j] <- .annotation[[2]][which(.annotation[[1]] %in%
                                                 tbl$term[j])]
      }

    }
    
    fit_list[[i]] <- dplyr::select(tbl,
                                   term,
                                   estimate,
                                   conf.low,
                                   conf.high,
                                   p.value)
    fit_list[[i]][ncol(fit_list[[i]]) + 1] <- NA
    fit_list[[i]][nrow(fit_list[[i]]) + 1, 7] <-
      broom::glance(model)$r.squared
    fit_list[[i]][nrow(fit_list[[i]]), 1] <- "R<sup>2</sup>"
    fit_list[[i]][nrow(fit_list[[i]]) + 1, 7] <-
      broom::glance(model)$adj.r.squared
    fit_list[[i]][nrow(fit_list[[i]]), 1] <-
      "Adjusted R<sup>2</sup>"
    fit_list[[i]][nrow(fit_list[[i]]) + 1, 1] <-
      paste0("<i>N</i> used: ", broom::glance(model)$nobs)
    fit_list[[i]][6] <- ifelse(fit_list[[i]]$p.value < 0.05, "&lt;.05*", "")
    names(fit_list[[i]]) <- c(
      "Predictor",
      "Estimate",
      "CI (low)",
      "CI (high)",
      "<i>p</i>-Value",
      "Significance",
      "R<sup>2</sup>"
    )
    }
  
  names(fit_list) <- .predictors
    
  if (.summary == TRUE) {
    
    fit_list[["summary"]] <-
      regression_lin_predictors_summary(.fit_list = fit_list
                                        , .outcome = .outcome
                                        , .annotation = .annotation
                                       )
  }

  fit_list
}
