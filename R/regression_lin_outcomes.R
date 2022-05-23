regression_lin_outcomes <- function(.data
                                    , .outcomes
                                    , .predictor
                                    , .covariates
                                    , .annotation
                                    , .std_prd
                                    , .summary
                                    , .interaction
                                    , ...
){
    fit_list <- vector(mode = "list", length = length(.outcomes))
    .outcomes <- .outcomes[which(!.outcomes %in% .predictor)]
    
    if (.std_prd == TRUE) {
        
      name <- paste0("scale(", .predictor, ")")
      pname <- paste0("std(", .annotation[.predictor, "pname"], ")")
      .annotation <- rbind(.annotation, c(name, pname, "", "", ""))
      
      rownames(.annotation) <- .annotation[[1]]
      .predictor <- paste0("scale(", .predictor, ")")
    }
    
    for (i in seq_along(.outcomes)) {
      
      if (!is.null(.covariates)) {
        formula <- paste0(paste(.outcomes[i]),
                                "~",
                                paste(.predictor, collapse = "+"),
                                "+",
                                paste(.covariates, collapse = "+")
                          )
      } else {
        formula <- paste0(paste(.outcomes[i]),
                                "~",
                                paste(.predictor, collapse = "+"))
      }
      
      model <- lm(formula, data = .data, x = TRUE)

      tbl <- broom::tidy(model, conf.int = TRUE)
      
      for (j in 2:nrow(tbl)) {
        tbl$term[j] <- .annotation[[2]][which(.annotation[[1]] %in% tbl$term[j])]
        
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
      fit_list[[i]][nrow(fit_list[[i]]), 1] <-
        "R<sup>2</sup>"
      fit_list[[i]][nrow(fit_list[[i]]) + 1, 7] <-
        broom::glance(model)$adj.r.squared
      fit_list[[i]][nrow(fit_list[[i]]), 1] <-
        "Adjusted R<sup>2</sup>"
      fit_list[[i]][nrow(fit_list[[i]]) + 1, 1] <-
        paste0("<i>N</i> used: ",
               broom::glance(model)$nobs)
      fit_list[[i]][6] <-
        ifelse(fit_list[[i]]$p.value < .05, "&lt;.05*", "")
      names(fit_list[[i]]) <- c(
        "Term",
        "Estimate",
        "CI (low)",
        "CI (high)",
        "<i>p</i>-Value",
        "Significance",
        "R<sup>2</sup>"
      )
    }
    
    names(fit_list) <- .outcomes
    
    if (.summary == TRUE) {
      summary_table <- data.frame()
      for (i in seq_along(fit_list)) {
        summary_table <- rbind(summary_table, fit_list[[i]][2,])
        summary_table[i, 7] <-
          as.numeric(stringr::str_remove(fit_list[[i]][nrow(fit_list[[i]]),
                                                       1], "<i>N</i> used: "))
      }
      names(summary_table)[7] <- "<i>N</i> used: "
      fit_list[["summary"]] <- summary_table
    }    

    fit_list
  }
