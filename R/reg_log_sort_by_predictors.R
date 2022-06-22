#' @title reg_log_sort_by_predictors
#'
#' @description This function calls the actual reg_log_outcomes
#' function that shuffles through the provided outcomes.
#'
#' @param .data A data.frame or .mids object.
#' @param .outcomes A character vector containing the outcomes.
#' @param .predictors A character vector containing the predictors.
#' @param .covariates A character vector containing covariates.
#' @param .annotation A matrix or data.frame of format (name, pname, unit,
#' short_pname, comment) that contains pretty names for the used variables.
#' @param .cpus Input number of desired cpus to use. Useful only in case of big
#' datasets and multiple outcomes/predictors.
#' @param .std_prd If TRUE, predictors are standardized.
#' @param .std_cov Character vector of covariates that should be standardized.
#' @param .summary If TRUE, an additional summary of all analyses is returned.
#' @param .interaction Can be used to specify interactions using a list of
#' character vectors containing the interaction variables, e.g.
#' list(c("variable1", "variable2"), c("variable2", "variable3")).
#' @param .firth If TRUE, a Firth-corrected version of glm in brglm() is called.
#' @param ... Optional input passed directly to the regression function.
#'
#' @importFrom foreach "%dopar%"
#'
reg_log_sort_by_predictors <- function(.data
                                       , .outcomes
                                       , .predictors
                                       , .covariates
                                       , .annotation
                                       , .cpus
                                       , .std_prd
                                       , .std_cov
                                       , .summary
                                       , .interaction
                                       , .firth
                                       , ...
){

  # Process on single CPU
  if (.cpus == 1) {

    # Call the reg_log_outcomes function
    fit_list <- lapply(.predictors, function(predictor) {
      reg_log_outcomes(.data = .data
                              , .outcomes = .outcomes
                              , .predictor = predictor
                              , .covariates = .covariates
                              , .annotation = .annotation
                              , .std_prd = .std_prd
                              , .std_cov = .std_cov
                              , .summary = .summary
                              , .interaction = .interaction
                              , .firth
                              , ...
                              )
    })
    fit_list

    # Processing in parallel on multiple CPUS
  } else if (.cpus > 1) {

    print(paste0("Parallel processing. Using ", .cpus, " cores."))

    # Register cluster
    my_cluster <- parallel::makeCluster(.cpus)

    ## Export summary function because the foreach export did not work properly
    parallel::clusterExport(cl = my_cluster,
                            varlist = c("reg_log_outcomes_summary"),
                            envir = environment()
                           )

    ## Actual registering of cluster
    doParallel::registerDoParallel(cl = my_cluster)

    n <- length(.predictors)

    # Calculate regressions for each outcome on a single CPU
    fit_list <- foreach::foreach(
      i = 1:n,
      .packages = c("broom", "dplyr", "stringr", "tidyselect"),
      .export = c("reg_log_outcomes",
                  "reg_log_outcomes_summary"
                 )

    ) %dopar% {

      ## Get one predictor to distribute to a core
      predictor <- .predictors[i]

      ## Fit regression models for specified outcome
      fit_list_outcomes <- reg_log_outcomes(
        .data = .data
        , .outcomes = .outcomes
        , .predictor = predictor
        , .covariates = .covariates
        , .annotation = .annotation
        , .std_prd = .std_prd
        , .std_cov = .std_cov
        , .summary = .summary
        , .interaction = .interaction
        , .firth
        , ...
      )

      ## Return the list of results per predictor
      return(fit_list_outcomes)
    }

    # Stop the cluster
    parallel::stopCluster(cl = my_cluster)
  }

  # Name the list of lists using the respective outcomes
  ## For standardized predictors
  if (.std_prd == TRUE) {
    names(fit_list) <- paste0("std(", .predictors, ")")

  ## For non-standardized predictors
  } else {
    names(fit_list) <-  .predictors
  }

  # Define class
  class(fit_list) <- c("by_predictors", "reglog", "list")

  # Declare additional class attribute for summary
  if (.summary == TRUE) {
    class(fit_list) <- c("with_summary", class(fit_list))
  } else if (.summary == FALSE) {
    class(fit_list) <- c("without_summary", class(fit_list))
  }

  # Return results
  fit_list
}

# Exposing 'i' to global environment
globalVariables(c("i"))
