rm(list = ls())
unloadNamespace("analyzeD")
remove.packages("analyzeD")

devtools::load_all()

internal_data <- function(size = 1
                          , bin_out = TRUE
                          , character_out = FALSE
) {
  library(palmerpenguins)
  # Get palmer penguins dataset as d
  # Output the dataframe to the environment
  .data <- as.data.frame(penguins)
  .data$year2 <- .data$year * sample(1:100, nrow(.data), replace = T)
  .data$year3 <- .data$year2 * sample(1:100, nrow(.data), replace = T)

  if (bin_out == TRUE) {
    .data[[3]] <- ifelse(.data[[3]] > 40, 1, 0)
    .data[[4]] <- ifelse(.data[[4]] > 18, 1, 0)
  }

  if (character_out) {
    .data[, c(3)] <- lapply(.data[,c(3)], as.character)
  }

  .data_miss <<- missMethods::delete_MCAR(.data, p = 0.08)
  .imp_data <<- mice::mice(.data_miss, m = 5, method = "cart")
  .data <<- mice::complete(.imp_data, 1)

  # Make an annotation file
  .annotation <-
    data.frame(
      "name" = names(.data),
      "pname" = toupper(names(.data)),
      "unit" = c(rep(NA, length(names(.data)))),
      "short_pname" = c(rep(NA, length(names(.data)))),
      "comment" = c(rep(NA, length(names(.data))))
    )
  rownames(.annotation) <- .annotation[[1]]
  .annotation <<- .annotation

    .outcomes <<- names(.data)[3:4]
    .predictors <<- names(.data)[5:6]
    .covariates <<- names(.data)[8:10]
    .outcome <<- .outcomes[1]
    .predictor <<- .predictors[1]
    .std_prd <<- FALSE
    .summary <<- FALSE
    .interaction <<- c("year * year2", "year2 * year3")
}

internal_data(bin_out = FALSE)


create_table(
      regression_lin(
        .data = .imp_data
      , .outcomes = .outcomes
      , .predictors = c(.predictors)
      # , .covariates = .covariates
      , .annotation = .annotation
      , .sort_by = "outcomes"
      , .cpus = 1
      , .summary = TRUE
      , .std_prd = FALSE
      , .std_cov = c("year",  "year3")
      , .interaction = list(c("year", "year2"),
                            c("year2", "year3")
                           )
      , .imputed_predictors = TRUE
      , .imputed_outcomes = FALSE
    )
    , .only_summary = TRUE
)

print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")

create_table(
  regression_lin(
      .data = .imp_data
    , .outcomes = .outcomes
    , .predictors = c("base_model", .predictors)
    , .covariates = .covariates
    , .annotation = .annotation
    , .sort_by = "predictors"
    , .cpus = 1
    , .summary = TRUE
    , .std_prd = TRUE
    , .std_cov = c("year", "year3")
    , .interaction = list(c("year", "year2"),
                          c("year2", "year3")
                         )
    , .imputed_predictors = FALSE
    , .imputed_outcomes = FALSE
  )
  , .only_summary = T
)

print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")


internal_data(bin_out = TRUE)
  
create_table(  
  regression_log(
      .data = .imp_data
    , .outcomes = .outcomes
    , .predictors = c("base_model", .predictors)
    , .covariates = .covariates
    , .annotation = .annotation
    , .sort_by = "predictors"
    , .summary = TRUE
    , .std_prd = TRUE
    , .std_cov = TRUE
    , .interaction = list(c("year", "year2"),
                          c("year2", "year3"))
    , .firth = TRUE
    , .imputed_predictors = FALSE
    , .imputed_outcomes = FALSE
  )
  , .only_summary = TRUE
)

print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")

create_table(
  regression_log(
    .data = .imp_data
    , .outcomes = .outcomes
    , .predictors = c("base_model", .predictors)
    , .covariates = .covariates
    , .annotation = .annotation
    , .sort_by = "predictors"
    , .cpus = 1
    , .summary = TRUE
    , .std_prd = TRUE
    , .std_cov = c("year", "year3")
    , .interaction = list(c("year", "year2"),
                          c("year2", "year3"))
    , .firth = TRUE
    , .imputed_predictors = FALSE
    , .imputed_outcomes = FALSE
  )
  , .only_summary = TRUE
)

print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")
print("------------")

create_table(
  regression_log(
    .data = .imp_data
    , .outcomes = .outcomes
    , .predictors = c(.predictors)
    # , .covariates = .covariates
    , .annotation = .annotation
    , .sort_by = "predictors"
    , .cpus = 1
    , .summary = TRUE
    , .std_prd = TRUE
    , .std_cov = c("year", "year3")
    , .interaction = list(c("year", "year2"),
                          c("year2", "year3"))
    , .firth = TRUE
    , .imputed_predictors = FALSE
    , .imputed_outcomes = FALSE
  )
  , .only_summary = TRUE
)





