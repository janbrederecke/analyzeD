internal_data <- function(size = 1
) {
  library(palmerpenguins)
  # Get palmer penguins dataset as d
  # Output the dataframe to the environment
  .data <- as.data.frame(penguins)
  .data$year2 <- .data$year * sample(1:100, nrow(.data), replace = T)
  .data$year3 <- .data$year2 * sample(1:100, nrow(.data), replace = T)

  .data <<- .data
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
  .std_prd <<- FALSE
  .summary <<- FALSE
  .interaction <<- c("year * year2", "year2 * year3")
}

internal_data()
regression_lin(
  .data = .data
, .outcomes = .outcomes
, .predictors = .predictors
, .covariates = .covariates
, .annotation = .annotation
, .sort_by = "outcomes"
, .cpus = 2
, .summary = TRUE
, .std_prd = TRUE)
