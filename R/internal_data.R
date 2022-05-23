internal_data <- function(size = 1
) {
  library(palmerpenguins)
  # Get palmer penguins dataset as d 
  # Output the dataframe to the environment
  d <<- penguins
  
  # Make an annotation file
  annotation <-
    data.frame(
      "name" = names(d),
      "pname" = toupper(names(d)),
      "unit" = c(rep(NA, length(names(d)))),
      "short_pname" = c(rep(NA, length(names(d)))),
      "comment" = c(rep(NA, length(names(d))))
    )
  rownames(annotation) <- annotation[[1]]
  annotation <<- annotation
  
  outcomes <<- names(d)[3:4]
  predictors <<- names(d)[5:6]
  covariates <<- names(d)[8]
}


regression_lin(.data = as.data.frame(d)
               , .outcomes = outcomes
               , .predictors = predictors
               , .covariates = covariates
               , .sort_by = "predictors"
               , .cpus = 1
               , .std_prd = TRUE
               , .subset = "species != 'Adelie'")

