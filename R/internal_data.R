internal_data <- function(size = 1
) {
  # Get iris dataset as d 
  d <- iris
  names(d) <- tolower(names(d))
  
  # Output the dataframe to the environment
  d <<- d
  
  # Make an annotation file
  annotation <-
    data.frame(
      "name" = names(d),
      "pname" = toupper(names(d)),
      "unit" = c(NA, NA, NA, NA, NA),
      "short_pname" = c(NA, NA, NA, NA, NA),
      "comment" = c(NA, NA, NA, NA, NA)
    )
  rownames(annotation) <- annotation[[1]]
  annotation <<- annotation
  
  outcomes <<- names(d)[4:5]
  predictors <<- names(d)[1:3]
}
