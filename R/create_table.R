
#' @title create_table
#'
#' @description Generic function to create HTML tables from lists of results
#'
#' @param .fit_list A list of fits produced using functions of the analyseD
#' package.
#' @param ... Additional arguments to be passed to the function.
#' @export
#'
create_table <- function(.fit_list
                         , ...
){
  UseMethod("create_table")
}