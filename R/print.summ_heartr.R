#' print.summ_heartr
#'
#' Provides a nice tabulation for the output of  code{link{summ_heartr}}.
#'
#' @param x An object of class \code{"summ_heartr"} outputted from the \code{link{summ_heartr}} function.
#' @param ... Catches unused arguments to \code{print} (not currently implemented).
#'
#' @return An aesthetically pleasing table nicely displaying the summary statistics.
#'
#' @export
#' @importFrom gt "gt" "tab_header" "md" "cols_label" "tab_style" "cell_borders" "cols_width"
#' "px" "cells_body" "cell_fill" "opt_table_font" "google_font" "cols_align"
#' @importFrom dplyr "everything"
#' @author Ciara Olohan - <\email{ciara.olohan.2023@@mumail.ie}>
#' @seealso \code{\link{print}}, \code{\link{summ_heartr}}
#' @examples
#' # example code
#'
#'
#'
#'
#'


# dealing with 'no visible binding' note as recommended in lecture 11
utils::globalVariables("Variable")

print.summ_heartr<- function(x, ...) {


  if(!inherits(x, "summ_heartr")) stop("x must be of class summ_heartr")

  z <- unclass(x)
  class(z) <- "data.frame"

  gt_table <-
    gt::gt(z) |>
    gt::tab_header(title= gt::md("**Summary**")) |>
    gt::cols_label(
      Variable = gt::md("*Variable*"),
      Mean = gt::md("**Mean**"),
      Minimum = gt::md("**Minimum**"),
      Q1 = gt::md("**1st Quartile**"),
      Median = gt::md("**Median**"),
      Q3 = gt::md("**3rd Quartile**"),
      Maximum = gt::md("**Maximum**"),
      SD = gt::md("**Standard, <br>Deviation**"),
      Var = gt::md("**Variance**")) |>
    gt::tab_style(
      style = gt::cell_borders( color = "pink"),
      locations = gt::cells_body())  |>
    gt::cols_width(
      Variable ~ gt::px(90),
      Mean ~ gt::px(90),
      Minimum ~ gt::px(90),
      Q1 ~ gt::px(90),
      Median ~ gt::px(90),
      Q3 ~ gt::px(90),
      Maximum ~ gt::px(90),
      SD ~ gt::px(90),
      Var ~ gt::px(90)) |>
    gt::tab_style(
      locations = gt::cells_body(
        columns = Variable,
        rows = dplyr::everything()),
      style = gt::cell_fill(color="#FFEDFB")) |>
    gt::opt_table_font(font = gt::google_font("Roboto Condensed"),
                       weight = 450) |>
    gt::cols_align("center")

  print(gt_table)
  invisible(x)
}




