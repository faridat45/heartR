#' Print summ_heartr output
#'
#' Provides a nice customisable table for the output of the summ_heartr function.
#'
#' @param x An object of class \code{"summ_heartr"} outputted from the
#' summ_heartr function.
#' @param bordercolour Colour of the cell borders. Must be in quotations.
#' Default colour of pink is used.
#' @param fillcolour Colour of the Variable cells. Must be in quotations.
#' Default colour of #FFEDFB is used
#' @param font Google style used in the table. Must be a Google font which can
#' be found \href{https://fonts.google.com/}{here}.
#' @param ... Catches unused arguments to \code{print} (not currently
#' implemented).
#'
#' @return An aesthetically pleasing table nicely displaying the summary
#' statistics.
#'
#' @seealso \code{\link{print}}, \code{\link{summ_heartr}}
#'
#' @export
#' @author Ciara Olohan - <\email{ciara.olohan.2023@@mumail.ie}>
#' @importFrom gt "gt" "tab_header" "md" "cols_label" "tab_style" "cell_borders"
#' "cols_width" "px" "cells_body" "cell_fill" "opt_table_font" "google_font"
#' "cols_align"
#' @importFrom dplyr "everything"
#' @examples
#' # example code
#'
#'
#'
#'
print.summ_heartr<- function(x, bordercolour = "pink", fillcolour = "#FFEDFB",
                             font = "Roboto Condensed", ...) {


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
      style = gt::cell_borders( color = bordercolour),
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
      style = gt::cell_fill(color= fillcolour)) |>
    gt::opt_table_font(font = gt::google_font(font),
                       weight = 450) |>
    gt::cols_align("center")

  print(gt_table)
  invisible(x)
}

utils::globalVariables("Variable")


