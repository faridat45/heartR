#' summ_heartr
#'
#' Creates a list of the following summary statistics for one or more numeric variables:
#' mean, minimum, 1st quartile, median, 3rd quartile, maximum, standard deviation and variance.
#'
#' @param ... One or more numeric variables.
#'
#' @returns An object of class summ_heartr containing the following statistics for each input variable:
#' mean, minimum, 1st quartile, median, 3rd quartile, maximum, standard deviation and variance.
#'
#' @note A dedicated \code{\link{print.summ_heartr}} method is provided for objects of class \code{"summ_heartr"}.
#'
#' @seealso \code{\link{print.summ_heartr}}
#' @export
#' @author Ciara Olohan - <\email{ciara.olohan.2023@@mumail.ie}>
#' @importFrom stats "quantile" "median" "sd" "var"
#' @examples
#' summ_heartr(heart_dat$age, heart_dat$cholesterol)
#'
#'
#'
summ_heartr <- function(...){
  input <- list(...)

  names1 <- sapply(substitute(list(...))[-1], deparse)
  newnames <- sapply(names1, function(y){
    splitname <- strsplit(y, "\\$")[[1]]
    utils::tail(splitname, 1)
  })

  summ <- function(x, name){
    if(!is.numeric(x)) warning("argument is not numeric")

    data.frame(
      Variable = name,
      Mean = mean(x),
      Minimum = min(x),
      Q1 = stats::quantile(x, 0.25),
      Median = stats::median(x),
      Q3 = stats::quantile(x, 0.75),
      Maximum = max(x),
      SD = stats::sd(x),
      Var = stats::var(x)
    )
  }

  if(length(input) == 1) {
    finalsumm <- summ(input[[1]], newnames[1])
    class(finalsumm) <- "summ_heartr"
    return(finalsumm)
  }

  if (length(input) > 1) {
    list_output <- mapply(summ, input, name = newnames, SIMPLIFY = FALSE)

    finalsumm <- do.call(rbind, list_output)
    class(finalsumm) <- "summ_heartr"
    return(finalsumm)
  }
}


