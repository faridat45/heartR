
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
      Q3 = stats::quantile(x),
      Maximum = max(x),
      SD = stats::sd(x),
      Var = stats::var(x)
    )
  }

  if(length(input) == 1) {
    finalsumm <- summ(input[[1]], newnames[1])
    class(finalsum) <- "summ_heartr"
    return(finalsumm)
  }

  if (length(input) > 1) {
    list_output <- mapply(summ, input, name = newnames, SIMPLIFY = FALSE)

    finalsumm <- do.call(rbind, list_output)
    class(finalsumm <- "summ_heartr")
    return(finalsumm)
  }
}





