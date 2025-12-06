

summ_heartr <- function(x, type) {
  if( !is.numeric(x)) {
    warning ("argument is not numeric") }

   if ( type == "mean") {
   Mean <- mean(x)
  class(Mean) <- "mean"
  return(Mean) }

  if ( type == "median") {
    Median <- median(x)
  class(Median) <- "median"
  return(Median)}

  if ( type == "maximim") {
    Max <- max(x)
    class(Max) <- "maximum"
    return(Max)}

  if ( type == "minimum") {
    Min <- min(x)
    class(Min) <- "minimum"
    return(Min)}

  if ( type == "iqr") {
    iqr <- IQR(x)
    class(iqr) <- "iqr"
    return(iqr)}
}






#summaries to add
#five num, PLUS MORE


