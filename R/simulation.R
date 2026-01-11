# dealing with 'no visible binding' note as recommended in lecture 11
utils::globalVariables(c("p_hat","heart_dat"))

#' simulation
#'
#' This function generates reproducible synthetic datasets, suitable for binary
#' classification. The current version only works for datasets in which the
#' response variable has exactly two possible values.
#'
#' @param seed random seed for reproducibility
#' @param n number of rows in the synthetic datasets. Defaults to the number of
#' rows in \code{data}.
#' @param data dataset the user wishes to replicate. If \code{NULL}, heart
#' disease dataset is used.
#' @param contVars character vector of continuous numeric variables in
#' \code{data}.
#' If \code{NULL} then all numeric variables are used.
#' @param catVars character vector of categorical variables in \code{data}. If
#' \code{NULL} then all factor/ categorical variables, excluding the outcome,
#' are used
#' @param outcome name of the binary outcome variable in \code{data}
#' @param thres numeric threshold between 0 and 1 used to convert predicted
#' probabilities into binary outcomes. Default set to 0.5.
#' @param mu optional named vector of means used to simulate continuous
#' variables
#' @param sigma optional covariance matrix to simulate continuous variables.
#'
#' @return A synthetic dataset with continuous, categorical and outcome
#' variables
#'
#' @importFrom MASS "mvrnorm"
#' @importFrom stats "glm" "cov" "predict" "binomial" "as.formula"
#' @importFrom utils "data"
#' @importFrom dplyr "select" "setdiff" "where"
#'
#' @export
#' @author Faridat Adeniji - <\email{faridaadeniji@@gmail.com}>
#'
#' @note
#'
#' Datasets like \code{mtcars} contains no categorical variables. However,
#' variables (e.g. \code{cyl}, \code{gear}) may be considered to be of that
#' nature. Ensure variables in any dataset is of the correct type before passing
#' it to this function.
#'
#' @examples
#' \dontrun{
#'   sim_mtcars <- simulation(data = mtcars, outcome = "am")
#' }
#'
#' sim_data <- simulation(seed = 123)
#'
#' sim_data2 <- simulation(seed = 23, n = 50)
#' sim_data2
#'
#' sim_data3 <- simulation(
#'   data = heart_dat,
#'   n = 100,
#'   mu = c(age = 55, cholesterol = 220, restingBP = 130)
#' )
#'
#' sim_dat4 <- simulation(
#'   data = heart_dat,
#'   n = 80,
#'   contVars = c("age", "cholesterol","restingBP"),
#'   catVars = c("sex","chestPT","exerAngina")
#' )
#'
#' sim_dat5 <- simulation(
#'   data = heart_dat,
#'   n = 100,
#'   contVars = c("age", "cholesterol","restingBP"),
#'   catVars = c("sex","chestPT","exerAngina"),
#'   sigma = cov(heart_dat[, c("age", "cholesterol","restingBP")])
#'  )
#'
simulation <- function(seed = 403, n = NULL,
                       data = NULL,
                       contVars = NULL,
                       catVars = NULL,
                       outcome = "target",
                       thres = 0.5,
                       mu = NULL, sigma = NULL){

  if(is.null(data)){
    utils::data("heart_dat", package = "HeartR")
    data <- heart_dat
  }
  set.seed(seed)
  if(!is.numeric(thres)||thres <=0||thres>=1){
    stop("thres must be a single number between 0 and 1 (exclusive)")
  }

  if(is.null(n)){
    n <- nrow(data)
  }
  # automatically select numeric variables if not specified
  if(is.null(contVars)){
    contVars <- data |>
      dplyr::select(dplyr::where(is.numeric))|>
      names()
  }
  contVars <- dplyr::setdiff(contVars, outcome)
  # making sure values stay within min/max
  bounds <-function(x,min,max){
    x[x<min] <- min
    x[x>max] <- max
    return(x)
  }

  # for later bounding
  bounds2 <- lapply(contVars, function(x){
    c(
      min = min(data[[x]], na.rm = TRUE),
      max = max(data[[x]], na.rm = TRUE)
    )
  })
  names(bounds2) <- contVars


  defaultMU <- colMeans(data[, contVars])

  if(!is.null(mu)){
    if(is.null(names(mu))){
      stop("mu must be a named vector")
    }
    defaultMU[names(mu)] <- mu
  }
  mu <- defaultMU

  # covariance matrix
  if(is.null(sigma)){
    sigma <- stats::cov(data[, contVars])
  }else{
    if(!is.matrix(sigma)){
      stop("sigma should be a covariance matrix")
    }
  }

  # continuous variables using multivariate normal distribution
  contData <- MASS::mvrnorm(n, mu,sigma)
  contData <- as.data.frame(round(contData))

  # applying bounds
  for(x in contVars){
    contData[[x]] <- bounds(contData[[x]],
                            bounds2[[x]]["min"],
                            bounds2[[x]]["max"])
  }

  # automatically finding categorical variables
  if(is.null(catVars)){
    catVars <- data |>
      dplyr::select(where(\(x) is.factor(x) || is.character(x)))|>
      names()
    #remove outcome var in case it's in catVars
    catVars <- dplyr::setdiff(catVars, outcome)
  }

  catData <- lapply(catVars, function(x){
    probs <- prop.table(table(data[[x]]))
    sampledDat <- sample(names(probs), n, replace = TRUE, prob = probs)
    factor(sampledDat, levels = levels(data[[x]]))
  })
  catData <- as.data.frame(catData)
  names(catData) <- catVars

  if(length(catData) != 0 && length(contData) != 0)
  {
    synthetic_data <- cbind(contData, catData)
  }else if(length(catData) == 0 && length(contData) != 0){
    synthetic_data <- contData

  }else if(length(catData) != 0 && length(contData) == 0){
    synthetic_data <- catData
  }

  predictors <- c(contVars,catVars)
  if(length(unique(data[[outcome]])) != 2){
    stop("outcome model currently only works for binary outcomes")
  }

  if(!is.factor(data[[outcome]])){
    data[[outcome]] <- as.factor(data[[outcome]])
  }
  formula <-stats::as.formula(paste(outcome,"~",paste(predictors,collapse="+")))
  outcome_model <- stats::glm(formula, family = binomial, data = data)

  outcomeLevels <- levels(data[[outcome]])

  synthetic_data$p_hat <- stats::predict(outcome_model,
                                         newdata = synthetic_data,
                                         type = "response")
  synthetic_data[[outcome]] <- factor(ifelse(synthetic_data$p_hat >= thres,
                                             outcomeLevels[2],
                                             outcomeLevels[1]))
  synthetic_data <- synthetic_data |>
    dplyr::select(-p_hat)

  return(synthetic_data)

}
