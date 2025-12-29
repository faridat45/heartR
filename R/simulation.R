# dealing with 'no visible binding' note as recommended in lecture 11
utils::globalVariables(c("p_hat","heart_dat"))

#' Simulation
#'
#' This function generates reproducible synthetic datasets, suitable for binary
#' classification. The current version only works for datasets in which the response
#' variable has exactly two possible values.
#'
#' @param seed random seed for reproducibility
#' @param n number of rows in the synthetic datasets.Deaults to the number of rows in
#' \code{data}.
#' @param data dataset the user wishes to replicate. If \code{NULL}, a heart
#' disease dataset is used.
#' @param contVars character vector of continuous numeric variables in \code{data}.
#' If \code{NULL} then all numeric variables are used.
#' @param catVars character vector of categorical variables in \code{data}. If
#' \code{NULL} then all factor or categorical variables, excluding the outcome,
#' are used
#' @param outcome_model fitted binary logistic regression model. If \code{NULL}
#' a logistic regression model is fit using \code{glm()}
#' @param outcome name of the binary outcome variable in \code{data}
#' @param mu optional named vector of means used to simulate continuous variables.
#' @param sigma optional covariance matrix to simulate continuous variables.
#'
#' @return A synthetic dataset with continuous, categorical and outcome variables
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats glm cov predict binomial median IQR as.formula
#' @importFrom utils data
#' @importFrom dplyr select setdiff
#'
#' @export
#' @author Faridat Adeniji - <\email{faridaadeniji@@gmail.com}>
#' @examples
#' sim_data <- simulation(seed = 123)
#' sim_data2 <- simulation(seed = 23, n = 50)
#' sim_mtcars <- simulation(data = mtcars, outcome = "am")
simulation <- function(seed = 403, n = NULL,
                       data = NULL,
                       contVars = NULL,
                       catVars = NULL,
                       outcome_model = NULL,
                       outcome = "Heart_Disease",
                       mu = NULL, sigma = NULL){
  if(is.null(data)){
    data("heart_dat", package = "HeartR", envir = environment())
    data <- heart_dat
  }
  set.seed(seed)

  if(is.null(n)){
    n <- nrow(data)
  }
  if(is.null(contVars)){
    contVars <- names(data[sapply(data,is.numeric)])
  }

  bounds <-function(x,min,max){
    x[x<min] <- min
    x[x>max] <- max
    return(x)
  }

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
    sigma <- cov(data[, contVars])
  }else{
    if(!is.matrix(sigma)){
      stop("sigma should be a covariance matrix")
    }
  }

  # multivariate normal data
  contData <- MASS::mvrnorm(n, mu,sigma)
  contData <- as.data.frame(round(contData))

  for(x in contVars){
    contData[[x]] <- bounds(contData[[x]], bounds2[[x]]["min"], bounds2[[x]]["max"])
  }

  # automatically finding names of categorical variables
  if(is.null(catVars)){
    catVars <- names(data[sapply(data, function(x) is.factor(x) || is.character(x))])
    catVars <- setdiff(catVars, outcome) #remove response var in case it's in catVars
  }
  catData <- lapply(catVars, function(x){
    probs <- prop.table(table(data[[x]]))
    sample(names(probs), n, replace = TRUE, prob = probs)
  })
  catData <- as.data.frame(catData)
  names(catData) <- catVars



  #synthetic_data <- data.frame(contData,Gender,Smoking,Alcohol_Intake,
                               #Physical_Activity,Diet, Stress_Level,Hypertension,Diabetes,
                               #Hyperlipidemia,Family_History,Previous_Heart_Attack)
  if(!length(catData) == 0 && !length(contData) == 0)
  {
    synthetic_data <- cbind(contData, catData)
  }else if(length(catData) == 0 && !length(contData) == 0){
    synthetic_data <- contData

  }else if(!length(catData) == 0 && length(contData) == 0){
    synthetic_data <- catData
  }

  if(is.null(outcome_model)){
    if(length(unique(data[[outcome]])) != 2){
      stop("outcome_model currently only works for binary outcomes")
    }
    formula <- as.formula(paste(outcome,"~."))
    outcome_model <- glm(formula, family = binomial, data = data)
  }

  synthetic_data$p_hat <- predict(outcome_model, newdata = synthetic_data, type = "response")
  synthetic_data[[outcome]] <- ifelse(synthetic_data$p_hat >= 0.5, 1, 0)
  synthetic_data <- synthetic_data |>
  select(-p_hat)

  return(synthetic_data)

}
