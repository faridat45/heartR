#' Create a Heart Disease Data Object
#'
#' `heart_data()` creates a structured object containing a dataset and the response
#' variable indicating heart disease. This object is used as input for modeling
#' and plotting functions.
#'
#' @param data A data.frame containing the heart disease dataset with predictors
#'   and the response variable.
#' @param response A character string specifying the column name of the response
#'   variable.
#'
#' @return An object of class `"heart_data"` containing:
#'   \itemize{
#'     \item `data`: the input dataset
#'     \item `response`: the name of the response variable
#'   }
#'
#' @examples
#' \dontrun{
#' heart <- heart_data(data = heart_disease_data, response = "Heart_Disease")
#' heart
#' }
#'
#' @export
heart_data <- function(data, response) {
  structure(
    list(
      data = data,
      response = response
    ),
    class = "heart_data"
  )
}

heart <- heart_data(data = heart_dat, response = "Heart_Disease")
#' Fit a Heart Disease Model
#'
#' `model()` fits a predictive model to a `heart_data` object. Users can choose
#' between logistic regression (for binary classification, given the fact that
#' some of are in 0s and 1s) and random forest.
#'
#' @param x A `heart_data` object containing the dataset and response variable.
#' @param method Character. The modeling method to use:
#'   \itemize{
#'     \item `"glm"`: Logistic regression
#'     \item `"rf"`: Random forest
#'   }
#'
#' @return An object of class `"heart_model"` containing:
#'   \itemize{
#'     \item `fit`: the fitted model object
#'     \item `method`: modeling method used
#'     \item `data`: input dataset
#'     \item `response`: response variable name
#'   }
#'
#' @examples
#' \dontrun{
#' heart <- heart_data(data = heart_disease_data, response = "Heart_Disease")
#'
#' # Logistic regression
#' heart_glm <- model(heart, method = "glm")
#'
#' # Random forest
#' heart_rf <- model(heart, method = "rf")
#' }
#'
#' @export
model <- function(x, method = c("glm", "rf")) {
  method <- match.arg(method)

  formula <- as.formula(paste(x$response, "~ ."))

  fit <- switch(
    method,
    glm = glm(formula, data = x$data, family = binomial),
    rf  = randomForest::randomForest(formula, data = x$data)
  )

  structure(
    list(
      fit = fit,
      method = method,
      data = x$data,
      response = x$response
    ),
    class = "heart_model"
  )
}

#' Heart Data Plotting Function
#' Aim: to Visualize the heart data and explore how predictors relate to heart disease.
#' `plot_heart` allows users to explore their heart disease dataset visually.
#' It provides multiple types of plots to investigate relationships between predictors
#' and the response variable (`Heart_Disease`)
#' Also able to explore relationship between predictors as well
#' @param heart A `heart_data` object created by `heart_data()` that contains the dataset and response variable.
#' @param type indicate which type of plot to generate. Options are:
#'   \describe{
#'     \item{"hist"}{Histogram with density overlay for a numeric predictor.}
#'     \item{"density"}{Density plot for a numeric predictor.}
#'     \item{"scatter"}{Scatter plot of a numeric predictor vs the response variable.}
#'     \item{"boxplot"}{Boxplot of a categorical predictor vs the response variable.}
#'     \item{"correlation"}{Correlation matrix heatmap for all numeric predictors.}
#'   }
#' @param var_x represent the character string that specify variable to plot on the x-axis.
#' is required for "hist", "density", "scatter", and "boxplot" types.
#' @param var_y Optional character string that specify variable to plot on the y-axis.
#' if not inputed, the response variable is used by default
#' @return A ggplot object showing the requested visualization.
#'
#' @details
#' This function is designed to help explore which predictors might impact heart disease.
#' - For numeric predictors, you can visualize distributions, density, and relationship with heart disease.
#' - For categorical predictors, you can see differences in heart disease rates across groups.
#' - The correlation matrix helps identify relationships between numeric predictors.
#' (1 = highest, 0 = lowest)
#' @examples
#' # Create heart_data object
#' heart <- heart_data(data = heart_disease_data, response = "Heart_Disease")
#'
#' # Histogram of BMI
#' plot_heart(heart, type = "hist", var_x = "BMI")
#'
#' # Density of Age
#' plot_heart(heart, type = "density", var_x = "Age")
#'
#' # Scatter plot Age vs Heart_Disease
#' plot_heart(heart, type = "scatter", var_x = "Age")
#'
#' # Boxplot Gender vs Heart_Disease
#' plot_heart(heart, type = "boxplot", var_x = "Gender")
#'
#' # Correlation matrix of all numeric predictors
#' plot_heart(heart, type = "correlation")
#'
#' @export


plot_heart <- function(heart, type = c("hist", "density", "scatter", "boxplot", "correlation"),
                       var_x = NULL, var_y = NULL) {

  type <- match.arg(type)
  data <- heart$data
  response <- heart$response

  # Default y variable
  var_y <- var_y %||% response

  # Require var_x for plots that need it
  if (type != "correlation" && is.null(var_x)) stop("Please provide 'var_x' for this plot type.")

  switch(type,

         hist = ggplot(data, aes(x = .data[[var_x]])) +
           geom_histogram(aes(y = ..density..), bins = 30, col = "skyblue") +
           geom_density(color = "black", size = 1) +
           labs(title = paste("Histogram & Density of", var_x)) +
           theme_minimal(),

         density = ggplot(data, aes(x = .data[[var_x]])) +
           geom_density(fill = "purple") +
           labs(title = paste("Density of", var_x), x = var_x, y = "Density") +
           theme_minimal(),

         scatter = {
           ggplot(data, aes(x = .data[[var_x]], y = as.numeric(.data[[var_y]]))) +
             geom_point(color = "darkblue") +
             labs(title = paste("Scatter:", var_x, "vs", var_y),
                  x = var_x,
                  y = var_y) +
             theme_minimal()
         },

         boxplot = ggplot(data, aes(x = .data[[var_x]], y = .data[[var_y]])) +
           geom_boxplot(fill = "skyblue") +
           labs(title = paste("Boxplot:", var_x, "vs", var_y)) +
           theme_minimal(),

         correlation = {
           num_data <- data[sapply(data, is.numeric)]
           cor_df <- as.data.frame(as.table(cor(num_data, use = "complete.obs")))
           ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
             geom_tile() +
             geom_text(aes(label = round(Freq, 2)), color = "white", size = 3) +
             scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0) +
             labs(title = "Correlation Matrix") +
             theme_minimal()
         }
  )
}

