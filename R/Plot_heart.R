#' Heart Data Plotting Function
#' Aim: to Visualize the heart data and explore how predictors relate to heart disease.
#' `plot_heart` allows users to explore their heart disease dataset visually.
#' It provides multiple types of plots to investigate relationships between predictors.
#' and the response variable (`Heart_Disease`).
#' Also able to explore relationship between predictors as well.
#' @param heart_dat is the dataset.
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
#' This function is designed to help explore which predictors have an impact heart disease.
#' - For numeric predictors, you can visualize distributions, density, and relationship with heart disease.
#' - For categorical predictors, you can see differences in heart disease rates across groups.
#' - The correlation matrix helps identify relationships between numeric predictors.
#' (1 = highest, 0 = lowest)
#'
#' @examples
#' data("heart_dat")
#' heart <- heart_dat
# Histogram
#'plot_heart(heart, "hist", var_x = "BMI")
# Density
#plot_heart(heart, "density", var_x = "Age")
# Scatter: predictor vs predictor
#'plot_heart(heart, "scatter", var_x = "Age", var_y = "BMI")
# Boxplot: categorical vs numeric
#'plot_heart(heart, "boxplot", var_x = "Age", var_y = "Heart_Disease")
# Correlation matrix
#'plot_heart(heart, "correlation")
#'
#'@importFrom tidyverse
#'@importFrom ("stats", "cor")
#'@importFrom ggplot2
#'@importFrom plotly
#'@export
#'
plot_heart <- function(data,
                       type = c("hist", "density", "scatter", "boxplot", "correlation"),
                       var_x = NULL,
                       var_y = NULL) {

  type <- match.arg(type)

  # Simple checks
  if (type != "correlation" && is.null(var_x)) {
    stop("Please provide 'var_x' for this plot type.")
  }

  if (type %in% c("scatter", "boxplot") && is.null(var_y)) {
    stop("Please provide 'var_y' for this plot type.")
  }

  switch(

    type,

    # Histogram
    hist =
      ggplot(data, aes_string(x = data[[var_x]])) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     fill = "skyblue",
                     color = "black") +
      geom_density(color = "red", linewidth = 1) +
      labs(title = paste("Distribution of", var_x),
           x = var_x,
           y = "Density") +
      theme_minimal(),

    # Density
    density =
      ggplot(data, aes_string(x = var_x)) +
      geom_density(fill = "purple", alpha = 0.4) +
      labs(title = paste("Density of", var_x),
           x = var_x,
           y = "Density") +
      theme_minimal(),

    # Scatter plot
    scatter =
      ggplot(data, aes_string(x = var_x, y = var_y)) +
      geom_point() +
      labs(title = paste(var_x, "vs", var_y),
           x = var_x,
           y = var_y) +
      theme_minimal(),

    # Boxplot
    boxplot =
      ggplot(data, aes_string(x = var_x, y = var_y)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste(var_y, "by", var_x),
           x = var_x,
           y = var_y) +
      theme_minimal(),

    # Correlation matrix
    correlation = {
      num_data <- data[sapply(data, is.numeric)] # convert the factors to numeric
      cor_df <- as.data.frame(as.table(cor(num_data)))

      ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = round(Freq, 2)), size = 3) +
        scale_fill_gradient2(low = "blue",
                             mid = "white",
                             high = "red",
                             midpoint = 0) +
        labs(title = "Correlation Matrix") +
        theme_minimal()
    }
  )
}





