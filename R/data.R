#' Heart Disease Dataset
#'
#' Synthetic heart disease detection dataset containing 50000 patients
#' (24890 male and 25110 female)
#'
#' @format A data frame with 50000 observations and 21 variables
#' \describe{
#'  \item{Age}{Age of the patient (numeric)}
#'  \item{Gender}{categorical variable with levels \code{female, male}}
#'  \item{Weight}{weight in kilograms (numeric)}
#'  \item{Height}{height, centimeters (numeric)}
#'  \item{BMI}{body mass index (numeric)}
#'  \item{Smoking}{categorical, levels = \code{Never, Current, Former}}
#'  \item{Alcohol_Intake}{categorical variable with levels \code{None, Low, Moderate, High}}
#'  \item{Physical_Activity}{categorical variable with levels \code{Sedentary, Active, Moderate}}
#'  \item{Diet}{categorical variable with levels \code{Unhealthy, Average, Healthy}}
#'  \item{Stress_Level}{categorical variable with levels \code{Low, Medium, High}}
#'  \item{Hypertension}{binary variable \code{0, 1}}
#'  \item{Diabetes}{binary variable \code{0, 1}}
#'  \item{Hyperlipidemia}{binary variable \code{0, 1}}
#'  \item{Family_History}{binary variable \code{0, 1}}
#'  \item{Previous_Heart_Attack}{binary variable, levels = \code{0, 1}}
#'  \item{Systolic_BP}{systolic blood pressure (numeric)}
#'  \item{Diastolic_BP}{diastolic blood pressure (numeric)}
#'  \item{Heart_Rate}{resting heart rate (numeric)}
#'  \item{Blood_Sugar_Fasting}{fasting blood sugar level (numeric)}
#'  \item{Cholesterol_Total}{total cholesterol level (numeric)}
#'  \item{Heart_Disease}{binary outcome variable: 0 = No disease, 1 = disease}
#' }
#'
#' @docType data
#' @keywords datasets
#' @usage data("heart_dat")
"heart_dat"
