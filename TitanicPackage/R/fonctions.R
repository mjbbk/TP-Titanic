#' Calculate Titanic survival rates
#'
#' Calculates survival percentages by sex, class, or age.
#'
#' @param variable Name of the variable to analyze.
#' @return Survival percentages for each category.
#' @export

taux_survie <- function(variable) {
  data <- as.data.frame(Titanic)
  tableau <- xtabs(Freq ~ data[[variable]] + Survived, data = data)
  round(prop.table(tableau, 1)[, "Yes"] * 100, 2)
}