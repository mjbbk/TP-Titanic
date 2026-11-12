#' Calculate Titanic survival rates
#'
#' Calculate survival proportions from passenger-level TitanicSurvival data.
#' @param data A data frame containing survived and the grouping column.
#'   survived must contain only "no" and "yes", with no missing values.
#' @param variable Either "sex" or "passengerClass".
#' @return A named numeric array of survival proportions between zero and one.
#' @export
#' @examples
#' passengers <- data.frame(sex = c("female", "male"),
#'                          survived = c("yes", "no"))
#' taux_survie(passengers, "sex")
taux_survie <- function(data, variable) {
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (length(variable) != 1L || !variable %in% c("sex", "passengerClass"))
    stop("variable must be sex or passengerClass")
  if (!all(c("survived", variable) %in% names(data)))
    stop("data must contain survived and the grouping column")
  if (anyNA(data$survived) || !all(data$survived %in% c("no", "yes")))
    stop("survived must contain only no and yes, with no missing values")
  if (anyNA(data[[variable]])) stop("the grouping column must not contain missing values")
  tapply(data$survived == "yes", data[[variable]], mean)
}

#' Calculate survival rates by sex
#' @inheritParams taux_survie
#' @return A named numeric array of survival proportions by sex.
#' @export
taux_survie_sex <- function(data) taux_survie(data, "sex")

#' Calculate survival rates by passenger class
#' @inheritParams taux_survie
#' @return A named numeric array of survival proportions by passenger class.
#' @export
taux_survie_class <- function(data) taux_survie(data, "passengerClass")
