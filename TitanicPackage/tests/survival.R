library(TitanicPackage)
fixture <- data.frame(sex = c("female", "female", "female", "male"),
                      passengerClass = c("1st", "1st", "2nd", "2nd"),
                      survived = c("yes", "no", "yes", "no"))
stopifnot(isTRUE(all.equal(as.numeric(taux_survie_sex(fixture)), c(2/3, 0))))
stopifnot(isTRUE(all.equal(as.numeric(taux_survie_class(fixture)), c(0.5, 0.5))))
stopifnot(inherits(try(taux_survie(fixture, "missing"), silent = TRUE), "try-error"))
fixture$survived[1] <- NA
stopifnot(inherits(try(taux_survie_sex(fixture), silent = TRUE), "try-error"))
df <- carData::TitanicSurvival
for (variable in c("sex", "passengerClass")) {
  observed <- taux_survie(df, variable)
  expected <- vapply(names(observed), function(group) {
    selected <- df$survived[df[[variable]] == group]
    sum(selected == "yes") / length(selected)
  }, numeric(1))
  stopifnot(isTRUE(all.equal(as.numeric(observed), unname(expected))))
}
