#' Calculate Number of Fish Caught by Age
#'
#' Estimates the number of fish caught by age class using a grain–age key
#' (either in wide or long format) and a table of total numbers caught per grain class.
#'
#' @param grain_key A data frame with either wide-format columns (\code{Age-0}, \code{Age-1}, ...)
#' or long-format columns (\code{age}, \code{proportion}).
#' @param grain_catch A data frame output from \code{\link{calc_grain_catch}}, containing total number of fish
#' by \code{year}, \code{quarter}, and \code{Grain_class}.
#'
#' @return A data frame with number of fish caught by age, grain class, quarter, and year.
#'
#' @importFrom dplyr mutate left_join select
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#' @export
calc_catch_by_age <- function(grain_key, grain_catch) {

  # Reshape from wide to long if needed
  if (any(grepl("^Age-", names(grain_key)))) {
    grain_key <- tidyr::pivot_longer(
      grain_key,
      cols = starts_with("Age-"),
      names_to = "age",
      names_prefix = "Age-",
      values_to = "proportion"
    ) |>
      dplyr::mutate(age = as.character(age))
  }

  # Join and compute catch at age
  catch_by_age <- grain_key |>
    dplyr::left_join(grain_catch, by = c("year", "quarter", "Grain_class")) |>
    dplyr::mutate(num_at_age = round(num_total * proportion, 0)) |>
    dplyr::select(year, quarter, Grain_class, age, num_at_age)

  return(catch_by_age)
}
