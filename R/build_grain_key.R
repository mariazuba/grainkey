#' Construct Grain–Age Key from Individual Biological Observations
#'
#' Builds a grain–age key based on individual biological records, specifically total weight (in grams)
#' and age (in years). Samples are classified into commercial grain size classes (approximated by fish/kg),
#' and the function estimates the proportion-at-age and mean weight-at-age within each grain class, by year and quarter.
#'
#' The method uses the inverse of the average weight to estimate the number of individuals per kilogram ("Grain"),
#' which is then assigned to predefined grain size classes. For each class, it computes:
#' \itemize{
#'   \item Proportion of individuals per age group (\code{grain_key})
#'   \item Mean individual weight per age group (\code{weight_key})
#' }
#'
#' @param data_biological A data frame with individual-level biological data, including:
#'   \describe{
#'     \item{operation_date}{Sampling date (format: YYYY-MM-DD)}
#'     \item{edad}{Age of the individual (integer; typically 0–3)}
#'     \item{ptot_g}{Total individual weight in grams (numeric)}
#'   }
#'
#' @return A list with:
#'   \describe{
#'     \item{grain_key}{Proportion-at-age by grain class, year, and quarter}
#'     \item{weight_key}{Mean weight-at-age by grain class, year, and quarter}
#'     \item{long_data}{Merged long-format data frame with intermediate variables}
#'     \item{data}{Processed input data}
#'     \item{N_by_age}{Number of individuals by age}
#'     \item{W_by_age}{Mean weight by age}
#'     \item{grain_data}{Grain values and classes}
#'     \item{pct_by_age}{Proportions at age}
#'     \item{weighted_pct}{Weighted proportions}
#'     \item{long_pct}{Long-format proportion data}
#'     \item{long_weighted}{Long-format weighted proportion data}
#'     \item{long_n}{Long-format number at age}
#'     \item{long_weight}{Long-format mean weight at age}
#'     \item{merged}{Merged long-format output}
#'   }
#'
#' @importFrom dplyr distinct mutate count transmute group_by summarise left_join pull across
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom lubridate year month quarter
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#'
#' @export
build_grain_key <- function(data_biological) {

  ages <- data_biological |>
    dplyr::distinct(edad) |>
    dplyr::pull(edad) |>
    as.character() |>
    sort()

  data <- data_biological |>
    dplyr::mutate(
      operation_date = as.Date(operation_date),
      year = lubridate::year(operation_date),
      month = lubridate::month(operation_date),
      quarter = lubridate::quarter(operation_date),
      edad = as.character(edad)
    )

  N_by_age <- data |>
    dplyr::count(year, quarter, month, operation_date, edad) |>
    tidyr::pivot_wider(names_from = edad, values_from = n, values_fill = 0) |>
    dplyr::mutate(N_Total = rowSums(dplyr::across(all_of(ages))))

  W_by_age <- data |>
    dplyr::group_by(year, quarter, month, operation_date, edad) |>
    dplyr::summarise(ptot_g_mean = mean(ptot_g, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = edad, values_from = ptot_g_mean) |>
    dplyr::mutate(W_total = rowMeans(dplyr::across(all_of(ages)), na.rm = TRUE))

  grain_data <- W_by_age |>
    dplyr::transmute(
      year, quarter, month, operation_date,
      Grain = 1000 / W_total,
      Grain_class = dplyr::case_when(
        Grain <= 30 ~ "<=30",
        Grain <= 50 ~ "31-50",
        Grain <= 83 ~ "51-83",
        TRUE ~ "84-125"
      )
    )

  pct_by_age <- N_by_age |>
    dplyr::mutate(dplyr::across(all_of(ages), ~ . / N_Total, .names = "Pct_age_{.col}")) |>
    dplyr::left_join(grain_data, by = c("year", "quarter", "month", "operation_date"))

  weighted_pct <- pct_by_age |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("Pct_age_"),
      ~ . * Grain,
      .names = "W_age_{substr(.col, 9, 9)}"
    ))

  long_pct <- pct_by_age |>
    tidyr::pivot_longer(dplyr::starts_with("Pct_age_"), names_to = "edad", names_prefix = "Pct_age_", values_to = "percentage")

  long_weighted <- weighted_pct |>
    tidyr::pivot_longer(dplyr::starts_with("W_age_"), names_to = "edad", names_prefix = "W_age_", values_to = "weighting")

  long_n <- N_by_age |>
    dplyr::left_join(grain_data, by = c("year", "quarter", "month", "operation_date")) |>
    tidyr::pivot_longer(all_of(ages), names_to = "edad", values_to = "number")

  merged <- list(long_weighted, long_pct, long_n) |>
    purrr::reduce(dplyr::left_join, by = c("year", "quarter", "month", "operation_date", "Grain_class", "Grain", "edad"))

  long_weight <- W_by_age |>
    tidyr::pivot_longer(all_of(ages), names_to = "edad", values_to = "weight")

  full_long <- dplyr::left_join(
    long_weight,
    merged,
    by = c("year", "quarter", "month", "operation_date", "edad")
  )

  weighted_summary <- function(data, var, weight, prefix) {
    data |>
      dplyr::group_by(year, quarter, Grain_class, edad) |>
      dplyr::summarise(
        mean_value = sum(.data[[var]] * .data[[weight]], na.rm = TRUE) /
          sum(.data[[weight]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(names_from = edad, values_from = mean_value, names_prefix = prefix)
  }

  weight_key <- weighted_summary(full_long, "weight", "weighting", "Age-")
  grain_key <- weighted_summary(full_long, "percentage", "Grain", "Age-")

  list(
    grain_key = grain_key,
    weight_key = weight_key,
    long_data = full_long,
    data = data,
    N_by_age = N_by_age,
    W_by_age = W_by_age,
    grain_data = grain_data,
    pct_by_age = pct_by_age,
    weighted_pct = weighted_pct,
    long_pct = long_pct,
    long_weighted = long_weighted,
    long_n = long_n,
    long_weight = long_weight,
    merged = merged
  )
}
