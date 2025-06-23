#' Calculate Number of Fish by Commercial Grain Size and Quarter
#'
#' Estimates the number of anchovies caught per commercial grain size class and quarter,
#' based on landed weight and assumed units per kilogram. Optionally allows reassignment
#' of \code{"TALLA 1"} to another commercial size class to adjust for inconsistent pricing behavior.
#'
#' @param data A data frame with columns:
#'   \describe{
#'     \item{FECHA_VENTA}{Date of sale (format: YYYY-MM-DD)}
#'     \item{TALLA}{Commercial size class (e.g. "TALLA 1" to "TALLA 4")}
#'     \item{TOTAL_KILOS}{Landed weight in kilograms}
#'     \item{TOTAL_EUROS}{Total value in euros}
#'   }
#' @param reassign Logical. If \code{TRUE}, reassigns \code{"TALLA 1"} to another class.
#' @param talla1_to Character. New commercial size class to assign to \code{"TALLA 1"}.
#'
#' @return A data frame with estimated total kilograms, number of fish, and mean weight
#' (in grams) by \code{year}, \code{quarter}, and \code{Grain_class}.
#'
#' @importFrom dplyr mutate group_by summarise if_else case_when
#' @importFrom lubridate year quarter
#' @importFrom magrittr %>%
#'
#' @export
calc_grain_catch <- function(data, reassign = TRUE, talla1_to = "TALLA 2") {

  # Lookup tables
  talla_to_units <- c(
    "TALLA 1" = "<30",
    "TALLA 2" = "31-50",
    "TALLA 3" = "51-83",
    "TALLA 4" = "84-125"
  )

  units_to_avg <- c(
    "<30" = 30,
    "31-50" = (31 + 50) / 2,
    "51-83" = (51 + 83) / 2,
    "84-125" = (84 + 125) / 2
  )

  data <- dplyr::mutate(
    data,
    TALLA_mod = ifelse(reassign & TALLA == "TALLA 1", talla1_to, TALLA),
    Grain_class = talla_to_units[TALLA_mod],
    grain_avg = units_to_avg[Grain_class],
    price_kg = TOTAL_EUROS / TOTAL_KILOS,
    year = lubridate::year(FECHA_VENTA),
    quarter = lubridate::quarter(FECHA_VENTA),
    num_catch = TOTAL_KILOS * grain_avg
  )

  grain_catch <- data |>
    dplyr::group_by(year, quarter, Grain_class, grain_avg) |>
    dplyr::summarise(
      kg_total = round(sum(TOTAL_KILOS, na.rm = TRUE), 0),
      num_total = round(sum(num_catch, na.rm = TRUE), 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      avg_weight_g = round(1000 / grain_avg, 1)
    )

  return(grain_catch)
}
