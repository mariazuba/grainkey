#' Plot Grain Age Key
#'
#' Visualizes either the proportion-at-age or the mean weight-at-age by grain class, year, and quarter,
#' using the output of \code{\link{build_grain_key}}.
#'
#' @param grain_key A list returned by \code{build_grain_key()}, containing at least:
#'   \code{grain_key} (proportion-at-age) and \code{weight_key} (mean weight-at-age).
#' @param plot_type Character string indicating which type of plot to return. Options:
#'   \describe{
#'     \item{"percentage"}{Plot proportion-at-age (default)}
#'     \item{"mean_weight"}{Plot mean weight-at-age}
#'   }
#'
#' @return A \code{ggplot2} object showing grain-age distribution or mean weight.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_bw theme element_text scale_fill_brewer
#' @importFrom dplyr mutate starts_with
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' results <- build_grain_key(data_bio)
#' plot_grain_key(results, plot_type = "percentage")
#' plot_grain_key(results, plot_type = "mean_weight")
#' }
#' @export
plot_grain_key <- function(grain_key, plot_type = "percentage") {

  # Validate input list
  if (!("grain_key" %in% names(grain_key)) || !("weight_key" %in% names(grain_key))) {
    stop("Input must be a list from build_grain_key() with 'grain_key' and 'weight_key'.")
  }

  # Validate plot_type
  if (!plot_type %in% c("percentage", "mean_weight")) {
    stop("plot_type must be one of: 'percentage', 'mean_weight'.")
  }

  if (plot_type == "percentage") {
    plot_data <- tidyr::pivot_longer(
      grain_key$grain_key,
      cols = starts_with("Age-"),
      names_to = "age",
      names_prefix = "Age-",
      values_to = "proportion"
    ) |>
      dplyr::mutate(
        YearQuarter = paste0(year, "-Q", quarter),
        age = factor(age, levels = sort(unique(age)))
      )

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Grain_class, y = proportion, fill = age)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ YearQuarter, ncol = 4) +
      ggplot2::labs(
        x = "Grain class (fish/kg)",
        y = "Proportion at age",
        fill = "Age",
        title = "Grain age key: age distribution by grain class"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_fill_brewer(palette = "Set2")
  }

  if (plot_type == "mean_weight") {
    plot_data <- tidyr::pivot_longer(
      grain_key$weight_key,
      cols = starts_with("Age-"),
      names_to = "age",
      names_prefix = "Age-",
      values_to = "weight"
    ) |>
      dplyr::mutate(
        YearQuarter = paste0(year, "-Q", quarter),
        age = factor(age, levels = sort(unique(age)))
      )

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Grain_class, y = weight, fill = age)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ YearQuarter, ncol = 4) +
      ggplot2::labs(
        x = "Grain class (fish/kg)",
        y = "Mean individual weight (g)",
        fill = "Age",
        title = "Mean weight-at-age by grain class"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_fill_brewer(palette = "Set2")
  }

  return(p)
}
