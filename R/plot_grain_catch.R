#' Plot Number of Fish by Grain Class and Quarter
#'
#' Generates a bar plot of the estimated number of anchovies caught per grain class and quarter,
#' using the output from \code{\link{calc_grain_catch}}. Grain classes are automatically ordered
#' based on their labels (e.g., "31-50", "51-83").
#'
#' @param grain_data A data frame with columns:
#'   \itemize{
#'     \item \code{year}: Year of catch (numeric)
#'     \item \code{quarter}: Quarter of catch (numeric)
#'     \item \code{Grain_class}: Commercial grain class (character)
#'     \item \code{num_total}: Estimated total number of fish caught
#'   }
#'
#' @return A \code{ggplot2} bar plot (facetted by year-quarter)
#'
#' @importFrom dplyr distinct mutate pull
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_bw theme element_text scale_fill_brewer
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#'
#' @export
plot_grain_catch <- function(grain_data) {

  # Define levels for grain class based on input
  grain_levels <- grain_data %>%
    dplyr::distinct(Grain_class) %>%
    dplyr::pull(Grain_class) %>%
    stats::na.omit() %>%
    unique() %>%
    sort()

  grain_data <- dplyr::mutate(
    grain_data,
    grain_class = factor(Grain_class, levels = grain_levels),
    year_quarter = paste0(year, "-Q", quarter)
  )

  ggplot2::ggplot(grain_data, ggplot2::aes(x = grain_class, y = num_total, fill = grain_class)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~ year_quarter, ncol = 3) +
    ggplot2::labs(
      title = "Estimated number by grain class and quarter",
      x = "Grain class (units/kg)",
      y = "Total number of fish",
      fill = "Grain class"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_brewer(palette = "Set2")
}
