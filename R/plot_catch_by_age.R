#' Plot Number of Fish Caught by Age and Grain Class
#'
#' Creates a bar plot of the number of anchovies caught by age and grain class,
#' grouped by year and quarter, using the output from \code{\link{calc_catch_by_age}}.
#'
#' @param catch_by_age A data frame containing columns:
#'   \itemize{
#'     \item \code{year}: Year of catch (numeric)
#'     \item \code{quarter}: Quarter of catch (numeric)
#'     \item \code{age}: Age of individuals (integer or factor)
#'     \item \code{num_at_age}: Estimated number of fish caught at age
#'     \item \code{Grain_class}: Commercial grain class
#'   }
#'
#' @return A \code{ggplot2} object: a facetted bar plot by year and quarter
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_bw theme element_text scale_fill_brewer
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
plot_catch_by_age <- function(catch_by_age) {

  catch_by_age <- dplyr::mutate(
    catch_by_age,
    age = factor(age, levels = sort(unique(age))),
    year_quarter = paste0(year, "-Q", quarter)
  )

  ggplot2::ggplot(catch_by_age, ggplot2::aes(x = age, y = num_at_age, fill = Grain_class)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~ year_quarter, ncol = 3) +
    ggplot2::labs(
      title = "Estimated number by age and quarter",
      x = "Age",
      y = "Number of fish",
      fill = "Grain class"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::scale_fill_brewer(palette = "Pastel1")
}
