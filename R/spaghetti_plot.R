#' Tidy Spaghetti Plot.
#'
#' @description Function to make a Spaghetti Plot using Grammar of Graphics.
#'
#' @param dataf A dataframe or tibble.
#' @param id Variable with the experimental unit index.
#' @param x Variable to \code{x} axis.
#' @param y Variable to \code{y} axis (response variable).
#' @param group Grouping variable (if exists).
#' @param wrap Formula to separate plot (facet).
#' @param mfrow Number of rows and columns of facets.
#' @param funcs Functions to be added to plot.
#' @param xlab String with the label of \code{x} axis.
#' @param ylab String with the label of \code{y} axis.
#' @param legend_title String with the legend title.
#' @param legend_labels String with the legend labels.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object with the plot.
#'
#' @author Matheus Castro
#'     \href{mailto::mtcastro43@@gmail.com}{mtcastro43@@gmail.com}
#'     (\href{https://orcid.org/0000-0001-7879-7089}{ORCID}).
#'
#' @examples
#' spaghetti_plot(PotRoy, Individual, Age, Distance, Sex, xlab = "Age (years)",
#'                ylab = "Distance (cm)", legend_title = "Sex",
#'                legend_labels = c("Female", "Male"), wrap = ~Sex,
#'                funcs = mean)
#'
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#'
#' @export
#'
spaghetti_plot <- function(dataf, id, x, y, group = NULL, wrap = NULL,
                           mfrow = NULL, funcs = NULL, xlab = NULL, ylab = NULL,
                           legend_title = NULL, legend_labels = NULL) {
  dataf %<>% as_tibble

  p <- dataf %>%
    ggplot +
    aes(x = {{x}}, y = {{y}}) +
    geom_line(aes(group = {{id}}, color = {{group}})) +
    geom_point(aes(group = {{id}}, color = {{group}})) +
    facet_wrap(wrap, nrow = mfrow[1], ncol = mfrow[2]) +
    theme_calc() +
    labs(x = if (is.null(xlab)) {
      waiver()
    } else {
      xlab
    },
    y = if (is.null(ylab)) {
      waiver()
    } else {
      ylab
    },
    color = if (is.null(legend_title)) {
      waiver()
    } else {
      legend_title
    }) +
    scale_color_discrete(
      labels = if (is.null(legend_labels)) {
        waiver()
      } else {
        legend_labels
      }) +
    theme(legend.title = element_text(hjust = 0.5, size = 12))

  if (!is.null(funcs)) {
    auxt <- dataf %>%
      group_by({{x}}) %>%
      summarise(across({{y}}, funcs)) %>%
      pivot_longer(cols = -{{x}})
    p +
      geom_line(data = auxt, aes(x = {{x}}, y = !!sym("value"),
                                 group = !!sym("name")))
  } else p
}
