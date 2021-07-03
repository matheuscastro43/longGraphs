#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @importFrom tibble deframe
#' @importFrom rlang as_string
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @export
spaghetti_plot <- function(dataf, id, x, y, group = NULL, grid = NULL,
                           funcs = NULL, xlab = NULL, ylab = NULL,
                           legend_title = NULL, legend_labels = NULL) {
  dataf %<>% as_tibble

  p <- dataf %>%
    ggplot +
    aes(x = {{x}}, y = {{y}}) +
    geom_line(aes(group = {{id}}, color = {{group}})) +
    geom_point(aes(group = {{id}}, color = {{group}})) +
    facet_grid(grid) +
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
    theme(legend_title = element_text(hjust = 0.5, size = 12))

  if (!is.null(funcs)) {
    auxt <- dataf %>%
      group_by({{x}}) %>%
      summarise(across({{y}}, funcs)) %>%
      pivot_longer(cols = -{{x}})
    p +
      geom_line(data = auxt, aes(x = {{x}}, y = value, group = name))
  } else p
}
