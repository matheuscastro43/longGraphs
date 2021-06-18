#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @export
spaghetti_plot = function(dataf, id, x, y, group, xlab = NULL, ylab = NULL,
                          legend.title = NULL, legend.labels = NULL){
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  id <- as.character(substitute(id))
  groupp <- as.character(substitute(group))

  tibble(dataf) %>%
    ggplot() +
    aes_string(x = x, y = y) +
    geom_line(aes_string(group = id, color = groupp)) +
    geom_point(aes_string(group = id, color = groupp)) +
    geom_line(data = . %>% group_by_at(x) %>%
                summarise_at(.vars = y, .funs = mean),
              aes_string(y = y), size = 1) +
    theme_calc() +
    labs(x = if(is.null(xlab)){waiver()}else{xlab},
         y = if(is.null(ylab)){waiver()}else{ylab},
         color = if(is.null(legend.title)){waiver()}else{legend.title},
         tag = "By: longGraphs") +
    scale_color_discrete(
      labels = if(is.null(legend.labels)){waiver()}else{legend.labels}) +
    theme(plot.tag.position = "topright", plot.tag =
            element_text(size = 5, angle = 90))
}

