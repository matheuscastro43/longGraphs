#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @export
spaghetti_plot = function(dataf, id, x, y, group, xlab = NULL, ylab = NULL,
                          legend.title = NULL, legend.labels = NULL){
  as_tibble(dataf) %>%
    ggplot() +
    aes(x = {{x}}, y = {{y}}) +
    geom_line(aes(group = {{id}}, color = {{group}})) +
    geom_point(aes(group = {{id}}, color = {{group}})) +
    geom_line(data = . %>% group_by({{x}}) %>% summarise(medias = mean({{y}})),
              aes(y = medias), size = 1) +
    theme_calc() +
    labs(x = if(is.null(xlab)){waiver()}else{xlab},
         y = if(is.null(ylab)){waiver()}else{ylab},
         color = if(is.null(legend.title)){waiver()}else{legend.title}) +
    scale_color_discrete(
      labels = if(is.null(legend.labels)){waiver()}else{legend.labels})
}
