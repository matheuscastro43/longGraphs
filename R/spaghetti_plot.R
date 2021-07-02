#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @export
spaghetti_plot <- function(dataf, id, x, y, group, funcs = NULL, xlab = NULL,
                           ylab = NULL, legend.title = NULL,
                           legend.labels = NULL){
  dataf %<>% as_tibble

  p <- dataf %>%
    ggplot +
    aes(x = {{x}}, y = {{y}}) +
    geom_line(aes(group = {{id}}, color = {{group}})) +
    geom_point(aes(group = {{id}}, color = {{group}})) +
    theme_calc() +
    labs(x = if(is.null(xlab)){waiver()}else{xlab},
         y = if(is.null(ylab)){waiver()}else{ylab},
         color = if(is.null(legend.title)){waiver()}else{legend.title}) +
    scale_color_discrete(
      labels = if(is.null(legend.labels)){waiver()}else{legend.labels}) +
    theme(legend.title = element_text(hjust = 0.5, size = 12))

  if(!is.null(funcs)){
    auxt <- dataf %>%
      group_by({{x}}) %>%
      summarise(across({{y}}, funcs)) %>%
      pivot_longer(cols = -{{x}})
    p +
      geom_line(data = auxt, aes(x = {{x}}, y = value, group = name))
  }else p
}
