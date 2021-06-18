#' @import ggplot2
#' @export
profile = function(dataset, response, index, id, group = NA, legend = "group", nGroups = NULL, dolTheme = TRUE){
  response = as.character(substitute(response))
  index = as.character(substitute(index))
  id = as.character(substitute(id))
  group = as.character(substitute(group))
  thereGr = !is.na(group)

  aux = as.data.frame(dataset)
  names(aux)[names(aux) == response] = "response"
  names(aux)[names(aux) == index] = "index"
  names(aux)[names(aux) == id] = "id"
  if(thereGr){
    names(aux)[names(aux) == group] = "group"
    aux$group = as.factor(aux$group)
    if(!is.null(nGroups))
      levels(aux$group) = nGroups
  }
  else group = NULL

  p = ggplot(aux, aes(x = index, y = response, group = id, color = group)) +
    geom_line() +
    geom_point() +
    labs(color = legend) +
    ylab(response) +
    xlab(index)
  if(dolTheme){p = p + theme_bw()}

  return(p)
}
