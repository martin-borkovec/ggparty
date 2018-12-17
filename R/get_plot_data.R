# transforms recursive structure of object of type "party" to dataframe

get_plot_data <- function(party, i = 1, level = 0, plot_data = NULL){
  plot_data <- data.frame(id = numeric(length(party)),
                          x = NA,
                          y = NA,
                          type = NA,
                          breaks = NA,
                          splitvar = NA,
                          level = NA,
                          kids = NA,
                          terminal = NA,
                          parent = NA)


recursive_helper(party, plot_data = plot_data)

}

recursive_helper <- function(party, i = 1, level = 0, plot_data = NULL){

  if (i <= 0) return(plot_data)
  partynode <- party[[i]]$node

  # first time hitting the node
  if (plot_data$id[i] == 0) {
    plot_data[i, "id"] <- i
    plot_data[i, "level"] <- level
    plot_data[i, "kids"] <- length(kids_node(partynode))
    plot_data[i, "terminal"] <- ifelse(plot_data[i, "kids"] == 0, TRUE, FALSE)
    plot_data[i, "parent"] <- max(plot_data$id[plot_data$level == (level - 1)], 0, na.rm = T)
  }

  # if node is terminal, go back one step
  if (plot_data[i, "terminal"] == TRUE){
    plot_data <- recursive_helper(party, i - 1, level = level, plot_data)
  }

  # if not all kids done, go to next kid
  done_kids <- done_kids(i, plot_data)
  if (done_kids != plot_data[i, "kids"]){
    plot_data <- recursive_helper(party,
                                  i = max(plot_data$id) + 1,
                                  level = plot_data[i, "level"] + 1,
                                  plot_data)
  } else {
    # if all kids of node done, go back another step
    plot_data <- recursive_helper(party, i - 1, level = level, plot_data)
  }

  return(plot_data)
}

done_kids <- function(i, plot_data = NULL){
 sum(plot_data$parent == i, na.rm = TRUE)
}


