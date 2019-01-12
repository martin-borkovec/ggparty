
# transforms recursive structure of object of type "party" to dataframe

get_plot_data <- function(party_object) {

  ids <- nodeids(party_object)

  plot_data <- data.frame(id = ids,
                          x = NA,
                          y = NA,
                          breaks = NA,
                          index = I(rep(list(NA), length(party_object))),
                          right = NA,
                          splitvar = NA,
                          level = NA,
                          kids = NA,
                          parent = NA)


  # first loop --------------------------------------------------------------
  # to get kids and parents

  for (i in ids) {
    party_split <- party_object[[i]]$node$split
    party_node <- party_object[[i]]$node


    plot_data[i, "kids"] <- length(kids_node(party_node))
    done_data <- plot_data[1:(i - 1), ]
    done_data$parent <- factor(done_data$parent, levels = paste(1:(i - 1)))

    plot_data[i, "parent"] <- ifelse(i == 1, NA, max(which(done_data$kids > table(done_data$parent))))
  }

  # second loop -------------------------------------------------------------
  # to get splitvar, index and breaks

  for (i in ids) {
    party_split <- party_object[[i]]$node$split
    party_node <- party_object[[i]]$node
    split_index <- party_split$index
    split_breaks <- party_split$breaks

    if (!is.null(party_split$varid)) {

      kids <- which(plot_data$parent == i)

      split_var <- names(party_object[[i]]$data)[party_split$varid]
      plot_data[i, "splitvar"] <- split_var

      # index
      if (!is.null(split_index) & is.null(split_breaks)){

        var_levels <- levels(party_object$data[,split_var])



        for (j in 1:length(split_index)) {
          kid <- kids[split_index[j]]
          if (is.na(plot_data$index[kid])) {
            plot_data[kid, "index"] <- var_levels[j]
          } else {
            plot_data[kid, "index"][[1]] <- list(c(plot_data[kid, "index"][[1]], var_levels[j]))
          }
        }}

      # breaks

      if (!is.null(split_breaks)) {
        if(is.null(split_index)) split_index <- 1:(length(split_breaks) + 1)
        for (j in 1:length(split_index)) {

          kid <- kids[split_index[j]]
          if (party_split$right == T) {

            if (j == 1) {
              plot_data[kid, "breaks"] <- paste0("(-inf, ", split_breaks[1], "]")
            } else if (j == length(split_index)) {
              plot_data[kid, "breaks"] <- paste0("(", split_breaks[j - 1], ", inf)")
            } else {
              plot_data[kid, "breaks"] <- paste0("(",split_breaks[j - 1],", ",
                                                 split_breaks[j], "]")
            }
          }
        }
      }
    }
  }


  # add levels and layout ---------------------------------------------------


  plot_data <- add_levels(plot_data, endnode_level = depth(party_object))
  plot_data <- add_layout(plot_data)
  return(plot_data)
}


# add_layout --------------------------------------------------------------


add_layout <- function(plot_data) {
  terminal_level <- max(plot_data$level)
  terminal_data <- plot_data[plot_data$level == terminal_level, ]
  inner_data <- plot_data[plot_data$level != terminal_level, ]

  for (i in 1:nrow(terminal_data)) {
    i_id <- terminal_data$id[i]
    plot_data[i_id, "y"] <- 0
    plot_data[i_id, "x"] <- (i * 2 - 1)  / (nrow(terminal_data) * 2)
  }

  for (i in 1:nrow(inner_data)) {
    i_level <- inner_data$level[i]
    i_id <- inner_data$id[i]
    plot_data[i_id, "y"] <- 1 - i_level / max(plot_data$level)
    parents <- i_id
    if (i_level != max(inner_data$level)) {
      for (j in (i_level + 1):(terminal_level - 1)) {
        parents <- c(parents, plot_data[plot_data$level  == j & plot_data$parent %in% parents, "id"])
      }
    }
    plot_data[i_id, "x"] <- mean(plot_data$x[plot_data$parent %in% parents & plot_data$kids == 0])
  }
  plot_data$x_parent <- c(plot_data$x[plot_data$parent])
  plot_data$y_parent <- c(plot_data$y[plot_data$parent])
  plot_data$x_edge <- (plot_data$x + plot_data$x_parent) / 2
  plot_data$y_edge <- (plot_data$y + plot_data$y_parent) / 2

  return(plot_data)
}


# add_levels() ---------------------------------------------------------

add_levels <- function(plot_data, endnode_level){
  #root level
  plot_data$level[1] <- 0
  # inner levels
  for (id in plot_data$id) {
    plot_data[!is.na(plot_data$parent) & plot_data$parent == id, "level"] <- plot_data[plot_data$id == id, "level"] + 1
  }
  # level endnodes
  plot_data$level[plot_data$kids == 0] <- endnode_level
  return(plot_data)
}



