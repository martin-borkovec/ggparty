# transforms recursive structure of object of type "party" to dataframe
get_plot_data <- function(party_object, horizontal = FALSE) {
  ids <- nodeids(party_object)
  plot_data <- data.frame(id = ids,
                          parent = NA,
                          breaks = NA,
                          index = I(rep(list(NA), length(party_object))),
                          info = I(rep(list(NA), length(party_object))),
                          splitvar = NA,
                          level = NA,
                          kids = NA,
                          x = NA,
                          y = NA)
  plot_data <- add_kids_parents(party_object, plot_data)
  plot_data <- add_splitvar_breaks_index(party_object, plot_data)
  plot_data <- add_info(party_object, plot_data)
  plot_data <- add_levels(plot_data, endnode_level = depth(party_object))
  plot_data <- add_layout(plot_data, horizontal)
  plot_data <- add_data(party_object, plot_data)
  return(plot_data)
}

# add_kids_parents() --------------------------------------------------------


add_kids_parents <- function(party_object, plot_data){

  for (i in plot_data$id) {
    party_split <- party_object[[i]]$node$split
    party_node <- party_object[[i]]$node

    plot_data[i, "kids"] <- length(kids_node(party_node))
    # get done iterations
    done_data <- plot_data[1:(i - 1), ]
    # already assigned parents
    done_data$parent <- factor(done_data$parent, levels = paste(1:(i - 1)))
    # assign as parent Node with highest ID which has more kids than times it is
    # assigned as parent
    plot_data[i, "parent"] <- ifelse(i == 1,
                                     NA,
                                     max(which(done_data$kids >
                                                 table(done_data$parent))))
  }
  return(plot_data)
}

# add_splitvar_breaks_index() ---------------------------------------------


add_splitvar_breaks_index <- function(party_object, plot_data) {
  for (i in plot_data$id) {
    party_split <- party_object[[i]]$node$split
    party_node <- party_object[[i]]$node
    split_index <- party_split$index
    split_breaks <- party_split$breaks
    # check if node has a splitvar
    if (!is.null(party_split$varid)) {
      kids <- which(plot_data$parent == i)
      split_var <- names(party_object[[i]]$data)[party_split$varid]
      plot_data[i, "splitvar"] <- split_var

      # index
      # if only index provided, splitvar categorical. assign children according
      # to factor levels
      if (!is.null(split_index) & is.null(split_breaks)){
        var_levels <- levels(party_object$data[,split_var])
        # iterate through index
        for (j in 1:length(split_index)) {
          # get kid index is pointing to
          kid <- kids[split_index[j]]
          # if first index  for kid, just assign according factor level
          if (is.na(plot_data$index[kid])) {
            plot_data[kid, "index"] <- var_levels[j]
            # else add factor level to present level(s)
          } else {
            plot_data[kid, "index"][[1]] <- list(c(plot_data[kid, "index"][[1]],
                                                   var_levels[j]))
          }
        }
      }

      # check whether intervals of continuous variable defined by breaks
      if (!is.null(split_breaks)) {
        # if no index provided, intervals are supposed to be assigned
        # consecutively to kids. assign index accordingly.
        if(is.null(split_index)) split_index <- 1:(length(split_breaks) + 1)
        # iterate through index
        for (j in 1:length(split_index)) {
          kid <- kids[split_index[j]]
          # for first interval use -inf as lower bound
          if (j == 1) {
            plot_data[kid, "index"] <- paste(ifelse(party_split$right == T,
                                                     "\u2264","<"),
                                              split_breaks[1])
            # for last interval use inf as upper bound
          } else if (j == length(split_index)) {
            plot_data[kid, "index"] <- paste(ifelse(party_split$right == T,
                                                     ">","<"),
                                              split_breaks[j - 1])
            # else use break[j-1] for lower interval bound
          } else {
            plot_data[kid, "index"] <- paste0(ifelse(party_split$right == T,
                                                      "(","["),
                                               split_breaks[j - 1],", ",
                                               split_breaks[j],
                                               ifelse(party_split$right == T,
                                                      "]",")"))
          }
        }
      }
    }
  }
  return(plot_data)
}


# add_info() ----------------------------------------------------------------


add_info <- function(party_object, plot_data) {
  for (i in plot_data$id) {
    plot_data[i, "info"][[1]] <- list(party_object[[i]]$node$info)
  }
  return(plot_data)
}

# add_levels() ---------------------------------------------------------


add_levels <- function(plot_data, endnode_level) {
  #root level
  plot_data$level[1] <- 0
  # inner levels
  # assign levels as level of parent + 1
  for (id in plot_data$id) {
    plot_data[!is.na(plot_data$parent) & plot_data$parent == id, "level"] <-
      plot_data[plot_data$id == id, "level"] + 1
  }
  # level endnodes
  plot_data$level[plot_data$kids == 0] <- endnode_level
  return(plot_data)
}


# add_layout() --------------------------------------------------------------
# adds coordinates for nodes, their parents and the joining edges' labels


add_layout <- function(plot_data, horizontal) {
  terminal_level <- max(plot_data$level)

  # assign coordinates to endnodes
  terminal_data <- plot_data[plot_data$level == terminal_level, ]
  for (i in 1:nrow(terminal_data)) {
    i_id <- terminal_data$id[i]
    plot_data[i_id, "y"] <- 0
    # divide x axis up between all terminal nodes
    plot_data[i_id, "x"] <- (i * 2 - 1)  / (nrow(terminal_data) * 2)
  }

  # assign coordinates to remaining nodes
  inner_data <- plot_data[plot_data$level != terminal_level, ]
  for (i in 1:nrow(inner_data)) {
    i_level <- inner_data$level[i]
    i_id <- inner_data$id[i]
    # assign y based on level of node
    plot_data[i_id, "y"] <- 1 - i_level / max(plot_data$level)
    ## get all terminal nodes descended from node i
    # iteratively identify all descendents
    parents <- i_id
    if (i_level != max(inner_data$level)) {
      for (j in (i_level + 1):(terminal_level - 1)) {
        parents <- c(parents, plot_data[plot_data$level  == j &
                                          plot_data$parent %in% parents, "id"])
      }
    }
    # assign x coordinate based on mean x of terminal kids
    plot_data[i_id, "x"] <- mean(plot_data$x[plot_data$parent %in%
                                               parents & plot_data$kids == 0])
  }

  # flip coordinates if layout should be horizontal
  if (horizontal == TRUE) {
    tmp <- plot_data$x
    plot_data$x <- 1 - plot_data$y
    plot_data$y <- tmp
  }

  # assigna parent and edge coordinates
  plot_data$x_parent <- c(plot_data$x[plot_data$parent])
  plot_data$y_parent <- c(plot_data$y[plot_data$parent])

  return(plot_data)
}


# add_data() --------------------------------------------------------------

# add_data <- function(party_object, plot_data) {
#          node_data <- do.call(rbind,
#                               lapply(plot_data$id,
#                                      function(i) {
#                                        cbind(id = i, party_object[[i]]$data)
#                                        }))
#          names(node_data)[-1] <- paste0("data_", names(node_data))[-1]
#          plot_data <- inner_join(plot_data, node_data, by = "id")
#
#   return(plot_data)
# }

add_data <- function(party_object, plot_data) {
  data_columns <- names(party_object[[1]]$data)
  for (column in data_columns) {
    data_column <- paste0("data_", column)
    plot_data[[data_column]] <- rep(list(NA), nrow(plot_data))
    #plot_data[[column]] <- rep(list(NA), nrow(plot_data))
  }

  for (i in plot_data$id) {
    for (column in data_columns) {
      data_column <- paste0("data_", column)
      plot_data[i, data_column][[1]] <- list(party_object[[i]]$data[column])
      #plot_data[i,column][[1]] <- list(party_object[[i]]$data[column])
    }
  }
  return(plot_data)
}
