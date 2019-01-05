
  # transforms recursive structure of object of type "party" to dataframe

get_plot_data <- function(party, i = 1, level = 0, plot_data = NULL) {
  plot_data <- data.frame(id = numeric(length(party)),
                          x = NA,
                          y = NA,
                          breaks = NA,
                          index = NA,
                          #operator = NA,
                          right = NA,
                          splitvar = NA,
                          level = 0,
                          kids = NA,
                          terminal = NA,
                          parent = NA)

recursive_helper(party, plot_data = plot_data)
}

recursive_helper <- function(party, i = 1, level = 0, plot_data = NULL) {

  if (i <= 0) return(plot_data)
  partynode <- party[[i]]$node
  partysplit <- partynode$split
  parent_id <- max(plot_data$id[plot_data$level == (level - 1)], 0, na.rm = T)
  current_kid <- get_done_kids(parent_id, plot_data) + 1

  # first time hitting the node
  if (plot_data$id[i] == 0) {
    plot_data[i, "id"] <- i
    plot_data[i, "level"] <- level
    plot_data[i, "splitvar"] <- ifelse(is.null(partysplit$varid),
                                       NA,
                                       names(party[[1]]$data)[partysplit$varid])
    plot_data[i, "kids"] <- length(kids_node(partynode))
    plot_data[i, "terminal"] <- ifelse(plot_data[i, "kids"] == 0, formatinfo_node(party[[i]]$node), NA)
    plot_data[i, "parent"] <- parent_id

    # if not root determine parent and it's split info
    if (parent_id > 0) {
    partysplit_parent <- party[[parent_id]]$node$split

    # store breaks of continous parent split variable
    if (is.null(partysplit_parent$breaks)) {
      plot_data[i, "breaks"] <- NA
      } else {
        if (partysplit_parent$right == T &
            current_kid == 1) {
          plot_data[i, "breaks"] <- paste0("(-inf, ", partysplit_parent$breaks[current_kid], "]")
        }
        if (partysplit_parent$right == T &
            current_kid == plot_data[parent_id, "kids"]) {
          plot_data[i, "breaks"] <- paste0("(", partysplit_parent$breaks[current_kid - 1], ", inf)")
        }
        if (partysplit_parent$right == T &
            current_kid != 1 &
            current_kid !=  plot_data[parent_id, "kids"]) {
          plot_data[i, "breaks"] <- paste0("(",partysplit_parent$breaks[current_kid - 1],", ",
                 partysplit_parent$breaks[current_kid], "]")
        }
      }

    # store breaks of categorical parent split variable
    if (is.null(partysplit_parent$index)) {
      plot_data[i, "index"] <- NA
    } else {
      levels <- levels(unlist(party$data[plot_data[parent_id, "splitvar"]]))
      plot_data[i, "index"] <- levels[partysplit_parent$index[current_kid]]
    }
    }
  }
# determine if and how to recursively call again --------------------------

  # if node is terminal, go back one step
  if (!is.na(plot_data[i, "terminal"])) {
    plot_data <- recursive_helper(party, i - 1, level = level, plot_data)
  }

  # if not all kids done, go to next kid
  done_kids <- get_done_kids(i, plot_data)
  if (done_kids != plot_data[i, "kids"]) {
    plot_data <- recursive_helper(party,
                                  i = max(plot_data$id) + 1,
                                  level = plot_data[i, "level"] + 1,
                                  plot_data)
  } else {
    # if all kids of node done, go back another step
    plot_data <- recursive_helper(party, i - 1, level = level, plot_data)
  }
  plot_data <- level_endnodes(plot_data)
  plot_data <- add_layout(plot_data)
  return(plot_data)
}


# get_done_kids() ---------------------------------------------------------

# check how man kids of node already done
get_done_kids <- function(i, plot_data = NULL) {
 sum(plot_data$parent == i, na.rm = TRUE)
}


# add_layout() ------------------------------------------------------------

add_layout <- function(plot_data) {
  for (i in 1:nrow(plot_data)) {
    i_level <- plot_data$level[i]
    plot_data[i, "y"] <- 1 - i_level / max(plot_data$level)
    width <- sum(plot_data$level == i_level)
    x_position <- which(i == which(plot_data$level == i_level))
    plot_data[i, "x"] <- x_position  / (width + 1)
  }
  plot_data[1, "x"] <- 0.5
  plot_data$x_parent <- c(NA, plot_data$x[plot_data$parent])
  plot_data$y_parent <- c(NA, plot_data$y[plot_data$parent])
  plot_data$x_edge <- (plot_data$x + plot_data$x_parent) / 2
  plot_data$y_edge <- (plot_data$y + plot_data$y_parent) / 2

  return(plot_data)
}

# level_endnodes() ------------------------------------------------------------
level_endnodes <- function(plot_data) {
  bottom_level <- max(plot_data$level)
  plot_data$level[!is.na(plot_data$terminal)] <- bottom_level
  return(plot_data)
}
