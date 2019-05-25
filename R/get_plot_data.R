# transforms recursive structure of object of type "party" to dataframe
get_plot_data <- function(party_object, horizontal = FALSE, terminal_space = 0.2,
                          add_vars = NULL) {
  #browser()
  ids <- nodeids(party_object)
  plot_data <- data.frame(id = ids,
                          x = NA,
                          y = NA,
                          parent = NA,
                          birth_order = NA,
                          breaks_label = I(rep(list(NA), length(party_object))),
                          #index = I(rep(list(NA), length(party_object))),
                          info = NA,
                          info_list = I(rep(list(NA), length(party_object))),
                          splitvar = NA,
                          level = NA,
                          kids = NA,
                          nodesize = NA,
                          p.value = NA,
                          horizontal = FALSE
                          )
  plot_data <- add_kids_parents(party_object, plot_data)
  plot_data <- add_splitvar_breaks_index(party_object, plot_data)
  plot_data <- add_info(party_object, plot_data)
  plot_data <- add_levels(plot_data, endnode_level = depth(party_object))
  plot_data <- add_layout(plot_data, horizontal, terminal_space)
  plot_data <- add_data(party_object, plot_data)
  plot_data <- add_vars(party_object, plot_data, add_vars)
  plot_data$horizontal <- horizontal
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
    plot_data[i, "birth_order"] <- sum(plot_data$parent == plot_data$parent[i],
          na.rm = TRUE)
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
      if (!is.null(split_index) & is.null(split_breaks)) {
        var_levels <- levels(party_object$data[,split_var])
        # iterate through index
        for (j in 1:length(split_index)) {
          if (is.na(split_index[j])) next
          # get kid index is pointing to
          kid <- kids[split_index[j]]
          # if first index  for kid, just assign according factor level
          if (is.na(plot_data$breaks_label[kid])) {
            plot_data[kid, "breaks_label"] <- var_levels[j]
            # else add factor level to present level(s)
          } else {
            plot_data[kid, "breaks_label"][[1]] <- list(c(plot_data[kid, "breaks_label"][[1]],
                                                   var_levels[j]))
          }
        }
      }

      # check whether intervals of continuous variable defined by breaks
      if (!is.null(split_breaks)) {
        # if no index provided, intervals are supposed to be assigned
        # consecutively to kids. assign index accordingly.
        if (is.null(split_index)) split_index <- 1:(length(split_breaks) + 1)
        # iterate through index
        for (j in 1:length(split_index)) {
          kid <- kids[split_index[j]]
          # for first interval use -inf as lower bound
          if (j == 1) {
            # check whether more intervals lead to this kid. If so, don't use inequality signs
            if (split_index[j] %in% split_index[-j]) {
              split_interval <- paste0("(-Inf, ",
                                       split_breaks[j],
                                       ifelse(party_split$right == TRUE,
                                              "]",")"))
            } else {
              split_interval <- paste(ifelse(party_split$right == TRUE,
                                             "NA <= NA*","NA <  NA*"),
                                      #"\u2264","<"),
                                      split_breaks[1])
            }
            # for last interval use inf as upper bound
          } else if (j == length(split_index)) {
            # check whether more intervals lead to this kid. If so, don't use inequality signs
            if (split_index[j] %in% split_index[-j]) {
              split_interval <- paste0(ifelse(party_split$right == TRUE,
                                              "(","["),
                                       split_breaks[j - 1],
                                       ", Inf)")
            } else {
              split_interval <- paste(ifelse(party_split$right == TRUE,
                                             "NA >  NA*","NA >= NA*"),
                                      split_breaks[j - 1])
            }
            # else use break[j-1] for lower interval bound
          } else {
            split_interval <- paste0(ifelse(party_split$right == TRUE,
                                            "(","["),
                                     split_breaks[j - 1],", ",
                                     split_breaks[j],
                                     ifelse(party_split$right == TRUE,
                                            "]",")"))
          }

          if (is.na(plot_data$breaks_label[kid])) {
            plot_data[kid, "breaks_label"] <- split_interval
          }
          else {
            # plot_data[kid, "breaks_label"][[1]] <- list(c(plot_data[kid, "breaks_label"][[1]],
            #                                               split_interval))
            plot_data[kid, "breaks_label"][[1]] <- paste(plot_data[kid, "breaks_label"][[1]],
                                                          split_interval, sep = " | ")
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
    if (is.null(party_object[[i]]$node$info)) next
    if (!is.list(party_object[[i]]$node$info))
      plot_data[i, "info"] <- party_object[[i]]$node$info
    else {
      plot_data[i, "info_list"][[1]] <- list(party_object[[i]]$node$info)
      plot_data[i, "p.value"] <- ifelse(is.null(party_object[[i]]$node$info$p.value),
                                        NA,
                                        party_object[[i]]$node$info$p.value)
    }
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


add_layout <- function(plot_data, horizontal, terminal_space) {
  terminal_level <- max(plot_data$level)

  # assign coordinates to endnodes
  terminal_data <- plot_data[plot_data$level == terminal_level, ]
  for (i in 1:nrow(terminal_data)) {
    i_id <- terminal_data$id[i]
    plot_data[i_id, "y"] <- terminal_space
    # divide x axis up between all terminal nodes
    plot_data[i_id, "x"] <- (i * 2 - 1)  / (nrow(terminal_data) * 2)
  }

  # assign coordinates to remaining nodes
  inner_data <- plot_data[plot_data$level != terminal_level, ]
  for (i in 1:nrow(inner_data)) {
    i_level <- inner_data$level[i]
    i_id <- inner_data$id[i]
    # assign y based on level of node
    plot_data[i_id, "y"] <- 1 - i_level / max(plot_data$level) * (1 - terminal_space)
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

  # assign parent and edge coordinates
  plot_data$x_parent <- c(plot_data$x[plot_data$parent])
  plot_data$y_parent <- c(plot_data$y[plot_data$parent])

  return(plot_data)
}


# add_data() --------------------------------------------------------------

add_data <- function(party_object, plot_data) {

  #check for surv objects
  data <- expand_surv(party_object[[1]]$data)
  # create vector with all variables of tree data
  data_columns <- names(data)

  #check whether elements are present
  fitted_values <- !is.null(party_object$node$info$object$fitted.values)
  residuals <- !is.null(party_object$node$info$object$residuals)
  fitted_nodes <- !is.null(party_object$fitted$`(fitted)`)
  #check whether elements are present
  if (fitted_values) {
    data_columns <- c(data_columns, "fitted_values")
  }
  if (residuals) {
    data_columns <- c(data_columns, "residuals")
  }
  if (fitted_nodes) {
    data_columns <- c(data_columns, "fitted_nodes")
  }


  # initialize data.frame with columns of lists -----------------------------

  for (column in data_columns) {
    data_column <- paste0("nodedata_", column)
    plot_data[[data_column]] <- rep(list(NA), nrow(plot_data))
  }

  for (i in plot_data$id) {
    node_data <- expand_surv(party_object[[i]]$data)
    # add nodesize to plot_data
    plot_data[i, "nodesize"] <- nrow(node_data)

    # add fitted_values, residuals and p.vlaue if present ---------------------
    if (fitted_values) {
      node_data <- cbind(node_data,
                         "fitted_values" = party_object[[i]]$node$info$object$fitted.values)
    }

    if (residuals) {
      node_data <- cbind(node_data,
                         "residuals" = party_object[[i]]$node$info$object$residuals)
    }

    if (fitted_nodes) {
      node_data <- cbind(node_data,
                         "fitted_nodes" = party_object[[i]]$fitted$`(fitted)`)
    }

    for (column in data_columns) {
      data_column <- paste0("nodedata_", column)
      plot_data[i, data_column][[1]] <- list(node_data[[column]])
    }
  }
  return(plot_data)
}


# expand_surv() -----------------------------------------------------------
# takes dataframe and in case surv objects present, expands them and returns new
# dataframe which contains new columns
expand_surv <- function(data) {
  data_check <- data
  for (i in 1:ncol(data_check)) {
    if (identical(methods::is(data_check[[i]]), "Surv")) {
      data <- data[, !names(data) %in% names(data_check[i])]
      new_columns <- as.matrix(data_check[[i]])
      new_columns <- as.data.frame(new_columns)
      names(new_columns) <- paste0(names(data_check[i]),".", names(new_columns))
      data <- cbind(data, new_columns)
    }
  }
  data
}


# add_vars() --------------------------------------------------------------


add_vars <- function(party_object, data, add_vars) {

  for (i in seq_along(add_vars)) {
    for (j in seq_len(nrow(data))) {
    if (is.character(add_vars[[i]])) {
        new <- eval(parse(text = paste0("party_object[[", j, "]]", add_vars[[i]])))
        data[j, names(add_vars)[i]] <- ifelse(is.null(new), NA, new)
      }
    if (is.function(add_vars[[i]])) {
      new <- add_vars[[i]](data[j, ], party_object[j])
      if (is.null(new)) {
      if (substring(names(add_vars)[i], 1, 5) == "nodedata_")
        new <- list(rep(NA, data$nodesize))
      else
        new <- NA
      }
      data[j, names(add_vars)[i]][[1]] <- new
    }
  }}
  data
}

