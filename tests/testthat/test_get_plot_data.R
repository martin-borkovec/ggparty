context("get_plot_data subfunctions on different partykit objects")

test_kids <- function(party_object) {
  test_that("valid number of children", {
    for (i in get_plot_data(party_object)$id) {
      expect_equal(get_plot_data(party_object)[i, ]$kids,
                   length(party_object[i]$node))
    }
  })
}

test_kids(py)
test_kids(t2)
test_kids(ct)
test_kids(ct2)
test_kids(tr_tree)

test_parents_id <- function(party_object) {
  test_that("valid id of parent", {
    expect_success(expect_output(get_plot_data(party_object)[1, ]$parent, NA))
    for (i in 2:length(get_plot_data(party_object)$id)) {
      done_data <- get_plot_data(party_object)[1:(i - 1),]
      done_data$parent <-
        factor(done_data$parent, levels = paste(1:(i - 1)))
      expect_equal(get_plot_data(party_object)[i, ]$parent, max(which(
        done_data$kids > table(done_data$parent)
      )))
    }
  })
}

test_parents_id(py)
test_parents_id(t2)
test_parents_id(ct)
test_parents_id(ct2)
test_parents_id(tr_tree)


test_split_variable <- function(party_object) {
  test_that("valid split variable", {
    for (i in 2:length(get_plot_data(party_object)$id)) {
      if (!is.na(get_plot_data(party_object)[i, "splitvar"])) {
        expect_equal(get_plot_data(party_object)[i, "splitvar"] ,
                     names(party_object[[i]]$data)[party_object[[i]]$node$split$varid])
      }
    }
  })
}

test_split_variable(py)
test_split_variable(t2)
test_split_variable(ct)
test_split_variable(ct2)
test_split_variable(tr_tree)

test_valid_indexes <- function(party_object) {
  test_that("valid indexes", {
    expect_success(expect_output(get_plot_data(party_object)[1, ]$index, NA))
    for (i in 1:length(get_plot_data(party_object)$id)) {
      if (!is.null(get_plot_data(party_object)[i, "index"]) &&
          is.null(get_plot_data(party_object)[i, "breaks"])) {
        party_split <- party_object[[i]]$node$split
        party_node <- party_object[[i]]$node
        split_index <- party_split$index
        if (is.null(split_index)) {
          next
        }
        split_var <- names(party_object[i]$data)[party_split$varid]
        kids <- which(get_plot_data(party_object)$parent == i)
        var_levels <- levels(party_object$data[, split_var])
        for (j in 1:length(split_index)) {
          kid <- kids[split_index[j]]
          expect_equal(get_plot_data(party_object)[kid, "index"][[1]],
                       levels(party_object[i]$data[, split_var])[j])
        }
      }
      else{
        expect_success(expect_output(get_plot_data(party_object)[i, ]$index, NA))
      }
    }
  })
}

test_valid_indexes(py)
test_valid_indexes(t2)
test_valid_indexes(ct)
test_valid_indexes(ct2)
test_valid_indexes(tr_tree)

test_valid_breaks <- function(party_object) {
  test_that("valid breaks", {
    expect_success(expect_output(get_plot_data(party_object)[1, ]$index, NA))
    for (i in 1:length(get_plot_data(party_object)$id)) {
      if (!is.null(get_plot_data(py)[i, "breaks"]) &&
          is.null(get_plot_data(party_object)[i, "index"])) {
        party_split <- party_object[[i]]$node$split
        party_node <- party_object[[i]]$node
        split_breaks <- party_split$breaks
        if (is.null(split_breaks)) {
          next
        }
        split_var <- names(party_object[i]$data)[party_split$varid]
        kids <- which(get_plot_data(party_object)$parent == i)

        var_levels <- levels(party_object$data[, split_var])
        for (j in 1:length(split_breaks)) {
          kid <- kids[split_breaks[j]]
          expect_equal(get_plot_data(party_object)[kid, "index"][[1]],
                       levels(party_object[i]$data[, split_var])[j])
        }
      }
      else{
        expect_success(expect_output(get_plot_data(party_object)[i, ]$index, NA))

      }
    }
  })
}

test_valid_breaks(py)
test_valid_breaks(t2)
test_valid_breaks(ct)
test_valid_breaks(ct2)
test_valid_breaks(tr_tree)

test_add_info <- function(party_object) {
  test_that("add_info function", {
    for (i in get_plot_data(party_object)$id) {
      expect_equal(get_plot_data(party_object)[i, "info"][[1]],
                   list(party_object[[i]]$node$info)[[1]])
    }
  })
}

test_add_info(py)
test_add_info(t2)
test_add_info(ct)
test_add_info(ct2)
test_add_info(tr_tree)




##########
test_add_levels <- function(party_object) {
  test_that("add_levels function", {
    for (i in 1:length(get_plot_data(party_object)$id)) {
      parent <- get_plot_data(party_object)[i, "parent"]
      if (i == 1) {
        expect_equal(get_plot_data(party_object)$level[1], 0)
      }
      else if (get_plot_data(party_object)[i, "kids"] == 0) {
        expect_equal(get_plot_data(party_object)$level[i],
                     depth(party_object))
      }
      else
        (expect_equal(get_plot_data(party_object)$level[i],
                      get_plot_data(party_object)[parent, "level"] + 1))
    }
  })
}

test_add_levels(py)
test_add_levels(t2)
test_add_levels(ct)
test_add_levels(ct2)
test_add_levels(tr_tree)
###########
test_layout_terminal_nodes <- function(party_object) {
  test_that("add_layout function terminals", {
    terminal_data <- get_plot_data(party_object)[get_plot_data(party_object)$level == max(get_plot_data(party_object)$level), ]
    for (j in 1:(nrow(terminal_data))) {
      j_id <- terminal_data$id[j]
      expect_equal(get_plot_data(party_object)$y[j_id], 0.2)
      numerator <- ((j * 2) - 1)
      denominator <- nrow(terminal_data) * 2
      expect_equal(get_plot_data(party_object)$x[j_id],
                   (numerator  / denominator))
    }
  })
}

test_layout_terminal_nodes(py)
test_layout_terminal_nodes(t2)
test_layout_terminal_nodes(ct)
test_layout_terminal_nodes(ct2)
test_layout_terminal_nodes(tr_tree)

###########
test_layout_inner_nodes <- function(party_object) {
  terminal_space <- 0.2
  terminal_level <- max(get_plot_data(party_object)$level)
  test_that("add_layout function inner nodes", {
    inner_data <-
      get_plot_data(party_object)[get_plot_data(party_object)$level != max(get_plot_data(party_object)$level), ]
    for (i in 1:(nrow(inner_data))) {
      i_level <- inner_data$level[i]
      i_id <- inner_data$id[i]
      expect_equal(
        get_plot_data(party_object)$y[i_id],
        1 - i_level / max(get_plot_data(party_object)$level) * (1 - terminal_space)
      )
      parents <- i_id
      if (i_level != max(inner_data$level)) {
        for (j in (i_level + 1):(terminal_level - 1)) {
          parents <- c(parents, get_plot_data(party_object)[get_plot_data(party_object)$level  == j &
                                                              get_plot_data(party_object)$parent %in% parents, "id"])
        }
      }
      expect_equal(get_plot_data(party_object)$x[i_id],
                   mean(get_plot_data(party_object)$x[get_plot_data(party_object)$parent %in% parents &
                                                        get_plot_data(party_object)$kids == 0]))
    }
  })
}


test_layout_inner_nodes(py)
test_layout_inner_nodes(t2)
test_layout_inner_nodes(ct)
test_layout_inner_nodes(ct2)
test_layout_inner_nodes(tr_tree)


#########
test_add_data <- function(party_object) {
  test_that("add_data function", {
    data_columns <- names(party_object[[1]]$data)
    for (i in get_plot_data(party_object)$id) {
      node_data <- party_object[[i]]$data
      if (!is.null(party_object$node$info$object$fitted.values)) {
        node_data <-
          cbind(node_data,
                "fitted_values" = party_object[[i]]$node$info$object$fitted.values)
      }
      for (column in data_columns) {
        data_column <- paste0("data_", column)
        expect_equal(get_plot_data(party_object)[i, data_column][[1]], node_data[column])
      }
    }
  })
}

test_add_data(py)
test_add_data(t2)
test_add_data(ct)
test_add_data(ct2)
test_add_data(tr_tree)
