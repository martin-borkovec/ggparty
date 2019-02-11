context("get_plot_data subfunctions")

test_that("valid number of kids", {
  for (i in get_plot_data(party(pn, WeatherPlay))$id) {
    expect_equal(get_plot_data(party(pn, WeatherPlay))[i,]$kids, length(py[i]$node))
  }
})

test_that("valid id of parent", {
  expect_success(expect_output(get_plot_data(party(pn, WeatherPlay))[1,]$parent, NA))
  for (i in 2:length(get_plot_data(party(pn, WeatherPlay))$id)) {
    done_data <- get_plot_data(party(pn, WeatherPlay))[1:(i - 1), ]
    done_data$parent <- factor(done_data$parent, levels = paste(1:(i - 1)))
    expect_equal(get_plot_data(party(pn, WeatherPlay))[i,]$parent, max(which(done_data$kids > table(done_data$parent))))
    }
})

test_that("valid split variable", {
  for (i in 2:length(get_plot_data(py)$id)){
    if(!is.na(get_plot_data(py)[i, "splitvar"])){
    expect_equal(get_plot_data(py)[i, "splitvar"] , names(py[[i]]$data)[ py[[i]]$node$split$varid])
      }
  }
})

test_that("valid indexes", {
  expect_success(expect_output(get_plot_data(py)[1,]$index, NA))
  for (i in 1:length(get_plot_data(py)$id)){
    if(!is.na(get_plot_data(py)[i, "index"]) && is.na(get_plot_data(py)[i, "breaks"])){
      party_split <- py[[i]]$node$split
      party_node <- py[[i]]$node
      split_index <- party_split$index
      if(is.null(split_index)){
        next
      }
      split_var <- names(py[i]$data)[party_split$varid]
      kids <- which(get_plot_data(py)$parent == i)

      var_levels <- levels(py$data[,split_var])
      for(j in 1:length(split_index)){
        kid <- kids[split_index[j]]
        # print(kid)
        # print(get_plot_data(py)[kid, "index"][[1]])
        # print(levels(py[i]$data[,split_var])[j])
        expect_equal(get_plot_data(py)[kid, "index"][[1]], levels(py[i]$data[,split_var])[j])
          }
    }
    else{
      expect_success(expect_output(get_plot_data(py)[i,]$index, NA))

    }
    }
})

test_that("valid breaks", {
  expect_success(expect_output(get_plot_data(py)[1,]$index, NA))
  for (i in 1:length(get_plot_data(py)$id)){
    if(!is.na(get_plot_data(py)[i, "breaks"]) && is.na(get_plot_data(py)[i, "index"])){
      party_split <- py[[i]]$node$split
      party_node <- py[[i]]$node
      split_breaks <- party_split$breaks
      if(is.null(split_breaks)){
        next
      }
      split_var <- names(py[i]$data)[party_split$varid]
      kids <- which(get_plot_data(py)$parent == i)

      var_levels <- levels(py$data[,split_var])
      for(j in 1:length(split_breaks)){
        kid <- kids[split_breaks[j]]
        # print(kid)
        # print(get_plot_data(py)[kid, "index"][[1]])
        # print(levels(py[i]$data[,split_var])[j])
        expect_equal(get_plot_data(py)[kid, "breaks"][[1]], levels(py[i]$data[,split_var])[j])
      }
    }
    else{
      expect_success(expect_output(get_plot_data(py)[i,]$breaks, NA))

    }
  }
  })

test_that("add_info function", {
  for (i in get_plot_data(py)$id){
    expect_equal(get_plot_data(py)[i,"info"][[1]], list(py[[i]]$node$info)[[1]])
    }
  })

test_that("add_levels function", {
for (i in 1:length(get_plot_data(py)$id)){
  parent <- get_plot_data(py)[i,"parent"]
  if (i == 1){
    expect_equal(get_plot_data(py)$level[1], 0)
    }
  else if (get_plot_data(py)[i, "kids"] == 0){
    expect_equal(get_plot_data(py)$level[i], depth(py))
  }
  else (
    expect_equal(get_plot_data(py)$level[i], get_plot_data(py)[parent, "level"] + 1)
    )
  }
})


test_that("add_layout function terminals", {
  terminal_data <- get_plot_data(py)[get_plot_data(py)$level == max(get_plot_data(py)$level),]
  for(j in 1:(nrow(terminal_data))){
    j_id <- terminal_data$id[j]
    expect_equal(get_plot_data(py)$y[j_id], 0)
    numerator <-((j * 2) - 1)
    denominator<- nrow(terminal_data)*2
    expect_equal(get_plot_data(py)$x[j_id], (numerator  / denominator))
  }
})

test_that("add_layout function inner nodes", {
  inner_data <- get_plot_data(py)[get_plot_data(py)$level != max(get_plot_data(py)$level),]
  for(i in 1:(nrow(inner_data))){
    i_level <- inner_data$level[i]
    i_id <- inner_data$id[i]
    expect_equal(get_plot_data(py)$y[i_id], 1 - i_level / max(get_plot_data(py)$level))
    expect_equal(get_plot_data(py)$x[i_id], mean(get_plot_data(py)$x[get_plot_data(py)$parent %in% i_id & get_plot_data(py)$kids == 0]))
  }
})

  test_that("add_data function", {
    data_columns <- names(py[[1]]$data)
    for (i in get_plot_data(py)$id) {
      node_data <- py[[i]]$data
      if (!is.null(py$node$info$object$fitted.values)) {
        node_data <- cbind(node_data, "fitted_values" = py[[i]]$node$info$object$fitted.values)
        }
      for (column in data_columns) {
        data_column <- paste0("data_", column)
        expect_equal(get_plot_data(py)[i, data_column][[1]], node_data[column] )
        }
      }
    })
