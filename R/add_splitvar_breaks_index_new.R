add_splitvar_breaks_index_new <- function(party_object, plot_data, round_digits = NULL) {

  plot_data$breaks_label <- I(rep(list(NA), length(party_object)))

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
        # check if breaks are supposed to be rounded and apply if so
        if(!is.null(round_digits)) split_breaks <- round(split_breaks, round_digits)
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
  return(plot_data["breaks_label"])
}
