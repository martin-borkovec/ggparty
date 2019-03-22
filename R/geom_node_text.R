#' @export

geom_node_text <- function(mapping = NULL, arg_lists = NULL, x_nudge = 0,
                           y_nudge = 0, ids = NULL, extract_info = NULL) {

  #browser()
  #aestext <- syms(nodetext)

  if (is.list(mapping)) {
    startbreaks <- endbreaks <- character(0)

    for (i in seq_along(mapping)) {
      if (i > 1)                startbreaks <- rep("\n", i - 1)
      else                      startbreaks <- ""
      if (i < length(mapping)) endbreaks <- rep("\n", length(mapping) - i)
      else                      endbreaks <- ""


      new <- substring(deparse(mapping[[i]]$label), first = 2)
      #new <- paste0(startbreaks[i], " ", new, " \ ", endbreaks[i], ")")
      new <- quo(paste(!!startbreaks,!!parse(text = new)[[1]], !!endbreaks))
      mapping[[i]]$label <- new
    }
   # browser()
  }


  # first <- which.max(nchar(nodetext))
  # mapping <- aes(label = paste(!!(startbreaks[first]),
  #                              !!(aestext[[first]]),
  #                              !!(endbreaks[first])))
  #
  #   mapping <- aes(label = paste(!!startbreaks[first],
  #                                       !!nodetext[[first]],
  #                                       !!endbreaks[first]))
  #





  #mapping <- adjust_mapping(default_mapping, mapping)

  layer_list <- list(layer(
    data = NULL,
    mapping = mapping[[1]],
    stat = StatParty,
    geom = GeomNodetext,
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = TRUE,
    params = c(list(ids = ids,
                    na.rm = TRUE
                    ))#,
    # arg_lists[[first]])
  ))

  # layer_list <- c(layer_list, layer(
  #   data = NULL,
  #   mapping = mapping[[2]],
  #   stat = StatParty,
  #   geom = "text",
  #   position = position_nudge(x = x_nudge, y = y_nudge),
  #   inherit.aes = TRUE,
  #   params = c(list(ids = ids,
  #                   na.rm = TRUE,
  #                   extract_info = extract_info,
  #                   #col = "red",
  #                   lineheight = 2))#,
    #fontface = "bold"))#,
    # arg_lists[[first]])
  # ))

  # layer_list <- c(layer_list, layer(
  #   data = NULL,
  #   mapping = mapping[[3]],
  #   stat = StatParty,
  #   geom = "text",
  #   position = position_nudge(x = x_nudge, y = y_nudge),
  #   inherit.aes = TRUE,
  #   params = c(list(ids = ids,
  #                   na.rm = TRUE,
  #                   extract_info = extract_info,
  #                   # col = "red",
  #                   #fontface = "bold",
  #                   # size = 4,
  #                   lineheight = 2)
  #   )
  # ))

  # for (i in seq_along(nodetext)) {
  #
  #   if(i == first) next
  #
  #   mapping <- aes(label = paste(!!(startbreaks[i]),
  #                                !!(aestext[[i]]),
  #                                !!(endbreaks[i])))
  #   ))
  # }
  layer_list
}


GeomNodetext <- ggproto(
  "GeomNodetext",
  Geom,
  required_aes = c("x", "y", "id"),
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        gglist,
                        plot_call,
                        width,
                        height,
                        ids,
                        shared_axis_labels,
                        scales,
                        predict_arg
                        ) {


    #data <- coord$transform(data, panel_params)

    grob_list <- ggplotGrob(ggplot(data) +
                              geom_label2(aes(x, y, label = paste("ROFL!:", id)),
                                         parameters = list(coord, panel_params)) +
                              theme_void())
      #,
                                                   # aes(label = formatC(p.value, format = "e", digits = 2)),
                                                   # aes(label = splitvar))))
      # grob_list <- grob_list$grobs[[5]]$children$
    grob_list <- grob_list$grobs[[find_grob(grob_list$grobs, "panel")]]


    #class(grob_list) <- "gList"
    #ggname("geom_labelO", grid::grobTree(children = grob_list))
    return(grob_list)
}
)
