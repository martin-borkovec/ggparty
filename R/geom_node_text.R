#' @export

geom_node_text <- function(mapping = NULL, arg_lists = NULL, x_nudge = 0,
                           y_nudge = 0, ids = NULL, extract_info = NULL) {

  #browser()
  #aestext <- syms(nodetext)

  # if (is.list(mapping)) {
  #   startbreaks <- endbreaks <- character(0)
  #   for (i in seq_along(mapping)) {
  #     if (i > 1)                startbreaks <- rep("\n", i - 1)
  #     else                      startbreaks <- ""
  #     if (i < length(mapping)) endbreaks <- rep("\n", length(mapping) - i)
  #     else                      endbreaks <- ""
  #
  #
  #     new <- substring(deparse(mapping[[i]]$label), first = 2)
  #     #new <- paste0(startbreaks[i], " ", new, " \ ", endbreaks[i], ")")
  #     new <- quo(paste(!!startbreaks,!!parse(text = new)[[1]], !!endbreaks))
  #     mapping[[i]]$label <- new
  #   }
  #   # browser()
  # }

  layer_list <- list()

# LEGEND ------------------------------------------------------------------


  for (i in seq_along(mapping))
    layer_list <- c(layer_list, layer(
      data = NULL,
      mapping = mapping[[i]],
      stat = StatParty,
      geom = GeomLabel2,
      position = position_nudge(x = x_nudge, y = y_nudge),
      inherit.aes = TRUE,
      params = c(list(ids = ids,
                      na.rm = TRUE,
                      fill = NA,
                      only_legend = T)


    )))


# Labels -------------------------------------------------------------------


  layer_list <- c(layer_list, layer(
    data = NULL,
    mapping = NULL,
    stat = StatParty,
    geom = GeomNodetext,
    position = position_nudge(x = x_nudge, y = y_nudge),
    inherit.aes = TRUE,
    params = c(list(ids = ids,
                    na.rm = TRUE,
                    aes_list = mapping
                    ))
  ))

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
                        ids,
                        scales,
                        aes_list
                        ) {


    #data <- coord$transform(data, panel_params)

    # line_sizes <- c(8, 4, 4)
    # line_size_factor <- line_sizes / 4
    #
    # attach(data)
    # max_lengths <- numeric()
    #
    # for(j in data$id) {
    #   line_lengths <- numeric()
    #   for (i in seq_along(aes_list)) {
    #     line_lengths <- nchar(rlang::eval_tidy(aes_list[[i]]$label))
    #     line_lengths <- line_lengths * line_size_factor[i]
    #   }
    #   longest_line[j] <- which.max(line_lengths, na.rm = T)
    # }
    # browser()
    # detach(data)

# BOXES -------------------------------------------------------------------

browser()
    boxes <- ggplotGrob(ggplot(data) +
                              geom_label2(aes(x, y, label = paste("ROFL!: \n", id,"\n lol","\n lol"),
                                              col = splitvar),
                                          size = 8,
                                         parameters = list(coord, panel_params),
                                         only_border = F,
                                         fixed_height = NULL#unit(56 * 0.75, "mm")
                                         ) +
                              theme_void())

    grob_list <- boxes$grobs[[find_grob(boxes$grobs, "panel")]]$children

    grob_list <- c(grob_list,  roundrectGrob(0.5, 0.5, default.units = "native",
                                             width = grobWidth(grob_list[[1]]),
                                             height = grobHeight(grob_list[[1]]))
    )

# TEXTS --------------------------------------------------------------------

    # texts <- ggplotGrob(ggplot(data) +
    #                       geom_label2(aes(x, y, label = paste("ROFL!:", id),
    #                                       col = splitvar),
    #                                   parameters = list(coord, panel_params)) +
    #                       theme_void())
    #
    # grob_list <- c(grob_list, texts$grobs[[find_grob(texts$grobs, "panel")]])$children

    #class(grob_list) <- "gList"
    #ggname("geom_labelO", grid::grobTree(children = grob_list))
    return(grob_list)
}
)
