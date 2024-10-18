#' visualize directional exposure
#'
#' @description `fire_exp_dir_plot()` plots directional exposure following
#'   methods from Beverly and Forbes 2023 as a standardized radial plot,
#'   mapped with a basemap, or as a table.
#'
#' @param transects SpatVector (output from [fire_exp_dir()])
#' @param value the same Spatvector of value as a point or simplified polygon
#' used for [fire_exp_dir()]
#' @param map Boolean, when `TRUE`: returns a map of the viable transects. The
#'   default is `FALSE`
#'
#'
#' @return a standardized plot as a ggplot object, or if `map = TRUE` a
#' standardized map as a ggplot object

#' @export
#' @examples
#' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # generate an example point ---------------------------------
#' wkt <- "POINT (500000 5000000)"
#' pt <- terra::vect(wkt, crs = haz)
#' # -----------------------------------------------------------
#'
#' exp <- fire_exp(haz)
#'
#' # compute transects
#' transects <- fire_exp_dir(exp, pt)
#'
#' # radial plot of directional exposure
#' fire_exp_dir_plot(transects, pt)
#'
#' #' # mapped directional exposure transects
#' # note: example data is drawn in the ocean so basemap is not representative
#' fire_exp_dir_plot(transects, pt, map = TRUE)



fire_exp_dir_plot <- function(transects, value, map = FALSE) {
  stopifnot("`transects` must be a SpatVector object"
            = class(transects) == "SpatVector")
  stopifnot("`value` must be a SpatVector object"
            = class(value) == "SpatVector")

  if (map == TRUE) {
    #prepare for plotting

    t <- tidyterra::filter(transects, .data$viable == 1) %>%
      terra::project("EPSG:3857")

    e <- transects %>%
      terra::project("EPSG:3857") %>%
      terra::rescale(1.5)

    tile <- maptiles::get_tiles(e, "Esri.WorldImagery", crop = TRUE, zoom = 11)

    v <- terra::project(value, "EPSG:3857")

    cred <- maptiles::get_credit("Esri.WorldImagery")

    caption <- paste("Basemap", substr(cred, 1, 63), "\n",
                     substr(cred, 63, nchar(cred)))

    plt <- ggplot2::ggplot() +
      tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.9) +
      tidyterra::geom_spatvector(data = t,
                                 ggplot2::aes(color = factor(.data$seg))) +
      ggplot2::scale_color_manual(
        values = c(
          "to5" = "darkred",
          "to10" = "darkorange",
          "to15" = "yellow"
        ),
        limits = c("to5", "to10", "to15"),
        breaks = c("to5", "to10", "to15"),
        labels = c("Value to 5 km", "5 km to 10 km", "10 km to 15 km"),
        drop = FALSE
      ) +
      tidyterra::geom_spatvector(
        data = v,
        fill = NA,
        colour = "black",
        linewidth = 0.7
      ) +
      ggspatial::annotation_scale(location = "bl") +
      ggspatial::annotation_north_arrow(
        location = "bl",
        which_north = TRUE,
        pad_y = grid::unit(0.3, "in"),
        height = grid::unit(0.3, "in"),
        width = grid::unit(0.3, "in")
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title = "Directional Exposure",
        subtitle = "Map generated with fireexposuR()",
        color = "Segment",
        caption = caption
      ) +
      ggplot2::coord_sf(expand = FALSE)
    return(plt)
  } else {
    df <- as.data.frame(transects) %>%
      tidyr::pivot_wider(names_from = "seg", values_from = "viable") %>%
      dplyr::mutate(in15 = .data$to15) %>%
      dplyr::mutate(blank15 = 1) %>%
      dplyr::mutate(in10 = .data$to10 * 2 / 3) %>%
      dplyr::mutate(blank10 = 2 / 3) %>%
      dplyr::mutate(in5 = .data$to5 * 1 / 3) %>%
      dplyr::mutate(blank5 = 1 / 3)


    caption <- ifelse(
      terra::geomtype(value) == "points",
      "Centre of plot represents point feature",
      strwrap(
        "centre of plot represents all transect origins
                              along the boundary of the value polygon"
      )
    )


    plt <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$deg)) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$blank15),
        stat = "identity",
        width = 1,
        fill = "white",
        color = "white"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$in15),
        stat = "identity",
        width = 1,
        fill = "yellow",
        color = "yellow"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$blank10),
        stat = "identity",
        width = 1,
        fill = "white",
        color = "white"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$in10),
        stat = "identity",
        width = 1,
        fill = "darkorange",
        color = "darkorange"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$blank5),
        stat = "identity",
        width = 1,
        fill = "white",
        color = "white"
      ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data$in5),
        stat = "identity",
        width = 1,
        fill = "darkred",
        color = "darkred"
      ) +
      ggplot2::geom_line(ggplot2::aes(y = 1 / 3),
                         linewidth = 0.5,
                         color = "black") +
      ggplot2::geom_line(ggplot2::aes(y = 2 / 3),
                         linewidth = 0.5,
                         color = "black") +
      ggplot2::geom_line(ggplot2::aes(y = 1),
                         linewidth = 0.5,
                         color = "black") +
      ggplot2::geom_point(ggplot2::aes(y = 0)) +
      ggplot2::annotate(
        "label",
        x = 0,
        y = 1 / 3,
        label = "5 km",
        size = 3
      ) +
      ggplot2::annotate(
        "label",
        x = 0,
        y = 2 / 3,
        label = "10 km",
        size = 3
      ) +
      ggplot2::annotate(
        "label",
        x = 0,
        y = 1,
        label = "15 km",
        size = 3
      ) +
      ggplot2::coord_polar() +
      ggplot2::ylim(0, 1) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background =  ggplot2::element_rect(fill = "transparent"),
        plot.background = ggplot2::element_rect(fill = "transparent",
                                                color = NA),
        axis.text.x = ggplot2::element_text(
          color = "black",
          size = 15,
          face = "bold"
        )
      ) +
      ggplot2::scale_x_continuous(breaks = c(90, 180, 270, 360),
                                  labels = c("E", "S", "W", "N")) +
      ggplot2::labs(title = "Directional Exposure",
                    subtitle = "Plot generated with fireexposuR()",
                    caption = caption)
    return(plt)
  }
}
