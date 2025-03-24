#' Plot directional exposure in a radial plot
#'
#' @description `fire_exp_dir_plot()` plots the viable directional exposure
#' pathways identified with [fire_exp_dir()] in a standardized radial plot.
#'
#' @details
#' The radial plot produced by this function is based on the figures presented
#' in Beverly and Forbes (2023). The plots put the transect origin (the value)
#' at the center as a point, and labels the distances from the value at the end
#' of the transect segments. If the value used to generate the transects was a
#' polygon feature, the transect origins will still be drawn as a center point.
#'
#' The plot is returned as a ggplot object which can be exported/saved to
#' multiple image file formats.
#'
#' @references
#' Beverly JL, Forbes AM (2023) Assessing directional vulnerability to
#' wildfire. *Natural Hazards* **117**, 831-849.
#' [DOI](https://doi.org/10.1007/s11069-023-05885-3)
#'
#'
#' @param transects SpatVector (output from [fire_exp_dir()])
#' @param labels (Optional) a vector of three strings. Custom formatting for the
#' distance labels on the transect segments. If left blank, the function will
#' automatically label the distances in meters.
#' @param title (Optional) String. A custom title for the plot. The default is
#'   `"Directional Vulnerability"`
#'
#'
#' @return a ggplot object.

#' @export
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate an example point
#' point_wkt <- "POINT (400000 6050000)"
#' point <- terra::vect(point_wkt, crs = hazard)
#'
#' # compute exposure metric
#' exposure <- fire_exp(hazard)
#'
#' # generate transects
#' transects <- fire_exp_dir(exposure, point)
#'
#'
#' # radial plot
#' fire_exp_dir_plot(transects)
#'
#' # customize labels
#' fire_exp_dir_plot(transects, labels = c("5 km", "10 km", "15 km"))
#'


fire_exp_dir_plot <- function(transects,
                              labels,
                              title = "Directional Vulnerability") {
  stopifnot("`title` must be a character string"
            = class(title) == "character",
            "`transects` must be a SpatVector object"
            = class(transects) == "SpatVector")


  seg_lengths <- terra::perim(terra::project(transects, "EPSG:4326"))[1:3]
  seg2_prop <- sum(seg_lengths[1:2]) / sum(seg_lengths)
  seg1_prop <- seg_lengths[1] / sum(seg_lengths)

  if (missing(labels)) {
    l1 <- paste(round(seg_lengths[1]), "m")
    l2 <- paste(round(sum(seg_lengths[1:2])), "m")
    l3 <- paste(round(sum(seg_lengths)), "m")
    labels <- c(l1, l2, l3)
  }

  stopifnot("`labels` must be a vector of three character objects"
            = class(labels) == "character" && length(labels) == 3)

  df <- as.data.frame(transects) %>%
    tidyr::pivot_wider(names_from = "seg", values_from = "viable") %>%
    dplyr::mutate(in3 = .data$seg3) %>%
    dplyr::mutate(blank3 = 1) %>%
    dplyr::mutate(in2 = .data$seg2 * seg2_prop) %>%
    dplyr::mutate(blank2 = seg2_prop) %>%
    dplyr::mutate(in1 = .data$seg1 * seg1_prop) %>%
    dplyr::mutate(blank1 = seg1_prop)



  plt <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$deg)) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$blank3),
      stat = "identity",
      width = 1,
      fill = "white",
      color = "white"
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$in3),
      stat = "identity",
      width = 1,
      fill = "yellow",
      color = "yellow"
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$blank2),
      stat = "identity",
      width = 1,
      fill = "white",
      color = "white"
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$in2),
      stat = "identity",
      width = 1,
      fill = "darkorange",
      color = "darkorange"
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$blank1),
      stat = "identity",
      width = 1,
      fill = "white",
      color = "white"
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(y = .data$in1),
      stat = "identity",
      width = 1,
      fill = "darkred",
      color = "darkred"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = seg1_prop),
                       linewidth = 0.5,
                       color = "black") +
    ggplot2::geom_line(ggplot2::aes(y = seg2_prop),
                       linewidth = 0.5,
                       color = "black") +
    ggplot2::geom_line(ggplot2::aes(y = 1),
                       linewidth = 0.5,
                       color = "black") +
    ggplot2::geom_point(ggplot2::aes(y = 0)) +
    ggplot2::annotate(
      "label",
      x = 0,
      y = seg1_prop,
      label = labels[1],
      size = 3
    ) +
    ggplot2::annotate(
      "label",
      x = 0,
      y = seg2_prop,
      label = labels[2],
      size = 3
    ) +
    ggplot2::annotate(
      "label",
      x = 0,
      y = 1,
      label = labels[3],
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
    ggplot2::labs(title = title,
                  subtitle = "Plot generated with {fireexposuR}")
  return(plt)
}
