#' Map directional exposure
#'
#' @description `fire_exp_dir_map()` plots directional exposure transects onto
#' a map.
#'
#' @details
#' This function returns a standardized map with basic cartographic elements.
#'
#' The plot is returned as a ggplot object which can be exported/saved to
#' multiple image file formats.
#'
#' ## Spatial reference
#'
#' This function dynamically pulls map tiles for a base map. The inputs are
#' projected to WGS 84/Pseudo-Mercator ([EPSG:3857](https://epsg.io/3857)) to
#' align them with the map tiles.
#'
#' ## Zoom level
#' The map tile zoom level may need to be adjusted. If the base map is blurry,
#' increase the zoom level. Higher zoom levels will slow down the function, so
#' only increase if necessary. Reference the
#' [OpenStreetMap Wiki](https://wiki.openstreetmap.org/wiki/Zoom_levels) for
#' more information on zoom levels.
#'
#'
#' @param transects SpatVector. Output from [fire_exp_dir()]
#' @param value (Optional) SpatVector. Adds the value to the map. Use the same
#' value feature used to generate the transects with [fire_exp_dir()]
#' @param zoom_level (Optional). Numeric. set the zoom level for the base map
#' tile. See details. The default is `10`.
#' @param labels (Optional) a vector of three strings. Custom formatting for the
#' distances in the legend. If left blank, the function will automatically label
#' the distances in meters.
#' @param title (Optional) String. A custom title for the plot. The default is
#' `"Directional Vulnerability"`
#'
#'
#' @return a map of directional exposure transects as a ggplot object

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
#' # map with customized distance labels
#' fire_exp_dir_map(transects, labels = c("5 km", "10 km", "15 km"),
#'                  zoom_level = 9)
#'



fire_exp_dir_map <- function(transects,
                             value,
                             zoom_level = 10,
                             labels,
                             title = "Directional Vulnerability") {
  stopifnot("`transects` must be a SpatVector object"
            = class(transects) == "SpatVector",
            "`zoom_level` must be a number"
            = class(zoom_level) == "numeric",
            "`title` must be a character string"
            = class(title) == "character")

  if (missing(labels)) {
    seg_lengths <- terra::perim(terra::project(transects, "EPSG:4326"))[1:3]

    l1 <- paste(round(seg_lengths[1]), "m")
    l2 <- paste(round(sum(seg_lengths[1:2])), "m")
    l3 <- paste(round(sum(seg_lengths)), "m")
    labels <- c(l1, l2, l3)
  }
  stopifnot("`labels` must be a vector of three character objects"
            = class(labels) == "character" && length(labels) == 3)

  labs <- c(paste("Value to", labels[1]),
            paste(labels[1], "to", labels[2]),
            paste(labels[2], "to", labels[3]))

  t <- tidyterra::filter(transects, .data$viable == 1) %>%
    terra::project("EPSG:3857")

  e <- transects %>%
    terra::project("EPSG:3857") %>%
    terra::rescale(1.3)

  tile <- maptiles::get_tiles(e, "Esri.WorldImagery", crop = TRUE,
                              zoom = zoom_level)

  cred <- maptiles::get_credit("Esri.WorldImagery")

  caption <- paste("Basemap", substr(cred, 1, 63), "\n",
                   substr(cred, 63, nchar(cred)))

  plt <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.9) +
    tidyterra::geom_spatvector(data = t,
                               ggplot2::aes(color = factor(.data$seg))) +
    ggplot2::scale_color_manual(
      values = c(
        "seg1" = "darkred",
        "seg2" = "darkorange",
        "seg3" = "yellow"
      ),
      limits = c("seg1", "seg2", "seg3"),
      breaks = c("seg1", "seg2", "seg3"),
      labels = labs,
      drop = FALSE
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
      title = title,
      subtitle = "Map generated with {fireexposuR}",
      color = "Transect segment",
      caption = caption
    ) +
    ggplot2::coord_sf(expand = FALSE, default = TRUE)


  if (missing(value)) {
    return(plt)
  } else {
    stopifnot("`value` must be a SpatVector object"
              = class(value) == "SpatVector")
    v <- terra::project(value, "EPSG:3857")
    plt <- plt +
      tidyterra::geom_spatvector(
        data = v,
        fill = NA,
        colour = "black",
        linewidth = 0.7
      ) +
      ggplot2::coord_sf(expand = FALSE, default = TRUE)
    return(plt)
  }
}
