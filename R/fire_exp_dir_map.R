#' Map directional exposure
#'
#' @description `fire_exp_dir_map()` plots directional exposure transects onto
#' a map.
#'
#' @details
#' This function returns a standardized map with basic cartographic elements.
#'
#' The plot is returned as a tmap object which can be further customized using
#' tmap commands or exported/saved to multiple image file formats. See
#' [tmap::tmap_save()] for export details.
#'
#' ## Spatial reference
#'
#' This function dynamically pulls map tiles for a base map. The crs is set
#' automatically. See [tmap::tm_crs()] for details.
#'
#'
#'
#' @param transects SpatVector. Output from [fire_exp_dir()]
#' @param value (Optional) SpatVector. Adds the value to the map. Use the same
#' value feature used to generate the transects with [fire_exp_dir()]
#' @param labels (Optional) a vector of three strings. Custom formatting for the
#' distances in the legend. If left blank, the function will automatically label
#' the distances in meters.
#' @param title (Optional) String. A custom title for the plot. The default is
#' `"Directional Vulnerability"`
#'
#'
#' @return a map of directional exposure transects as a tmap object

#' @export
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate an example point
#' point_wkt <- "POINT (345000 5876000)"
#' point <- terra::vect(point_wkt, crs = hazard)
#'
#' # compute exposure metric
#' exposure <- fire_exp(hazard)
#'
#' # generate transects
#' transects <- fire_exp_dir(exposure, point, interval = 5)
#'
#' fire_exp_dir_map(transects)
#'



fire_exp_dir_map <- function(transects,
                             value,
                             labels,
                             title = "Directional Vulnerability") {
  stopifnot("`transects` must be a SpatVector object"
            = class(transects) == "SpatVector",
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

  cols <- c("darkred", "darkorange", "yellow2")

  t <- tidyterra::filter(transects, .data$viable == 1)

  plt <- tmap::tm_shape(sf::st_as_sf(t), bbox = sf::st_as_sf(transects)) +
    tmap::tm_lines(lwd = 2,
                   col = "seg",
                   col.scale = tmap::tm_scale_ordinal(values = cols,
                                                      labels = labs),
                   col.legend = tmap::tm_legend(title = "Transect segment"))

  if (!missing(value)) {
    stopifnot("`value` must be a SpatVector object"
              = class(value) == "SpatVector")
    plt <- plt + tmap::tm_shape(sf::st_as_sf(value)) +
      tmap::tm_borders(lwd = 2,
                       col = "black")
  }

  plt <- plt + tmap::tm_basemap("Esri.WorldImagery") +
    tmap::tm_credits("Basemap: Esri World Imagery",
                     color = "white") +
    tmap::tm_title(title) +
    tmap::tm_compass(
      type = "arrow",
      position = c("LEFT", "BOTTOM"),
      color.light = "black",
      color.dark = "white",
      text.color = "white"
    ) +
    tmap::tm_scalebar(position = tmap::tm_pos_out("center", "bottom"),
                      text.size = 0.9) +
    tmap::tm_layout(inner.margins = 0.05) +
    tmap::tm_crs("auto")

  return(plt)
}
