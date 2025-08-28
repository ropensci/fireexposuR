#' Map exposure with a classified scale (Deprecated)
#'
#' @description This function still works, but will be removed in future
#' versions of the package. The same functionality is now included in
#'  [fire_exp_map()]. .
#'
#'
#' @details
#'
#' This function returns a standardized map with basic cartographic elements.
#'
#' The plot is returned as a ggplot object which can be exported/saved to
#' multiple image file formats.
#'
#' This function visualizes the outputs from [fire_exp()] with classes.
#' Classes can be chosen from the pre-set `"local"` and `"landscape"` options,
#' or customized. To use a custom classification scheme, it should be defined
#' with a list of numeric vectors defining the upper limits of the breaks. A
#' Nil class is added automatically for exposure values of exactly zero.
#'
#' Local classification breaks are predefined as `c(0.15, 0.3, 0.45, 1)`:
#' * Nil (0)
#' * 0 - 0.15
#' * 0.15 - 0.3
#' * 0.3 - 0.45
#' * 0.45 - 1
#'
#' Landscape classification breaks are predefined as `c(0.2, 0.4, 0.6, 0.8, 1)`:
#' * Nil (0)
#' * 0 - 0.2
#' * 0.2 - 0.4
#' * 0.4 - 0.6
#' * 0.6 - 0.8
#' * 0.8 - 1
#'
#' ## Spatial reference
#'
#' This function dynamically pulls map tiles for a base map.
#' The inputs are projected to WGS 84/Pseudo-Mercator
#' ([EPSG:3857](https://epsg.io/3857)) to align them with the map tiles.
#'
#' ## Zoom level
#' The map tile zoom level may need to be adjusted. If the base map is blurry,
#' increase the zoom level. Higher zoom levels will slow down the function, so
#' only increase if necessary. Reference the
#' [OpenStreetMap Wiki](https://wiki.openstreetmap.org/wiki/Zoom_levels) for
#' more information on zoom levels.
#'
#' @seealso [fire_exp_map_class()]
#'
#' @param exposure SpatRaster (e.g. from [fire_exp()])
#' @param aoi (Optional) SpatVector of an area of interest to mask exposure
#' @param classify character, either `"local"`, `"landscape"`, or `"custom"`,
#' to specify classification scheme to use. The default is `"local"`.
#' If set to `"custom"`: the parameter `class_breaks` must be used.
#' @param class_breaks vector of numeric values between 0-1 of the upper limits
#' of each custom class. Ignored unless `classify = "custom"`. See details.
#' @param zoom_level (Optional) numeric, set the zoom level for the basemap
#' based on the extent of your data if defaults are not appropriate. See
#' details. Defaults if:
#' * `classify = "local"` or `"custom"` the zoom level default is `12`
#' * `classify = "landscape"` the zoom level default is `7`
#' @param title (Optional) String. A custom title for the plot. The default
#' is `"Classified Exposure"`
#'
#'
#' @return a standardized map is returned as a ggplot object
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # read example area of interest
#' polygon_path <- system.file("extdata", "polygon.shp", package ="fireexposuR")
#' aoi <- terra::vect(polygon_path)
#'
#' # compute exposure
#' exposure <- fire_exp(hazard)
#'
#' fire_exp_map_class(exposure, aoi, classify = "local")
#'

fire_exp_map_class <- function(exposure, aoi, classify = c("local", "landscape",
                                                           "custom"),
                               class_breaks, zoom_level,
                               title = "Classified Exposure") {


  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster",
            "`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0
               && round(terra::minmax(exposure)[2], 0) <= 1),
            "`exposure` layer must have a CRS defined"
            = terra::crs(exposure) != "")

  classify <- match.arg(classify)

  names(exposure) <- "exposure"
  exp <- exposure

  if (!missing(aoi)) {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector",
              "`aoi` extent must be within `exposure` extent"
              = terra::relate(aoi, exposure, "within"),
              "`exposure` and `aoi` must have same CRS"
              = terra::same.crs(exposure, aoi))

    exp <- exp %>%
      terra::crop(aoi) %>%
      terra::mask(aoi)
    b <- terra::project(aoi, "EPSG:3857")
    # get extent to clip tile
    e <- terra::rescale(b, 1.2)
    expb <- terra::crop(exp, aoi, mask = TRUE) %>%
      terra::project("EPSG:3857")
  } else {
    e <- terra::rescale(terra::as.polygons(terra::ext(exp),
                                           terra::crs(exp)), 1.2) %>%
      terra::project("EPSG:3857")
    expb <- terra::project(exp, "EPSG:3857")
  }

  classify <- match.arg(classify)

  if (classify == "landscape") {
    zoom_level <- ifelse(missing(zoom_level), 7, zoom_level)
    class_breaks <- c(0.2, 0.4, 0.6, 0.8, 1)
  }

  if (classify == "local") {
    class_breaks <- c(0.15, 0.3, 0.45, 1)
  }

  zoom_level <- ifelse(missing(zoom_level), 12, zoom_level)

  class_breaks <- sort(class_breaks)

  # class_breaks checks
  stopifnot("`class_breaks` must be a vector of numbers"
            = class(class_breaks) == "numeric")
  stopifnot("`class_breaks` must have 1 as the maximum value"
            = max(class_breaks) == 1)
  stopifnot("`class_breaks` must be greater than 0"
            = class_breaks > 0)

  .Deprecated("fire_exp_map")

  class_labels <- character()

  label_breaks <- c(0, class_breaks)
  for (i in seq_along(label_breaks)) {
    class_labels[i] <- paste(label_breaks[i], "-", label_breaks[i + 1])
  }

  class_labels <- c("Nil", utils::head(class_labels, -1))

  lut <- data.frame(start = c(0, 0, utils::head(class_breaks, -1)),
                    end = c(0, class_breaks),
                    factor = 0:length(class_breaks),
                    label = class_labels)

  rcmats <- as.matrix(lut[, 1:3])
  expbc <- terra::classify(expb, rcmats, include.lowest = TRUE)

  levels(expbc) <- lut[, 3:4]

  n_color <- length(class_breaks)

  cols <- c("grey40", tidyterra::whitebox.colors(n_color, palette = "bl_yl_rd"))
  names(cols) <- lut$label

  tile <- maptiles::get_tiles(e, "Esri.WorldImagery", zoom = zoom_level) %>%
    terra::crop(e)
  cred <- maptiles::get_credit("Esri.WorldImagery")
  caption <- paste("Basemap", substr(cred, 1, 63), "\n",
                   substr(cred, 63, nchar(cred)))

  plt <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile) +
    tidyterra::geom_spatraster(data = expbc, alpha = 0.8) +
    ggplot2::scale_fill_manual(values = cols,
                               na.value = NA,
                               na.translate = FALSE) +
    ggplot2::theme_void() +
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(location = "bl",
                                      which_north = TRUE,
                                      pad_y = grid::unit(0.3, "in"),
                                      height = grid::unit(0.3, "in"),
                                      width = grid::unit(0.3, "in")) +
    ggplot2::labs(title = title,
                  subtitle = "Map generated with fireexposuR()",
                  fill = "Exposure",
                  caption = caption)

  if (!missing(aoi)) {
    plt <- plt +
      tidyterra::geom_spatvector(data = b, fill = NA, linewidth = 0.6)
  }

  plt <- plt + ggplot2::coord_sf(expand = FALSE)

  return(plt)
}
