#' Visualize exposure in a map
#'
#' @description `fire_exp_map()` produces a map with sensible defaults that can
#' be customized.
#'
#' @details
#'
#' This function returns a map with basic cartographic elements and a
#' standardized colour scale. .
#'
#' The plot is returned as a tmap object which can be further customized using
#' tmap commands or exported/saved to multiple image file formats. See
#' [tmap::tmap_save()] for export details.
#'
#' This function visualizes the outputs from [fire_exp()].
#' The map can be returned with a continuous scale or can be classified.
#'
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
#' This function dynamically pulls map tiles for a base map. The crs is set
#' automatically. See [tmap::tm_crs()] for details.
#'
#'
#' @param exposure SpatRaster (e.g. from [fire_exp()])
#' @param aoi (Optional) SpatVector of an area of interest to mask exposure
#' @param classify character, either `"local"`, `"landscape"`, or `"custom"`,
#' to specify classification scheme to use. The default is `"local"`.
#' If set to `"custom"`: the parameter `class_breaks` must be used.
#' @param class_breaks vector of numeric values between 0-1 of the upper limits
#' of each custom class. Ignored unless `classify = "custom"`. See details.

#' @param title (Optional) String. A custom title for the plot. The default
#' is `"Wildfire exposure"`
#'
#'
#' @return a map is returned as a tmap object.
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#'
#' # compute exposure
#' exposure <- fire_exp(hazard)
#'
#'
#' fire_exp_map(exposure)
#'

fire_exp_map <- function(exposure, aoi, classify,
                         class_breaks, title = "Wildfire Exposure") {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster",
            "`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0
               && round(terra::minmax(exposure)[2], 0) <= 1),
            "`exposure` layer must have a CRS defined"
            = terra::crs(exposure) != "")

  names(exposure) <- "exposure"
  exp <- exposure

  if (!missing(aoi)) {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector",
              "`aoi` extent must be within `exposure` extent"
              = terra::relate(aoi, exp, "within"),
              "`exposure` and `aoi` must have same CRS"
              = terra::same.crs(exp, aoi))

    aoi_r <- terra::rasterize(aoi, exp)

    exp_aoi <- exp * aoi_r

    exp <- terra::trim(exp_aoi, padding = 10)
  }


  if (!missing(classify)) {
    stopifnot("`classify` must be one of: 'local', 'landscape', or 'custom'"
              = classify %in% c("local", "landscape", "custom"))

    if (classify == "custom") {
      stopifnot("must provide 'class_breaks' if `classify` = 'custom'"
                = !missing(class_breaks))
    }

    if (classify == "landscape") {
      class_breaks <- c(0.2, 0.4, 0.6, 0.8, 1)
    }

    if (classify == "local") {
      class_breaks <- c(0.15, 0.3, 0.45, 1)
    }

    class_breaks <- sort(class_breaks)

    # class_breaks checks
    stopifnot("`class_breaks` must be a vector of numbers"
              = class(class_breaks) == "numeric",
              "`class_breaks` must have 1 as the maximum value"
              = max(class_breaks) == 1,
              "`class_breaks` must be greater than 0"
              = class_breaks > 0)

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
    exp <- terra::classify(exp, rcmats, include.lowest = TRUE)

    levels(exp) <- lut[, 3:4]

    n_color <- length(class_breaks)

    cols <- c("grey40", tidyterra::whitebox.colors(n_color,
                                                   palette = "bl_yl_rd"))
    names(cols) <- lut$label


    plt <- tmap::tm_shape(exp) +
      tmap::tm_raster(
        col.scale = tmap::tm_scale_categorical(values = cols),
        col_alpha = 0.7,
        col.legend = tmap::tm_legend(title = "Exposure")
      )
  } else {
    cols <- tidyterra::whitebox.colors(n = 100, palette = "bl_yl_rd")

    plt <- tmap::tm_shape(exp) +
      tmap::tm_raster(
        col.scale = tmap::tm_scale_continuous(values = cols, limits = c(0, 1)),
        col_alpha = 0.7,
        col.legend = tmap::tm_legend(title = "Exposure")
      )
  }

  if (!missing(aoi)) {
    plt <- plt + tmap::tm_shape(sf::st_as_sf(aoi)) +
      tmap::tm_borders(lwd = 2)
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
