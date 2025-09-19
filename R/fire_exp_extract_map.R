#' Visualize exposure to values in a map
#'
#' @description `fire_exp_extract_map()` standardizes the visualization of
#' outputs from [fire_exp_extract()] as a map by classifying
#' exposure into predetermined exposure classes.
#'
#' The plot is returned as a tmap object which can be further customized using
#' tmap commands or exported/saved to multiple image file formats. See
#' [tmap::tmap_save()] for export details.
#'
#' @details
#' This function visualizes the outputs from [fire_exp_extract()] with classes.
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
#' #' Landscape classification breaks are predefined
#' as `c(0.2, 0.4, 0.6, 0.8, 1)`:
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
#'
#' @param values_ext Spatvector of points or polygons from [fire_exp_extract()]
#' @param classify character, either `"local"`, `"landscape"`, or `"custom"`,
#' to specify classification scheme to use. The default is `"local"`. If set to
#' `"custom"`: the parameter `class_breaks` must be used.
#' @param class_breaks vector of numeric values between 0-1. Ignored unless
#'`classify = "custom"`. See details.
#' @param method character, either `"max"` or `"mean"`. If `values_ext` are
#' polygons the default is `"max"`.This parameter is ignored when `values_ext`
#' are point features.
#' @param title (Optional) String. Ignored when `map = FALSE`. A custom title
#' for the plot. The default is `"Classified Exposure to Values"`
#'
#'
#'
#' @return a map is returned as a tmap object
#'
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
#' # generate random points within the aoi polygon
#' points <- terra::spatSample(aoi, 20)
#'
#' # compute exposure
#' exposure <- fire_exp(hazard)
#'
#' values_exp <- fire_exp_extract(exposure, points)
#'
#' # visualize example points in standardized map
#' fire_exp_extract_map(values_exp)
#'
fire_exp_extract_map <- function(values_ext,
                                 classify = c("local", "landscape", "custom"),
                                 class_breaks,
                                 method = c("max", "mean"),
                                 title = "Classified Exposure to Values") {
  ext <- values_ext
  stopifnot("`values_ext` must be a SpatVector of point or polygon features"
            = (class(ext) == "SpatVector" &&
                 terra::geomtype(ext) %in% c("points", "polygons")),
            "`values_ext` missing exposure attribute. Use fire_exp_extract()"
            = any(terra::names(ext) %in% c("exposure", "mean_exp", "max_exp")))
  if (terra::geomtype(ext) == "polygons") {
    method <- match.arg(method)
    if (method == "mean") {
      method <- "Mean"
      ext <- ext %>%
        dplyr::rename(exposure = "mean_exp")
    }
    if (method == "max") {
      method <- "Maximum"
      ext <- ext %>%
        dplyr::rename(exposure = "max_exp")
    }
  } else {
    method <- "NA"
  }
  classify <- match.arg(classify)

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

  rules <- c("exposure == 0 ~ 0",
             utils::tail(c(sprintf("dplyr::between(exposure, %f, %f) ~ %f",
                                   lut$start, lut$end, lut$factor)), -1))

  lut2 <- as.factor(lut$factor)

  names(lut2) <- lut$label

  ext_class <- ext %>%
    dplyr::mutate(class = do.call(dplyr::case_when,
                                  c(lapply(rules, str2lang)))) %>%
    dplyr::mutate(class_range = names(lut2)[match(.data$class, lut2)])

  ext_class$class_range <- factor(ext_class$class_range, levels = names(lut2))


  n_color <- length(class_breaks)

  cols <- c("grey40", tidyterra::whitebox.colors(n_color,
                                                 palette = "bl_yl_rd"))
  names(cols) <- class_labels

  if (terra::geomtype(ext_class) == "points") {

    plt <- tmap::tm_shape(sf::st_as_sf(ext_class)) +
      tmap::tm_symbols(
        size = 0.6,
        lwd = 2,
        fill = "class_range",
        fill.scale = tmap::tm_scale_categorical(values = cols),
        fill.legend = tmap::tm_legend(title = "Exposure Class",
                                      position = tmap::tm_pos_out(
                                        "right"))
      )
  } else {
    plt <- tmap::tm_shape(sf::st_as_sf(ext_class)) +
      tmap::tm_polygons(
        fill = "class_range",
        fill.scale = tmap::tm_scale_categorical(values = cols),
        fill.legend = tmap::tm_legend(title = "Exposure Class",
                                      position = tmap::tm_pos_out(
                                        "right"))
      )
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
    tmap::tm_layout(inner.margins = 0.2) +
    tmap::tm_crs("auto")


  return(plt)
}
