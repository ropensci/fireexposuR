#' Visualize exposure to values in a summary table
#'
#' @description `fire_exp_extract_summary()` standardizes the visualization of
#' outputs from [fire_exp_extract()] as a summary table by classifying
#' exposure into predetermined exposure classes.
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
#'
#'
#' @return a summary table is returned as a data frame
#'
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # read example area of interest geometry
#' geom_file_path <- "extdata/polygon_geometry.csv"
#' geom <- read.csv(system.file(geom_file_path, package = "fireexposuR"))
#'
#' # generate an area of interest polygon with the geometry
#' aoi <- terra::vect(as.matrix(geom), "polygons", crs = hazard)
#'
#' # generate random points within the aoi polygon
#' points <- terra::spatSample(aoi, 100)
#'
#' # compute exposure
#' exposure <- fire_exp(hazard)
#'
#' values_exp <- fire_exp_extract(exposure, points)
#'
#' # summarize example points in a table
#' fire_exp_extract_summary(values_exp, classify = "local")
#'
#'
fire_exp_extract_summary <- function(values_ext,
                                     classify = c("local",
                                                  "landscape",
                                                  "custom"),
                                     class_breaks,
                                     method = c("max",
                                                "mean")) {
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

  df <- as.data.frame(ext_class) %>%
    dplyr::count(.data$class_range) %>%
    dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
    dplyr::mutate(method = method) %>%
    dplyr::select(c("class_range", "n", "prop", "method"))

  return(df)
}
