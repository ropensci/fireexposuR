#' Summarize exposure by class
#'
#' @description `fire_exp_summary()` creates a summary table of area and
#' proportions of exposure in predetermined or custom exposure classes.
#'
#' @details
#'
#' This function summarizes the outputs from [fire_exp()] with classes.
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
#' The table reports the number of pixels, the proportion, and area in hectares
#' and meters squared in each class.
#'
#'
#' @param exposure SpatRaster from [fire_exp()]
#' @param aoi (optional) SpatVector of an area of interest to mask exposure for
#' summary
#' @param classify character, either `"local"`, `"landscape"`, or `"custom"`,
#' to specify classification scheme to use. The default is `"local"`.
#' If set to `"custom"`: the parameter `class_breaks` must be used.
#' @param class_breaks vector of numeric values between 0-1 of the upper limits
#' of each custom class. Ignored unless `classify = "custom"`. See details.
#'
#' @returns a summary table as a data frame object
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # read example area of interest polygon geometry
#' geom_file_path <- "extdata/polygon_geometry.csv"
#' geom <- read.csv(system.file(geom_file_path, package = "fireexposuR"))
#' aoi <- terra::vect(as.matrix(geom), "polygons", crs = hazard)
#'
#' # Compute exposure
#' exposure <- fire_exp(hazard)
#'
#' # Summary for full extent of data
#' fire_exp_summary(exposure, classify = "landscape")
#'
#' # Summary masked to an area of interest
#' fire_exp_summary(exposure, aoi, classify = "landscape")
#'
fire_exp_summary <- function(exposure, aoi,
                             classify = c("landscape", "local", "custom"),
                             class_breaks) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster",
            "`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0
               && round(terra::minmax(exposure)[2], 0) <= 1),
            "`exposure` layer must have a CRS defined"
            = terra::crs(exposure) != "",
            "Linear units of `exposure` layer must be in meters"
            = terra::linearUnits(exposure) == 1)
  classify <- match.arg(classify)

  names(exposure) <- "exposure"
  exp <- exposure
  res <- terra::res(exp)[1]

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


  if (!missing(aoi)) {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector")
    aoi <- terra::project(aoi, exp)

    exp <- exp %>%
      terra::crop(aoi) %>%
      terra::mask(aoi)
  }
  df <- as.data.frame(exp) %>%
    dplyr::mutate(exposure = round(exposure, 5))

  rules <- c("exposure == 0 ~ 0",
             utils::tail(c(sprintf("dplyr::between(exposure, %f, %f) ~ %f",
                                   lut$start, lut$end, lut$factor)), -1))

  lut2 <- as.factor(lut$factor)

  names(lut2) <- lut$label

  df <- df %>%
    dplyr::mutate(class = do.call(dplyr::case_when,
                                  c(lapply(rules, str2lang)))) %>%
    dplyr::mutate(class_range = names(lut2)[match(.data$class, lut2)])

  df$class_range <- factor(df$class_range, levels = names(lut2))


  df <- df %>%
    dplyr::count(.data$class_range) %>%
    dplyr::mutate(npixels = .data$n) %>%
    dplyr::mutate(prop = .data$npixels / sum(.data$npixels)) %>%
    dplyr::mutate(aream2 = .data$npixels * res * res) %>%
    dplyr::mutate(areaha = .data$aream2 / 10000) %>%
    dplyr::select(-"n")

  return(df)
}
