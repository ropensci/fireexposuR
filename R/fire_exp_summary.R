#' Summarize exposure by class
#'
#' @description `fire_exp_summary()` creates a summary table of exposure by the
#' specified classification scheme. The table reports the number of pixels, the
#' proportion, and area in hectares and meters squared by class.
#'
#' Landscape classification breaks are:
#' * Low (0-20%)
#' * Moderate (20-40%)
#' * High (40-60%),
#' * Very High (60-80%)
#' * Extreme (80-100%)
#'
#' Local classification breaks are:
#' * Nil (0%)
#' * Low (>0-15%)
#' * Moderate (15-30%)
#' * High (30-45%)
#' * Extreme (45%+)
#'
#' @param exposure SpatRaster from [fire_exp()]
#' @param aoi (optional) SpatVector of an area of interest to mask exposure for
#'   summary
#' @param classify character, either `"landscape"` or `"local"`. default is
#'   `"landscape"`.
#'
#' @returns a summary table as a data frame object
#' @export
#'
#' @examples
#' #' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # generate example AOI polygon -----------------------------
#' filepath <- "extdata/builtsimpleexamplegeom.csv"
#' g <- read.csv(system.file(filepath, package = "fireexposuR"))
#' v <- terra::vect(as.matrix(g), "polygons", crs = haz)
#' # ----------------------------------------------------------
#'
#' exp <- fire_exp(haz)
#'
#' # for full extent of data
#' fire_exp_summary(exp, classify = "landscape")
#'
#' # for a masked area
#' fire_exp_summary(exp, v, classify = "landscape")
#'
fire_exp_summary <- function(exposure, aoi,
                             classify = c("landscape", "local")) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("Linear units of exposure layer must be in meters"
            = terra::linearUnits(exposure) == 1)
  classify <- match.arg(classify)

  names(exposure) <- "exposure"
  exp <- exposure
  res <- terra::res(exp)[1]
  lut <- 0:5
  names(lut) <- c("Nil", "Low", "Moderate", "High", "Very High", "Extreme")

  if (!missing(aoi)) {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector")
    aoi <- terra::project(aoi, exp)

    exp <- exp %>%
      terra::crop(aoi) %>%
      terra::mask(aoi)
  }
  df <- as.data.frame(exp)
  if (classify == "landscape") {
    df <- df %>%
      dplyr::mutate(classexp = as.factor(
        dplyr::case_when(
          exposure >= 0.8 ~ 5,
          exposure >= 0.6 ~ 4,
          exposure >= 0.4 ~ 3,
          exposure >= 0.2 ~ 2,
          exposure >= 0 ~ 1
        )
      ))
  }
  if (classify == "local") {
    df <- df %>%
      dplyr::mutate(classexp = as.factor(
        dplyr::case_when(
          exposure >= 0.45 ~ 5,
          exposure >= 0.3 ~ 3,
          exposure >= 0.15 ~ 2,
          exposure > 0 ~ 1,
          exposure == 0 ~ 0
        )
      ))
  }


  df <- df %>%
    dplyr::count(.data$classexp) %>%
    dplyr::mutate(class = names(lut)[match(.data$classexp, lut)]) %>%
    dplyr::mutate(npixels = .data$n) %>%
    dplyr::mutate(prop = .data$npixels / sum(.data$npixels)) %>%
    dplyr::mutate(aream2 = .data$npixels * res * res) %>%
    dplyr::mutate(areaha = .data$aream2 / 10000) %>%
    dplyr::select(-c("classexp", "n"))

  return(df)
}
