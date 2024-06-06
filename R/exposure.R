#' Compute the wildfire exposure metric
#'
#' `exposure()` computes the wildfire exposure metric from a hazard fuel raster.
#' The hazard fuel raster must be prepared by the user. Forbes and Beverly 2024
#' details suggestions for data acquisition and preparation in accordance with
#' various budget limitations and user experience levels. Computing wildfire
#' exposure for different transmission distances may require unique hazard rasters
#' for each distance. See Beverly et al. 2021, Beverly et al. 2010, and Schmidt
#' et al. 2024 for further references for hazard raster preparation.
#'
#' @param hazard a SpatRaster that represents hazardous fuels for the
#'     transmission distance specified in tdist
#' @param tdist a character vector, can be:
#'      * `"l"` for long-range embers (Default)
#'      * `"s"` for short-range embers
#'      * `"r"` for radiant heat
#' @param nonburnable (optional) a SpatRaster that represents the burnable
#'     landscape. Any cells that cannot receive wildfire (e.g. open water,
#'     rock) should be of value 1, all other cells should be NODATA. This
#'     parameter should be provided if preparing data for [validateexp()]
#'
#' @return A SpatRaster object of exposure values
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' lhaz <- terra::rast(system.file("extdata/LHazAB2020.tif", package = "fireexposuR"))
#' nonburnable <- terra::rast(system.file("extdata/nonburnableAB2020.tif", package = "fireexposuR"))
#' lexp <- exposure(lhaz, tdist = "l")
#' lexpburnable <- exposure(lhaz, tdist = "l", nonburnable)
#'
#' # each transmission distance has a resolution requirement and exposure will
#' # not run if resolution is insufficient
#' try(exposure(lhaz, tdist = "s"))
#'
exposure <- function(hazard, tdist = c("l", "s", "r"), nonburnable) {
  stopifnot("`hazard` must be a SpatRaster object" = class(hazard) == "SpatRaster")
  stopifnot("`hazard` layer must have values between 0-1" =
              (terra::minmax(hazard)[1] >= 0 && terra::minmax(hazard)[2] <= 1))
  tdist <- match.arg(tdist)

  haz <- hazard
  res <- terra::res(haz)[1]
  if (tdist == "l") {
    stopifnot("Insufficient data resolution for Long-Range Ember Exposure Assessment" = res < 150)
    annulus <- c(res, 500)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  if (tdist == "s") {
    stopifnot("Insufficient data resolution for Short-Range Ember Exposure Assessment" = res < 33)
    annulus <- c(res, 100)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  if (tdist == "r") {
    stopifnot("Insufficient data resolution for Radiant Heat Exposure Assessment" = res < 10)
    annulus <- c(res, 30)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(haz, wgtwindow, fun = sum) %>%
    tidyterra::rename(exposure = .data$focal_sum)
  if (missing(nonburnable)) {
    return(exp)
  } else {
    stopifnot("`nonburnable` must be a SpatRaster object" = class(nonburnable) == "SpatRaster")
    expb <- terra::mask(exp, nonburnable, inverse = TRUE)
    return(expb)
  }
}
