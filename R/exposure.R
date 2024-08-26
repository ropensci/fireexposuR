#' Compute the wildfire exposure metric
#'
#' `exposure()` computes the wildfire exposure metric from a hazard fuel raster.
#' The hazard fuel raster must be prepared by the user. Forbes and Beverly 2024
#' (manuscript in preparation)
#' details suggestions for data acquisition and preparation in accordance with
#' various budget limitations and user experience levels. Computing wildfire
#' exposure for different transmission distances may require unique hazard
#' rasters for each distance. See Beverly et al. 2021, Beverly et al. 2010, and
#' Schmidt et al. 2024 for further references for hazard raster preparation.
#'
#' @param hazard a SpatRaster that represents hazardous fuels for the
#'   transmission distance specified in tdist
#' @param tdist a character vector, can be: `"l"` for long-range
#'   embers (Default), `"s"` for short-range embers or, `"r"` for radiant heat
#' @param nonburnable (optional) a SpatRaster that represents the burnable
#'   landscape. Any cells that cannot receive wildfire (e.g. open water, rock)
#'   should be of value 1, all other cells should be NODATA. This parameter
#'   should be provided if preparing data for [validateexp()]
#'
<<<<<<< HEAD
#' @return A SpatRaster object of exposure values between 0-1
=======
#' @return A SpatRaster object of exposure values
#'
#' @details
#' There are minimum raster resolution requirements for each transmission distance.
#' Radiant heat requires data of 10 m or finer, short-range embers 90 m or finer, and
#' long-range embers requires 150 m or finer.
#'
#'
>>>>>>> c6c2262e18f136e87f06c245a49cc5934e660c78
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' # generate example hazard data -----------------------------
#' set.seed(0)
#' e <- c(45,55,495,505) * 10000
#' r <- terra::rast(resolution = 100, extent = terra::ext(e))
#' terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
#' r <- terra::sieve(r, threshold = 50, directions = 4)
#' haz <- terra::sieve(r, threshold = 500, directions = 4)
#' # -----------------------------------------------------------
#'
#' # compute long range eposure from
#' exp <- exposure(haz, tdist = "l")
#' exp
#'
#' # each transmission distance has a resolution requirement and exposure() will
#' # not run if resolution is too coarse
#' try(exposure(haz, tdist = "r"))
#'
exposure <- function(hazard, tdist = c("l", "s", "r"), nonburnable) {
  stopifnot("`hazard` must be a SpatRaster object"
            = class(hazard) == "SpatRaster")
  stopifnot("`hazard` layer must have values between 0-1"
            = (terra::minmax(hazard)[1] >= 0 && terra::minmax(hazard)[2] <= 1))
  tdist <- match.arg(tdist)

  if (terra::crs(hazard, describe = TRUE)$name == "unknown") {
    message("Input CRS is undefined: If exposure() output will be used in
            other fireexposur() functions a CRS must be defined")
  }

  haz <- hazard
  res <- terra::res(haz)[1]

  if (tdist == "l") {
    stopifnot("Insufficient resolution for longrange ember exposure assessment"
              = res <= 150)
    annulus <- c(res, 500)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  if (tdist == "s") {
    stopifnot("Insuffucient resolution for shortrange ember exposure assessment"
              = res <= 33)
    annulus <- c(res, 100)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  if (tdist == "r") {
    stopifnot("Insufficient resolution for radiant heat exposure assessment"
              = res <= 10)
    annulus <- c(res, 30)
    window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  }
  stopifnot("Extent of hazard raster too small for exposure assessment"
            = terra::nrow(window) < 2 * terra::nrow(haz))
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(haz, wgtwindow, fun = sum) %>%
    tidyterra::rename(exposure = .data$focal_sum)
  if (missing(nonburnable)) {
    return(exp)
  } else {
    stopifnot("`nonburnable` must be a SpatRaster object"
              = class(nonburnable) == "SpatRaster")
    expb <- terra::mask(exp, nonburnable, inverse = TRUE)
    return(expb)
  }
}
