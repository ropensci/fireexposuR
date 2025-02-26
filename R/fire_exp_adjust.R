#' Compute the wildfire exposure metric with custom transmission distances
#'
#' @description For advanced users. Adjust the transmission distances from the
#' defaults used in [fire_exp()].
#'
#' @details
#' If the transmission distances from the wildfire exposure literature are not
#' representative of the wildland fuels in your area of interest, this function
#' can be used to change the transmission distance to a custom distance. It is
#' highly recommended that any exposure layers produced with this function are
#' validated with observed fire history using the [fire_exp_validate()]
#' function.
#'
#' ### Spatial Reference
#'
#' The exposure raster will be returned in the same CRS as the input hazard
#' layer. A crs must be defined if the outputs will be used in other functions
#' in this package.
#'
#' ### Spatial resolution
#'
#' For a specified transmission distance, the spatial resolution must be at
#' least 3 times finer. For example, for a transmission distance of 300 meters
#' the input data should have a resolution of 100 meters or finer.
#'
#' @seealso [fire_exp()] for background information
#'
#' @param hazard a SpatRaster that represents hazardous fuels for the
#'   transmission distance specified in `tdist`
#' @param tdist Numeric, transmission distance in meters
#' @param no_burn (Optional)  SpatRaster that represents the non-burnable
#'   landscape. Any cells that cannot receive wildfire (e.g. open water, rock)
#'   and any cells that are not natural (e.g. built environment,
#'   irrigated agricultural areas) should be of value 1, all other cells
#'   should be NODATA. This parameter should be provided if preparing data
#'   for [fire_exp_validate()]
#'
#' @return SpatRaster object of exposure values
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # compute long range exposure with custom disance of 800 m
#' exposure800 <- fire_exp_adjust(hazard, tdist = 800)
#'
fire_exp_adjust <- function(hazard, tdist, no_burn) {
  stopifnot("`hazard` must be a SpatRaster object"
            = class(hazard) == "SpatRaster",
            "`hazard` layer must have values between 0-1"
            = (round(terra::minmax(hazard)[1]) >= 0
               && round(terra::minmax(hazard)[2]) <= 1),
            "`tdist` must be numeric"
            = is.numeric(tdist))
  if (terra::crs(hazard) == "") {
    message("Input CRS is undefined: If output will be used in
            other fireexposuR functions a CRS must be defined")
  }
  if (!missing(no_burn)) {
    stopifnot("`no_burn` must be a SpatRaster object"
              = class(no_burn) == "SpatRaster",
              "`no_burn` must be a SpatRaster object"
              = class(no_burn) == "SpatRaster",
              "`no_burn` and `hazard` must have same CRS"
              = terra::same.crs(hazard, no_burn),
              "`no_burn` must only contain values of 1 or NA"
              = unique(terra::values(no_burn) %in% c(1, NA, NaN)),
              "`no_burn` extent must be within `hazard` extent"
              = terra::relate(no_burn, hazard, "within"))
  }

  res <- terra::res(hazard)[1]
  stopifnot("insufficient resolution for chosen exposure transmission distance"
            = res <= tdist / 3)


  annulus <- c(res, tdist)
  window <- MultiscaleDTM::annulus_window(annulus, "map", res)
  wgtwindow <- window / sum(window, na.rm = TRUE)
  exp <- terra::focal(hazard, wgtwindow, fun = sum) %>%
    tidyterra::rename(exposure = "focal_sum")
  if (missing(no_burn)) {
    return(exp)
  } else {
    expb <- terra::mask(exp, no_burn, inverse = TRUE)
    return(expb)
  }
}
