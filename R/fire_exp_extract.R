#' Extract exposure values to features
#'
#' @description `fire_exp_extract()` extracts the underlying exposure value for
#' each feature in the values layer.
#'
#'
#' @details
#' **DOCUMENTATION IN DEVELOPMENT**
#' Values can be provided as either points or
#' polygons, and must be singlepart features (i.e. the attribute table has one
#' row per value). If the values are polygon features both the maximum and mean
#' exposure is computed.
#'
#' @param exposure SpatRaster (e.g. from [fire_exp()])
#' @param values Spatvector of points or polygons
#'
#' @return a SpatVector object with new attributes
#'
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate example area of interest geometry
#' geom_file_path <- "extdata/polygon_geometry.csv"
#' geom <- read.csv(system.file(geom_file_path, package = "fireexposuR"))
#' aoi <- terra::vect(as.matrix(geom), "polygons", crs = hazard)
#'
#' # generate random points within the aoi polygon
#' points <- terra::spatSample(aoi, 100)
#'
#' # compute exposure
#' exposure <- fire_exp(hazard)
#'
#' fire_exp_extract(exposure, points)
#'

fire_exp_extract <- function(exposure,
                       values) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0 && round(terra::minmax(exposure)[2], 0) <= 1))
  stopifnot("`values` must be a SpatVector object of point or polygon features"
            = (class(values) == "SpatVector" &&
                 terra::geomtype(values) %in% c("points", "polygons")))

  names(exposure) <- "exposure"
  exp <- exposure

  if (terra::geomtype(values) == "points") {
    ext <- terra::extract(exp, values, bind = TRUE)
  } else {
    ext1 <- terra::extract(exp, values, fun = mean, bind = TRUE) %>%
           dplyr::rename(mean_exp = exposure)
    ext2 <- terra::extract(exp, values, fun = max, bind = TRUE) %>%
      dplyr::select("exposure") %>%
      dplyr::rename(max_exp = exposure)
    ext <- cbind(ext1, ext2)
  }
  return(ext)
}
