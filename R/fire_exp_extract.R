#' Extract exposure values to features
#'
#' @description `fire_exp_extract()` extracts the underlying exposure value for
#' each feature in the values layer. Values can be provided as either points or
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
#' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # read example AOI
#' filepath <- "extdata/builtsimpleexamplegeom.csv"
#' g <- read.csv(system.file(filepath, package = "fireexposuR"))
#' v <- terra::vect(as.matrix(g), "polygons", crs = haz)
#' # generate random points
#' pts <- terra::spatSample(v, 200)
#' # ----------------------------------------------------------
#'
#' exp <- fire_exp(haz)
#'
#' # extract exposure to point values
#' fire_exp_extract(exp, pts)
#'

fire_exp_extract <- function(exposure,
                       values) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
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
      dplyr::rename(max_exp = exposure) %>%
      dplyr::select("max_exp")
    ext <- cbind(ext1, ext2)
  }
  return(ext)
}
