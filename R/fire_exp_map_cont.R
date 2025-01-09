#' Map exposure with a continuous scale
#'
#' @description `fire_exp_map_cont()` produces a standardized map of exposure
#' with a continuous scale for the full extent of the data or masked to an area
#' of interest.
#'
#' @details
#' **DOCUMENTATION IN DEVELOPMENT**
#'
#' @param exposure SpatRaster from [fire_exp()]
#' @param aoi (optional) SpatVector of an area of interest to mask the exposure
#'
#' @return a map is returned as a ggplot object
#' @export
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # Compute exposure
#' exposure <- fire_exp(hazard)
#'
#' fire_exp_map_cont(exposure)
#'
fire_exp_map_cont <- function(exposure, aoi) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`exposure` layer must have values between 0-1"
            = (terra::minmax(exposure)[1] >= 0 && terra::minmax(exposure)[2] <= 1))
  exp <- exposure
  if (missing(aoi)) {
    r <- exp
  } else {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector")
    stopifnot("`aoi` extent must be within `exposure` extent"
              = terra::relate(aoi, exposure, "within"))
    stopifnot("`exposure` and `aoi` must have same CRS"
              = terra::same.crs(exposure, aoi))
    r <- terra::crop(exp, aoi) %>%
      terra::mask(aoi)
  }
  plt <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    tidyterra::geom_spatvector(fill = NA) +
    tidyterra::scale_fill_whitebox_c(palette = "bl_yl_rd",
                                     limits = c(0, 1)) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Landscape Fire Exposure",
                  subtitle = "Map generated with fireexposuR()",
                  fill = "Exposure") +
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = TRUE,
      pad_y = grid::unit(0.3, "in"),
      height = grid::unit(0.3, "in"),
      width = grid::unit(0.3, "in")
    ) +
    ggplot2::coord_sf(expand = FALSE)
  return(plt)

}
