#' Map exposure with a continuous scale
#'
#' @description `mapexpcont()` produces a standardized map of exposure with a
#'   continuous scale. An optional area of interest can be used as a mask. The
#'   ggplot object returned can be further modified with the ggplot library.
#'
#' @param exposure SpatRaster from [exposure()]
#' @param aoi (optional) SpatVector of an area of interest to mask the exposure
#'
#' @return a map is returned as a ggplot object
#' @export
#' @seealso [exposure()], [ggplot()]
#' @examples
#'
#' #' # generate example hazard data -----------------------------
#' set.seed(0)
#' e <- c(45,55,495,505) * 10000
#' r <- terra::rast(resolution = 100, extent = terra::ext(e))
#' terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
#' terra::crs(r) <- "EPSG:32608"
#' r <- terra::sieve(r, threshold = 50, directions = 4)
#' haz <- terra::sieve(r, threshold = 500, directions = 4)
#' # -------------------------------------------------------------
#'
#' exp <- exposure(haz)
#' mapexpcont(exp)
#'
mapexpcont <- function(exposure, aoi) {
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
