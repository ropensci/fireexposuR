#' Map continuous exposure across a landscape
#'
#' @description `maplandscape()` is a helper function to produce a standardized
#' map of exposure across a landscape with a continuous scale. The ggplot object
#' returned can be further modified with the ggplot library.
#'
#' @param exposure SpatRaster from [exposure()]
#' @param aoi (optional) SpatVector of an area of interest to mask exposure for
#'   summary
#'
#' @return a map is returned as a ggplot object
#' @export
#' @seealso [exposure()], [ggplot()]
#'
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
#' maplandscape(exp)
#'
#' # IGNORE BELOW
#' # filepath <- "extdata/LExpAB2020.tif"
#' # lexp <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # filepath <- "extdata/fpa.shp"
#' # fpa <- terra::vect(system.file(filepath, package = "fireexposuR"))
#'
maplandscape <- function(exposure, aoi) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")

  exp <- exposure
  if (missing(aoi)) {
    r <- exp
  } else {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector")
    r <- terra::crop(exp, aoi) %>%
      terra::mask(aoi)
  }
  plt <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    tidyterra::geom_spatvector(fill = NA) +
    tidyterra::scale_fill_whitebox_c(palette = "bl_yl_rd",
                                     limits = c(0,1)) +
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
