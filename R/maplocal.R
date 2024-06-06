#' Map classified exposure for a localized area
#'
#' @description `maplocal()` is a helper function to produce a standardized map
#'   of exposure within a localized area with exposure classes. The ggplot
#'   object returned can be further modified with the ggplot library.
#'
#'   #' Local classification breaks are:
#' * Nil (0%)
#' * Low (>0-15%)
#' * Moderate (15-30%)
#' * High (30-45%)
#' * Extreme (45%+)
#'
#' @param exposure SpatRaster from [exposure()]
#' @param aoi SpatVector of an area of interest to mask exposure for summary
#'
#' @return a standardized map is returned as a ggplot object
#' @export
#' @seealso [exposure()], [ggplot()]
#'
#' @examples
#' filepath <- "extdata/LExpAB2020.tif"
#' lexp <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' filepath <- "extdata/WHITbuilt.shp"
#' WHITbuilt <- terra::vect(system.file(filepath, package = "fireexposuR"))
#'
#' maplocal(lexp, WHITbuilt)
#'

maplocal <- function(exposure, aoi) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`aoi` must be a SpatVector object"
            = class(aoi) == "SpatVector")

  exp <- exposure
  built <- aoi
  # project built for mapping
  b <- terra::project(built, "EPSG:3857")
  # get extent to clip tile
  e <- terra::rescale(b, 1.5)
  # mask exposure with built area, project for mapping
  expb <- terra::crop(exp, built, mask = TRUE) %>%
    terra::project("EPSG:3857")
  # get tile from mapping service
  tile <- maptiles::get_tiles(e, "Esri.WorldImagery", zoom = 13) %>%
    terra::crop(e)

  # reclassify the values for local scale
  m <- c(0, 0, 0,
         0, 0.15, 1,
         0.15, 0.3, 2,
         0.3, 0.45, 3,
         0.45, 1, 4)
  rcmats <- matrix(m, ncol = 3, byrow = TRUE)

  # reclassify with local classified reclass matrix
  expbc <- terra::classify(expb, rcmats, include.lowest = TRUE)
  levels(expbc) <- data.frame(id = 0:4,
                              expclass = c("Nil", "Low", "Moderate",
                                           "High", "Extreme"))

  cols <- c("grey", "yellow", "orange", "red", "darkred")


  cred <- maptiles::get_credit("Esri.WorldImagery")
  caption <- paste("Basemap", substr(cred, 1, 63), "\n",
                   substr(cred, 63, nchar(cred)))

  plt <- ggplot2::ggplot(b) +
    tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.9) +
    tidyterra::geom_spatraster(data = expbc, alpha = 0.8) +
    tidyterra::geom_spatvector(fill = NA, linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = cols,
                               na.value = NA,
                               na.translate = FALSE) +
    ggplot2::theme_void() +
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(location = "bl",
                                      which_north = TRUE,
                                      pad_y = grid::unit(0.3, "in"),
                                      height = grid::unit(0.3, "in"),
                                      width = grid::unit(0.3, "in")) +
    ggplot2::labs(title = "Classified Exposure",
                  subtitle = "Map generated with fireexposuR()",
                  fill = "Exposure Class",
                  caption = caption) +
    ggplot2::coord_sf(expand = FALSE)

  return(plt)
}
