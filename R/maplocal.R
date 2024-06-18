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
#' # generate example hazard data -----------------------------
#' set.seed(0)
#' e <- c(45,55,495,505) * 10000
#' r <- terra::rast(resolution = 100, extent = terra::ext(e))
#' terra::values(r) <- sample(c(0,1), terra::ncell(r), replace = TRUE)
#' terra::crs(r) <- "EPSG:32608"
#' r <- terra::sieve(r, threshold = 50, directions = 4)
#' haz <- terra::sieve(r, threshold = 500, directions = 4)
#' # generate example AOI polygon -----------------------------
#' filepath <- "extdata/builtsimpleexamplegeom.csv"
#' g <- read.csv(system.file(filepath, package = "fireexposuR"))
#' m <- as.matrix(g)
#' v <- terra::vect(m, "polygons", crs = haz)
#' # ----------------------------------------------------------
#'
#' exp <- exposure(haz)
#' maplocal(exp, v)
#'

maplocal <- function(exposure, aoi) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`aoi` must be a SpatVector object"
            = class(aoi) == "SpatVector")
  names(exposure) <- 'exposure'
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
