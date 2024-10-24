#' Map exposure with a classified scale
#'
#' @description `fire_exp_map_class()` produces a standardized map of exposure with
#' exposure classes. This function dynamically pulls map tiles for a base map,
#' so it is recommended the area of interest is localized. The zoom level may
#' need to be adjusted based on the extent of your data; see
#' [OpenStreetMap Wiki](https://wiki.openstreetmap.org/wiki/Zoom_levels) for
#' more information on zoom levels.
#' For mapping large extents it is recommended (and will be faster) to use
#' [fire_exp_map_cont()] which does not use base maps.
#'
#' Scales and colors are determined by the parameter `classify`
#' which can be set to `"local"` or `"landscape"`.
#'
#' Landscape classification breaks are:
#' * Low (0-20%)
#' * Moderate (20-40%)
#' * High (40-60%),
#' * Very High (60-80%)
#' * Extreme (80-100%)
#'
#' Local classification breaks are:
#' * Nil (0%)
#' * Low (>0-15%)
#' * Moderate (15-30%)
#' * High (30-45%)
#' * Extreme (45%+)
#'
#'
#'
#' @param exposure SpatRaster (e.g. from [fire_exp()])
#' @param classify character, either `"local"` or `"landscape"` to specify
#'   classification scheme to use.
#' @param aoi SpatVector of an area of interest to mask exposure
#' @param zoom (Optional). numeric, set the zoom level for the basemap based
#' on the extent of your data if defaults are not appropriate. Defaults if:
#' * `classify = "local"` the zoom level default is `13`
#' * `classify = "landscape"` the zoom level default is `6`
#'
#'
#' @return a standardized map is returned as a ggplot object
#' @export
#'
#' @examples
#' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # generate example AOI polygon -----------------------------
#' filepath <- "extdata/builtsimpleexamplegeom.csv"
#' g <- read.csv(system.file(filepath, package = "fireexposuR"))
#' v <- terra::vect(as.matrix(g), "polygons", crs = haz)
#' # ----------------------------------------------------------
#'
#' exp <- fire_exp(haz)
#' fire_exp_map_class(exp, classify = "local", v)
#'

fire_exp_map_class <- function(exposure, classify = c("local", "landscape"),
                        aoi, zoom) {
  stopifnot("`exposure` must be a SpatRaster object"
            = class(exposure) == "SpatRaster")
  stopifnot("`exposure` layer must have values between 0-1"
            = (round(terra::minmax(exposure)[1], 0) >= 0 && round(terra::minmax(exposure)[2], 0) <= 1))
  stopifnot("`aoi` must be a SpatVector object"
            = class(aoi) == "SpatVector")
  stopifnot("`aoi` extent must be within `exposure` extent"
            = terra::relate(aoi, exposure, "within"))
  stopifnot("`exposure` and `aoi` must have same CRS"
            = terra::same.crs(exposure, aoi))
  classify <- match.arg(classify)

  names(exposure) <- "exposure"
  exp <- exposure

  # project aoi for mapping
  b <- terra::project(aoi, "EPSG:3857")
  # get extent to clip tile
  e <- terra::rescale(b, 1.5)
  # mask exposure to aoi, project for mapping
  expb <- terra::crop(exp, aoi, mask = TRUE) %>%
    terra::project("EPSG:3857")


  if (classify == "local") {
    zl <- ifelse(missing(zoom), 13, zoom)
    tile <- maptiles::get_tiles(e, "Esri.WorldImagery", zoom = zl) %>%
      terra::crop(e)
    cred <- maptiles::get_credit("Esri.WorldImagery")
    caption <- paste("Basemap", substr(cred, 1, 63), "\n",
                     substr(cred, 63, nchar(cred)))
    # reclassify the values for local scale
    m <- c(0, 0, 0,
           0, 0.15, 1,
           0.15, 0.3, 2,
           0.3, 0.45, 3,
           0.45, 1, 4)
    rcmats <- matrix(m, ncol = 3, byrow = TRUE)
    expbc <- terra::classify(expb, rcmats, include.lowest = TRUE)
    levels(expbc) <- data.frame(id = 0:4,
                                expclass = c("Nil (0%)", "Low (>0-15%)",
                                             "Moderate (15-30%)",
                                             "High (30-45%)",
                                             "Extreme (45%+)"))
    cols <- c("grey", "yellow", "orange", "red", "darkred")
  } else {
    zl <- ifelse(missing(zoom), 6, zoom)
    tile <- maptiles::get_tiles(e, "Esri.WorldGrayCanvas", zoom = zl) %>%
      terra::crop(e)
    caption <- maptiles::get_credit("Esri.WorldGrayCanvas")
    # reclassify the values for landscape scale
    m <- c(0, 0.2, 0,
           0.2, 0.4, 1,
           0.4, 0.6, 2,
           0.6, 0.8, 3,
           0.8, 1, 4)
    rcmats <- matrix(m, ncol = 3, byrow = TRUE)
    expbc <- terra::classify(expb, rcmats, include.lowest = TRUE)
    levels(expbc) <- data.frame(id = 0:4,
                                expclass = c("Low (0-20%)", "Moderate (20-40%)",
                                             "High (40-60%)",
                                             "Very High (60-80%)",
                                             "Extreme (80-100%)"))
    cols <- tidyterra::whitebox.colors(5, palette = "bl_yl_rd")

  }


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
