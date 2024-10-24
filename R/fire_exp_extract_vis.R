#' Visualize by extracted exposure
#'
#' @description `fire_exp_extract_vis()` standardizes the visualization of
#' outputs from [fire_exp_extract()] as a summary table or a map by classifying
#' exposure into predetermined exposure classes.
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
#' @param values_ext Spatvector of points or polygons from [fire_exp_extract()]
#' @param classify character, either `"local"` or `"landscape"` to specify
#'   classification scheme to use. The default is `"local`"
#' @param method character, either `"max"` or `"mean"`. If `values_ext` are
#' polygons the default is `"max"`.This parameter is ignored when `values_ext`
#' are point features.
#' @param map Boolean. When `TRUE`, a map is returned as a ggplot object. The
#' default is `FALSE`.
#'
#' @return a summary table is returned as a data frame object, Unless:
#' `map = TRUE`: a ggplot object
#'
#' @export
#'
#' @examples
#' # read example hazard data ----------------------------------
#' filepath <- "extdata/hazard.tif"
#' haz <- terra::rast(system.file(filepath, package = "fireexposuR"))
#' # read example AOI polygon geometry
#' filepath <- "extdata/builtsimpleexamplegeom.csv"
#' g <- read.csv(system.file(filepath, package = "fireexposuR"))
#' v <- terra::vect(as.matrix(g), "polygons", crs = haz)
#' # generate random points within polygon
#' pts <- terra::spatSample(v, 200)
#' # ----------------------------------------------------------
#'
#' exp <- fire_exp(haz)
#'
#' vals_exp <- fire_exp_extract(exp, pts)
#'
#' # summarize example points in a table
#' fire_exp_extract_vis(vals_exp, classify = "local")
#'
#' # visualize example points in standardized map
#' fire_exp_extract_vis(vals_exp, map = TRUE)
#'
fire_exp_extract_vis <- function(values_ext,
                       method = c("max", "mean"),
                       classify = c("local", "landscape"),
                       map = FALSE) {
  ext <- values_ext
  stopifnot("`values_ext` must be a SpatVector object of point or polygon features"
            = (class(ext) == "SpatVector" &&
                 terra::geomtype(ext) %in% c("points", "polygons")))
  stopifnot("`values_ext` missing exposure attributes. Use fire_exp_extract() first"
            = length(terra::names(ext)) > 0)
  stopifnot("`values_ext` missing exposure attributes. Use fire_exp_extract() first"
            = any(terra::names(ext) %in% c("exposure", "mean_exp", "max_exp")))
  if (terra::geomtype(ext) == "polygons") {
    method <- match.arg(method)
    if (method == "mean"){
      method <- "Mean"
      ext <- ext %>%
        dplyr::rename(exposure = "mean_exp")
    }
    if (method == "max") {
      method <- "Maximum"
      ext <- ext %>%
        dplyr::rename(exposure = "max_exp")
    }
  } else {
    method <- "Point"
  }
  classify <- match.arg(classify)

  if (classify == "landscape") {
    title_str <- "Landscape Scale"
    ext <- ext %>%
      dplyr::mutate(scale = classify) %>%
      dplyr::mutate(
        exp_class = dplyr::case_when(
          exposure >= 0.8 ~ 5,
          exposure >= 0.6 ~ 4,
          exposure >= 0.4 ~ 3,
          exposure >= 0.2 ~ 2,
          exposure >= 0 ~ 1
        )
      )
  }
  if (classify == "local") {
    title_str <- "Localized Scale"
    ext <- ext %>%
      dplyr::mutate(scale = classify) %>%
      dplyr::mutate(
        exp_class = dplyr::case_when(
          exposure >= 0.45 ~ 5,
          exposure >= 0.3 ~ 3,
          exposure >= 0.15 ~ 2,
          exposure > 0 ~ 1,
          exposure == 0 ~ 0
        )
      )
  }

  lut <- 0:5
  names(lut) <- c("Nil", "Low", "Moderate", "High", "Very High", "Extreme")
  ext <- ext %>%
    dplyr::mutate(class = names(lut)[match(.data$exp_class, lut)])

  ext$class <- factor(ext$class, levels = names(lut))


  if (map == TRUE) {
    v <- terra::project(ext, "EPSG: 3857")
    e <- terra::rescale(v, 1.5)
    tile <- maptiles::get_tiles(e, "Esri.WorldGrayCanvas") %>%
      terra::crop(e)

    cols <- c(
      "Nil" = "grey",
      "Low" = "yellow",
      "Moderate" = "orange",
      "High" = "darkorange",
      "Very High" = "red",
      "Extreme" = "darkred"
    )


    plt <- ggplot2::ggplot() +
      tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.8) +
      ggspatial::annotation_scale(location = "bl") +
      ggspatial::annotation_north_arrow(
        location = "bl",
        which_north = TRUE,
        pad_y = grid::unit(0.3, "in"),
        height = grid::unit(0.3, "in"),
        width = grid::unit(0.3, "in")
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title = paste(method,"Classified Exposure to Values,", title_str),
        subtitle = "Map generated with fireexposuR()",
        caption = maptiles::get_credit("Esri.WorldGrayCanvas")
      )

    if (terra::geomtype(v) == "points") {
      plt <- plt +
        tidyterra::geom_spatvector(data = v,
                                   ggplot2::aes(color = factor(.data$class)),
                                   shape = 16) +
        ggplot2::scale_color_manual(values = cols) +
        ggplot2::labs(color = "Exposure Class") +
        ggplot2::coord_sf(expand = FALSE)
    } else {
      plt <- plt +
        tidyterra::geom_spatvector(data = v,
                                   ggplot2::aes(fill = factor(.data$class)),
                                   color = NA) +
        ggplot2::scale_fill_manual(values = cols) +
        ggplot2::labs(fill = "Exposure Class") +
        ggplot2::coord_sf(expand = FALSE)
    }
    return(plt)
  } else {
      df <- as.data.frame(ext) %>%
        dplyr::count(class) %>%
        dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
        dplyr::mutate(method = method) %>%
        dplyr::mutate(scale = classify) %>%
        dplyr::select(c("scale", "method", "class", "n", "prop"))
      return(df)
      }
    }

