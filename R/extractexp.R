#' Title
#'
#' @param exposure spatraster from exposure()
#' @param values spatvector of points or polygons
#' @param method character, either "max" or "mean". required when values are
#'      polygons
#' @param classify character, either "local" or "landscape" to specify
#'      classification style
#' @param summary logical, if True returns a summary table
#' @param map logical, if true returns a ggplot map of values coloured by
#'      classification
#'
#' @return spatvector unless summary or map = T
#' @export
#'
#' @examples
extractexp <- function(exposure,
                       values,
                       method = c("max", "mean"),
                       classify = c("landscape", "local"),
                       summary = FALSE,
                       map = FALSE) {
  exp <- exposure
  if (terra::geomtype(values) == "points") {
    ext <- terra::extract(exp, values, bind = TRUE)
  } else {
    if (terra::geomtype(values) != "polygons") {
      stop("Values feature must be points or polygons")
    }
    if (missing(method)) {
      stop("Must specify a method if values are polygon features")
    }
    if (method == "mean") {
      ext <- terra::extract(exp, values, fun = mean, bind = TRUE)
    } else {
      ext <- terra::extract(exp, values, fun = max, bind = TRUE)
    }
  }
  if (missing(classify)) {
    if (summary == FALSE && map == FALSE) {
      return(ext)
      stop()
    } else {
      return(ext)
      stop("Must set classify parameter to 'local' or 'landscape' if
           either summary or map are true")
    }
  }
  if (classify == "landscape") {
    ext <- ext %>%
      dplyr::mutate(scale = classify) %>%
      dplyr::mutate(
        classexp = dplyr::case_when(
          exposure >= 0.8 ~ 5,
          exposure >= 0.6 ~ 4,
          exposure >= 0.4 ~ 3,
          exposure >= 0.2 ~ 2,
          exposure >= 0 ~ 1
        )
      )
  } else if (classify == "local") {
    ext <- ext %>%
      dplyr::mutate(scale = classify) %>%
      dplyr::mutate(
        classexp = dplyr::case_when(
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
    dplyr::mutate(class = names(lut)[match(.data$classexp, lut)])

  if (summary == TRUE) {
    df <- as.data.frame(ext) %>%
      dplyr::count(class) %>%
      dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
      dplyr::select(c(.data$class, .data$n, .data$prop))
    return(df)
    if (map == TRUE) {
      stop("Only one of summary or map can be set to true")
    }
  }
  if (map == TRUE) {
    v <- terra::project(ext, "EPSG: 3857")
    e <- terra::rescale(v, 1.1)
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
        title = "Classified Exposure to Values",
        subtitle = "Map generated with fireexposuR()",
        caption = maptiles::get_credit("Esri.WorldGrayCanvas")
      )

    if (terra::geomtype(v) == "points") {
      plt <- plt +
        tidyterra::geom_spatvector(data = v,
                                   ggplot2::aes(color = factor(class)),
                                   size = 1) +
        ggplot2::scale_color_manual(values = cols) +
        ggplot2::labs(color = paste("Exposure Class (", classify, ")",
                                    sep = "")) +
        ggplot2::coord_sf(expand = FALSE)
    } else {
      plt <- plt +
        tidyterra::geom_spatvector(data = v,
                                   ggplot2::aes(fill = factor(.data$classexp)),
                                   color = NA) +
        ggplot2::scale_fill_manual(values = cols, labels = names(cols)) +
        ggplot2::labs(fill = paste("Exposure Class (", classify, ")",
                                   sep = "")) +
        ggplot2::coord_sf(expand = FALSE)
    }
    return(plt)
    stop()
  } else {
    return(ext)
  }
}
