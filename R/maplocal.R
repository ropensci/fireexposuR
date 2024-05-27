maplocal <- function (exp, built, tdist = c("l", "s", "r")) {
  # project built for mapping
  b <- terra::project(built, "EPSG:3857")
  # get extent to clip tile
  e <- terra::rescale(b, 1.5)
  # mask exposure with built area, project for mapping
  expb <- terra::crop(exp, built, mask = T) %>%
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
  rcmats <- matrix(m, ncol=3, byrow=TRUE)

  # reclassify with local classified reclass matrix
  expbc <- terra::classify(expb, rcmats, include.lowest=TRUE)

  cols <- c("grey", "yellow", "orange", "red", "darkred")
  labs <- c("Nil", "Low", "Moderate", "High", "Extreme")

  tstr <- ifelse(tdist == "l", "Long-Range Ember Spotting",
                 ifelse(tdist == "s", "Short-Range Ember Spotting", "Radiant Heat"))

  title <- paste("Classified", tstr, "Transmission Exposure")
  cred <- maptiles::get_credit("Esri.WorldImagery")
  caption <- paste("Basemap", substr(cred, 1, 63), "\n", substr(cred, 63, nchar(cred)))

  plt <- ggplot2::ggplot(b) +
    tidyterra::geom_spatraster_rgb(data = tile, alpha = 0.9) +
    tidyterra::geom_spatraster(data = as.factor(expbc)) +
    tidyterra::geom_spatvector(fill = NA, linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = cols, labels = labs, na.value = NA, na.translate = F) +
    ggplot2::theme_void() +
    ggspatial::annotation_scale(location = 'bl') +
    ggspatial::annotation_north_arrow(location = "bl",
                           which_north = T,
                           pad_y = grid::unit(0.3, 'in'),
                           height = grid::unit(0.3, 'in'),
                           width = grid::unit(0.3, 'in')) +
    ggplot2::labs(title = title,
         subtitle = "Map generated with fireexposuR()",
         fill = "Exposure Class",
         caption = caption) +
    ggplot2::coord_sf(expand = F)

  return(plt)
}
