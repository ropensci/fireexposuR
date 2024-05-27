maplandscape <- function(exp, aoi) {
  if (missing(aoi)) {
    r <- exp
  } else {
    r <- terra::crop(exp, aoi) %>%
      terra::mask(aoi)
  }
  plt <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    tidyterra::geom_spatvector(fill = NA) +
    tidyterra::scale_fill_whitebox_c(palette = "bl_yl_rd") +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Landscape Fire Exposure",
         subtitle = "Map generated with fireexposuR()",
         fill = "Exposure") +
    ggspatial::annotation_scale(location = 'bl') +
    ggspatial::annotation_north_arrow(location = "bl",
                           which_north = T,
                           pad_y = grid::unit(0.3, 'in'),
                           height = grid::unit(0.3, 'in'),
                           width = grid::unit(0.3, 'in')) +
    ggplot2::coord_sf(expand = F)
  return(plt)

}
