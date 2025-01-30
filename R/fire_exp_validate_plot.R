#' Validate exposure with observed fires
#'
#' @description For advanced users. `fire_exp_validate()` compares the
#'   proportion of exposure classes in a the study area to the proportion of
#'   exposure classes within burned areas. A random sample is taken to account
#'   for spatial autocorrelation.
#'
#' @details
#' **DOCUMENTATION IN DEVELOPMENT**
#'
#' @seealso [fire_exp_validate()]
#'
#' @param validation_table The output table from [fire_exp_validate()]
#'
#' @return a standardized ggplot
#'
#' @export
#'
#' @examples
#' # read example hazard data
#' hazard_file_path <- "extdata/hazard.tif"
#' hazard <- terra::rast(system.file(hazard_file_path, package = "fireexposuR"))
#'
#' # generate example non-burnable cells data
#' geom_file_path <- "extdata/polygon_geometry.csv"
#' geom <- read.csv(system.file(geom_file_path, package = "fireexposuR"))
#' polygon <- terra::vect(as.matrix(geom), "polygons", crs = hazard)
#' no_burn <- terra::rasterize(polygon, hazard)
#'
#' # generate example fire polygons by buffering random points
#' points <- terra::spatSample(terra::rescale(hazard, 0.8),
#'                             30, as.points = TRUE)
#' fires <- terra::buffer(points, 800)

#' # PLEASE NOTE THIS EXAMPLE DATA DOES NOT GENERATE MEANINGFUL RESULTS
#'
#' # compute exposure and remove non-burnable cells
#' exposure <- fire_exp(hazard, no_burn = no_burn)
#'
#' # results as table
#' validation_outputs <- fire_exp_validate(exposure, fires)
#'
#' # results as bar chart
#' fire_exp_validate_plot(validation_outputs)
#'

fire_exp_validate_plot <- function(validation_table) {
  props <- validation_table

  stopifnot("`validation_table` does not have expected attributes.
            Use outputs from `fire_exp_validate()`"
            = any(names(props) %in% c("exposure", "of", "prop")))

  props <- dplyr::arrange(props, dplyr::desc(.data$group))

  props$exposure <- as.factor(props$exposure)

  labs <- unique(props$exp_vals)

  plt <- ggplot2::ggplot(props, ggplot2::aes(x = .data$exposure,
                                             y = .data$prop,
                                             fill = .data$group,
                                             color = .data$group)) +
    ggplot2::geom_col(position = "identity", linetype = 2, linewidth = 0.8) +
    ggplot2::facet_grid(~.data$of) +
    ggplot2::scale_fill_manual(values = c("transparent", "gray")) +
    ggplot2::scale_color_manual(values = c("black", "transparent")) +
    ggplot2::scale_y_continuous(expand =
                                  ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_x_discrete(name = "Exposure Class",
                              labels = labs) +
    ggplot2::labs(y = "Proportion") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1))

  return(plt)
}
