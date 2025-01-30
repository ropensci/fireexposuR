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
#' @seealso [fire_exp_validate_plot()]
#'
#' @param burnableexposure A SpatRaster of exposure, non-burnable cells should
#'    be removed using optional parameter `no_burn = `in [fire_exp()].
#' @param fires A SpatVector of observed fire perimeters
#' @param aoi (Optional) A SpatVector that delineates an area of interest
#' @param class_breaks (Optional) vector of numeric values between 0-1 of the
#' upper limits of each class. The default is `c(0.2, 0.4, 0.6, 0.8, 1)`. See
#' details.
#' @param samplesize Proportion of areas to sample. The default is `0.005`
#' (0.5%)
#'
#' @return a table of number of cells (n) and proportions (prop) for
#' expected and observed exposure classes within a sampled area (Sample) and
#' across the full extent (Total).
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
#' # validation table
#' fire_exp_validate(exposure, fires)
#'

fire_exp_validate <- function(burnableexposure, fires, aoi,
                              class_breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                              samplesize = 0.005) {
  names(burnableexposure) <- "exposure"
  expb <- burnableexposure
  stopifnot("`burnableexposure` must be a SpatRaster object"
            = class(expb) == "SpatRaster")
  stopifnot("Linear units of `exposure` layer must be in meters"
            = terra::linearUnits(expb) == 1)
  stopifnot("`exposure` layer must have values between 0-1"
            = (round(terra::minmax(expb)[1], 0) >= 0
               && round(terra::minmax(expb)[2], 0) <= 1))
  stopifnot("`fires` must be a SpatVector object"
            = class(fires) == "SpatVector")
  if (!missing(aoi)) {
    stopifnot("`aoi` must be a SpatVector object"
              = class(aoi) == "SpatVector")
  }

  # class_breaks checks
  stopifnot("`class_breaks` must be a vector of numbers"
            = class(class_breaks) == "numeric")
  stopifnot("`class_breaks` must have 1 as the maximum value"
            = max(class_breaks) == 1)
  stopifnot("`class_breaks` must be greater than 0"
            = class_breaks > 0)

  class_labels <- character()

  label_breaks <- c(0, class_breaks)
  for (i in seq_along(label_breaks)) {
    class_labels[i] <- paste(label_breaks[i], "-", label_breaks[i + 1])
  }

  class_labels <- c("Nil", utils::head(class_labels, -1))

  lut <- data.frame(start = c(0, 0, utils::head(class_breaks, -1)),
                    end = c(0, class_breaks),
                    factor = 0:length(class_breaks),
                    label = class_labels)

  rcmats <- as.matrix(lut[, 1:3])

  classexp <- terra::classify(expb, rcmats, include.lowest = TRUE)

  if (missing(aoi)) {
    studyarea <- classexp
    studyareafires <- fires
  } else {
    studyarea <- terra::crop(classexp, aoi, overwrite = TRUE) %>%
      terra::mask(aoi)
    studyareafires <- terra::crop(fires, aoi)
  }

  firesarea <- terra::crop(studyarea, studyareafires, overwrite = TRUE) %>%
    terra::mask(studyareafires)

  df1 <- dplyr::count(as.data.frame(studyarea), .data$exposure) %>%
    dplyr::mutate(of = "Total") %>%
    dplyr::mutate(group = "Expected") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))


  df2 <- dplyr::count(as.data.frame(firesarea), .data$exposure) %>%
    dplyr::mutate(of = "Total") %>%
    dplyr::mutate(group = "Observed") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))

  samplestudyareasize <- round(sum(df1$n) * samplesize)
  samplefiresareasize <- round(sum(df2$n) * samplesize)

  props <- rbind(df1, df2)

  df3 <- dplyr::count(terra::spatSample(studyarea,
                                        samplestudyareasize,
                                        na.rm = TRUE,
                                        as.df = TRUE,
                                        method = "random"),
                      .data$exposure) %>%
    dplyr::mutate(of = "Sample") %>%
    dplyr::mutate(group = "Expected") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))

  props <- rbind(props, df3)

  df4 <- dplyr::count(terra::spatSample(firesarea,
                                        samplefiresareasize,
                                        na.rm = TRUE,
                                        as.df = TRUE,
                                        method = "random"),
                      .data$exposure) %>%
    dplyr::mutate(of = "Sample") %>%
    dplyr::mutate(group = "Observed") %>%
    dplyr::mutate(prop = .data$n / sum(.data$n))

  lut2 <- as.factor(lut$factor)

  names(lut2) <- lut$label

  props <- rbind(props, df4) %>%
    dplyr::mutate(exp_vals = names(lut2)[match(.data$exposure, lut2)]) %>%
    dplyr::select("exposure", "exp_vals",
                  "of", "group", "n", "prop")


  return(props)
}
