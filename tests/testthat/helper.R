haz <- function() {
  hazard_path <- system.file("extdata", "hazard.tif", package = "fireexposuR")
  terra::rast(hazard_path)

}

nb <- function() {
  nb_path <- system.file("extdata", "nb.tif", package = "fireexposuR")
  terra::rast(nb_path)
}

exposure <- function(nb) {
  if (missing(nb)) {
    fire_exp(haz())
  } else {
    fire_exp(haz(), no_burn = nb)
  }
}

polygon <- function() {
  polygon_path <- system.file("extdata", "polygon.shp", package = "fireexposuR")
  terra::vect(polygon_path)
}

pts <- function(n = 20) {
  haz <- haz()
  v <- polygon()
  terra::spatSample(v, n)
}


fires <- function(n = 20) {
  pts <- terra::spatSample(terra::rescale(haz(), 0.8), n, as.points = TRUE)
  terra::buffer(pts, 800)
}
