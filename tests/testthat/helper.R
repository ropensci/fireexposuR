haz <- function() {
  hazard_path <- system.file("extdata", "hazard.tif", package ="fireexposuR")
  haz <- terra::rast(hazard_path)
}

exposure <- function(nb) {
  if (missing(nb)) {
    fire_exp(haz())
  } else {
    fire_exp(haz(), no_burn = nb)
  }
}

pol <- function() {
  haz <- haz()
  geo_path <- system.file("extdata", "polygon_geometry.csv", package ="fireexposuR")
  geo <- read.csv(geo_path)
  terra::vect(as.matrix(geo), "polygons", crs = haz)
}

pts <- function(n = 20) {
  haz <- haz()
  v <- pol()
  terra::spatSample(v, n)
}

nb <- function() {
  haz <- haz()
  v <- pol()
  terra::rasterize(v, haz)
}

fires <- function(n = 20) {
  pts <- terra::spatSample(terra::rescale(haz(), 0.8), 20, as.points = TRUE)
  fires <- terra::buffer(pts, 800)
}

# landscape scale aoi
aoi <- function() {
  e <- c(39, 40, 604, 605) * 10000
  aoi <- terra::as.polygons(terra::ext(e), crs = haz())
}
