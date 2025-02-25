pts <- function(n = 20) {
  hazard_path <- system.file("extdata", "hazard.tif", package ="fireexposuR")
  haz <- terra::rast(hazard_path)
  geo_path <- system.file("extdata", "polygon_geometry.csv", package ="fireexposuR")
  g <- read.csv(geo_path)
  v <- terra::vect(as.matrix(g), "polygons", crs = haz)
  nb <- terra::rasterize(v, haz)
  terra::spatSample(v, n = 3)
}
