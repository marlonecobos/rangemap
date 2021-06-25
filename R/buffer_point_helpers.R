# All these functions were developed by:
# Whuber, https://gis.stackexchange.com/users/664/whuber
#
# Functions gathered from:
# The Geographic Information Systems Stack Exchange (online community)
# https://gis.stackexchange.com/questions/250389/euclidean-and-geodesic-buffering-in-r

degrees.to.radians <- function(phi) phi * (pi / 180)
radians.to.degrees <- function(phi) phi * (180 / pi)


# Create a 3X3 matrix to rotate the North Pole to latitude `phi`, longitude 0.
# Solution: A rotation is a linear map, and therefore is determined by its
#           effect on a basis.  This rotation does the following:
#           (0,0,1) -> (cos(phi), 0, sin(phi))  {North Pole (Z-axis)}
#           (0,1,0) -> (0,1,0)                  {Y-axis is fixed}
#           (1,0,0) -> (sin(phi), 0, -cos(phi)) {Destination of X-axis}

rotation.create <- function(phi, is.radians=FALSE) {
  if (!is.radians) phi <- degrees.to.radians(phi)
  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  matrix(c(sin.phi, 0, -cos.phi, 0, 1, 0, cos.phi, 0, sin.phi), 3)
}


# Convert between geocentric and spherical coordinates for a unit sphere.
# Assumes `latlon` in degrees.  It may be a 2-vector or a 2-row matrix.
# Returns an array with three rows for x,y,z.
#
latlon.to.xyz <- function(latlon) {
  latlon <- degrees.to.radians(latlon)
  latlon <- matrix(latlon, nrow=2)
  cos.phi <- cos(latlon[1,])
  sin.phi <- sin(latlon[1,])
  cos.lambda <- cos(latlon[2,])
  sin.lambda <- sin(latlon[2,])
  rbind(x = cos.phi * cos.lambda,
        y = cos.phi * sin.lambda,
        z = sin.phi)
}


xyz.to.latlon <- function(xyz) {
  xyz <- matrix(xyz, nrow=3)
  radians.to.degrees(rbind(phi=atan2(xyz[3,], sqrt(xyz[1,]^2 + xyz[2,]^2)),
                           lambda=atan2(xyz[2,], xyz[1,])))
}


# Create a circle of radius `r` centered at the North Pole, oriented positively.
# `r` is measured relative to the sphere's radius `R`.  For the authalic Earth,
# r==1 corresponds to 6,371,007.2 meters.
#
# `resolution` is the number of vertices to use in a polygonal approximation.
# The first and last vertex will coincide.

circle.create <- function(r, resolution=360, R=6371007.2) {
  phi <- pi/2 - r / R                       # Constant latitude of the circle
  resolution <- max(1, ceiling(resolution)) # Assures a positive integer
  radians.to.degrees(rbind(phi=rep(phi, resolution+1),
                           lambda=seq(0, 2*pi, length.out = resolution+1)))
}


# Rotate around the y-axis, sending the North Pole to `phi`; then
# rotate around the new North Pole by `lambda`.
# Output is in geographic (spherical) coordinates, but input points may be
# in Earth-centered Cartesian or geographic.
# No effort is made to clamp longitudes to a 360 degree range.  This can
# facilitate later computations.  Clamping is easily done afterwards if needed:
# reduce the longitude modulo 360 degrees.

rotate <- function(p, phi, lambda, is.geographic=FALSE) {
  if (is.geographic) p <- latlon.to.xyz(p)
  a <- rotation.create(phi)   # First rotation matrix
  q <- xyz.to.latlon(a %*% p) # Rotate the XYZ coordinates
  q + c(0, lambda)            # Second rotation
}
