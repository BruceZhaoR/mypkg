#' Open dir
#'
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
open_dir <- function(dir = getwd()){
  if (.Platform$OS.type == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

#' Open file
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
open_file <- function(file_path) {
  if (file.access(file_path, mode = 4) == 0) {
    file.show(file_path)
  } else
    stop("Can't open the file")
}





# # Abandon
# cal_gps_dist <- function(lng1, lat1, lng2, lat2) {
#   # angle to radian
#   lng1 <- lng1 * pi / 180
#   lat1 <- lat1 * pi / 180
#   lng2 <- lng2 * pi / 180
#   lat2 <- lat2 * pi / 180
#   tmp <- cos(lat1) * cos(lat2) * cos(lng2 - lng1) + sin(lat1) * sin(lat2)
#   # handle unexpected things
#   if (tmp >= 1) {
#     dist <- 6378.137 * acos(round(tmp, 10))
#   } else {
#     dist <- 6378.137 * acos(tmp)
#   }
#   dist
# }


#' Use gps location to calculate the spherical distance of the earth
#'
#' Return kilometers
#'
#' @param lng1 Start longitude angle eg. 121.12545.
#' @param lat1 Start latitude angle eg.32.12345.
#' @param lng2 End longitude angle eg. 121.12545.
#' @param lat2 End latitude angle eg.32.12345.
#'
#' @return km
#' @export
#'
#' @examples
#' cal_gps_dist1(121, 32, 114, 23)
#'
cal_gps_dist <- function(lng1, lat1, lng2, lat2) {
  # angle to radian
  lng1 <- lng1 * pi / 180
  lat1 <- lat1 * pi / 180
  lng2 <- lng2 * pi / 180
  lat2 <- lat2 * pi / 180
  dist <- 6378.137 * 2 * asin(sqrt(
    sin( (lat2 - lat1)/2 )^2 + cos(lat1)*cos(lat2)*sin( (lng2 -lng1)/2 )^2
  ))
  dist
}

# coordinate transformation functions;

GCJ2BD <- function(gcj_lon, gcj_lat) {
  x_pi = pi * 3000.0 / 180.0
  x = gcj_lon
  y = gcj_lat
  z = sqrt(x * x + y * y) + 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) + 0.000003 * cos(x * x_pi)
  bd_lon = z * cos(theta) + 0.0065
  bd_lat = z * sin(theta) + 0.006
  c(bd_lon, bd_lat)
}


BD2GCJ <- function(bd_lon, bd_lat) {
  x_pi = pi * 3000.0 / 180.0
  x = bd_lon - 0.0065
  y = bd_lat - 0.006
  z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
  gg_lon = z * cos(theta)
  gg_lat = z * sin(theta)
  c(gg_lon, gg_lat)
}

out_of_china <- function(lon, lat){
  a = (lon < 72.004) | (lon > 137.8347)
  b = (lat < 0.8293) | (lat > 55.8271)
  any(a,b)
}

transformlng <- function(lon, lat){
  ret = 300.0 + lon + 2.0 * lat + 0.1 * lon * lon + 0.1 * lon * lat + 0.1 * sqrt(abs(lon))
  ret <- ret + (20.0 * sin(6.0 * lon * pi) + 20.0 * sin(2.0 * lon * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(lon * pi) + 40.0 * sin(lon / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (150.0 * sin(lon / 12.0 * pi) + 300 * sin(lon / 30.0 * pi)) * 2.0 / 3.0
  ret
}

transformlat <- function(lon, lat){
  ret = -100.0 + 2.0 * lon + 3.0 * lat + 0.2 * lat * lat + 0.1 * lon * lat + 0.2 * sqrt(abs(lon))
  ret <- ret + (20.0 * sin(6.0 * lon * pi) + 20.0 * sin(2.0 * lon * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
  ret
}

delta <- function(lon, lat){
  a = 6378245.0  # a: 卫星椭球坐标投影到平面地图坐标系的投影因子。
  ee = 0.00669342162296594323  # ee: 椭球的偏心率。
  dLat = transformlat(lon - 105.0, lat - 35.0)
  dLon = transformlng(lon - 105.0, lat - 35.0)
  radLat = lat / 180.0 * pi
  magic = sin(radLat)
  magic = 1 - ee * magic * magic
  sqrtMagic = sqrt(magic)
  dLat = (dLat * 180.0) / ((a * (1 - ee)) / (magic * sqrtMagic) * pi)
  dLon = (dLon * 180.0) / (a / sqrtMagic * cos(radLat) * pi)
  c(dLon, dLat)
}

WGS2GCJ <- function(wgs_lon, wgs_lat) {
  if (out_of_china(wgs_lon, wgs_lat)) {
    c(wgs_lon, wgs_lat)
  } else {
    d = delta(wgs_lon, wgs_lat)
    wgs_lon = wgs_lon + d[[1]]
    wgs_lat = wgs_lat + d[[2]]
    c(wgs_lon, wgs_lat)
  }
}
