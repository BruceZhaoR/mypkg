#' Convert png to svg
#'
#' @param png_path path to the png file,character.
#' @param svg_path out path of the svg file, character.
#' @param width numeric, e.g: 200
#' @param height numeric, e.g: 200
#'
#' @return a svg file
#'
#' @details The quality of svg file depends on the png file.
#'
#' @examples
#' png_path <- system.file("img", "Rlogo.png", package="png")
#' svg_path <- tempfile(fileext = ".svg")
#' png2svg(png_path, svg_path)
#' file.show(svg_path)
#' png2svg(png_path, svg_path, 200, 200)
#'
png2svg <- function(png_path, svg_path, width = 800, height = 800) {
  header <- '<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
  <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="{width}px" height="{height}px" viewBox="0 0 {width} {height}" enable-background="new 0 0 {width} {height}" xml:space="preserve">
  <image id="image0" width="{width}" height="{height}" x="0" y="0"
  xlink:href='
  header <- glue::glue(header)
  png_uri <- base64enc::dataURI(file = png_path, mime = "image/png", encoding = "base64")
  result <- paste0(header, "\"", png_uri, "\" /> \n</svg>")
  readr::write_file(result, svg_path)
}


#' Remove image backgroud, make it transparent.
#'
#' @param image Path or url to the image, support 'jpg', 'jpeg', 'png' and 'gif'.
#' @param bg The background color you want to remove, eg. 'white','#ffffff'.
#' @param effect The affect area of the given background color, 0-100, 30 is enough.
#'
#' @return A new png format image.
#' @export
#'
#' @examples
#' remove_bg("http://echarts.baidu.com/images/company/company8.png","#f47b2a",effect = 25)
#' remove_bg("http://echarts.baidu.com/images/company/company5.png","white",effect = 0)
remove_bg <- function(image, bg = "white", effect = 5){
  tmp <- magick::image_read(image) %>%
    magick::image_trim()
  if (grepl("\\.png", image)) {
    out <- tmp %>% magick::image_transparent(color = bg, fuzz = effect)
  } else {
    out <- tmp %>% magick::image_convert(format = "png") %>%
      magick::image_transparent(color = bg, fuzz = effect)
  }
  magick::image_write(out, path = gsub("\\.png$|\\.jpg$",replacement = "-new.png", image))
}



