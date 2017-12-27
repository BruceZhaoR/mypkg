
#' Title
#'
#' @param num numeric
#'
#' @examples
number_2_date <- function(num) {
  index <- ifelse(is.na(num[1]),num[10],num[1])
  stopifnot(is.numeric(index))
  if(nchar(index) == 13) {
    as.POSIXct(num/1000, tz = "Asia/Shanghai",origin = "1970-01-01")
  } else{
    if (nchar(index) == 10) {
      as.POSIXct(num, tz = "Asia/Shanghai",origin = "1970-01-01")
    } else {
      stop("The length of number should be 13 or 10")
    }
  }
}

#' Title
#'
#' @param file
#'
#' @return
#' @export jsonlite
#'
#' @examples
#' write_csv(parse_json("data/3-1.json"),"data/parse-3-1.csv")
#' write_csv(parse_json("data/3-2.json"),"data/parse-3-2.csv")
#' write_csv(parse_json("data/3-3.json"),"data/parse-3-3.csv")
#'
parse_json <- function(file) {
  tmp_json <- jsonlite::fromJSON(file)
  tmp_saT <- tmp_json$mT$saT
  tmp_sT <- tmp_json$mT$sT
  tmp_spd <- tmp_json$mT$spd
  tibble(saT = as.character(number_2_date(tmp_saT)),
         sT = as.character(number_2_date(tmp_sT)),
         spd = tmp_spd)
}

parse_json_2 <- function(file) {
  tmp_json <- jsonlite::fromJSON(file)
  tmp_serverTime <- tmp_json$mT$serverTime
  tmp_gT <- tmp_json$mT$gT
  tmp_sT <- tmp_json$mT$sT
  tmp_spd <- tmp_json$mT$spd
  tibble(serverTime = tmp_serverTime,
         saT = as.character(number_2_date(tmp_gT)),
         sT = as.character(number_2_date(tmp_sT)),
         spd = tmp_spd)
}
