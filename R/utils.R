open_dir <- function(dir = getwd()){
  if (.Platform$OS.type == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

open_file <- function(file_path) {
  if (file.access(file_path, mode = 4) == 0) {
    file.show(file_path)
  } else
    stop("Can't open the file")
}
