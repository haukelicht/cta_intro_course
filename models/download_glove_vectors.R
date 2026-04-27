model_path <- file.path("models")
base_url <- "https://nlp.stanford.edu/data/wordvecs"

download_glove_model <- function(file.name) {
  dest <- file.path(model_path, file.name)
  if (file.exists(dest))
      return(invisible(dest))
  if (!dir.exists(model_path))
    dir.create(model_path, recursive = TRUE)
  
  require(curl)
  
  h <- new_handle()
  on.exit(handle_reset(h))
  handle_setopt(
    h,
    followlocation = TRUE,
    timeout = 0,          # no overall timeout
    connecttimeout = 60,  # allow slow connects
    low_speed_time = 300, # only fail if too slow for too long
    low_speed_limit = 1
  )
  
  url <- paste0(base_url, "/", file.name)
  curl_download(url, destfile = dest, handle = h)

  message("Model downloaded to: ", dest)
  invisible(dest)
}


download_glove_model("glove.2024.wikigiga.50d.zip")
download_glove_model("glove.2024.wikigiga.200d.zip")
