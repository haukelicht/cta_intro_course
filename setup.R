# setup renv
if (!"renv" %in% row.names(installed.packages())) {
  install.packages("renv")
}

if (file.exists("renv.lock")) {
  renv::load()
  renv::restore()
} else {
  renv::init(bare = TRUE)
  pkgs <- readLines("requirements.txt")
  renv::install(pkgs)
  renv::snapshot()
}
