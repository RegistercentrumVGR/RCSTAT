.onLoad <- function(libname, pkgname) {
  api_get <<- memoise::memoise(api_get)
  add_groups_long <<- memoise::memoise(add_groups_long)
}
