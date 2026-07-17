.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  assign("api_get", memoise::memoise(api_get), envir = ns)
  assign("add_groups_long", memoise::memoise(add_groups_long), envir = ns)
}
