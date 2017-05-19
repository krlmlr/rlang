#' @useDynLib rlang, .registration = TRUE
NULL

.onLoad <- function(lib_name, pkg_name) {
  port_back("names", pkg_name)
}
