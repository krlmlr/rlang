#' Assign a backport in a package namespace
#'
#' @description
#'
#' This function should be called from [.onLoad()] hooks. It checks
#' for the current R version and assigns the requested backports if
#' they are not available for that version.
#'
#' Currently, only a few functions that appeared in 3.2.0 are
#' available: `dir.exists()`, `names.environment()` and `trimws()`.
#'
#' @param fns_names Character vector giving the names of the functions
#'   to backport.
#' @param pkg_name The name of the package where the backports will be
#'   assigned. This is passed to [ns_env()]. Can also be an
#'   environment (this is handy for testing when your namespace is
#'   locked).
#' @export
#' @examples
#' # Add a backport in your .onLoad() hook:
#' .onLoad <- function(lib_name, pkg_name) {
#'   port_back(c("trimws", "dir.exists"), pkg_name)
#' }
port_back <- function(fns_names, pkg_name) {
  input <- fns_names

  for (version in names(backports)) {
    fns_names <- port_back_version(fns_names, pkg_name, version)

    if (is_empty(fns_names)) {
      return(invisible(input))
    }
  }

  fns_names <- chr_enumerate(fns_names)
  abort(sprintf("Can't find backports `%s()`", fns_names))
}
port_back_version <- function(fns_names, pkg_name, version) {
  fns <- backports[[version]]
  found <- fns_names %in% names(fns)

  if (getRversion() < version) {
    if (!is_env(pkg_name)) {
      env <- ns_env(pkg_name)
    }
    env_bind(env, !!! fns[found])
  }

  fns_names[!found]
}


backports_3.2.0 <- list(
  dir.exists = function(paths) {
    dir_exists <- function(path) {
      !identical(path, "") && file.exists(paste0(path, .Platform$file.sep))
    }
    map_lgl(paths, dir_exists)
  },

  names = function(x) {
    if (is.environment(x)) {
      ls(x, all.names = TRUE)
    } else {
      base::names(x)
    }
  },

  trimws = function(x, which = c("both", "left", "right")) {
    switch(match.arg(which),
      left = sub("^[ \t\r\n]+", "", x, perl = TRUE),
      right = sub("[ \t\r\n]+$", "", x, perl = TRUE),
      both = trimws(trimws(x, "left"), "right")
    )
  }
)

backports <- list(
  `3.2.0` = backports_3.2.0
)
