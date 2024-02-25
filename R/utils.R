as_pkg_name <- function(x) {
  gsub("^[[:digit:]]|(-|_)+", "", x)
}

#' Is path a R package?
#'
#' @importFrom usethis proj_get
#' @noRd
is_package <- function(base_path = proj_get()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}

#' Adapted from usethis:::valid_package_name()
valid_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", x) && !grepl("\\.$", x)
}
