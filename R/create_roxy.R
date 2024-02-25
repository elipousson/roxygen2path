#' Helper functions using `{sinew}` to make roxygen2 skeletons for directories,
#' files, and objects
#'
#' These functions are wrappers for [sinew::makeOxyFile()] and
#' [sinew::makeOxygen()].
#'
#' @name create_roxy
#' @aliases make_roxy
NULL

#' @inheritParams sinew::makeOxyFile
#' @seealso
#'  [sinew::makeOxyFile()]
#' @rdname create_roxy
#' @export
dir_roxy <- function(path, markdown = TRUE, quiet = FALSE, ...) {
  check_installed("sinew")
  sinew::makeOxyFile(
    input = path,
    markdown = markdown,
    ...,
    verbose = !quiet
  )
}

#' @rdname create_roxy
#' @export
file_roxy <- function(path, markdown = TRUE, quiet = FALSE, ...) {
  check_installed("sinew")
  sinew::makeOxyFile(
    input = path,
    markdown = markdown,
    ...,
    verbose = !quiet
  )
}

#' @inheritParams sinew::makeOxygen
#' @rdname create_roxy
#' @export
obj_roxy <- function(obj, title = NULL, description = NULL, markdown = TRUE, ...) {
  check_installed("sinew")
  sinew::makeOxygen(
    obj = obj,
    title = title,
    description = description,
    markdown = markdown,
    ...
  )
}
