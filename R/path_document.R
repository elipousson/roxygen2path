#' Create a temporary directory for a R package
#'
#' @inheritParams fs::file_temp
#' @importFrom fs is_file is_dir path_dir path_rel path file_exists file_delete
pkg_dir_temp <- function(path,
                         tmp_dir = tempdir(),
                         overwrite = FALSE,
                         call = caller_env()) {
  pkg <- path

  if (is_file(path)) {
    path <- path_dir(path)
  }

  if (is_dir(path)) {
    pkg <- with_project(
      path,
      basename(getwd()),
      quiet = TRUE
    )
  }

  if (!valid_package_name(pkg)) {
    pkg <- as_pkg_name(pkg)
  }

  check_string(pkg, call = call)

  path <- path(tmp_dir, pkg)

  namespace_path <- path(path, "NAMESPACE")

  if (file_exists(namespace_path)) {
    if (!overwrite) {
      cli_abort(
        c("A package {.pkg {pkg}} already exists at {.path {tmp_dir}}",
          "i" = "Set {.code overwrite = TRUE} to overwrite a pre-existing package."
        ),
        call = call
      )
    }

    file_delete(namespace_path)
  }

  path
}

#' Create a temporary R package from a script or
#'
#' @inheritParams usethis::create_package
#' @export
#' @importFrom usethis create_package
create_temp_package <- function(path,
                                pkg = NULL,
                                fields = list(),
                                rstudio = FALSE,
                                check_name = TRUE,
                                open = FALSE,
                                tmp_dir = tempdir(),
                                overwrite = FALSE,
                                call = caller_env()) {
  path <- pkg_dir_temp(
    pkg %||% path,
    tmp_dir = tmp_dir,
    overwrite = overwrite,
    call = call
  )

  create_package(
    path = path,
    fields = fields,
    rstudio = rstudio,
    check_name = check_name,
    open = open
  )

  invisible(path)
}


#' Create a temporary package and use [roxygenise()] to create and load documentation
#'
#' Inspired by [document::document()]
#'
#' @inheritParams create_temp_package
#' @inheritParams fs::dir_ls
#' @param ... Unused at present.
#' @export
#' @importFrom fs dir_ls file_copy
#' @importFrom withr with_dir
#' @importFrom roxygen2 roxygenise
#' @importFrom usethis use_roxygen_md use_package_doc
path_document <- function(path,
                          pkg = NULL,
                          fields = list(),
                          tmp_dir = tempdir(),
                          markdown = TRUE,
                          overwrite = FALSE,
                          roclets = NULL,
                          load_code = NULL,
                          clean = FALSE,
                          quiet = FALSE,
                          allow_file = TRUE,
                          glob = "*.R",
                          ...) {
  if (quiet) {
    rlang::local_options(
      usethis.quiet = quiet,
      cli.default_handler = suppressMessages
    )
  }

  if (!allow_file && is_file(path)) {
    cli_abort(
      c("{.arg path} must be a directory, not a file.",
        "i" = "Set {.code allow_file = TRUE} to use a file path."
      )
    )
  }

  pkg_dir <- create_temp_package(
    path,
    pkg = pkg,
    fields = fields,
    tmp_dir = tmp_dir,
    overwrite = overwrite
  )

  new_r_path <- path(pkg_dir, "R")

  if (is_dir(path)) {
    path <- dir_ls(glob = glob)
  }

  file_copy(
    path = path,
    new_path = new_r_path,
    overwrite = overwrite
  )

  withr::with_dir(
    pkg_dir,
    {
      if (markdown) {
        use_roxygen_md(overwrite = TRUE)
      }

      use_package_doc(FALSE)
    }
  )

  roxygen2::roxygenise(
    package.dir = pkg_dir,
    roclets = roclets,
    load_code = load_code,
    clean = clean
  )
}

#' [path_roxygenise()] is the same as [path_document()]
#'
#' @rdname path_document
#' @name path_roxygenise
#' @export
path_roxygenise <- path_document
