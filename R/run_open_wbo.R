#' Run open-wbo_static
#'
#' Run the bundled `open-wbo_static` binary with user-supplied parameters.
#'
#' @param args Character vector of arguments passed to `open-wbo_static`.
#'
#' @return Character string containing the output from `open-wbo_static`.
#' @export
run_open_wbo <- function(args = character()) {
  bin_name <- if (.Platform$OS.type == "windows") {
    "open-wbo_static.exe"
  } else {
    "open-wbo_static"
  }
  bin_path <- system.file("bin", bin_name, package = "Rwbo")
  if (!nzchar(bin_path) && .Platform$OS.type == "windows") {
    bin_path <- system.file("bin", "open-wbo_static", package = "Rwbo")
  }
  if (!nzchar(bin_path)) {
    stop("open-wbo_static not found. Please reinstall Rwbo.", call. = FALSE)
  }

  # open-wbo_static uses non-zero exit statuses (20 for UNSAT, 30 for OPTIMUM).
  output <- suppressWarnings(system2(
    bin_path,
    args = args,
    stdout = TRUE,
    stderr = ""
  ))
  output <- paste(output, collapse = "\n")

  return(output)
}
