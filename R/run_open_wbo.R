#' Run open-wbo_static
#'
#' Run the bundled `open-wbo_static` binary with user-supplied parameters.
#'
#' @details
#' `args` is passed directly to the `open-wbo_static` command-line tool, so supply
#' the path to a WCNF file along with any solver flags you want to enable. To see
#' the full list of supported options for your bundled binary, run
#' `run_open_wbo("--help")`. The help text is emitted on stderr, so it appears in
#' your console but is not returned by `run_open_wbo()`.
#'
#' Common solver options include toggles such as `-forceunsat`/`-no-forceunsat`,
#' `-adapt`/`-no-adapt`, `-print-model`/`-no-print-model`, and parameter settings
#' like `-algorithm <int>`, `-cpu-lim <int>`, `-mem-lim <int>`, and
#' `-verbosity <int>`.
#'
#' @param args Character vector of arguments passed to `open-wbo_static`.
#'
#' @return Character string containing the output from `open-wbo_static`.
#'
#' @examples
#' \donttest{
#' wcnf_file <- tempfile(fileext = ".wcnf")
#' writeLines(c(
#'   "p wcnf 1 2 2",
#'   "2 1 0",
#'   "1 -1 0"
#' ), wcnf_file)
#' run_open_wbo(args = wcnf_file)
#' }
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
