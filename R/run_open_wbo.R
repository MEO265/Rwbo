#' Run open-wbo
#'
#' Run the bundled `open-wbo` binary with user-supplied parameters.
#'
#' @details
#' `args` is passed directly to the `open-wbo` command-line tool, so supply
#' the path to a WCNF file along with any solver flags you want to enable. To see
#' the full list of supported options for your bundled binary, run
#' `run_open_wbo("--help")`. The help text is emitted on stderr, so it appears in
#' your console but is not returned by `run_open_wbo()`.
#'
#' The executable bundled with the package is named `open-wbo_static` (or
#' `open-wbo_static.exe` on Windows). The build aims to be as static as the
#' platform allows: non-Darwin builds attempt to use the static target, but they
#' fall back to the release binary when a static build is unavailable. macOS
#' builds always use the release binary, because the Apple toolchain does not
#' provide fully static system libraries (the `libSystem` umbrella is only
#' available dynamically), and static linking of system frameworks is not
#' supported. As a result, some system libraries remain dynamically linked and a
#' 100% static build is not guaranteed.
#'
#' Common solver options include toggles such as `-forceunsat`/`-no-forceunsat`,
#' `-adapt`/`-no-adapt`, `-print-model`/`-no-print-model`, and parameter settings
#' like `-algorithm <int>`, `-cpu-lim <int>`, `-mem-lim <int>`, and
#' `-verbosity <int>`.
#'
#' @param args Character vector of arguments passed to `open-wbo`.
#'
#' @return Character string containing the output from `open-wbo`.
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

  # open-wbo uses non-zero exit statuses (20 for UNSAT, 30 for OPTIMUM).
  output <- suppressWarnings(system2(
    bin_path,
    args = args,
    stdout = TRUE,
    stderr = ""
  ))
  output <- paste(output, collapse = "\n")

  return(output)
}
