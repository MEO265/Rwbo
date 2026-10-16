#' Run open-wbo_static
#'
#' Run the bundled `open-wbo_static` binary with user-supplied parameters.
#'
#' @details
#' Supply one unquoted command-line argument per element of `args`, including
#' the path to a WCNF file and any solver flags. Arguments are quoted internally,
#' so paths containing spaces do not need extra quoting. To see the full list of
#' supported options for your bundled binary, run
#' `run_open_wbo("--help")`. The help text is emitted on stderr, so it appears in
#' your console but is not returned by `run_open_wbo()`.
#'
#' Common solver options include toggles such as `-bmo`/`-no-bmo`,
#' `-symmetry`/`-no-symmetry`, and `-print-model`/`-no-print-model`.
#' Numeric settings use `-name=value` syntax, for example `-algorithm=1`,
#' `-cpu-lim=60`, `-mem-lim=1024`, and `-verbosity=1`.
#'
#' Solver exit statuses 0, 10 (SAT), 20 (UNSAT), 30 (OPTIMUM), and 40 (UNKNOWN)
#' return the captured output. Other exit statuses raise an error containing the
#' status and any captured output. Solver diagnostics on stderr remain visible
#' in the console.
#'
#' @param args Character vector with one unquoted argument per element, passed
#'   to `open-wbo_static`.
#' @param timeout Maximum elapsed time in whole seconds, as a non-negative
#'   number. The default, 0, imposes no time limit. A timeout raises an error.
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
#' run_open_wbo(args = c("-algorithm=1", "-no-print-model", wcnf_file))
#' }
#' @export
run_open_wbo <- function(args = character(), timeout = 0L) {
  if (!is.character(args) || anyNA(args)) {
    stop("'args' must be a character vector without missing values.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L ||
        is.na(timeout) || !is.finite(timeout) || timeout < 0L || timeout != floor(timeout)) {
    stop("'timeout' must be a single finite non-negative whole number.", call. = FALSE)
  }

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

  # Non-zero statuses can represent valid solver results.
  output <- suppressWarnings(system2(
    bin_path,
    args = shQuote(args),
    stdout = TRUE,
    stderr = "",
    timeout = timeout
  ))
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  if (!status %in% c(0L, 10L, 20L, 30L, 40L)) {
    error_message <- if (status == 124L) {
      paste0("open-wbo_static timed out after ", timeout, " seconds (exit status 124).")
    } else {
      paste0("open-wbo_static failed with exit status ", status, ".")
    }
    stop(
      error_message,
      if (length(output) > 0L) paste0("\n", paste(output, collapse = "\n")),
      call. = FALSE
    )
  }
  output <- paste(output, collapse = "\n")

  return(output)
}
