write_wcnf <- function(lines, prefix = "file") {
  wcnf <- tempfile(pattern = prefix, fileext = ".wcnf")
  writeLines(lines, wcnf)
  return(wcnf)
}

expect_optimal_wcnf <- function(output, wcnf, objective) {
  output_lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  expect_identical(grep("^s ", output_lines, value = TRUE), "s OPTIMUM FOUND")

  objective_lines <- grep("^o ", output_lines, value = TRUE)
  expect_gt(length(objective_lines), 0L)
  expect_identical(as.numeric(sub("^o ", "", tail(objective_lines, 1L))), objective)

  model_lines <- grep("^v ", output_lines, value = TRUE)
  model <- scan(text = paste(sub("^v ", "", model_lines), collapse = " "), quiet = TRUE)
  model <- model[model != 0L]

  wcnf_lines <- readLines(wcnf)
  header <- scan(text = sub("^p wcnf ", "", wcnf_lines[[1L]]), quiet = TRUE)
  expect_length(model, header[[1L]])
  expect_setequal(abs(model), seq_len(header[[1L]]))

  clauses <- lapply(wcnf_lines[-1L], function(line) {
    return(scan(text = line, quiet = TRUE))
  })
  clause_weights <- vapply(clauses, function(clause) {
    return(clause[[1L]])
  }, numeric(1L))
  satisfied <- vapply(clauses, function(clause) {
    return(any(clause[-c(1L, length(clause))] %in% model))
  }, logical(1L))

  expect_true(all(satisfied[clause_weights >= header[[3L]]]))
  expect_identical(sum(clause_weights[clause_weights < header[[3L]] & !satisfied]), objective)
  return(invisible(NULL))
}

test_that("run_open_wbo solves a tiny MaxSAT instance", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 2",
    "2 1 0",
    "1 -1 0"
  ))
  output <- run_open_wbo(args = wcnf)

  expect_optimal_wcnf(output, wcnf, objective = 1.0)
})

test_that("run_open_wbo reports unsat for conflicting hard clauses", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 10",
    "10 1 0",
    "10 -1 0"
  ))
  output <- run_open_wbo(args = wcnf)

  output_lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  expect_identical(grep("^s ", output_lines, value = TRUE), "s UNSATISFIABLE")
  expect_length(grep("^v ", output_lines), 0L)
})

test_that("run_open_wbo solves a weighted MaxSAT instance", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 5",
    "5 1 0",
    "3 -1 0"
  ))
  output <- run_open_wbo(args = wcnf)

  expect_optimal_wcnf(output, wcnf, objective = 3.0)
})

test_that("run_open_wbo handles all-soft clauses", {
  wcnf <- write_wcnf(c(
    "p wcnf 2 2 10",
    "3 1 0",
    "2 -1 2 0"
  ))
  output <- run_open_wbo(args = wcnf)

  expect_optimal_wcnf(output, wcnf, objective = 0.0)
})

test_that("run_open_wbo handles paths containing spaces and shell metacharacters", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 2",
    "2 1 0",
    "1 -1 0"
  ), prefix = "input with spaces & ")

  output <- run_open_wbo(args = wcnf)

  expect_optimal_wcnf(output, wcnf, objective = 1.0)
})

test_that("run_open_wbo passes multiple solver flags with an input path", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 2",
    "2 1 0",
    "1 -1 0"
  ))

  output <- run_open_wbo(args = c("-algorithm=1", "-no-bmo", "-verbosity=1", wcnf))

  expect_optimal_wcnf(output, wcnf, objective = 1.0)
})

test_that("run_open_wbo raises an error for an unknown solver flag", {
  expect_error(
    run_open_wbo(args = "-unknown-rwbo-option"),
    "open-wbo_static failed with exit status 1.",
    fixed = TRUE
  )
})

test_that("run_open_wbo includes solver diagnostics for a missing input file", {
  missing_wcnf <- tempfile(pattern = "missing input ", fileext = ".wcnf")

  error <- tryCatch(run_open_wbo(args = missing_wcnf), error = identity)

  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "open-wbo_static failed with exit status 50.", fixed = TRUE)
  expect_match(conditionMessage(error), "Could not open file:", fixed = TRUE)
  expect_match(conditionMessage(error), missing_wcnf, fixed = TRUE)
})

test_that("run_open_wbo returns output for each valid solver exit status", {
  for (status in c(0L, 10L, 20L, 30L, 40L)) {
    runner <- run_open_wbo
    environment(runner) <- list2env(list(
      system.file = function(...) {
        return("open-wbo_static")
      },
      system2 = function(...) {
        return(structure(c("o 1", "s SATISFIABLE"), status = status))
      }
    ), parent = environment(run_open_wbo))

    expect_identical(runner(args = "input.wcnf"), "o 1\ns SATISFIABLE")
  }
})

test_that("run_open_wbo reports a missing bundled binary", {
  runner <- run_open_wbo
  environment(runner) <- list2env(list(system.file = function(...) {
    return("")
  }), parent = environment(run_open_wbo))

  expect_error(runner(args = "--help"), "open-wbo_static not found.", fixed = TRUE)
})

test_that("run_open_wbo rejects invalid argument vectors", {
  for (invalid_args in list(NULL, 1L, NA_character_, c("--help", NA_character_))) {
    expect_error(
      run_open_wbo(args = invalid_args),
      "'args' must be a character vector without missing values.",
      fixed = TRUE
    )
  }
})

test_that("run_open_wbo rejects invalid timeouts", {
  invalid_timeouts <- list(NULL, numeric(), c(0L, 1L), NA_real_, NaN, Inf, -1L, 0.5, 1.5, "1", TRUE, 1L + 1i)
  for (invalid_timeout in invalid_timeouts) {
    expect_error(
      run_open_wbo(args = "--help", timeout = invalid_timeout),
      "'timeout' must be a single finite non-negative whole number.",
      fixed = TRUE
    )
  }
})

test_that("run_open_wbo enforces its elapsed-time limit", {
  script <- tempfile(fileext = ".R")
  writeLines("Sys.sleep(10)", script)
  rscript_name <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  rscript_path <- file.path(R.home("bin"), rscript_name)

  runner <- run_open_wbo
  environment(runner) <- list2env(list(system.file = function(...) {
    return(rscript_path)
  }), parent = environment(run_open_wbo))

  expect_error(
    runner(args = c("--vanilla", script), timeout = 1L),
    "open-wbo_static timed out after 1 seconds (exit status 124).",
    fixed = TRUE
  )
})
