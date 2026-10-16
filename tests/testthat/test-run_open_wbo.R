write_wcnf <- function(lines, prefix = "file") {
  wcnf <- tempfile(pattern = prefix, fileext = ".wcnf")
  writeLines(lines, wcnf)
  return(wcnf)
}

runner_with_bindings <- function(...) {
  runner <- run_open_wbo
  environment(runner) <- list2env(list(...), parent = environment(run_open_wbo))
  return(runner)
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
  expect_identical(anyDuplicated(abs(model)), 0L)

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

test_that("run_open_wbo preserves a long signed model from the real solver", {
  variables <- seq_len(5000L)
  literals <- ifelse(variables %% 2L == 0L, -variables, variables)
  wcnf <- write_wcnf(c(
    paste("p wcnf", length(variables), length(variables) + 1L, 10L),
    paste(10L, literals, 0L),
    "3 -1 0"
  ))
  output <- run_open_wbo(args = c("-algorithm=1", wcnf), timeout = 10L)

  model_lines <- grep("^v ", strsplit(output, "\n", fixed = TRUE)[[1L]], value = TRUE)
  expect_length(model_lines, 1L)
  expect_gt(nchar(model_lines, type = "bytes"), 8095L)
  model <- scan(text = sub("^v ", "", model_lines), quiet = TRUE)
  expect_identical(model, as.numeric(literals))
  expect_optimal_wcnf(output, wcnf, objective = 3.0)
})

test_that("LinearSU handles satisfiable and unsatisfiable WCNF without soft clauses", {
  sat_wcnf <- write_wcnf(c("p wcnf 2 2 10", "10 1 0", "10 -2 0"))
  unsat_wcnf <- write_wcnf(c("p wcnf 1 2 10", "10 1 0", "10 -1 0"))
  with_soft <- write_wcnf(c("p wcnf 1 3 10", "10 1 0", "10 -1 0", "1 1 0"))

  for (bmo in c("-bmo", "-no-bmo")) {
    output <- run_open_wbo(c("-algorithm=1", bmo, sat_wcnf))
    expect_optimal_wcnf(output, sat_wcnf, objective = 0.0)

    for (wcnf in c(unsat_wcnf, with_soft)) {
      output <- run_open_wbo(c("-algorithm=1", bmo, wcnf))
      lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
      expect_identical(grep("^s ", lines, value = TRUE), "s UNSATISFIABLE")
      expect_length(grep("^v ", lines), 0L)
    }
  }
})

test_that("run_open_wbo accepts empty hard clauses as UNSAT", {
  instances <- list(
    c("p wcnf 0 1 10", "10 0"),
    c("p wcnf 1 1 10", "10 0"),
    c("p wcnf 1 2 10", "10 1 0", "10 0"),
    c("p wcnf 1 2 10", "10 0", "1 1 0")
  )
  for (instance in instances) {
    wcnf <- write_wcnf(instance)
    for (flags in list(character(), "-algorithm=1", c("-algorithm=1", "-no-bmo"))) {
      output <- run_open_wbo(c(flags, wcnf))
      lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
      expect_identical(grep("^s ", lines, value = TRUE), "s UNSATISFIABLE")
      expect_length(grep("^v ", lines), 0L)
    }
  }
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
  results <- c("c regular exit", "s SATISFIABLE", "s UNSATISFIABLE", "s OPTIMUM FOUND", "s UNKNOWN")
  for (status in c(0L, 10L, 20L, 30L, 40L)) {
    capture <- new.env(parent = emptyenv())
    lines <- c("o 1", results[[match(status, c(0L, 10L, 20L, 30L, 40L))]])
    runner <- runner_with_bindings(
      system.file = function(...) {
        return("open-wbo_static")
      },
      system2 = function(command, args, stdout, stderr, timeout) {
        capture$output_file <- stdout
        writeLines(lines, stdout)
        return(status)
      }
    )

    expect_identical(runner(args = "input.wcnf"), paste(lines, collapse = "\n"))
    expect_false(file.exists(capture$output_file))
  }
})

test_that("run_open_wbo preserves physical lines, spaces, and incomplete last lines", {
  model_line <- paste("v", paste(c(seq_len(2500L), -seq_len(2500L)), collapse = " "))
  lines <- c("c diagnostic  ", "", model_line, "s UNKNOWN", "", "c final line  ")
  capture <- new.env(parent = emptyenv())
  runner <- runner_with_bindings(
    system.file = function(...) {
      return("solver with spaces & symbols")
    },
    system2 = function(command, args, stdout, stderr, timeout) {
      expect_identical(command, "solver with spaces & symbols")
      expect_identical(args, shQuote(c("-algorithm=1", "input with spaces & symbols.wcnf")))
      expect_identical(stderr, "")
      expect_identical(timeout, 7L)
      expect_type(stdout, "character")
      expect_length(stdout, 1L)
      capture$output_file <- stdout
      cat(paste(lines, collapse = "\n"), file = stdout)
      return(40L)
    }
  )

  expect_identical(
    runner(c("-algorithm=1", "input with spaces & symbols.wcnf"), timeout = 7L),
    paste(lines, collapse = "\n")
  )
  expect_false(file.exists(capture$output_file))
})

test_that("run_open_wbo returns an empty string for empty stdout", {
  capture <- new.env(parent = emptyenv())
  runner <- runner_with_bindings(
    system.file = function(...) {
      return("open-wbo_static")
    },
    system2 = function(command, args, stdout, stderr, timeout) {
      capture$output_file <- stdout
      writeLines(character(), stdout)
      return(0L)
    }
  )

  expect_identical(runner(), "")
  expect_false(file.exists(capture$output_file))
})

test_that("run_open_wbo reports unexpected exit statuses and removes stdout files", {
  for (status in c(1L, 5L, 50L, 127L, 124L)) {
    capture <- new.env(parent = emptyenv())
    lines <- c("c diagnostic", "", "s UNKNOWN")
    runner <- runner_with_bindings(
      system.file = function(...) {
        return("open-wbo_static")
      },
      system2 = function(command, args, stdout, stderr, timeout) {
        capture$output_file <- stdout
        writeLines(lines, stdout)
        warning("system2 status warning")
        return(status)
      }
    )

    error <- tryCatch(runner(timeout = 2L), error = identity)
    expect_s3_class(error, "error")
    message <- if (status == 124L) {
      "open-wbo_static timed out after 2 seconds (exit status 124)."
    } else {
      paste0("open-wbo_static failed with exit status ", status, ".")
    }
    expect_identical(conditionMessage(error), paste(c(message, lines), collapse = "\n"))
    expect_false(file.exists(capture$output_file))
  }
})

test_that("run_open_wbo treats exit status 5 with empty stdout as an error", {
  capture <- new.env(parent = emptyenv())
  runner <- runner_with_bindings(
    system.file = function(...) {
      return("open-wbo_static")
    },
    system2 = function(command, args, stdout, stderr, timeout) {
      capture$output_file <- stdout
      writeLines(character(), stdout)
      return(5L)
    }
  )

  error <- tryCatch(runner(), error = identity)
  expect_s3_class(error, "error")
  expect_identical(conditionMessage(error), "open-wbo_static failed with exit status 5.")
  expect_false(file.exists(capture$output_file))
})

test_that("run_open_wbo removes stdout files when launching or reading fails", {
  for (failure in c("launch", "read")) {
    capture <- new.env(parent = emptyenv())
    runner <- runner_with_bindings(
      system.file = function(...) {
        return("open-wbo_static")
      },
      system2 = function(command, args, stdout, stderr, timeout) {
        capture$output_file <- stdout
        writeLines("c partial output", stdout)
        if (failure == "launch") {
          stop("launch failed")
        }
        return(0L)
      },
      readLines = function(...) {
        stop("read failed")
      }
    )

    expect_error(runner(), paste(failure, "failed"), fixed = TRUE)
    expect_false(file.exists(capture$output_file))
  }
})

test_that("run_open_wbo can capture to a path containing spaces and ampersands", {
  wcnf <- write_wcnf(c("p wcnf 1 2 2", "2 1 0", "1 -1 0"))
  output_file <- tempfile(pattern = "stdout with spaces & ")
  runner <- runner_with_bindings(tempfile = function(...) {
    return(output_file)
  })

  expect_optimal_wcnf(runner(wcnf), wcnf, objective = 1.0)
  expect_false(file.exists(output_file))
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
  on.exit(unlink(script), add = TRUE)
  writeLines("Sys.sleep(10)", script)
  rscript_name <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  rscript_path <- file.path(R.home("bin"), rscript_name)

  output_file <- tempfile(pattern = "timeout stdout ")
  runner <- runner_with_bindings(
    system.file = function(...) {
      return(rscript_path)
    },
    tempfile = function(...) {
      return(output_file)
    }
  )

  start <- proc.time()[["elapsed"]]
  expect_error(
    runner(args = c("--vanilla", script), timeout = 1L),
    "open-wbo_static timed out after 1 seconds (exit status 124).",
    fixed = TRUE
  )
  expect_lt(proc.time()[["elapsed"]] - start, 8.0)
  expect_false(file.exists(output_file))
})
