sanitize_output <- function(output, wcnf_path) {
  return(gsub(wcnf_path, "<wcnf>", output, fixed = TRUE))
}

write_wcnf <- function(lines) {
  wcnf <- tempfile(fileext = ".wcnf")
  writeLines(lines, wcnf)
  return(wcnf)
}

snapshot_output <- function(output, name) {
  output_path <- tempfile(fileext = ".out")
  output_lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  output_lines <- output_lines[!grepl("^c\\s*$", output_lines)]
  output_lines <- output_lines[!startsWith(output_lines, "c WARNING")]
  writeLines(output_lines, output_path)
  return(expect_snapshot_file(output_path, name = name))
}

test_that("run_open_wbo solves a tiny MaxSAT instance", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 2",
    "2 1 0",
    "1 -1 0"
  ))
  output <- run_open_wbo(args = wcnf)
  output <- sanitize_output(output, wcnf)

  expect_type(output, "character")
  expect_true(nzchar(output))
  snapshot_output(output, "tiny-unweighted-output")
})

test_that("run_open_wbo reports unsat for conflicting hard clauses", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 10",
    "10 1 0",
    "10 -1 0"
  ))
  output <- run_open_wbo(args = wcnf)
  output <- sanitize_output(output, wcnf)

  expect_type(output, "character")
  expect_true(nzchar(output))
  snapshot_output(output, "unsat-hard-output")
})

test_that("run_open_wbo solves a weighted MaxSAT instance", {
  wcnf <- write_wcnf(c(
    "p wcnf 1 2 5",
    "5 1 0",
    "3 -1 0"
  ))
  output <- run_open_wbo(args = wcnf)
  output <- sanitize_output(output, wcnf)

  expect_type(output, "character")
  expect_true(nzchar(output))
  snapshot_output(output, "weighted-sat-output")
})

test_that("run_open_wbo handles all-soft clauses", {
  wcnf <- write_wcnf(c(
    "p wcnf 2 2 10",
    "3 1 0",
    "2 -1 2 0"
  ))
  output <- run_open_wbo(args = wcnf)
  output <- sanitize_output(output, wcnf)

  expect_type(output, "character")
  expect_true(nzchar(output))
  snapshot_output(output, "all-soft-output")
})
