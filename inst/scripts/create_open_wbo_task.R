#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(args, name, default) {
  prefix <- paste0("--", name, "=")
  arg_matches <- args[startsWith(args, prefix)]
  if (length(arg_matches) == 0L) {
    return(default)
  }
  return(sub(prefix, "", arg_matches[[1L]]))
}

n_vars <- as.integer(get_arg(args, "vars", 200L))
n_clauses <- as.integer(get_arg(args, "clauses", 3000L))
hard_ratio <- as.numeric(get_arg(args, "hard_ratio", 0.2))
soft_weight <- as.integer(get_arg(args, "soft_weight", 1L))
seed <- as.integer(get_arg(args, "seed", 1234L))
output_path <- get_arg(args, "output", "open_wbo_task.wcnf")

if (n_vars < 1L) {
  stop("--vars must be >= 1.")
}
if (n_clauses < 1L) {
  stop("--clauses must be >= 1.")
}
if (hard_ratio <= 0L || hard_ratio >= 1L) {
  stop("--hard_ratio must be between 0 and 1.")
}
if (soft_weight < 1L) {
  stop("--soft_weight must be >= 1.")
}

set.seed(seed)

hard_clauses <- ceiling(n_clauses * hard_ratio)
soft_clauses <- n_clauses - hard_clauses
top_weight <- soft_clauses * soft_weight + 1L

sample_clause <- function(n_vars, size) {
  vars <- sample.int(n_vars, size = size, replace = FALSE)
  signs <- sample(c(-1L, 1L), size = size, replace = TRUE)
  return(as.integer(vars * signs))
}

clauses <- vector("list", n_clauses)
weights <- integer(n_clauses)

for (idx in seq_len(n_clauses)) {
  clause_size <- sample(2:4, size = 1L)
  clauses[[idx]] <- sample_clause(n_vars, clause_size)
  if (idx <= hard_clauses) {
    weights[[idx]] <- top_weight
  } else {
    weights[[idx]] <- soft_weight
  }
}

lines <- character(n_clauses + 1L)
lines[[1L]] <- paste("p wcnf", n_vars, n_clauses, top_weight)

for (idx in seq_len(n_clauses)) {
  literals <- paste(clauses[[idx]], collapse = " ")
  lines[[idx + 1L]] <- paste(weights[[idx]], literals, 0L)
}

writeLines(lines, output_path)

message("Wrote ", output_path)
message("Suggested command:")
message("  Rwbo::run_open_wbo(c(\"-cpu-lim\", \"300\", \"", output_path, "\"))")
