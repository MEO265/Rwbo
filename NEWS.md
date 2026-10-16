# Rwbo (Development Version)

* Capture solver stdout through a temporary file so long model lines on Windows
  retain every literal and their physical line boundaries. Remove the file on
  success and failure, and read the exit status directly from `system2()`.
* Fix a native LinearSU/BMO crash for WCNF without soft clauses by using normal
  search when the objective has no weight levels. Hard-only SAT, UNSAT, and empty
  hard clauses now return their regular solver results with `-algorithm=1`.
* Add real-solver regression coverage for a 5000-variable signed model, hard-only
  WCNF, and empty hard clauses, plus stdout-file cleanup on error and timeout.
* Quote each command-line argument so input paths can contain spaces and shell
  metacharacters.
* Report unexpected solver exit statuses as R errors while accepting regular
  SAT, UNSAT, OPTIMUM, and UNKNOWN results.
* Add a `timeout` argument to limit elapsed solver runtime on all platforms.
* Correct the documented solver options and argument syntax.
* Check solver results semantically and cover wrapper error paths in tests.
* Respect R's compiler and linker settings when building Open-WBO, and preserve
  package resources when cleaning generated files.

# Rwbo 0.1.2

* Build without `gmpxx.h` by default.
* Add a non-Darwin static build fallback.
* Guard glibc-only FPU control to allow musl builds.

# Rwbo 0.1.1

* Initial release.
