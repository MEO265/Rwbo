# Rwbo (Development Version)

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
