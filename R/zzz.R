# `kors` is package data, resolved at run time from the lazy-load database rather
# than from the namespace that R CMD check walks
utils::globalVariables("kors")

# nocov start

.onLoad <- function(libname, pkgname) {
  # S7 methods defined on generics owned by *other* packages - `print()` for the
  # `landmark`, `template`, and `window_strategy` classes - are only wired up
  # when this is called. Without it those methods work under `load_all()` but
  # silently fall back to the default `print()` in an installed package.
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  # Nothing to set for now
}

# nocov end
