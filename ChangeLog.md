# Changelog for coformat

## v0.3.0.0

* A fairly major refactoring: isolated the description of the formatters.
* Change the casing of flags to be more in line with the common lisp-style.

## v0.2.1.0

* Allow the user to force-set formatter options.
* Fixed discovering the default style options: need to pass user-forced and hardcoded options,
  otherwise clang-format might return nonsense.

## v0.2.0.0

* Changed the scoring from Levenshtein distance to something more suitable for the task.
* Allow resuming the optimization process from an existing style file.

## v0.1.0.0

Initial release: it formats something and is even tolerant to the formatter's crashes.
