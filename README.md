# coformat

Code style formatters take a style description and format a code base accordingly.

*coformat* is _dual_ to them: it takes a code base and produces a code style description for it! This is achieved by a cunning combination of state-of-the-art type system features (`DataKinds`, `RankNTypes` and `GADTs`, among others) and modern machine learning techniques (there is a numerical optimization step after all).

More seriously, this project only supports `clang-format` for now, but plugging other formatters (perhaps for other languages) should be trivial enough, provided there is an easy way to get the list of available options and their possible values.
