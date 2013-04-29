0.8
---
* Removed the need for `textLiteral` and `textLiteral'` by using `fromString`. Use `stringLiteral` and `stringLiteral'` instead.

0.7.1
-----
* Added support for `Text`-based parsing.

0.7
---
* Added `Unlined` to support parsing solely within a line
* Simplified `TokenParsing` instances

0.6
---
* Disallowed nested comments in 'javaCommentStyle'
* More derived instances

0.5.2
-----
* Bugfix in `commaSep1`.

0.5.1
-----
* Taught zeroNumFloat about `0.`.
* Bugfix in `buildExpressionParser`.

0.5
---
* Split out `LookAheadParsing` since it wasn't used by other combinators here and isn't supported by `attoparsec`.

0.4.1
-----
* Added `token` to `TokenParsing`.

0.4
-----
* Updated build system
* Converted various style accessors to lenses and traversals
* More aggressive inlining
* Added CHANGELOG
