# Changelog

## [0.4.1](https://github.com/Walheimat/ship-mate/compare/v0.4.0...v0.4.1) (2024-01-28)


### Bug Fixes

* **submarine:** get window for all frames when hiding ([8f475e9](https://github.com/Walheimat/ship-mate/commit/8f475e9df138f7262b87366cce799d5fa3867e62))


### Features

* **bindings:** harmonize bindings ([2889e29](https://github.com/Walheimat/ship-mate/commit/2889e297cfe932f187d22911403b2821e16faf56))
* **ci:** add semantic-release ([7590684](https://github.com/Walheimat/ship-mate/commit/75906844fcd0ec792c7af1c136ee1ee7f36a1999))
* **lighter:** capitalize names in lighter menu ([7d2e319](https://github.com/Walheimat/ship-mate/commit/7d2e3191692146fc11db819c9a0bf4984e5a666b))
* **submarine:** complete buffer to recompile ([46af9fe](https://github.com/Walheimat/ship-mate/commit/46af9fe66494a13d35ca0319da22eac53ecba53a))

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Command `ship-mate-hidden-recompile` now allows selection of buffer
  to recompile.

### Changed

- Text in lighter menu is now capitalized.
- The bindings in `ship-mate-command-map` and
  `ship-mate-dinghy-mode-map` have been revised and harmonized.

### Fixed

- Hiding compilations within other frames works now.

## [0.4.0]

### Added

- Buffer completion now offers only the current project's buffers
  unless the command using it was called with a prefix argument.
- Command `ship-mate-show-results` to pop to a buffer. Bound to `s` in
  subcommands map.
- Minor modes now include their map in their docstrings.
- Calling `ship-mate-show-hidden` signals an error if there is no
  hidden buffer.
- It is now possible to have multiple hidden compilations running at
  the same time.
- Command `ship-mate-hide-visible` to hide a visible compilation.
- Commands created with `ship-mate-create-command` now attempt to bind
  using any letter that is part of the symbol name. If this fails, a
  warning is displayed and no binding takes place.

### Changed

- Buffer completion now uses `read-buffer`.
- Buffer completion now signals an error if there is no eligible
  buffer.
- History replacement now only happens if the history is full.
- All commands (including subcommands) are now bound in
  `ship-mate-command-map`.

### Removed

- Variable `ship-mate-subcommand-map`.

## [0.3.4]

### Fixed

- `recompile` is no longer captured in other modes derived from
  `compilation-mode`.
- `ship-mate-command--fuzzy-match` no longer matches empty strings,
  whitespace or `--`.

## [0.3.3]

### Added

- Command `ship-mate-hide` bound to `C-c C-q` in
  `ship-mate-dinghy-mode`. It hides a running compilation like other
  hidden compilations.
- The currently run command is now shown in the header in
  `ship-mate-dinghy-mode`.

### Changed

- Function that variable `ship-mate-command-fuzzy-match-function`
  should now return a plist on match, returning match, count and
  index. This is used during `ship-mate-command--update-history` to
  replace a matched entry if the match count is higher that two.
- Bindings in `ship-mate-dinghy-mode-map` now adhere to **Key Binding
  Conventions** (only binding to keys allocated for minor modes).

## [0.3.2]

### Added

- Additional commands are now bound to `ship-mate-subcommand-map`
  which is bound to `x` in `ship-mate-command-map`. This is now the
  only letter bound. The key used can be customized using
  `ship-mate-subcommands-key`.

### Fixed

- `ship-mate-select-command` now passes the prefix argument to the
  command.
- Calling `ship-mate-hidden-recompile` directly or indirectly quits
  windows displaying a `ship-mate` buffer.

## [v0.3.1]

### Added

- Calling `ship-mate-edit-environment` from outside a `ship-mate`
  buffer now completes `ship-mate` buffers in the project to edit
  their environment.
- Calling `ship-mate-command` with numeric prefix 5 now prompts the
  user to edit the environment in the minibuffer. The prefix value can
  be customized through `ship-mate-edit-environment-prefix`.
- `ship-mate-hidden-recompile` now prompts after customizable
  `ship-mate-prompt-for-hidden-buffer` which is now set to 2
  (seconds).
- Command `ship-mate-edit-history` to edit the history.
- A lighter that provides the `ship-mate-command-map` as a menu.
- Calling `ship-mate-command` with numeric prefix 3 now runs the
  command the same way `ship-mate-hidden-recompile` does. The prefix
  value can be customized using `ship-mate-hidden-compilation-prefix`.
- If a hidden compilation fails, the exit status is shown in the
  prompt.
- Hidden compilations can be surfaced by `ship-mate-show-hidden` or by
  clicking the mode line.

### Changed

- Calling `ship-mate-hidden-recompile` when the previous command
  wasn't a `ship-mate-command` no longer recompiles.
- Prompting is now the default for hidden compilation (see above).

## [v0.3.0]

### Added

- The history size can be customize using
  `ship-mate-command-history-size`.
- The function used to fuzzy-match commands against histories can now
  be customized using `ship-mate-command-fuzzy-match-function`.
- The generator used to generate a buffer-name function can now be
  customized using `ship-mate-command-buffer-name-function-generator`.
- Minor-mode `ship-mate-dinghy-mode` is enabled in all `compilation`
  buffers created by `ship-mate` unless `ship-mate-dinghy-enable` is
  `nil`. It displays the current `compilation-environment` and binds
  `ship-mate-edit-environment` as well as
  `ship-mate-command-next-buffer` and `ship-mate-command-prev-buffer`.
- Command `ship-mate-refresh-history` to either reset to defaults to
  clear a command's history (in the project).
- Command `ship-mate-hidden-recompile`, bound in
  `ship-mate-command-map` to `r`. It will call `recompile` but only
  show the compilation buffer once the process has finished. User may
  set `ship-mate-prompt-for-hidden-buffer` if they should be prompted
  first.

### Fixed

- Empty lines are filtered out by `ship-mate-environment--listify`
  which means calling `ship-mate-environment-apply` with an empty
  buffer also works.
- `recompile` is now advised to instead call `ship-mate-command` when
  a match happens unless the current buffer is a compilation buffer
  already.

## [v0.2.0]

### Added

- Variable `ship-mate-environment` that can be set to prefill
  `compilation-environment`.
- Compilation buffers are now named using the current project.
- Command `ship-mate-edit-environment`. It will open a buffer with the
  `compilation-environment` of the current compilation buffer and
  allows the user to freely edit and apply it.

### Changed

- Prefix for environment editing is now `environment`, no longer
  `env`.
- Prefixes have been harmonized, meaning there are no more plain
  `ship-mate-` functions.

## [v0.1.0]

Initial release of the package that was spliced out of my own
configuration.
