# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Additional commands are now bound to `ship-mate-subcommand-map`
  which is bound to `x` in `ship-mate-command-map`. This is now the
  only letter bound. The key used can be customized using
  `ship-mate-subcommands-key`.

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
