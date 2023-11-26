# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- The history size can be customize using
  `ship-mate-command-history-size`.
- The function used to fuzzy-match commands against histories can now
  be customized using `ship-mate-command-fuzzy-match-function`.
- The generator used to generate a buffer-name function can now be
  customized using `ship-mate-command-buffer-name-function-generator`.
- Minor-mode `ship-mate-dinghy-mode` is enabled in all `compilation`
  buffers unless `ship-mate-dinghy-enable` is `nil`. It displays the
  current `compilation-environment` and binds
  `ship-mate-edit-environment`.

### Fixed

- Empty lines are filtered out by `ship-mate-environment--listify`
  which means calling `ship-mate-environment-apply` with an empty
  buffer also works.

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
