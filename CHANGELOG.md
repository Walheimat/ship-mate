# Changelog

## [0.6.0](https://github.com/Walheimat/ship-mate/compare/v0.5.0...v0.6.0) (2024-06-29)


### Features

* **cmd:** variable that governs prompting ([09aea92](https://github.com/Walheimat/ship-mate/commit/09aea9209c622e0fdbb10ac296f22d062ac012ce))
* **command:** ship-mate-store-history ([3accc94](https://github.com/Walheimat/ship-mate/commit/3accc942f6ddafa6aa7ac4523aae80c1adde5e79))
* **environment:** allow setting environment per command ([f47f514](https://github.com/Walheimat/ship-mate/commit/f47f514a88e73fd43e092727579c74801101a269))
* **logging:** add and bind ship-mate-show-logs ([5ded9b5](https://github.com/Walheimat/ship-mate/commit/5ded9b543963a47c9d73c91f7c89d4b99d99490b))
* **logging:** add logging ([ad97d64](https://github.com/Walheimat/ship-mate/commit/ad97d642a91282e9499b70899da61d506f458d04))


### Bug Fixes

* **command:** don't overwrite meta data when marking ([06acee7](https://github.com/Walheimat/ship-mate/commit/06acee7fe5d40ee7aff79c5d0beaaca6ccbf12d0))
* **command:** don't record if command is nil ([b5ec1da](https://github.com/Walheimat/ship-mate/commit/b5ec1da10e4c7c20398304b715d3552f750cb1f0))
* **command:** require this-command to be non-nil ([7a2b86f](https://github.com/Walheimat/ship-mate/commit/7a2b86fdcb3b796532913cb7bcd37ccfd55e7201))
* **command:** update history during recompile in ship-mate buffers ([733f06f](https://github.com/Walheimat/ship-mate/commit/733f06f6dd83df4ef79c9e76be0ff635c817d368))
* **fuzzy:** actually return the top matches ([dddb648](https://github.com/Walheimat/ship-mate/commit/dddb6485adf26d9f4def3367e0449b5789947b1a))
* **fuzzy:** don't match single period ([aebcf38](https://github.com/Walheimat/ship-mate/commit/aebcf382ca8b41f2592599e260873cceed8a9351))
* **logging:** return map, not binding ([b85a8cb](https://github.com/Walheimat/ship-mate/commit/b85a8cb666df98902771f53dd182b41d4eb3f95c))


### Changes

* **command:** allow editing history before storage ([437160b](https://github.com/Walheimat/ship-mate/commit/437160bb1c724f4706dd1db5fa9fa95a7d029894))
* **command:** always use update-history ([ab4e641](https://github.com/Walheimat/ship-mate/commit/ab4e641e1da17f856d4e56edc9346ef0a4b71cda))
* **command:** disallow select and rerun outside of projects ([d1a3c3d](https://github.com/Walheimat/ship-mate/commit/d1a3c3df5593141a754cab1a33149774aada10d3))
* **command:** prompt on first run ([44d400b](https://github.com/Walheimat/ship-mate/commit/44d400b08e09f6333bcde770e161be386824026d))
* **command:** replace ship-mate--last-command ([65ae963](https://github.com/Walheimat/ship-mate/commit/65ae96345a667cc1c9c785a1a0dd72a5557473bf))
* **command:** ship-mate-command--{record=>record-last-command} ([d414539](https://github.com/Walheimat/ship-mate/commit/d4145392be4164692a56673cec968164bc52a98e))
* **fuzzy:** give penalty for initial incongruence ([e2658b9](https://github.com/Walheimat/ship-mate/commit/e2658b9e6f5fcb08ddbd319db143917657092a33))
* **history:** don't prompt for replacing ([2fbedf4](https://github.com/Walheimat/ship-mate/commit/2fbedf40af0f1a5654af5b7d9861f4d7190f0c4f))
* **rerun-command:** select command if there is no prev command ([2e9678a](https://github.com/Walheimat/ship-mate/commit/2e9678a239c507097d08de941504557ccfb0f050))

## [0.5.0](https://github.com/Walheimat/ship-mate/compare/v0.4.2...v0.5.0) (2024-03-10)


### Features

* **command:** ship-mate-rerun-command ([40f2690](https://github.com/Walheimat/ship-mate/commit/40f2690726cbf396c5aeb099b6adffc36a402435))


### Bug Fixes

* **command:** don't capture recompile outside of projects ([3834882](https://github.com/Walheimat/ship-mate/commit/383488220970ffc2ac61d5bc2194d0237de6a51e))
* **command:** use custom var and set it to 2 ([0c34933](https://github.com/Walheimat/ship-mate/commit/0c34933be7367ad85ce5de6b9f185710204d9245))
* **fuzzy:** allow full match when capturing ([37b748a](https://github.com/Walheimat/ship-mate/commit/37b748a8c85c6ff64da313ae4beba27a7480123d))
* **submarine:** close any ship-mate buffer ([acf2a94](https://github.com/Walheimat/ship-mate/commit/acf2a948c24b72424231bd7bc6b46bd369db98dd))
* **submarine:** use available bindings ([9695d78](https://github.com/Walheimat/ship-mate/commit/9695d780fc9529327c290a8fa24a124b53afd2c6))


### Changes

* **command:** string-trim commands ([2d274a7](https://github.com/Walheimat/ship-mate/commit/2d274a7d7676bdaad9a875896db0f637415a4d0f))
* **command:** universal prefix forces project select ([ea029c9](https://github.com/Walheimat/ship-mate/commit/ea029c900655b41ec7f77f559a00b2682f519db4))

## [0.4.2](https://github.com/Walheimat/ship-mate/compare/v0.4.1...v0.4.2) (2024-02-07)


### Bug Fixes

* **buffers:** use project-buffers by default ([dce7919](https://github.com/Walheimat/ship-mate/commit/dce79195f0c7590080b59769708f48e98fea14d0))
* **environment:** use regex to validate assignments ([328fe86](https://github.com/Walheimat/ship-mate/commit/328fe863cba7e3b4c2f4d2d7eb2d9249c46033e2))
* **fuzzy:** check ring length not size before replacing ([fddc098](https://github.com/Walheimat/ship-mate/commit/fddc098aa524462983f6e5fe8ce5b33fcb057c69))
* **fuzzy:** don't consider exact matches ([f0851f3](https://github.com/Walheimat/ship-mate/commit/f0851f3939c7afde6085e2ed98eb211570aa9794))


### Changes

* **fuzzy:** ask before replacing match ([a1acdfc](https://github.com/Walheimat/ship-mate/commit/a1acdfc25d5c2f064e4fab8be3f97d67a27b9c14))
* **ship-mate-dinghy:** factor out into own module ([0c98501](https://github.com/Walheimat/ship-mate/commit/0c98501587d3cd54d86aa63169f504837910a116))
* **ship-mate-edit:** factor out in-buffer editing ([32d6199](https://github.com/Walheimat/ship-mate/commit/32d61995e19fc9162cda88af5ce318376605823a))
* **submarine:** factor out into own module ([d2ecf09](https://github.com/Walheimat/ship-mate/commit/d2ecf0960a729062b41ba38c7821e0ed067886eb))

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
