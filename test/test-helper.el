;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'dinghy-rope)

;; Setup

(setq byte-compile-warnings '(not not-unused))

(dinghy-rope-setup-paths)
(dinghy-rope-setup-undercover (list "ship-mate.el"))
(dinghy-rope-setup-ert-runner)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
