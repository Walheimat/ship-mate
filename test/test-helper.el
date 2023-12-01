;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'bydi-ci)
(require 'bydi-report)

;; Setup

(setq byte-compile-warnings '(not not-unused))

(bydi-ci-setup-paths)
(bydi-report-setup-undercover (list "ship-mate.el"))
(bydi-report-setup-ert-runner)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
