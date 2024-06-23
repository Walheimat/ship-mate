;;; ship-mate-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate-dinghy)

(ert-deftest ship-mate-dinghy-mode ()
  (bydi (ship-mate-dinghy--reset-header-line-format)

    (ert-with-test-buffer (:name "dinghy")
      (should-error (ship-mate-dinghy-mode))

      (setq major-mode 'compilation-mode)

      (ship-mate-dinghy-mode)

      (bydi-was-called ship-mate-dinghy--reset-header-line-format))))

(ert-deftest ship-mate-dinghy--maybe-enable ()
  (bydi (ship-mate-dinghy-mode
         (:watch ship-mate-dinghy--command)
         (:mock process-command :return '("usr/bin/sh" "-c" "make test")))

    (let ((ship-mate-dinghy-enable nil))

      (ship-mate-dinghy--maybe-enable)

      (bydi-was-not-called ship-mate-dinghy-mode)

      (setq ship-mate-dinghy-enable t)

      (ert-with-test-buffer (:name "*ship-mate-maybe*")

        (setq ship-mate--this-command 'test)

        (ship-mate-dinghy--maybe-enable)

        (bydi-was-called ship-mate-dinghy-mode)
        (bydi-was-not-set ship-mate-dinghy--command)

        (ship-mate-dinghy--maybe-enable 'process)

        (bydi-was-set-to ship-mate-dinghy--command '("usr/bin/sh" "-c" "make test"))))))

(ert-deftest ship-mate-dinghy--print-variables ()
  (let ((compilation-environment nil))

    (should (string= "none" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (should (string= "TES MOC" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING" "TRY=ING" "MY=BEST"))

    (should (string= "active" (ship-mate-dinghy--print-variables)))))

(ert-deftest ship-mate-dinghy--print-command ()
  (let ((ship-mate-dinghy--command nil))

    (should (string= "?" (ship-mate-dinghy--print-command)))

    (setq ship-mate-dinghy--command '("/usr/bin/sh" "-c" "make test"))

    (should (string= "make test" (ship-mate-dinghy--print-command)))

    (setq ship-mate-dinghy--command '("/usr/bin/sh" "-c" "/usr/bin/fish"))

    (should (string= "/usr/bin/sh -c /usr/…" (ship-mate-dinghy--print-command)))))

(ert-deftest ship-mate-dinghy--reset-header-line-format ()
  (bydi ((:mock ship-mate-dinghy--print-variables :return "test")
         (:mock ship-mate-dinghy--print-command :return "make test"))

    (ert-with-test-buffer (:name "header")
      (setq-local ship-mate-dinghy-mode t)

      (ship-mate-dinghy--reset-header-line-format)

      (should (string= header-line-format "cmd[make test] env[test]"))

      (ship-mate-dinghy--reset-header-line-format (current-buffer))

      (should (string= header-line-format "cmd[make test] env[test]")))))

(ert-deftest ship-mate-dinghy-global-mode--setup-and-teardown ()
  (bydi ((:risky-mock add-hook :with always)
         (:risky-mock remove-hook :with always))

    (ship-mate-dinghy-global-mode--setup)
    (ship-mate-dinghy-global-mode--teardown)

    (bydi-was-called-n-times add-hook 2)
    (bydi-was-called-n-times remove-hook 2)))

(ert-deftest ship-mate-dinghy-global-mode--enable-and-disable ()
  (bydi (ship-mate-dinghy-global-mode--setup
         ship-mate-dinghy-global-mode--teardown)

    (ship-mate-dinghy-global-mode)

    (bydi-was-called ship-mate-dinghy-global-mode--setup)

    (ship-mate-dinghy-global-mode -1)

    (bydi-was-called ship-mate-dinghy-global-mode--teardown)))

;;; ship-mate-dinghy-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
