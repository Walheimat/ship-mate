;;; ship-mate-edit-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate-edit)


(ert-deftest ship-mate-edit-environment--internal--errors-for-non-comp ()
  :tags '(user-facing env)

  (should-error (ship-mate-edit-environment--internal)))

(ert-deftest ship-mate-environment--creates-buffer ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "env-test")

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (bydi ((:always ship-mate--command-buffer-p))
      (with-current-buffer (ship-mate-edit-environment--internal)

        (should (string= "TES=TING\nMOC=KING" (buffer-string)))))))

(ert-deftest ship-mate-edit-environment--listify ()
  :tags '(env)

  (ert-with-test-buffer (:name "listify")

    (insert "TES=TING\n\nMOC=KING")

    (let ((ship-mate-edit-environment-buffer-name (buffer-name)))

      (should (equal '("TES=TING" "MOC=KING") (ship-mate-edit-environment--listify))))))

(ert-deftest ship-mate-edit-environment--validate ()
  :tags '(env)

  (let ((list '("TES=TING" "MOC=KING")))

    (bydi ((:mock ship-mate-edit-environment--listify :return list))

      (should-not (ship-mate-edit-environment--validate))

      (setq list '("TESTING" "MOC=KING"))

      (should (equal '("Invalid assignments") (ship-mate-edit-environment--validate))))))

(ert-deftest ship-mate-edit-environment-apply--errors-if-validation-fails ()
  :tags '(user-facing env)

  (bydi ((:mock ship-mate-edit-environment--validate :return '("Test error a" "Test error b")))
    (should-error (ship-mate-edit-environment-apply) :type 'user-error)))

(ert-deftest ship-mate-edit-environment-apply ()
  :tags '(user-facing env)

  (bydi ((:ignore ship-mate-edit-environment--validate)
         (:mock ship-mate-edit-environment--listify :return '("TES=TING"))
         (:ignore ship-mate-edit-environment--quit)
         (:watch compilation-environment))

    (ert-with-test-buffer (:name "apply")
      (setq ship-mate-edit-environment--target-buffer (current-buffer))

      (ship-mate-edit-environment-apply)

      (bydi-was-set-to compilation-environment '("TES=TING"))

      (bydi-was-called ship-mate-edit-environment--quit))))

(ert-deftest ship-mate-edit-environment--quit ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "quit")
    (let ((ship-mate-edit-environment-buffer-name (buffer-name))
          (ship-mate-edit-environment--target-buffer (current-buffer)))

      (bydi ((:spy quit-window)
             (:watch ship-mate-edit-environment--target-buffer))

        (ship-mate-edit-environment--quit)

        (bydi-was-called quit-window)
        (bydi-was-set-to ship-mate-edit-environment--target-buffer nil)))))

(ert-deftest ship-mate-edit-environment-abort ()
  :tags '(user-facing env)

  (bydi (ship-mate-edit-environment--quit)

    (ship-mate-edit-environment-abort)

    (bydi-was-called ship-mate-edit-environment--quit)))

(ert-deftest ship-mate-edit-environment-clear ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "clear")
    (setq-local compilation-environment '("TES=TING"))

    (setq ship-mate-edit-environment--target-buffer (current-buffer))

    (bydi (ship-mate-edit-environment--quit)
      (ship-mate-edit-environment-clear)

      (should-not compilation-environment)
      (bydi-was-called ship-mate-edit-environment--quit))))

(ert-deftest ship-mate-edit-environment ()
  :tags '(user-facing env)

  (bydi ((:always ship-mate--command-buffer-p)
         ship-mate-edit-environment--internal)

    (call-interactively 'ship-mate-edit-environment)

    (bydi-was-called ship-mate-edit-environment--internal)))

(ert-deftest ship-mate-edit-environment--completes ()
  :tags '(user-facing env)

  (let ((buffer (current-buffer)))

    (bydi ((:ignore ship-mate--command-buffer-p)
           (:mock ship-mate--complete-buffer :return buffer)
           ship-mate-edit-environment--internal)

      (call-interactively 'ship-mate-edit-environment)

      (bydi-was-called ship-mate-edit-environment--internal :clear t)

      (setq buffer nil)

      (should-error (call-interactively 'ship-mate-edit-environment)))))

;;;; Editing history

(ert-deftest ship-mate-edit-history--completes-for-foreign ()
  :tags '(user-facing history)

  (bydi ((:mock ship-mate--complete-buffer :return (current-buffer))
         (:ignore ship-mate--command-buffer-p)
         ship-mate-edit-history--internal)

    (call-interactively 'ship-mate-edit-history)

    (bydi-was-called ship-mate-edit-history--internal)))

(ert-deftest ship-mate-history-edit--errors-for-non-ship-mate ()
  :tags '(user-facing history)

  (bydi ((:ignore ship-mate--command-buffer-p))

    (should-error (ship-mate-edit-history--internal))))

(ert-deftest ship-mate-history--creates-buffer ()
  :tags '(user-facing history)

  (ert-with-test-buffer (:name "history-test")

    (let ((ring (make-ring 1)))

      (ring-insert ring "make test")

      (setq-local ship-mate--this-command 'test)

      (bydi ((:always ship-mate--command-buffer-p)
             (:mock ship-mate-command--history :return ring))

        (with-current-buffer (ship-mate-edit-history--internal)

          (should (string= "make test" (buffer-string))))))))

(ert-deftest ship-mate-history--api ()
  :tags '(user-facing history)

  (bydi (ship-mate-edit-history--set-history
         ship-mate-edit-history--quit
         (:mock ship-mate-edit-history--listify :return '("make test")))

    (ship-mate-edit-history-abort)

    (bydi-was-called ship-mate-edit-history--quit :clear t)
    (bydi-was-not-called ship-mate-edit-history--set-history)

    (ship-mate-edit-history-apply)
    (ship-mate-edit-history-clear)

    (bydi-was-called-nth-with ship-mate-edit-history--set-history '(("make test")) 0)
    (bydi-was-called-nth-with ship-mate-edit-history--set-history nil 1)
    (bydi-was-called-n-times ship-mate-edit-history--quit 2)))

(ert-deftest ship-mate-edit-history--set-history ()
  :tags '(history)

  (let ((ring (make-ring 2)))

    (ring-insert ring "make test")

    (bydi ((:mock ship-mate-command--history :return ring))

      (ship-mate-edit-history--set-history '("make best" "make hest"))

      (should (equal '("make hest" "make best") (ring-elements ring))))))

(ert-deftest ship-mate-edit-history--quit ()
  :tags '(user-facing history)

  (ert-with-test-buffer (:name "history-quit")

    (let ((ship-mate-edit-history-buffer-name (buffer-name))
          (ship-mate-edit-history--command 'test))

      (bydi (quit-window
             (:watch ship-mate-edit-history--command))

        (ship-mate-edit-history--quit)

        (bydi-was-called quit-window)
        (bydi-was-set-to ship-mate-edit-history--command nil)))))

(ert-deftest ship-mate-edit-history--listify ()
  :tags '(history)

  (bydi ((:mock ship-mate-edit--listify-buffer :return '("make test" "make rest")))

    (should (equal '("make rest" "make test") (ship-mate-edit-history--listify)))))

(ert-deftest ship-mate-edit-history ()
  :tags '(user-facing history)

  (let ((buf))
    (bydi ((:sometimes ship-mate--command-buffer-p)
           (:mock ship-mate--complete-buffer :return buf)
           ship-mate-edit-history--internal)

      (call-interactively 'ship-mate-edit-history)

      (bydi-was-called ship-mate-edit-history--internal :clear t))))

(ert-deftest ship-mate-edit-setup-bindings ()
  :tags '(user-facing edit)

  (let ((ship-mate-command-map (make-sparse-keymap))
        (ship-mate-dinghy-mode-map (make-sparse-keymap)))

    (ship-mate-edit-setup-bindings)

    (should (where-is-internal 'ship-mate-edit-environment ship-mate-command-map))
    (should (where-is-internal 'ship-mate-edit-history ship-mate-command-map))

    (should (where-is-internal 'ship-mate-edit-environment ship-mate-dinghy-mode-map))
    (should (where-is-internal 'ship-mate-edit-environment ship-mate-dinghy-mode-map))))

;;; ship-mate-edit-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
