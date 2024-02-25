;;; ship-mate-submarine-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate-submarine)

(ert-deftest ship-mate-submarine--maybe-run ()
  :tags '(submarine)

  (bydi ((:watch ship-mate-command--executor))

    (ship-mate-submarine--maybe-run #'ignore nil nil nil)

    (bydi-was-set-to ship-mate-command--executor 'funcall)

    (let ((ship-mate-submarine-hidden-compilation-prefix 3))

      (ship-mate-submarine--maybe-run #'ignore nil nil 3)

      (bydi-was-set-to ship-mate-command--executor 'ship-mate-submarine--run))))

(ert-deftest ship-mate-submarine--in-progress ()
  :tags '(submarine)

  (let ((ship-mate-submarine--processes nil))

    (should-not (ship-mate-submarine--in-progress))

    (push 'process ship-mate-submarine--processes)

    (should (ship-mate-submarine--in-progress))))

(ert-deftest ship-mate-submarine--run ()
  :tags '(submarine)

  (bydi ((:ignore ship-mate-submarine--ensure-no-buffers)
         (:watch display-buffer-alist))

    (ert-with-test-buffer (:name "sub-run")

      (let ((ship-mate--current-command-name "test"))
        (shut-up (ship-mate-submarine--run (lambda () (current-buffer)))))

      (should (buffer-local-value 'ship-mate--hidden (current-buffer)))
      (bydi-was-called ship-mate-submarine--ensure-no-buffers)
      (bydi-was-set display-buffer-alist))))

(ert-deftest ship-mate-submarine--hide ()
  :tags '(submarine)

  (let ((ship-mate--this-command 'test))

    (bydi ((:othertimes ship-mate-submarine--get-process)
           (:watch ship-mate--hidden)
           quit-window)

      (should-error (ship-mate-submarine--hide))

      (bydi-toggle-sometimes)

      (ert-with-test-buffer (:name "sub-hide")
        (shut-up (ship-mate-submarine--hide))

        (bydi-was-set-to ship-mate--hidden t)))))

(ert-deftest ship-mate-submarine--get-process ()
  :tags '(submarine)

  (should-not (ship-mate-submarine--get-process nil))

  (ert-with-test-buffer (:name "sub-proc")

    (should-not (ship-mate-submarine--get-process (current-buffer)))

    (bydi ((:spy get-buffer-process)
           (:always ship-mate--command-buffer-p))

      (ship-mate-submarine--get-process (current-buffer))

      (bydi-was-called get-buffer-process))))

(ert-deftest ship-mate-submarine--ensure-no-buffers ()
  :tags '(submarine)

  (ert-with-test-buffer (:name "sub-ensure")

    (setq ship-mate--this-command 'test)

    (bydi ((:spy quit-window))

      (pop-to-buffer (current-buffer))

      (ship-mate-submarine--ensure-no-buffers)

      (bydi-was-called quit-window))))

(ert-deftest ship-mate-submarine--check ()
  :tags '(submarine)

  (let ((ship-mate-submarine--processes nil))

    (bydi ((:ignore ship-mate-submarine--clear-timer)
           (:spy ship-mate-submarine--surface))

      (ship-mate-submarine--check)

      (bydi-was-called ship-mate-submarine--clear-timer)
      (bydi-was-not-called ship-mate-submarine--surface)))

  (let ((ship-mate-submarine--processes '(a b))
        (ship-mate-submarine-prompt-for-hidden-buffer nil))

    (ert-with-test-buffer (:name "sub-check")
      (bydi ((:ignore ship-mate-submarine--clear-timer)
             (:sometimes process-live-p)
             (:mock process-buffer :return (current-buffer))
             (:mock process-exit-status :return 'status)
             (:sometimes ship-mate-submarine--hidden-buffer-p)
             run-with-idle-timer
             ship-mate-submarine--clear-process
             ship-mate-submarine--surface)

        ;; Process is live, won't surface.
        (ship-mate-submarine--check)
        (bydi-was-not-called ship-mate-submarine--surface)

        (bydi-toggle-volatile 'process-live-p)

        ;; Process is dead, will surface.
        (ship-mate-submarine--check)
        (bydi-was-called ship-mate-submarine--surface t)

        ;; Runs with idle timer if set.
        (setq ship-mate-submarine-prompt-for-hidden-buffer t)
        (ship-mate-submarine--check)
        (bydi-was-called ship-mate-submarine--clear-process t)
        (bydi-was-not-called ship-mate-submarine--surface)
        (bydi-was-called run-with-idle-timer t)

        ;; Clears process if not a hidden buffer.
        (bydi-toggle-volatile 'ship-mate-submarine--hidden-buffer-p)
        (ship-mate-submarine--check)
        (bydi-was-called ship-mate-submarine--clear-process)
        (bydi-was-not-called ship-mate-submarine--surface)))))

(ert-deftest ship-mate-submarine--hidden-buffer-p ()
  :tags '(submarine)

  (ert-with-test-buffer (:name "sub-hidden-p")
    (should-not (ship-mate-submarine--hidden-buffer-p (current-buffer)))

    (setq ship-mate--hidden t)

    (should (ship-mate-submarine--hidden-buffer-p (current-buffer)))))

(ert-deftest ship-mate-submarine--delayed-prompt ()
  :tags '(submarine)

  (bydi ((:always yes-or-no-p)
         ship-mate-submarine--surface)

    (ship-mate-submarine--delayed-prompt (current-time) 0 'process)
    (ship-mate-submarine--delayed-prompt (current-time) 1 'process)

    (bydi-was-called-n-times ship-mate-submarine--surface 2)))

(ert-deftest ship-mate-submarine--watch-process ()
  :tags '(submarine)

  (let ((ship-mate-submarine--processes nil)
        (ship-mate-submarine--timer))

    (bydi (run-with-timer
           (:mock process-buffer :return (current-buffer))
           (:othertimes ship-mate--command-buffer-p)
           (:watch ship-mate-submarine--timer)
           (:watch ship-mate-submarine--processes))

      (ship-mate-submarine--watch-process 'process)

      (bydi-was-not-set ship-mate-submarine--timer)

      (bydi-toggle-sometimes)

      (ship-mate-submarine--watch-process 'process)

      (bydi-was-called run-with-timer)
      (bydi-was-set ship-mate-submarine--timer)
      (bydi-was-set ship-mate-submarine--processes))))

(ert-deftest ship-mate-submarine--clear-process ()
  :tags '(submarine)

  (let ((ship-mate-submarine--processes '(a c)))

    (bydi (ship-mate-submarine--clear-timer
           (:watch ship-mate-submarine--processes))

      (ship-mate-submarine--clear-process 'b)

      ;; Always clear timer, don't update processes for unknown
      ;; processes.
      (bydi-was-called ship-mate-submarine--clear-timer)
      (bydi-was-not-set ship-mate-submarine--processes)

      ;; Clear process.
      (ship-mate-submarine--clear-process 'a)
      (bydi-was-set ship-mate-submarine--processes))))

(ert-deftest ship-mate-submarine--clear-timer ()
  :tags '(submarine)

  (let ((ship-mate-submarine--timer 'timer))

    (bydi ((:sometimes ship-mate-submarine--in-progress)
           cancel-timer
           (:watch ship-mate-submarine--timer))

      ;; Doesn't clear if in progress.
      (ship-mate-submarine--clear-timer)
      (bydi-was-not-called cancel-timer)
      (bydi-was-not-set ship-mate-submarine--timer)

      (bydi-toggle-sometimes)

      (ship-mate-submarine--clear-timer)
      (bydi-was-called cancel-timer)
      (bydi-was-set ship-mate-submarine--timer))))

(ert-deftest ship-mate-submarine--surface ()
  :tags '(submarine user-facing)

  (should-error (ship-mate-submarine--surface nil))

  (bydi (ship-mate-submarine--clear-process
         (:mock process-buffer :return (current-buffer))
         (:othertimes ship-mate--buffer-visible-p)
         (:watch ship-mate--hidden)
         (:spy pop-to-buffer))

    (ert-with-test-buffer (:name "sub-surf")

      (ship-mate-submarine--surface 'process)

      (bydi-was-called ship-mate-submarine--clear-process)
      (bydi-was-set-to ship-mate--hidden nil)
      (bydi-was-called pop-to-buffer t)

      ;; Doesn't surface already visible buffer.
      (bydi-toggle-sometimes)
      (ship-mate-submarine--surface 'process)
      (bydi-was-not-called pop-to-buffer))))

(ert-deftest ship-mate-submarine-lighter ()
  :tags '(submarine user-facing)

  (bydi ((:sometimes ship-mate-submarine--in-progress))

    (should (ship-mate-submarine-lighter))

    (bydi-toggle-sometimes)

    (should-not (ship-mate-submarine-lighter))))

(ert-deftest ship-mate-submarine-mode--setup-and-teardown ()
  :tags '(submarine)

  (let ((ship-mate-mode-lighter '("test"))
        (ship-mate-command-map (make-sparse-keymap))
        (ship-mate-dinghy-mode-map (make-sparse-keymap)))

    (bydi ((:spy add-hook)
           (:spy advice-add)
           (:spy remove-hook)
           (:spy advice-remove)
           (:watch ship-mate-mode-lighter))

      (ship-mate-submarine-mode--setup)

      ;; FIXME: Spying also adds advice ...
      (bydi-was-called advice-add)
      (bydi-was-called-n-times add-hook 1)

      (should (where-is-internal 'ship-mate-submarine-hidden-recompile ship-mate-command-map))
      (should (where-is-internal 'ship-mate-submarine-hide ship-mate-dinghy-mode-map))

      (ship-mate-submarine-mode--teardown)

      (bydi-was-called advice-remove)
      (bydi-was-called-n-times remove-hook 1)

      (bydi-was-set-n-times ship-mate-mode-lighter 2))))

(ert-deftest sip-mate-submarine-mode ()
  :tags '(submarine user-facing)

  (bydi (ship-mate-submarine-mode--setup
         ship-mate-submarine-mode--teardown)

    (ship-mate-submarine-mode)
    (ship-mate-submarine-mode -1)

    (bydi-was-called ship-mate-submarine-mode--setup)
    (bydi-was-called ship-mate-submarine-mode--teardown)))

(ert-deftest ship-mate-submarine-hidden-recompile ()
  :tags '(submarine user-facing)

  (ert-with-test-buffer (:name "recompile")
    (setq-local ship-mate--this-command 'test)

    (bydi ((:mock ship-mate--complete-buffer :return (current-buffer))
           ship-mate-submarine--run
           (:watch ship-mate--current-command-name))

      (call-interactively 'ship-mate-submarine-hidden-recompile)

      (bydi-was-set-to ship-mate--current-command-name 'test)
      (bydi-was-called-with ship-mate-submarine--run 'recompile))))

(ert-deftest ship-mate-submarine-show-hidden ()
  :tags '(user-facing submarine)

  (bydi (ship-mate-submarine--surface
         (:mock ship-mate--complete-buffer :with (lambda (_p l) (funcall l '(a . buffer))))
         (:mock process-buffer :return (current-buffer)))

    (let ((ship-mate-submarine--processes '(process)))

      (call-interactively 'ship-mate-submarine-show-hidden)

      (bydi-was-called ship-mate-submarine--surface 'process)
      (bydi-was-not-called ship-mate--complete-buffer)

      (setq ship-mate-submarine--processes '(a b))

      (call-interactively 'ship-mate-submarine-show-hidden)

      (bydi-was-called ship-mate-submarine--surface nil)
      (bydi-was-called ship-mate--complete-buffer))))

(ert-deftest ship-mate-hide-visible ()
  :tags '(user-facing submarine)

  (let ((buffers (list (current-buffer))))

    (bydi ((:mock ship-mate-command--buffers :return buffers)
           (:always ship-mate--buffer-visible-p)
           ship-mate-submarine--hide)

      (ship-mate-submarine-hide-visible)

      (bydi-was-called ship-mate-submarine--hide))))

(ert-deftest ship-mate-submarine-hide ()
  :tags '(user-facing submarine)

  (bydi ship-mate-submarine--hide
    (ship-mate-submarine-hide)

    (bydi-was-called ship-mate-submarine--hide)))

;;; ship-mate-submarine-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
