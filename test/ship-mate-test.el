;;; ship-mate-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate nil t)

(ert-deftest ship-mate--plist-keys--errors-if-invalid ()
  (should-error (ship-mate--plist-keys '(:test a :best))))

(ert-deftest ship-mate--plist-keys--extracts-keys ()
  (should (equal '(:test :this :function) (ship-mate--plist-keys '(:test "whether" :this "hacky" :function "works")))))

(ert-deftest ship-mate-environment--valid-env-p ()
  :tags '(user-facing)

  (should-not (ship-mate-environment--valid-env-p '("hello" "world")))
  (should-not (ship-mate-environment--valid-env-p "hello=world"))
  (should-not (ship-mate-environment--valid-env-p '("hello=world" "test")))
  (should (ship-mate-environment--valid-env-p '("hello=world" "test=ing")))
  (should-not (ship-mate-environment--valid-env-p '("hello=world" "test=ing" ""))))

(ert-deftest ship-mate-with-bounded-compilation ()
  :tags '(bounded-comp)

  (bydi ((:always project-current)
         (:mock project-buffers :with buffer-list))

    (let ((fun (lambda () (funcall compilation-save-buffers-predicate))))

      (should (ship-mate-with-bounded-compilation fun)))))

(ert-deftest ship-mate-with-bounded-compilation--ignored-outside-project ()
  :tags '(bounded-comp)

  (bydi ((:ignore project-current))

    (let ((fun #'always))

      (should (ship-mate-with-bounded-compilation fun)))))

(ert-deftest ship-mate-command--buffer-name ()
  :tags '(command)

  (let* ((ship-mate-command--current-command-name "test")
         (fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-test-test*")))

  (let ((fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-compile-test*"))))

(ert-deftest ship-mate-command ()
  :tags '(command)

  (defvar ship-mate-test-default-cmd nil)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-test-default-cmd "untest")
        (ship-mate-command-history nil)
        (entered-command nil))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project-buffers :return (list (current-buffer)))
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           (:mock compile :with (lambda (&rest _)
                                  (funcall compilation-save-buffers-predicate)
                                  (current-buffer)))
           (:mock read-shell-command :return entered-command)
           ship-mate-dinghy-mode)

      (setq entered-command "test")

      (ship-mate-command 'test t)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'ship-mate-command-history))
      (bydi-was-called-with compile '("test" nil))

      (setq entered-command "best")
      (ship-mate-command 'test t)
      (bydi-was-called-with compile '("best" nil))

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 0))))))

(ert-deftest ship-mate-command--only-inserted-once ()
  :tags '(command)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-command-history nil)
        (entered-command "test"))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project-buffers :return (list (current-buffer)))
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           (:mock compile :return (current-buffer))
           (:mock read-shell-command :return entered-command)
           ship-mate-dinghy-mode)

      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)))))
      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test))))))))

(ert-deftest ship-mate-command--no-let-bind-for-existing-env ()
  :tags '(command)

  (let ((compilation-environment nil)
        (env nil)
        (ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (history (make-ring 1)))

    (ring-insert history "make test")

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/env")
           (:mock project-name :return "Test Project")
           (:mock ship-mate-command--history :return history)
           (:mock ship-mate--local-value :return env)
           (:watch compilation-environment)
           (:mock compile :return (current-buffer)))

      (ship-mate-command 'test)

      (bydi-was-set compilation-environment)
      (setq compilation-environment '("TES=TING"))

      (bydi-clear-mocks-for 'compilation-environment)

      (ship-mate-command 'test)

      (bydi-was-not-set compilation-environment))))

(ert-deftest ship-mate-command--compile--editing-env ()
  (let ((env '("TES=TING"))
        (edited '("MOC=KING"))
        (compilation-environment nil))

    (bydi ((:mock ship-mate-environment--edit-in-minibuffer :return edited)
           (:mock ship-mate-environment--current-environment :return env)
           (:watch compilation-environment)
           compile)

      (ship-mate-command--compile "make test")

      (bydi-was-called ship-mate-environment--current-environment t)
      (bydi-was-not-called ship-mate-environment--edit-in-minibuffer)
      (bydi-was-set-to compilation-environment '("TES=TING") t)

      (setq compilation-environment env)
      (ship-mate-command--compile "make test" nil '(5))

      (bydi-was-not-called ship-mate-environment--current-environment)
      (bydi-was-called ship-mate-environment--edit-in-minibuffer)
      (bydi-was-set-to compilation-environment '("MOC=KING")))))

(ert-deftest ship-mate-environment--current-environment ()
  (ert-with-test-buffer (:name "last-env")

    (setq-local compilation-environment '("TES=TING"))

    (bydi (ship-mate--local-value)

      (let ((compilation-buffer-name-function (lambda (_) (buffer-name))))

        (should (equal '("TES=TING") (ship-mate-environment--current-environment)))))))

(ert-deftest ship-mate-command--history--inserts-multiple ()
  :tags '(command)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-command-history nil)
        (ship-mate-test-default-cmd '("make test" "test make")))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project--value-in-dir :return ship-mate-test-default-cmd))

      (let ((history (ship-mate-command--history 'test)))

        (should (string= "make test" (ring-ref history 0)))
        (should (string= "test make" (ring-ref history 1)))))))

(ert-deftest ship-mate-command--valid-default-p ()
  (should (ship-mate-command--valid-default-p "test"))
  (should (ship-mate-command--valid-default-p '("test" "make")))
  (should-not (ship-mate-command--valid-default-p '("test" make))))

(ert-deftest ship-mate-command--update-history ()
  :tags '(command)

  (let ((fake-history (make-ring 3))
        (ship-mate-command--last-command 'test))

    (ring-insert fake-history "make test")

    (bydi ((:mock ship-mate-command--history :return fake-history))

      (ship-mate-command--update-history "make new")

      (should (equal (ring-elements fake-history)
                     '("make test")))

      (ship-mate-command--update-history "make test FLAG=t")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=t" "make test")))

      (ship-mate-command--update-history "make way")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=t" "make test")))

      (ship-mate-command--update-history "make test")

      (should (equal (ring-elements fake-history)
                     '("make test" "make test FLAG=t"))))))

(ert-deftest ship-mate-command--capture ()
  :tags '(command)

  (let ((compile-history '("make test"))
        (compile-command "make best")
        (ship-mate-command--last-command nil)
        (history (make-ring 2))
        (matches nil))

    (bydi ((:mock ship-mate-command--history :return history)
           (:mock ship-mate-command--fuzzy-match-p :return matches)
           (:mock ship-mate--local-value :return environment)
           ship-mate-command)

      (ring-insert history "make history")

      (ship-mate-command--capture #'ignore)

      (bydi-was-not-called ship-mate-command)

      (setq ship-mate-command--last-command 'test
            matches t)

      (ship-mate-command--capture #'ignore)

      (bydi-was-called ship-mate-command))))

(ert-deftest ship-mate-command--capture--in-compilation-buffer ()
  :tags '(command)

  (ert-with-test-buffer (:name "*ship-mate-in-comp*")

    (let ((history (make-ring 1))
          (ship-mate-command-category 'test))

      (bydi ((:mock ship-mate-command--history :return history))

        (should (ship-mate-command--capture (lambda (&rest _) (current-buffer))))))))

(ert-deftest ship-mate-create-command ()
  :tags '(user-facing command)

  (bydi ((:mock make-hash-table :return 'hash-table))
    (bydi-match-expansion
     (ship-mate-create-command test)
     '(progn
        (defvar-local ship-mate-test-default-cmd nil "Default for `ship-mate-test'.")
        (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
               (interactive "P")
               (ship-mate-command 'test arg))
        (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
        (define-key ship-mate-command-map "t" 'ship-mate-test)
        (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p)))
    (bydi-match-expansion
     (ship-mate-create-command test :key "o" :default "make all")
     '(progn
        (defvar-local ship-mate-test-default-cmd "make all" "Default for `ship-mate-test'.")
        (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
               (interactive "P")
               (ship-mate-command 'test arg))
        (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
        (define-key ship-mate-command-map "o" 'ship-mate-test)
        (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p)))))

(ert-deftest ship-mate-command--buffers ()
  (let ((a (get-buffer-create "*ship-mate-a*"))
        (b (get-buffer-create "other"))
        (c (get-buffer-create "*ship-mate-c*"))
        (default-directory "/tmp/default"))

    (with-current-buffer a
      (setq default-directory "/tmp/other"))

    (with-current-buffer b
      (setq default-directory "/tmp/default"))

    (with-current-buffer c
      (setq default-directory "/tmp/default"))

    (bydi ((:mock buffer-list :return (list a b c)))

      (should (eq (length (ship-mate-command--buffers)) 1)))))

(ert-deftest ship-mate-command--next-and-prev ()
  (let ((current 'b)
        (buffers '(a b c)))
    (bydi ((:mock current-buffer :return current)
           (:mock ship-mate-command--buffers :return buffers)
           (:mock switch-to-buffer :with bydi-rf))

      (should (eq 'c (ship-mate-command-next-buffer)))
      (should (eq 'a (ship-mate-command-prev-buffer)))

      (setq current 'a)

      (should (eq 'b (ship-mate-command-next-buffer)))
      (should (eq 'c (ship-mate-command-prev-buffer)))

      (setq current 'c)

      (should (eq 'a (ship-mate-command-next-buffer)))
      (should (eq 'b (ship-mate-command-prev-buffer)))

      (setq buffers '(c))

      (should-error (ship-mate-command-next-buffer))
      (should-error (ship-mate-command-prev-buffer)))))

(ert-deftest ship-mate-select-command ()
  :tags '(user-facing command)

  (bydi ((:mock ship-mate--read-command :return "test")
         ship-mate-command)
    (call-interactively 'ship-mate-select-command)

    (bydi-was-called-with ship-mate-command (list 'test))))

(ert-deftest ship-mate--local-value ()
  (ert-with-temp-file project

    (bydi ((:always project-current)
           (:mock project-root :return project)
           (:mock project--value-in-dir :return 'text-mode))

      (should (equal (ship-mate--local-value 'major-mode) 'text-mode)))))

(ert-deftest ship-mate--read-command ()
  (bydi ((:always completing-read)
         (:mock ship-mate--plist-keys :return '("one" "two")))

    (ship-mate--read-command "Test: ")

    (bydi-was-called-with completing-read '("Test: " ("one" "two") nil t))))

(ert-deftest ship-mate-mode--setup ()
  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:risky-mock advice-add :with always)
           (:risky-mock add-hook :with always))
      (ship-mate-mode--setup)
      (bydi-was-called-n-times advice-add 3)
      (bydi-was-called-n-times add-hook 2))))

(ert-deftest ship-mate-mode--teardown()
  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:risky-mock advice-remove :with always)
           (:risky-mock remove-hook :with always))
      (ship-mate-mode--teardown)
      (bydi-was-called-n-times advice-remove 3)
      (bydi-was-called-n-times remove-hook 2))))

(ert-deftest ship-mate-mode ()
  :tags '(user-facing)

  (bydi (ship-mate-mode--setup
         ship-mate-mode--teardown)

    (let ((ship-mate-mode nil))

      (ship-mate-mode)
      (bydi-was-called ship-mate-mode--setup)

      (ship-mate-mode -1)
      (bydi-was-called ship-mate-mode--teardown))))

;;; -- Env editing

(ert-deftest ship-mate-environment--edit--errors-for-non-comp ()
  :tags '(user-facing env)

  (should-error (ship-mate-environment--edit)))

(ert-deftest ship-mate-environment--creates-buffer ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "env-test")

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (bydi ((:always ship-mate--command-buffer-p))
      (with-current-buffer (ship-mate-environment--edit)

        (should (string= "TES=TING\nMOC=KING" (buffer-string)))))))

(ert-deftest ship-mate-environment--listify ()
  :tags '(env)

  (ert-with-test-buffer (:name "listify")

    (insert "TES=TING\n\nMOC=KING")

    (let ((ship-mate-environment--buffer-name (buffer-name)))

      (should (equal '("TES=TING" "MOC=KING") (ship-mate-environment--listify))))))

(ert-deftest ship-mate-environment--validate ()
  :tags '(env)

  (let ((list '("TES=TING" "MOC=KING")))

    (bydi ((:mock ship-mate-environment--listify :return list))

      (should-not (ship-mate-environment--validate))

      (setq list '("TESTING" "MOC=KING"))

      (should (equal '("Invalid assignments") (ship-mate-environment--validate))))))

(ert-deftest ship-mate-environment-apply--errors-if-validation-fails ()
  :tags '(user-facing env)

  (bydi ((:mock ship-mate-environment--validate :return '("Test error a" "Test error b")))
    (should-error (ship-mate-environment-apply) :type 'user-error)))

(ert-deftest ship-mate-environment-apply ()
  :tags '(user-facing env)

  (bydi ((:ignore ship-mate-environment--validate)
         (:mock ship-mate-environment--listify :return '("TES=TING"))
         (:ignore ship-mate-environment--quit)
         (:watch compilation-environment))

    (ert-with-test-buffer (:name "apply")
      (setq ship-mate-environment--target-buffer (current-buffer))

      (ship-mate-environment-apply)

      (bydi-was-set-to compilation-environment '("TES=TING"))

      (bydi-was-called ship-mate-environment--quit))))

(ert-deftest ship-mate-environment--quit ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "quit")
    (let ((ship-mate-environment--buffer-name (buffer-name))
          (ship-mate-environment--target-buffer (current-buffer)))

      (bydi ((:spy quit-window)
             (:watch ship-mate-environment--target-buffer))

        (ship-mate-environment--quit)

        (bydi-was-called quit-window)
        (bydi-was-set-to ship-mate-environment--target-buffer nil)))))

(ert-deftest ship-mate-environment-abort ()
  :tags '(user-facing env)

  (bydi (ship-mate-environment--quit)

    (ship-mate-environment-abort)

    (bydi-was-called ship-mate-environment--quit)))

(ert-deftest ship-mate-environment-clear ()
  :tags '(user-facing env)

  (ert-with-test-buffer (:name "clear")
    (setq-local compilation-environment '("TES=TING"))

    (setq ship-mate-environment--target-buffer (current-buffer))

    (bydi (ship-mate-environment--quit)
      (ship-mate-environment-clear)

      (should-not compilation-environment)
      (bydi-was-called ship-mate-environment--quit))))

(ert-deftest ship-mate-edit-environment ()
  :tags '(user-facing env)

  (bydi ((:always ship-mate--command-buffer-p)
         ship-mate-environment--edit)

    (call-interactively 'ship-mate-edit-environment)

    (bydi-was-called ship-mate-environment--edit)))

(ert-deftest ship-mate-edit-environment--completes ()
  :tags '(user-facing env)

  (let ((buffer (current-buffer)))

    (bydi ((:ignore ship-mate--command-buffer-p)
           (:mock ship-mate--complete-buffer :return buffer)
           ship-mate-environment--edit)

      (call-interactively 'ship-mate-edit-environment)

      (bydi-was-called ship-mate-environment--edit t)

      (setq buffer nil)

      (should-error (call-interactively 'ship-mate-edit-environment)))))

(ert-deftest ship-mate-environment--edit-in-minibuffer ()
  (let ((read ""))

    (bydi ((:mock read-string :return read))

      (should-not (ship-mate-environment--edit-in-minibuffer nil))

      (setq read "TES=TING MOC=KING")

      (should (equal '("TES=TING" "MOC=KING") (ship-mate-environment--edit-in-minibuffer nil)))

      (setq read "TES=TING MOC+KING")

      (should-error (ship-mate-environment--edit-in-minibuffer nil)))))

;;; -- Dinghy

(ert-deftest ship-mate-dinghy-mode ()
  (bydi (ship-mate-dinghy--reset-header-line-format)

    (ert-with-test-buffer (:name "dinghy")
      (should-error (ship-mate-dinghy-mode))

      (setq major-mode 'compilation-mode)

      (ship-mate-dinghy-mode)

      (bydi-was-called ship-mate-dinghy--reset-header-line-format))))

(ert-deftest ship-mate-dinghy--maybe-enable ()
  (bydi (ship-mate-dinghy-mode)

    (let ((ship-mate-dinghy-enable nil))

      (ship-mate-dinghy--maybe-enable)

      (bydi-was-not-called ship-mate-dinghy-mode)

      (setq ship-mate-dinghy-enable t)

      (ert-with-test-buffer (:name "*ship-mate-maybe*")

        (ship-mate-dinghy--maybe-enable)

        (bydi-was-called ship-mate-dinghy-mode)))))

(ert-deftest ship-mate-dinghy--print-variables ()
  (let ((compilation-environment nil))

    (should (string= "none" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (should (string= "TES MOC" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING" "TRY=ING" "MY=BEST"))

    (should (string= "active" (ship-mate-dinghy--print-variables)))))

(ert-deftest ship-mate-dinghy--reset-header-line-format ()
  (bydi ((:mock ship-mate-dinghy--print-variables :return "test"))
    (ert-with-test-buffer (:name "header")
      (setq-local ship-mate-dinghy-mode t)

      (ship-mate-dinghy--reset-header-line-format)

      (should (string= header-line-format "env[test]"))

      (ship-mate-dinghy--reset-header-line-format (current-buffer))

      (should (string= header-line-format "env[test]")))))

(ert-deftest ship-mate-refresh-history ()
  (bydi (ship-mate-command--create-history
         (:mock ship-mate--read-command :return "test"))

    (call-interactively 'ship-mate-refresh-history)

    (bydi-was-called-with ship-mate-command--create-history (list 'test nil))))

(ert-deftest ship-mate-submarine--recompile ()
  :tags '(submarine)

  (let ((ship-mate-submarine--in-progress t))

    (should-error (ship-mate-submarine--recompile)))

  (let ((ship-mate--last-compilation-type 'other))

    (should-error (ship-mate-submarine--recompile)))

  (let ((ship-mate--last-compilation-type nil))

    (should-error (ship-mate-submarine--recompile)))

  (let ((ship-mate-submarine--in-progress nil)
        (ship-mate-submarine--buffer nil))
    (bydi ((:watch ship-mate-submarine--in-progress)
           (:watch ship-mate-submarine--buffer)
           (:watch display-buffer-alist)
           recompile)

      (shut-up (ship-mate-submarine--recompile))

      (bydi-was-called recompile)
      (bydi-was-set display-buffer-alist)
      (bydi-was-set ship-mate-submarine--in-progress)
      (bydi-was-set ship-mate-submarine--buffer))))

(ert-deftest ship-mate-submarine--check ()
  :tags '(submarine)

  (let ((ship-mate-submarine--process nil))

    (should-error (ship-mate-submarine--check)))

  (let ((ship-mate-submarine--process 'process)
        (ship-mate-submarine--timer 'timer)
        (ship-mate-submarine--in-progress t)
        (ship-mate-prompt-for-hidden-buffer t))

    (bydi ((:sometimes process-live-p)
           cancel-timer
           (:always yes-or-no-p)
           pop-to-buffer
           (:watch ship-mate-submarine--in-progress))

      (ship-mate-submarine--check)

      (bydi-was-not-set ship-mate-submarine--in-progress)
      (bydi-was-not-called pop-to-buffer)

      (bydi-toggle-sometimes)

      (ship-mate-submarine--check)

      (bydi-was-set ship-mate-submarine--in-progress)
      (bydi-was-called pop-to-buffer))))

(ert-deftest ship-mate-submarine--watch-process ()
  :tags '(submarine)

  (let ((ship-mate-submarine--in-progress nil)
        (ship-mate-submarine--process nil)
        (ship-mate-submarine--timer))

    (bydi (run-with-timer
           (:watch ship-mate-submarine--timer))

      (ship-mate-submarine--watch-process 'process)

      (bydi-was-not-set ship-mate-submarine--timer)

      (setq ship-mate-submarine--in-progress t)

      (ship-mate-submarine--watch-process 'process)

      (bydi-was-set ship-mate-submarine--timer))))

(ert-deftest ship-mate-hidden-recompile ()
  :tags '(user-facing submarine)

  (bydi ship-mate-submarine--recompile
    (ship-mate-hidden-recompile)

    (bydi-was-called ship-mate-submarine--recompile)))

(ert-deftest ship-mate--command-buffers ()
  (bydi (project-current
         (:mock project-buffers :return '(a b c))
         (:mock ship-mate--command-buffer-p :with (lambda (it) (eq it 'b))))

    (should (equal '(b) (ship-mate--command-buffers)))))

(ert-deftest ship-mate--complete-buffer ()
  (bydi ((:mock completing-read :with (lambda (_ coll &rest _) (caar coll)))
         (:mock ship-mate--command-buffers :return '(a b c))
         (:mock ship-mate--completion-candidate :with (lambda (it) (cons (symbol-name it) it))))

    (should (eq 'a (ship-mate--complete-buffer "Test: ")))

    (bydi-was-called-with completing-read '("Test: " ...))))

(ert-deftest ship-mate--completion-candidate ()
  (ert-with-test-buffer (:name "completion candidate")

    (setq-local compilation-arguments '("make test")
                ship-mate-command-category 'test)

    (should (equal (cons "Test [make test]" (current-buffer))
                   (ship-mate--completion-candidate (current-buffer))))))

;;; ship-mate-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
