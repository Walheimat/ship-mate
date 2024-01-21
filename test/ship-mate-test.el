;;; ship-mate-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate)

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

  (let* ((ship-mate--current-command-name "test")
         (fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-test-test*")))

  (let ((fun (ship-mate-command--buffer-name-function "test")))
    (should (string-equal (funcall fun 'test-mode) "*ship-mate-compile-test*"))))

(ert-deftest ship-mate-command ()
  :tags '(command)

  (defvar ship-mate-test-default-cmd nil)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-test-default-cmd "untest")
        (ship-mate--command-history nil)
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
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'ship-mate--command-history))
      (bydi-was-called-with compile '("test" nil))

      (setq entered-command "best")
      (ship-mate-command 'test t)
      (bydi-was-called-with compile '("best" nil))

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 0))))))

(ert-deftest ship-mate-command--only-inserted-once ()
  :tags '(command)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate--command-history nil)
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

(ert-deftest ship-mate-command--compile--submarine ()
  (bydi (ship-mate-submarine--run
         (:ignore ship-mate-environment--current-environment)
         (:mock prefix-numeric-value :return 3))

    (ship-mate-command--compile 'test)
    (let ((compilation-environment '("MOC=KING")))
      (ship-mate-command--compile 'test))

    (bydi-was-called-n-times ship-mate-submarine--run 2)))

(ert-deftest ship-mate-environment--current-environment ()
  (ert-with-test-buffer (:name "last-env")

    (setq-local compilation-environment '("TES=TING"))

    (bydi (ship-mate--local-value)

      (let ((compilation-buffer-name-function (lambda (_) (buffer-name))))

        (should (equal '("TES=TING") (ship-mate-environment--current-environment)))))))

(ert-deftest ship-mate-command--history--inserts-multiple ()
  :tags '(command)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate--command-history nil)
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
        (ship-mate--last-command 'test)
        (ship-mate-command-history-size 3))

    (ring-insert fake-history "make test")
    (ring-insert fake-history "make coverage")

    (bydi ((:mock ship-mate-command--history :return fake-history))

      (ship-mate-command--update-history "make new")

      (should (equal (ring-elements fake-history)
                     '("make coverage" "make test")))

      (ship-mate-command--update-history "make test FLAG=t")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=t" "make coverage" "make test")))

      (ship-mate-command--update-history "make way")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=t" "make coverage" "make test")))

      (ship-mate-command--update-history "make test")

      (should (equal (ring-elements fake-history)
                     '("make test" "make test FLAG=t" "make coverage")))

      (ship-mate-command--update-history "make coverage FLAG=t")

      (should (equal (ring-elements fake-history)
                     '("make coverage FLAG=t" "make test" "make test FLAG=t")))

      (ship-mate-command--update-history "make coverage  FLAG=t CAPTURE=t")

      (should (equal (ring-elements fake-history)
                     '("make coverage  FLAG=t CAPTURE=t" "make test" "make test FLAG=t")))

      ;; Don't match empty parts
      (ship-mate-command--update-history "something  coverage  different")

      (should (equal (ring-elements fake-history)
                     '("make coverage  FLAG=t CAPTURE=t" "make test" "make test FLAG=t")))

      (ship-mate-command--update-history "make coverage -- FLAG=t CAPTURE=t")

      (should (equal (ring-elements fake-history)
                     '("make coverage -- FLAG=t CAPTURE=t" "make test" "make test FLAG=t")))

      ;; Don't match deferred arg passing.
      (ship-mate-command--update-history "coverage -- else")

      (should (equal (ring-elements fake-history)
                     '("make coverage -- FLAG=t CAPTURE=t" "make test" "make test FLAG=t")))


      ;; Insert if match isn't good enough.
      (ship-mate-command--update-history "make test FLAG=nil")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=nil" "make coverage -- FLAG=t CAPTURE=t" "make test")))

      ;; Replace if good enough.
      (ship-mate-command--update-history "make test FLAG=nil OTHER=t")

      (should (equal (ring-elements fake-history)
                     '("make test FLAG=nil OTHER=t" "make coverage -- FLAG=t CAPTURE=t" "make test"))))))

(ert-deftest ship-mate-command--capture ()
  :tags '(command)

  (let ((compile-history '("make test"))
        (compile-command "make best")
        (ship-mate--last-command nil)
        (history (make-ring 2))
        (matches nil))

    (bydi ((:mock ship-mate-command--history :return history)
           (:mock ship-mate-command--fuzzy-match :return matches)
           (:mock ship-mate--local-value :return environment)
           ship-mate-command)

      (ring-insert history "make history")

      (ship-mate-command--capture #'ignore)

      (bydi-was-not-called ship-mate-command)

      (setq ship-mate--last-command 'test
            matches '(:match "match" :count 1 :index 0))

      (ship-mate-command--capture #'ignore)

      (bydi-was-called ship-mate-command))))

(ert-deftest ship-mate-command--capture--in-compilation-buffer ()
  :tags '(command)

  (ert-with-test-buffer (:name "*ship-mate-in-comp*")

    (let ((history (make-ring 1))
          (ship-mate--this-command 'test))

      (bydi ((:mock ship-mate-command--history :return history))

        (should (ship-mate-command--capture (lambda (&rest _) (current-buffer))))))))

(ert-deftest ship-mate-command--capture--in-derived ()
  :tags '(command)

  (ert-with-test-buffer (:name "*ship-mate-in-comp*")

    (let ((ship-mate--this-command 'test))

      (bydi ((:ignore ship-mate--command-buffer-p)
             (:always derived-mode-p)
             (:spy ship-mate-command)
             (:watch ship-mate--this-command))

        (ship-mate-command--capture #'ignore)

        (bydi-was-not-called ship-mate-command)
        (bydi-was-not-set ship-mate--this-command)))))

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

    (bydi-was-called-with ship-mate-command (list 'test nil))))

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

;;;; Env editing

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

;;;; Editing history

(ert-deftest ship-mate-edit-history--completes-for-foreign ()
  :tags '(user-facing history)

  (bydi ((:mock ship-mate--complete-buffer :return (current-buffer))
         (:ignore ship-mate--command-buffer-p)
         ship-mate-history--edit)

    (call-interactively 'ship-mate-edit-history)

    (bydi-was-called ship-mate-history--edit)))

(ert-deftest ship-mate-history-edit--errors-for-non-ship-mate ()
  :tags '(user-facing history)

  (bydi ((:ignore ship-mate--command-buffer-p))

    (should-error (ship-mate-history--edit))))

(ert-deftest ship-mate-history--creates-buffer ()
  :tags '(user-facing history)

  (ert-with-test-buffer (:name "history-test")

    (let ((ring (make-ring 1)))

      (ring-insert ring "make test")

      (setq-local ship-mate--this-command 'test)

      (bydi ((:always ship-mate--command-buffer-p)
             (:mock ship-mate-command--history :return ring))

        (with-current-buffer (ship-mate-history--edit)

          (should (string= "make test" (buffer-string))))))))

(ert-deftest ship-mate-history--api ()
  :tags '(user-facing history)

  (bydi (ship-mate-history--set-history
         ship-mate-history--quit
         (:mock ship-mate-history--listify :return '("make test")))

    (ship-mate-history-abort)

    (bydi-was-called ship-mate-history--quit t)
    (bydi-was-not-called ship-mate-history--set-history)

    (ship-mate-history-apply)
    (ship-mate-history-clear)

    (bydi-was-called-nth-with ship-mate-history--set-history '(("make test")) 0)
    (bydi-was-called-nth-with ship-mate-history--set-history nil 1)
    (bydi-was-called-n-times ship-mate-history--quit 2)))

(ert-deftest ship-mate-history--set-history ()
  :tags '(history)

  (let ((ring (make-ring 2)))

    (ring-insert ring "make test")

    (bydi ((:mock ship-mate-command--history :return ring))

      (ship-mate-history--set-history '("make best" "make hest"))

      (should (equal '("make hest" "make best") (ring-elements ring))))))

(ert-deftest ship-mate-history--quit ()
  :tags '(user-facing history)

  (ert-with-test-buffer (:name "history-quit")

    (let ((ship-mate-history--buffer-name (buffer-name))
          (ship-mate-history--command 'test))

      (bydi (quit-window
             (:watch ship-mate-history--command))

        (ship-mate-history--quit)

        (bydi-was-called quit-window)
        (bydi-was-set-to ship-mate-history--command nil)))))

(ert-deftest ship-mate-history--listify ()
  :tags '(history)

  (bydi ((:mock ship-mate--listify-buffer :return '("make test" "make rest")))

    (should (equal '("make rest" "make test") (ship-mate-history--listify)))))

(ert-deftest ship-mate-edit-history ()
  :tags '(user-facing history)

  (let ((buf))
    (bydi ((:sometimes ship-mate--command-buffer-p)
           (:mock ship-mate--complete-buffer :return buf)
           ship-mate-history--edit)

      (call-interactively 'ship-mate-edit-history)

      (bydi-was-called ship-mate-history--edit t))))

;;;; Dinghy

(ert-deftest ship-mate-dinghy-mode ()
  (bydi (ship-mate-dinghy--reset-header-line-format)

    (ert-with-test-buffer (:name "dinghy")
      (should-error (ship-mate-dinghy-mode))

      (setq major-mode 'compilation-mode)

      (ship-mate-dinghy-mode)

      (bydi-was-called ship-mate-dinghy--reset-header-line-format))))

(ert-deftest ship-mate-dinghy--maybe-enable ()
  (bydi (ship-mate-dinghy-mode
         (:watch ship-mate--command)
         (:mock process-command :return '("usr/bin/sh" "-c" "make test")))

    (let ((ship-mate-dinghy-enable nil))

      (ship-mate-dinghy--maybe-enable)

      (bydi-was-not-called ship-mate-dinghy-mode)

      (setq ship-mate-dinghy-enable t)

      (ert-with-test-buffer (:name "*ship-mate-maybe*")

        (ship-mate-dinghy--maybe-enable)

        (bydi-was-called ship-mate-dinghy-mode)
        (bydi-was-not-set ship-mate--command)

        (ship-mate-dinghy--maybe-enable 'process)

        (bydi-was-set-to ship-mate--command '("usr/bin/sh" "-c" "make test"))))))

(ert-deftest ship-mate-dinghy--print-variables ()
  (let ((compilation-environment nil))

    (should (string= "none" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING"))

    (should (string= "TES MOC" (ship-mate-dinghy--print-variables)))

    (setq compilation-environment '("TES=TING" "MOC=KING" "TRY=ING" "MY=BEST"))

    (should (string= "active" (ship-mate-dinghy--print-variables)))))

(ert-deftest ship-mate-dinghy--print-command ()
  (let ((ship-mate--command nil))

    (should (string= "?" (ship-mate-dinghy--print-command)))

    (setq ship-mate--command '("/usr/bin/sh" "-c" "make test"))

    (should (string= "make test" (ship-mate-dinghy--print-command)))

    (setq ship-mate--command '("/usr/bin/sh" "-c" "/usr/bin/fish"))

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

(ert-deftest ship-mate-refresh-history ()
  (bydi (ship-mate-command--create-history
         (:mock ship-mate--read-command :return "test"))

    (call-interactively 'ship-mate-refresh-history)

    (bydi-was-called-with ship-mate-command--create-history (list 'test nil))))

(ert-deftest ship-mate-submarine--recompile ()
  :tags '(submarine)

  (let ((ship-mate--last-compilation-type 'other)
        (ship-mate-submarine--processes '(process)))

    (should-error (ship-mate-submarine--recompile)))

  (let ((ship-mate--last-compilation-type nil)
        (ship-mate-submarine--processes '(process)))

    (should-error (ship-mate-submarine--recompile)))

  (let ((ship-mate--last-compilation-type 'ship-mate)
        (ship-mate--last-command 'test))

    (bydi (ship-mate-submarine--run)

      (ship-mate-submarine--recompile)

      (bydi-was-called ship-mate-submarine--run))))

(ert-deftest ship-mate-submarine--in-progress ()
  :tags '(submarine)

  (let ((ship-mate-submarine--processes nil))

    (should-not (ship-mate-submarine--in-progress))

    (push 'process ship-mate-submarine--processes)

    (should (ship-mate-submarine--in-progress))))

(ert-deftest ship-mate-submarine--run ()
  :tags '(submarine)

  (bydi ((:ignore ship-mate-submarine--ensure-no-hidden-buffers)
         (:watch display-buffer-alist))

    (ert-with-test-buffer (:name "sub-run")

      (let ((ship-mate--current-command-name "test"))
        (shut-up (ship-mate-submarine--run (lambda () (current-buffer)))))

      (should (buffer-local-value 'ship-mate--hidden (current-buffer)))
      (bydi-was-called ship-mate-submarine--ensure-no-hidden-buffers)
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

(ert-deftest ship-mate-submarine--ensure-no-hidden-buffer ()
  :tags '(submarine)

  (ert-with-test-buffer (:name "sub-ensure")

    (setq ship-mate--hidden t)

    (bydi ((:spy quit-window))

      (pop-to-buffer (current-buffer))

      (ship-mate-submarine--ensure-no-hidden-buffers)

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
        (ship-mate-prompt-for-hidden-buffer nil))

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
        (setq ship-mate-prompt-for-hidden-buffer t)
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

(ert-deftest ship-mate-show-hidden ()
  :tags '(user-facing submarine)

  (bydi (ship-mate-submarine--surface
         (:mock ship-mate--complete-buffer :with (lambda (_p l) (funcall l '(a . buffer))))
         (:mock process-buffer :return (current-buffer)))

    (let ((ship-mate-submarine--processes '(process)))

      (call-interactively 'ship-mate-show-hidden)

      (bydi-was-called ship-mate-submarine--surface 'process)
      (bydi-was-not-called ship-mate--complete-buffer)

      (setq ship-mate-submarine--processes '(a b))

      (call-interactively 'ship-mate-show-hidden)

      (bydi-was-called ship-mate-submarine--surface nil)
      (bydi-was-called ship-mate--complete-buffer))))

(ert-deftest ship-mate-hide-visible ()
  :tags '(user-facing submarine)

  (let ((buffers (list (current-buffer))))

    (bydi ((:mock ship-mate-command--buffers :return buffers)
           (:always ship-mate--buffer-visible-p)
           ship-mate-submarine--hide)

      (ship-mate-hide-visible)

      (bydi-was-called ship-mate-submarine--hide))))

(ert-deftest ship-mate-hidden-recompile ()
  :tags '(user-facing submarine)

  (bydi ship-mate-submarine--recompile
    (ship-mate-hidden-recompile)

    (bydi-was-called ship-mate-submarine--recompile)))

(ert-deftest ship-mate-hide ()
  :tags '(user-facing submarine)

  (bydi ship-mate-submarine--hide
    (ship-mate-hide)

    (bydi-was-called ship-mate-submarine--hide)))

(ert-deftest ship-mate--complete-buffer ()
  :tags '(completion)

  (bydi ((:watch minibuffer-completion-table)
         (:sometimes ship-mate--command-buffer-predicate)
         (:watch ship-mate--complete-for-all))

    (ert-simulate-keys '(?\C-m)
      (ship-mate--complete-buffer "Some prompt: "))

    (bydi-was-set-to ship-mate--complete-for-all nil t)
    (bydi-was-set minibuffer-completion-table)

    (ert-simulate-keys '(?\C-m)
      (let ((current-prefix-arg t))
        (funcall-interactively 'ship-mate--complete-buffer "Some prompt: ")))

    (bydi-was-set-to ship-mate--complete-for-all t)

    (bydi-toggle-sometimes)

    (should-error (ship-mate--complete-buffer "Some prompt: "))))

(ert-deftest ship-mate--command-buffer-predicate ()
  :tags '(completion)

  (ert-with-test-buffer (:name "buffer-pred")

    (bydi ((:always project-current)
           (:mock project-buffers :return (list (current-buffer))))

      (should-not (ship-mate--command-buffer-predicate (cons "b" (current-buffer))))
      (should-not (ship-mate--command-buffer-predicate (current-buffer)))

      (rename-buffer "*ship-mate-buffer-pred")

      (should (ship-mate--command-buffer-predicate (cons "b" (current-buffer))))
      (should (ship-mate--command-buffer-predicate (current-buffer))))))

(ert-deftest ship-mate-show-results ()
  (bydi ((:mock ship-mate--complete-buffer :return 'buffer)
         pop-to-buffer)

    (call-interactively 'ship-mate-show-results)

    (bydi-was-called-with pop-to-buffer 'buffer)))

;;; Utility

(ert-deftest ship-mate--buffer-visible-p ()
  (ert-with-test-buffer (:name "visible")
    (pop-to-buffer (current-buffer))

    (should (ship-mate--buffer-visible-p (current-buffer)))))

;;;; Lighter

(ert-deftest ship-mate-mode-lighter--function-segments ()
  :tags '(mode-line user-facing)

  (should (ship-mate-mode-lighter--title))
  (bydi ((:always ship-mate-submarine--in-progress))
    (should (ship-mate-mode-lighter--hidden))))

(ert-deftest ship-mate-mode-lighter--menu ()
  (defvar ship-mate-command-map)

  (let ((ship-mate-command-map (make-sparse-keymap)))

    (defun ship-mate-test ()
      nil)

    (define-key ship-mate-command-map (kbd "t") 'ship-mate-test)

    (bydi (popup-menu)
      (ship-mate-mode-lighter--menu)
      (bydi-was-called popup-menu))))

;;; ship-mate-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
