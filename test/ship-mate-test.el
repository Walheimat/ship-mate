;;; ship-mate-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for custom functions.

;;; Code:

(require 'ship-mate)

(ert-deftest ship-mate--plist-keys--errors-if-invalid ()
  :tags '(utility)

  (should-error (ship-mate--plist-keys '(:test a :best))))

(ert-deftest ship-mate--plist-keys--extracts-keys ()
  :tags '(utility)

  (should (equal '(:test :this :function) (ship-mate--plist-keys '(:test "whether" :this "hacky" :function "works")))))

(ert-deftest ship-mate-environment--valid-env-p ()
  :tags '(user-facing)

  (should-not (ship-mate-environment--valid-env-p '("hello" "world")))
  (should-not (ship-mate-environment--valid-env-p "hello=world"))
  (should-not (ship-mate-environment--valid-env-p '("hello=world" "test")))
  (should (ship-mate-environment--valid-env-p '("hello=world" "test=ing")))
  (should (ship-mate-environment--valid-env-p '((test . ("hello=world" "test=ing")))))
  (should (ship-mate-environment--valid-env-p '((test . ("hello=world"))
                                                (mock . ("test=ing"))
                                                (stub . nil)
                                                (nil . ("world=hello")))))
  (should-not (ship-mate-environment--valid-env-p '((test . ("hello=world" "testing")))))
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
  (defvar ship-mate-test-prompt nil)

  (let ((ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (ship-mate-test-default-cmd "untest")
        (ship-mate--command-history nil)
        (entered-command nil))

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/cmd")
           (:mock project-name :return "Test Project")
           (:mock project-buffers :return (list (current-buffer)))
           (:mock project--value-in-dir :return ship-mate-test-default-cmd)
           (:ignore ship-mate--local-value)
           (:mock compile :with (lambda (&rest _)
                                  (funcall compilation-save-buffers-predicate)
                                  (current-buffer)))
           (:mock read-shell-command :return entered-command)
           ship-mate-command--record-last-command
           ship-mate-command--mark-as-run
           (:othertimes ship-mate-command--has-run-p)
           ship-mate-dinghy-mode)

      (ship-mate-command 'test)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'ship-mate--command-history))

      (setq entered-command "test")

      (bydi-toggle-volatile 'ship-mate-command--has-run-p)
      (ship-mate-command 'test t)
      (bydi-was-called-with read-shell-command (list "Test project (Test Project): " "untest" 'ship-mate--command-history))
      (bydi-was-called-with compile '("test" nil))

      (setq entered-command "best ")
      (ship-mate-command 'test t)
      (bydi-was-called-with compile '("best" nil))

      (should (string-equal "test" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 1)))
      (should (string-equal "best" (ring-ref (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)) 0))))))

(ert-deftest ship-mate-command--only-inserted-once ()
  :tags '(command)

  (defvar ship-mate-test-prompt nil)

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
           ship-mate-command--record-last-command
           ship-mate-command--mark-as-run
           (:always ship-mate-command--has-run-p)
           ship-mate-dinghy-mode)

      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test)))))
      (ship-mate-command 'test)
      (should (eq 1 (ring-length (gethash "/tmp/cmd" (plist-get ship-mate-commands 'test))))))))

(ert-deftest ship-mate-command--no-let-bind-for-existing-env ()
  :tags '(command)

  (defvar ship-mate-test-prompt nil)

  (let ((compilation-environment nil)
        (env nil)
        (ship-mate-commands (list 'test (make-hash-table :test 'equal)))
        (history (make-ring 1)))

    (ring-insert history "make test")

    (bydi ((:always project-current)
           (:mock project-root :return "/tmp/env")
           (:mock project-name :return "Test Project")
           (:mock ship-mate-command--history :return history)
           (:mock ship-mate--local-value :return nil)
           (:watch compilation-environment)
           (:mock compile :return (current-buffer))
           ship-mate-dinghy-mode
           ship-mate-command--record-last-command
           ship-mate-command--mark-as-run
           (:always ship-mate-command--has-run-p))

      (ship-mate-command 'test)

      (bydi-was-set compilation-environment)
      (setq compilation-environment '("TES=TING"))

      (bydi-clear-mocks-for 'compilation-environment)

      (ship-mate-command 'test)

      (bydi-was-not-set compilation-environment))))

(ert-deftest ship-mate-command--compile--editing-env ()
  :tags '(command)

  (let ((env '("TES=TING"))
        (edited '("MOC=KING"))
        (compilation-environment nil))

    (bydi ((:mock ship-mate-environment--edit-in-minibuffer :return edited)
           (:mock ship-mate-environment--current-environment :return env)
           (:watch compilation-environment)
           compile)

      (ship-mate-command--compile 'test "make test")

      (bydi-was-called ship-mate-environment--current-environment t)
      (bydi-was-not-called ship-mate-environment--edit-in-minibuffer)
      (bydi-was-set-to compilation-environment '("TES=TING") t)

      (setq compilation-environment env)
      (ship-mate-command--compile 'test "make test" nil '(5))

      (bydi-was-not-called ship-mate-environment--current-environment)
      (bydi-was-called ship-mate-environment--edit-in-minibuffer)
      (bydi-was-set-to compilation-environment '("MOC=KING")))))

(ert-deftest ship-mate-command--current-project ()
  (bydi ((:always project-current)
         (:always project-prompt-project-dir)
         (:always project--find-in-directory))

    (should (ship-mate-command--current-project))

    (bydi-was-called project-current t)

    (let ((ship-mate-other-project-prefix 2))
      (should (ship-mate-command--current-project '(2))))

    (bydi-was-not-called project-current)
    (bydi-was-called project--find-in-directory)))

(ert-deftest ship-mate-environment--current-environment ()
  :tags '(environment)

  (ert-with-test-buffer (:name "last-env")

    (setq-local compilation-environment '("TES=TING"))

    (let ((compilation-buffer-name-function (lambda (_) (buffer-name))))

      (should (equal '("TES=TING") (ship-mate-environment--current-environment 'test)))))

  (ert-with-test-buffer (:name "last-env")

    (setq-local compilation-environment nil)

    (let ((compilation-buffer-name-function (lambda (_) (buffer-name))))

      (bydi ((:mock ship-mate--local-value :return '("TES=TING")))
        (should (equal '("TES=TING") (ship-mate-environment--current-environment 'test))))))

  (ert-with-test-buffer (:name "last-env")

    (setq-local compilation-environment nil)

    (bydi ((:mock ship-mate--local-value :return '((test . ("TES=TING")) (nil . ("MOC=KING")))))

      (let ((compilation-buffer-name-function (lambda (_) (buffer-name))))

        (should (equal '("MOC=KING") (ship-mate-environment--current-environment 'mock)))
        (should (equal '("TES=TING") (ship-mate-environment--current-environment 'test)))))))

(ert-deftest ship-mate-command--has-run-p ()
  :tags '(meta)

  (let ((ship-mate--project-meta (make-hash-table :test 'equal)))

    (should-not (ship-mate-command--has-run-p 'test 'project))

    (ship-mate-command--mark-as-run 'test 'project)

    ;; Repeat to make sure no duplicates.
    (ship-mate-command--mark-as-run 'test 'project)

    (should (ship-mate-command--has-run-p 'test 'project))

    (should-not (ship-mate-command--has-run-p 'mock 'project))

    (ship-mate-command--mark-as-run 'mock 'project)

    (should (ship-mate-command--has-run-p 'mock 'project))))

(ert-deftest ship-mate-command--record-and-retrieve-last-command ()
  :tags '(meta)

  (let ((ship-mate--project-meta (make-hash-table :test 'equal)))

    (bydi ((:watch ship-mate--last-command)
           (:mock project-current :return "test-project"))

      (ship-mate-command--record-last-command 'test "test-project")

      (should (equal (ship-mate-command--last-command) 'test))

      (ship-mate-command--record-last-command 'mock "test-project")

      (should (equal (ship-mate-command--last-command) 'mock)))))

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
  :tags '(command)

  (should (ship-mate-command--valid-default-p "test"))
  (should (ship-mate-command--valid-default-p '("test" "make")))
  (should-not (ship-mate-command--valid-default-p '("test" make))))

(ert-deftest ship-mate-command--update-history ()
  :tags '(command)

  (let ((fake-history (make-ring 3))
        (ship-mate-command-history-size 3))

    (ring-insert fake-history "make test")
    (ring-insert fake-history "make coverage")

    (bydi ((:mock ship-mate-command--history :return fake-history)
           (:mock ship-mate-command--last-command :return 'test)
           (:always yes-or-no-p))

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
        (history (make-ring 2))
        (matches nil))

    (bydi ((:mock ship-mate-command--history :return history)
           (:mock ship-mate-command--fuzzy-match :return matches)
           (:mock ship-mate--local-value :return environment)
           (:mock ship-mate-command--last-command :var last :initial nil)
           ship-mate-command)

      (ring-insert history "make history")

      (ship-mate-command--capture #'ignore)

      (bydi-was-not-called ship-mate-command)

      (setq last 'test
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

(ert-deftest ship-mate-command--capture--outside-project ()
  :tags '(command)

  (ert-with-test-buffer (:name "*ship-mate-in-comp*")

    (let ((ship-mate--this-command 'test))

      (bydi ((:ignore ship-mate--command-buffer-p)
             (:ignore derived-mode-p)
             (:ignore project-current)
             (:spy ship-mate-command)
             (:watch ship-mate--this-command))

        (ship-mate-command--capture #'ignore)

        (bydi-was-not-called ship-mate-command)
        (bydi-was-not-set ship-mate--this-command)))))

(ert-deftest ship-mate-create-command ()
  :tags '(user-facing command)

  (let ((key "t"))

    (bydi ((:mock make-hash-table :return 'hash-table)
           (:mock ship-mate-command--key-for-command :return key))
      (bydi-match-expansion
       (ship-mate-create-command test)
       '(progn
          (defvar-local ship-mate-test-default-cmd nil "Default for `ship-mate-test'.")
          (defvar-local ship-mate-test-prompt nil "Whether `ship-mate-test' should prompt.")
          (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
                 (interactive "P")
                 (ship-mate-command 'test arg))
          (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
          (define-key ship-mate-command-map "t" 'ship-mate-test)
          (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p)))

      (bydi-match-expansion
       (ship-mate-create-command test :key "C-o" :default "make all" :prompt t)
       '(progn
          (defvar-local ship-mate-test-default-cmd "make all" "Default for `ship-mate-test'.")
          (defvar-local ship-mate-test-prompt t "Whether `ship-mate-test' should prompt.")
          (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
                 (interactive "P")
                 (ship-mate-command 'test arg))
          (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
          (define-key ship-mate-command-map "t" 'ship-mate-test)
          (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p)))

      (setq key nil)
      (bydi-match-expansion
       (ship-mate-create-command test)
       '(progn
          (defvar-local ship-mate-test-default-cmd nil "Default for `ship-mate-test'.")
          (defvar-local ship-mate-test-prompt nil "Whether `ship-mate-test' should prompt.")
          (defun ship-mate-test (&optional arg) "Test the current project.\n\nSee `ship-mate-command' for behavior of ARG."
                 (interactive "P")
                 (ship-mate-command 'test arg))
          (setq ship-mate-commands (plist-put ship-mate-commands 'test hash-table))
          (ship-mate--warn "Failed to find eligible key for `test'")
          (put 'ship-mate-test-default-cmd 'safe-local-variable #'ship-mate-command--valid-default-p))))))

(ert-deftest ship-mate-command--buffers ()
  :tags '(command)

  (let ((a (get-buffer-create "*ship-mate-a*"))
        (b (get-buffer-create "other"))
        (c (get-buffer-create "*ship-mate-c*")))

    (bydi ((:mock project-buffers :return (list a b))
           (:mock buffer-list :return (list a b c))
           (:always project-current))

      (should (eq (length (ship-mate-command--buffers)) 1))
      (should (eq (length (ship-mate-command--buffers t)) 2)))

    (kill-buffer a)
    (kill-buffer b)
    (kill-buffer c)))

(defun ship-mate-test--keymap ()
  "Get a keymap with a few things bound."
  (let ((map (make-sparse-keymap)))

    (define-key map "t" #'ignore)
    (define-key map "s" #'ignore)
    (define-key map "e" #'ignore)
    (define-key map "o" #'ignore)

    map))

(ert-deftest ship-mate-command--key-for-command ()
  :tags '(user-facing command)

  (let ((ship-mate-command-map (ship-mate-test--keymap)))

    (should (string= "u" (ship-mate-command--key-for-command 'test "u")))
    (should-not (ship-mate-command--key-for-command 'test))
    (should (string= "a" (ship-mate-command--key-for-command 'toast)))))

(ert-deftest ship-mate-command--next-and-prev ()
  :tags '(command)

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
         (:always ship-mate--ensure-in-project)
         ship-mate-command)
    (call-interactively 'ship-mate-select-command)

    (bydi-was-called-with ship-mate-command (list 'test nil))))

(ert-deftest ship-mate-rerun-command--calls-command ()
  :tags '(user-facing command)

  (bydi ((:mock ship-mate-command--last-command :return 'test)
         (:always ship-mate--ensure-in-project)
         ship-mate-command)

    (call-interactively 'ship-mate-rerun-command)

    (bydi-was-called ship-mate-command--last-command)
    (bydi-was-called-with ship-mate-command '(test nil))))

(ert-deftest ship-mate-rerun-command--selects-if-none-recorded ()
  :tags '(user-facing command)

  (bydi ((:mock ship-mate-command--last-command :return nil)
         (:always ship-mate--ensure-in-project)
         ship-mate-command
         ship-mate-select-command)

    (call-interactively 'ship-mate-rerun-command)

    (bydi-was-called ship-mate-command--last-command)
    (bydi-was-not-called ship-mate-command)
    (bydi-was-called ship-mate-select-command)))

(ert-deftest ship-mate-store-history-as-default ()
  :tags '(user-facing)

  (bydi ((:mock ship-mate--read-command :return "test")
         (:othertimes ship-mate-command--store-history-as-default))

    (should-error (call-interactively 'ship-mate-store-history-as-default))

    (bydi-toggle-volatile 'ship-mate-command--store-history-as-default t)

    (call-interactively 'ship-mate-store-history-as-default)

    (bydi-was-called-n-times ship-mate-command--store-history-as-default 2)))

(ert-deftest ship-mate-command--store-history-as-default ()
  :tags '(command)

  (let* ((ring (make-ring 3)))

    (ring-insert ring "make test")
    (ring-insert ring "make coverage")

    (bydi ((:mock ship-mate-command--existing-history :return ring)
           (:mock ship-mate-history--edit-in-minibuffer :return '("make best"))
           modify-dir-local-variable
           save-buffer
           set-window-configuration)

      (ship-mate-command--store-history-as-default 'test)

      (bydi-was-called-with modify-dir-local-variable
        '(nil ship-mate-test-default-cmd ("make coverage" "make test") add-or-replace)
        t)

      (ship-mate-command--store-history-as-default 'test t)

      (bydi-was-called-with modify-dir-local-variable
        '(nil ship-mate-test-default-cmd ("make best") add-or-replace)))))

(ert-deftest ship-mate-history--edit-in-minibuffer ()
  :tags '(command history)

  (let ((history (make-ring 2)))

    (ring-insert history "make test")
    (ring-insert history "make install")
    (bydi ((:mock read-string :with (lambda (_ x) x)))

      (should (equal '("make install" "make test")
                     (ship-mate-history--edit-in-minibuffer history))))))

(ert-deftest ship-mate--local-value ()
  :tags '(utility)

  (ert-with-temp-file project

    (bydi ((:always project-current)
           (:mock project-root :return project)
           (:mock project--value-in-dir :return 'text-mode))

      (should (equal (ship-mate--local-value 'major-mode) 'text-mode)))))

(ert-deftest ship-mate--read-command ()
  :tags '(utility)

  (bydi ((:always completing-read)
         (:mock ship-mate--plist-keys :return '("one" "two")))

    (ship-mate--read-command "Test: ")

    (bydi-was-called-with completing-read '("Test: " ("one" "two") nil t))))

(ert-deftest ship-mate-mode--setup ()
  :tags '(mode-setup)

  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:risky-mock advice-add :with always)
           (:risky-mock add-hook :with always))
      (ship-mate-mode--setup)
      (bydi-was-called-n-times advice-add 3))))

(ert-deftest ship-mate-mode--teardown()
  :tags '(mode-setup)

  (let ((ship-mate-compile-functions '(recompile)))
    (bydi ((:risky-mock advice-remove :with always)
           (:risky-mock remove-hook :with always))
      (ship-mate-mode--teardown)
      (bydi-was-called-n-times advice-remove 3))))

(ert-deftest ship-mate-mode ()
  :tags '(user-facing)

  (bydi (ship-mate-mode--setup
         ship-mate-mode--teardown)

    (let ((ship-mate-mode nil))

      (ship-mate-mode)
      (bydi-was-called ship-mate-mode--setup)

      (ship-mate-mode -1)
      (bydi-was-called ship-mate-mode--teardown))))

(ert-deftest ship-mate-environment--edit-in-minibuffer ()
  :tags '(environment)

  (let ((read ""))

    (bydi ((:mock read-string :return read))

      (should-not (ship-mate-environment--edit-in-minibuffer nil))

      (setq read "TES=TING MOC=KING")

      (should (equal '("TES=TING" "MOC=KING") (ship-mate-environment--edit-in-minibuffer nil)))

      (setq read "TES=TING MOC+KING")

      (should-error (ship-mate-environment--edit-in-minibuffer nil)))))

(ert-deftest ship-mate-refresh-history ()
  :tags '(history)

  (bydi (ship-mate-command--create-history
         (:mock ship-mate--read-command :return "test"))

    (call-interactively 'ship-mate-refresh-history)

    (bydi-was-called-with ship-mate-command--create-history (list 'test nil))))

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
  :tags '(user-facing)

  (bydi ((:mock ship-mate--complete-buffer :return 'buffer)
         pop-to-buffer)

    (call-interactively 'ship-mate-show-results)

    (bydi-was-called-with pop-to-buffer 'buffer)))

;;; Utility

(ert-deftest ship-mate--buffer-visible-p ()
  :tags '(utility)

  (ert-with-test-buffer (:name "visible")
    (pop-to-buffer (current-buffer))

    (should (ship-mate--buffer-visible-p (current-buffer)))))

(ert-deftest ship-mate--warn ()
  :tags '(user-facing)

  (bydi display-warning
    (ship-mate--warn "Test message")

    (bydi-was-called-with display-warning '(ship-mate "Test message"))))

(ert-deftest ship-mate--ensure-in-project ()
  :tags '(user-facing)

  (bydi ((:sometimes project-current))

    (ship-mate--ensure-in-project)

    (bydi-toggle-volatile 'project-current)

    (should-error (ship-mate--ensure-in-project))))

;;;; Lighter

(ert-deftest ship-mate-mode-lighter--title ()

  (should (ship-mate-mode-lighter--title)))

(ert-deftest ship-mate-mode-lighter--menu ()
  :tags '(lighter)

  (defvar ship-mate-command-map)

  (let ((ship-mate-command-map (make-sparse-keymap)))

    (defun ship-mate-test () nil)
    (defun ship-mate-something-else-exquisite () nil)

    (define-key ship-mate-command-map (kbd "t") 'ship-mate-test)
    (define-key ship-mate-command-map (kbd "x") 'ship-mate-something-else-exquisite)

    (bydi (popup-menu
           (:spy define-key-after))
      (ship-mate-mode-lighter--menu)
      (bydi-was-called popup-menu)
      (bydi-was-called-nth-with define-key-after '(... (menu-item "Test" ship-mate-test)) 2)
      (bydi-was-called-nth-with
       define-key-after
       '(... (menu-item "Something Else Exquisite" ship-mate-something-else-exquisite))
       1))))

;;; ship-mate-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
