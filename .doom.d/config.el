;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Georg Daniel Tsambasis"
      user-mail-address "g.ts@outlook.jp")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'tsdh-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq auto-save-default t
      make-backup-files t)
(setq confirm-kill-emacs nil)
(setq evil-respect-visual-line-mode t)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

(after! org
  (setq org-default-notes-file '(file "~/Dropbox/org/g.org"))
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-on-weekday 0)
  (setq org-enforce-todo-dependencies nil)
  (setq org-enforce-todo-checkbox-dependencies nil)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence
           "TODO(t/!)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "DOING(s/!)"  ; A task that is in progress
           "WAIT(w@/!)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d@/!)"  ; Task successfully completed
           "KILL(k@/!)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T/!)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W@/!)"   ; Task is being held up or paused
           "|"
           "[X](D@/!)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("DOING" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))
  (setq org-capture-templates
        '(("t" "todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%^{Effort}p:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("s" "Scheduled" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%^{SCHEDULED}p:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("d" "dayScheduled" entry
           (file+olp+datetree +org-capture-todo-file "Inbox")
           "* TODO %?\n%^{SCHEDULED}p:LOGBOOK:\nAdded: %U\n:END:\n%i" :prepend t :time-prompt t)
          ("w" "weekScheduled" entry
           (file+olp "~/Dropbox/org/2022-W32.org" "2022-W32")
           "* TODO %?\n%^{SCHEDULED}p:LOGBOOK:\nAdded: %U\n:END:\n%i" :prepend t :time-prompt t)
          ;; ("j" "Journal" entry
          ;;  (file+olp+datetree +org-capture-journal-file)
          ;;  "* %U %?\n%i\n%a" :prepend t)
          ("g" "g priv")
          ("gt" "todo" entry
           (file+headline "~/Dropbox/org/private.org" "Inbox")
           "* TODO %?\n%^{Effort}p:LOGBOOK:\nAdded: %U\n:END:\n%i" :prepend t)
          ("gs" "scheduled" entry
           (file+headline "~/Dropbox/org/private.org"  "Inbox")
           "* TODO %?\n%^{SCHEDULED}p:LOGBOOK:\nAdded: %U\n:END:\n%i" :prepend t)

          ("c" "Checking")
          ("cr" "Riccardo" entry
           (file+headline "~/Dropbox/org/check.org" "Riccardo")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("cb" "Ben" entry
           (file+headline "~/Dropbox/org/check.org" "Ben")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("ci" "Brian" entry
           (file+headline "~/Dropbox/org/check.org" "Brian")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("ck" "Konrad" entry
           (file+headline "~/Dropbox/org/check.org" "Konrad")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("cc" "Ced" entry
           (file+headline "~/Dropbox/org/check.org" "Ced")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("cs" "Simon" entry
           (file+headline "~/Dropbox/org/check.org" "Simon")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("ct" "Theo" entry
           (file+headline "~/Dropbox/org/check.org" "Theo")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("cm" "Matt" entry
           (file+headline "~/Dropbox/org/check.org" "Matt")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)
          ("ci" "Inbox" entry
           (file+headline "~/Dropbox/org/check.org" "Inbox")
           "* [ ] %?\n:LOGBOOK:\nAdded: %U\n:END:\n%i\n%a" :prepend t)))
        (setq org-log-done 'time)
        (setq org-tag-alist '(("efficiency" . ?e) ("sg" . ?s) ("private" . ?p)))
        (setq org-clock-persist 'history)
        (org-clock-persistence-insinuate)


        ;; (require 'cider)
        (require 'ob-clojure)
        (setq org-babel-clojure-backend 'cider)
        (require 'cider)
        (setf org-babel-clojure-backend 'cider
                org-confirm-babel-evaluate nil)

        (defun org-babel-execute:redshift (s params)
        "Evaluate S with PARAMS as redshift sql query."
        (let ((s (s-replace-regexp "\"" "\\\\\"" s))
                (nrepl-sync-request-timeout (* 10 10)))
        (org-babel-execute:clojure
        (format "%s" (princ `(default-redshift-q ,(format  "\"%s\"" s))))
        params)))
        (add-to-list 'org-src-lang-modes '("redshift" . sql))

)
(after! org-pomodoro
  (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-keep-killed-pomodoro-time t))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(after! org-journal
  (setq org-journal-dir "~/Dropbox/org/journal")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-prefix "* ")
  (setq org-journal-date-format "%A, %B %d %Y

** TODO plan
:PROPERTIES:
:Effort:   0:15
:END:

** TODO support

** TODO resume

")
  (setq org-journal-time-prefix "** ")
  (setq org-journal-hide-entries-p nil)
  (setq org-journal-time-format "")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-enable-cache t)
  (setq org-journal-skip-carryover-drawers (list "LOGBOOK"))
  (defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+PROPERTY: Effort_ALL 0 0:10 0:15 0:20 0:25 0:30 0:45 1:00 1:30 2:00 2:30 3:00 3:30 4:00
#+COLUMNS: %40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM
#+BEGIN: clocktable :maxlevel 4 :scope file :block today
#+END:

* news
:PROPERTIES:
:Effort:   1:00
:END:

* break
:PROPERTIES:
:Effort:   3:45
:END:

* learn
:PROPERTIES:
:Effort:   2:00
:END:
"))))
  (setq org-journal-file-header 'org-journal-file-header-func)
)


;; (defun datascience-just-run-sql ()
;;   (interactive)
;;   (if-let* ((f (buffer-file-name))
;; 	    (f (when (member (file-name-extension f) '("sql")) f))
;; 	    (cider-repl (car
;; 			 (seq-filter
;; 			  (lambda (b)
;; 			    (with-current-buffer b
;; 			      (and (derived-mode-p 'cider-repl-mode)
;; 				   (process-live-p (get-buffer-process b))
;; 				   (buffer-match-p "redshift-queries" b))))
;; 			  (buffer-list))))
;; 	    (cider-repl
;; 	     (or cider-repl (user-error "No redshift-queries active repl"))))
;;       (with-current-buffer cider-repl
;; 	(cider-nrepl-sync-request:eval "(require '[org.sg.redshift-queries.user]")
;; 	(cider--pprint-eval-form
;; 	 (format
;; 	  "(org.sg.redshift-queries.user/q! {:Sql (slurp \"%s\")})" f)))
;;     (user-error "I thought your buffer visits a sql file")))`;; Whenever you reconfigure a package, make sure to wrap your config in an

(after! wanderlust

  )
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
