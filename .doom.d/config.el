;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Georg Daniel Tsambasis"
      user-mail-address "g.ts@outlook.jp")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(setq flycheck-disabled-checkers '(python-mypy))

;(eval (quote (+ 1 1)))

;; ######## PYTHON REPL ##########
;; set your path
(defconst cos-dir
  (or (getenv "COSDIR")
      (if
          (eq system-type 'windows-nt)
          "C:/ClashOfStreamers/"
        (file-name-as-directory "~/idlegame/"))))

(defvar idlegame-proj-name "IdleGame")
(defconst idlegame-dir (concat (file-name-as-directory cos-dir) "IdleGame/"))

(defconst idlegame-project-root (concat (file-name-as-directory cos-dir) "IdleGame/"))
(defconst idlegame-assets-dir (concat (file-name-directory idlegame-project-root) "Assets/"))
(defconst idlegame-sources-dir (concat (file-name-directory idlegame-assets-dir) "#/Sources/"))
(defconst idlegame-loadgroups-dir (concat (file-name-directory idlegame-assets-dir) "LoadGroups/"))




(defvar airtest-path (concat (file-name-as-directory cos-dir) "Airtest/"))
(defvar airtest-main-path (concat airtest-path "main/"))


(with-eval-after-load 'python
  (add-to-list 'python-shell-extra-pythonpaths airtest-path)
  (add-to-list 'python-shell-extra-pythonpaths airtest-main-path)
  (add-to-list 'python-shell-extra-pythonpaths
               (concat airtest-path "scripts"))
  (add-to-list 'python-shell-extra-pythonpaths "Airtest/main/venv/"))

(defun airtest-run-python (&rest args)
  (let ((default-directory airtest-path))
    (run-python
     (s-join
      " "
      (append
       (list "python")
       args))
     nil 0)))

(cl-defun airtest-generate (name
                            &optional
                            (feature-dir
                             (completing-read
                              "Airtest feature: "
                              (directory-directories
                               (concat airtest-path "main/tests/cos/")
                               t "\\w")))
                            (maintainer "Georg"))
  (interactive "sName: ")
  (airtest-run-python
   "-i" "-m" "tools.code_gen.generate_airtests"
   maintainer name (file-name-base feature-dir) name)
  (run-at-time 10 nil (lambda ())
               (airtest-run-python
                "-i"  "tools/add_airtest.py"
                "-n" (concat "Air"
                             (s-upper-camel-case (file-name-base feature-dir))
                             (s-upper-camel-case name))
                "-p"
                (file-relative-name
                 (car (directory-files (concat feature-dir "/" name) t  "__init"))
                 cos-dir)
                "-z" (s-upper-camel-case (file-name-base feature-dir))
                "-x" "All"
                "--maintainer" maintainer)))

(defvar airtest-buffer  "*air*")

(defun airtest-airtests ()
  "Return a list of airtests with indices."
  (interactive)
  (--remove
   (string-match-p
    (regexp-opt
     (list
      "__init__"
      "generate_composite_cos_airtest.py"
      "generate_cos_airtest.py"
      "test_airtest.py"))
    it)
   (directory-files-recursively
    (concat airtest-main-path "tests/cos/")
    ".*"
    nil)))

(defun airtest-read-airtest ()
  (interactive)
  (completing-read
   "airtest: "
   (airtest-airtests)))

(defvar airtest-last-airtest nil)

;; Putting those into the interactive call throws errs because helm uses the wrong fuzzy match fn
(cl-defun airtest-run-local (&key (file (or
                                         airtest-last-airtest
                                         (airtest-read-airtest)))
                                  (startup (y-or-n-p "With startup? ") startup-p))
  "Invoke `run-python' to run an airtest script.
FILE should be a script located inside the airtest project."
  (interactive)
  (setf airtest-last-airtest file)
  (let ((process-environment
         (append
          (list
           "TEST_RUNNER=Unity"
           "TEST_RUNNER=Unity"
           "RETRIES_ENABLED=false"
           "AIR_ACTIONS_ONLY_ONCE=yes"
           "AIR_SERVER_DNS=ws:/localhost:10777"
           (concat "PROJECT_ROOT=" cos-dir "/"))
          (unless startup
            '("SETUP_ACTIONS=[]" "TEARDOWN_ACTIONS=[]"))))
        (airtest-path cos-dir))
    (airtest-run-python
     "-i"
     "Airtest/main/lib/launcher/launcher.py"
     "--device"
     "LinuxLocalUnity:///"
     (file-relative-name file cos-dir))))

(defun airtest-run-dwm (&optional arg)
  "Run the last airtest, if set. Else ask and run.
If prefix ARG is non nil, do not run startup actions."
  (interactive "P")
  (save-some-buffers)
  (and
   (get-buffer "*Python*")
   (kill-buffer it))
  (airtest-run-local :startup (not arg)))

(defun airtest-run-specific ()
  (nilf airtest-last-airtest)
  (airtest-run-dwm))



(provide 'idlegame-definitions)
