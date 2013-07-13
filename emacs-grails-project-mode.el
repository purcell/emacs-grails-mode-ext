;; ================================
;; Extensions for grails-mode
;; Author : Rimero Solutions
;; Created : 08-Dec-2012
;;
;; License: GNU GPL v3 (http://www.gnu.org/licenses/gpl-3.0.txt)
;;
;; Sypnosis: You can run pre-defined or arbitrary Grails commands for a project:
;;    - Default Grails JVM options
;;    - Default Grails commandline options
;;    - Grails wrapper support with fallback on the default Grails executable.
;;    - You can browse documentation (wiki, guide, apidocs)
;;    - Menubar integration
;;    - Global minor mode with associated keymap
;; ================================
(require 'compile)
(require 'easymenu)
(require 'project-mode)
(require 'grails-mode)

(grails-mode t)

(defun grails-comint-magic (string)
  "Handle grails output gracefully."
  (let ((position (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char comint-last-output-start)
      (while (re-search-forward "\033\\[1A\033\\[[0-9]+D\033\\[0K" position t)
        (replace-match "" t t)
        (forward-line -1)
        (delete-region
         (line-beginning-position)
         (progn (forward-line 1) (point)))))))

(add-hook 'comint-output-filter-functions 'grails-comint-magic)

(defvar grails-executable-suffix "")

(when (eq system-type 'windows-nt)
  (setq grails-executable-suffix ".bat"))

(defcustom use-grails-wrapper-when-possible t
  "Use the Grails wrapper whenever available"
  :type 'boolean
  :group 'grails)

(defcustom grails-jvm-opts ""
  "JVM options to pass to Grails"
  :type '(string)
  :group 'grails)

(defcustom grails-executable "grails"
  "Path to Grails executable.
  By default, it's assumed that grails is in your PATH variable."
  :type '(string)
  :group 'grails)

(defcustom grails-url-wikidocs "http://grails.org/Documentation"
  "Grails Wiki documentation URL"
  :type 'string
  :group 'grails)

(defcustom grails-url-apidocs "http://grails.org/doc/latest/api/"
  "Grails documentation URL"
  :type 'string
  :group 'grails)

(defcustom grails-url-guide "http://grails.org/doc/latest/guide/single.html"
  "Grails Latest Guide URL"
  :type 'string
  :group 'grails)

(defun grails/command (str)
  "Run a Grails command (Non interactive)"

  (project-ensure-current)

  (let ((default-directory (expand-file-name (project-default-directory (project-current)))))
    (let (grails-commandLine grails-executable)

      ;; runs the grails command from the project directory
      (when use-grails-wrapper-when-possible
        (when (file-exists-p (concat default-directory "grailsw" grails-executable-suffix))
          (setq grails-commandLine (concat default-directory "grailsw" grails-executable-suffix))))

      (async-shell-command (concat grails-commandLine " " grails-jvm-opts " " str) "*Grails*"))))


(defun grails/read-param-and-run (input-hint grails-command)
  "Read an input parameter and invoke a given Grails command"

  (setq grails-command-argument (read-from-minibuffer input-hint))
  (grails/command (concat grails-command " " grails-command-argument)))

(defun grails/new-app ()
  "Create a new Grails project"

  (interactive)
  (setq grails-project-parent-directory (read-from-minibuffer "Project parent directory: "))
  (setq grails-project-name (read-from-minibuffer "Project name: "))
  (grails/new-project grails-project-name grails-project-parent-directory "create-app"))

(defun grails/new-plugin ()
  "Create a new Grails project"

  (interactive)
  (setq grails-project-parent-directory (read-from-minibuffer "Project parent directory: "))
  (setq grails-project-name (read-from-minibuffer "Project name: "))
  (grails/new-project grails-project-name grails-project-parent-directory "create-plugin"))

(defun grails/new-project (grails-project-name grails-project-parent-directory grails-new-app-command)

  (let ((default-directory (expand-file-name grails-project-parent-directory)))
    (shell-command (concat grails-executable " " (concat  " " grails-new-app-command " " grails-project-name)) "*Grails*")
    (project-new grails-project-name  (concat default-directory grails-project-name))
    (project-save)))

(defun grails/icommand ()
  "Enter a Grails command (Interactive)"

  (interactive)
  (grails/read-param-and-run "Goal:" ""))

(defun grails/create-domain ()
  "Create a Grails Domain Class"

  (interactive)
  (grails/read-param-and-run "Domain class:" "create-domain-class"))

(defun grails/create-controller ()
  "Create a Grails Controller"

  (interactive)
  (grails/read-param-and-run "Controller Domain class:" "create-controller"))

(defun grails/create-service ()
  "Create a Grails Service"

  (interactive)
  (grails/read-param-and-run "Service Domain class:" "create-service"))

(defun grails/create-taglib ()
  "Create a Grails Taglib"

  (interactive)
  (grails/read-param-and-run "TagLib Name:" "create-tag-lib"))

(defun grails/list-installed-plugins ()
  "List installed plugins"

  (interactive)
  (grails/command "list-plugins -installed"))

(defun grails/compile ()
  "Compile"

  (interactive)
  (grails/command "compile"))

(defun grails/refresh-dependencies ()
  "Refresh Grails Dependencies"

  (interactive)
  (grails/command "refresh-dependencies"))

(defun grails/browse-wiki-docs ()
  "Browse the API Documentation"

  (interactive)
  (if (boundp 'grails-url-wikidocs)
      (browse-url grails-url-wikidocs)
    (message "No Grails Wiki docs URL set. Customize the 'grails' group")))

(defun grails/browse-api-docs ()
  "Browse the API Documentation"

  (interactive)
  (if (boundp 'grails-url-apidocs)
      (browse-url grails-url-apidocs)
    (message "No Grails API URL set. Customize the 'grails' group")))


(defun grails/browse-latest-guide ()
  "Browse the official Grails Guide"

  (interactive)
  (if (boundp 'grails-url-guide)
      (browse-url grails-url-guide)
    (message "No Grails URL Guide set. Customize the 'grails' group")))

(defvar emacs-grails-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO erase some of the keybindings in grails-mode.el
    ;; instead of modifying directly the source code
    ;; remap some of grails-mode keybindings here after unsetting them
    (define-key map   (kbd "C-c ;rd") 'grails/refresh-dependencies)
    (define-key map   (kbd "C-c ;cp") 'grails/compile)
    (define-key map   (kbd "C-c ;e")  'grails/icommand)
    (define-key map   (kbd "C-c ;cd") 'grails/create-domain)
    (define-key map   (kbd "C-c ;cc") 'grails/create-controller)
    (define-key map   (kbd "C-c ;cs") 'grails/create-service)
    (define-key map   (kbd "C-c ;lp") 'grails/list-installed-plugins)
    (define-key map   (kbd "C-c ;pp") 'grails/package-plugin)
    map)
  "Keymap for the Emacs Grails Project Mode extensions minor mode")

(easy-menu-define emacs-grails-minor-mode-menu emacs-grails-minor-mode-map
  "Emacs Grails Project Mode Menu."
  '("Grails"
    ["Execute Command"      grails/icommand t]
    ["Create Domain Class"  grails/create-domain t]
    ["Create Controller"    grails/create-controller t]
    ["Create Service"       grails/create-service t]))

;;;###autoload
(define-minor-mode emacs-grails-minor-mode
  "Emacs Grails Project Mode Extensions"
  :lighter " GrailsX"
  :keymap 'emacs-grails-minor-mode-map
  :group  'grails
  :global t
  (easy-menu-add emacs-grails-minor-mode-menu))

(provide 'emacs-grails-project-mode)
