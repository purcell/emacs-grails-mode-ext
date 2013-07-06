;; ================================
;; Extensions for grails-mode
;; Author : Rimero Solutions
;; Created : 08-Dec-2012
;;
;; License: GNU GPL v3 (http://www.gnu.org/licenses/gpl-3.0.txt)
;; Sypnosis:
;;    - You can run pre-defined or arbitrary Grails commans for a project
;;    - You can browse documentation (wiki, guide, apidocs)
;;    - Menubar contributions in Grails mode
;;
;;
;; ================================
(require 'project-mode)
(require 'grails-mode)

(grails-mode 1)

(if (eq system-type 'windows-nt)
    (defvar grails-executable-suffix ".bat")
  (defvar grails-executable-suffix ""))

(defcustom use-grails-wrapper-when-possible t
  "Use the Grails wrapper whenever available"
  :type 'boolean
  :group 'grails)

(defcustom grails-executable
  "grails"
  "Path to Grails executable.
  By default, it's assumed that grails is in your PATH variable."
  :type '(string)
  :group 'grails)

(defcustom grails-url-wikidocs
  "http://grails.org/Documentation"
  "Grails Wiki documentation URL"
  :type 'string
  :group 'grails)

(defcustom grails-url-apidocs
  "http://grails.org/doc/latest/api/"
  "Grails documentation URL"
  :type 'string
  :group 'grails)

(defcustom grails-url-guide
  "http://grails.org/doc/latest/guide/single.html"
  "Grails Latest Guide URL"
  :type 'string
  :group 'grails)
;; --------------------------------
;; Main functions
;; --------------------------------
(defun grails/command (str)
  "Run a Grails command (Non interactive)"

  (project-ensure-current)

  (let ((default-directory (expand-file-name (project-default-directory (project-current)))))
    (setq grails-commandLine grails-executable)

    ;; runs the grails command from the project directory
    (when use-grails-wrapper-when-possible
      (when (file-exists-p (concat (project-default-directory (project-current)) "grailsw"))
        (setq grails-commandLine (concat (project-default-directory (project-current)) "grailsw" grails-executable-suffix))))
    (async-shell-command (concat grails-commandLine " " str) "*Grails*")))



(defun grails/read-param-and-run (input-hint grails-command)
  "Read an input parameter and invoke a given Grails command"

  (let (grails-command-argument)

    (setq grails-command-argument (read-from-minibuffer input-hint))
    (grails/command (concat grails-command " " grails-command-argument))))

;; --------------------------------
;; General functions
;; --------------------------------
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
    (project-refresh)
    (project-save)
    (project-load-and-select grails-project-name)))


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

;; --------------------------------
;; Plugin functions
;; --------------------------------
(defun grails/list-plugin ()
  "List Grails plugins"

  (interactive)
  (grails/command "list-plugin"))

(defun grails/install-plugin ()
  "Install a Grails plugin"

  (interactive)
  (grails/read-param-and-run "name optionalversion:" "install-plugin"))

(defun grails/uninstall-plugin ()
  "Uninstall a Grails plugin"

  (interactive)
  (grails/read-param-and-run "Plugin Name:" "uninstall-plugin"))

(defun grails/package-plugin ()
  "Package a Grails plugin"

  (interactive)
  (grails/command "package-plugin"))

;; --------------------------------
;; Other targets
;; --------------------------------
(defun grails/compile ()
  "Compile"

  (interactive)
  (grails/command "compile"))

(defun grails/refresh-dependencies ()
  "Refresh Grails Dependencies"

  (interactive)
  (grails/command "refresh-dependencies"))
;; --------------------------------
;; Browse docs (api, wiki, guide)
;; --------------------------------
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
    (message "No Grails URL guide set. Customize the 'grails' group")))


(defun grails/contribute-keys ()
  "Add keybindings"

  ;; TODO erase some of the keybindings in grails-mode.el
  ;; instead of modifying directly the source code
  ;; remap some of grails-mode keybindings here after unsetting them
  (global-set-key   (kbd "C-c ;rd") 'grails/refresh-dependencies)
  (global-set-key   (kbd "C-c ;cp") 'grails/compile)
  (global-set-key   (kbd "C-c ;e")  'grails/icommand)
  (global-set-key   (kbd "C-c ;cd") 'grails/create-domain)
  (global-set-key   (kbd "C-c ;cc") 'grails/create-controller)
  (global-set-key   (kbd "C-c ;cs") 'grails/create-service)
  (global-set-key   (kbd "C-c ;pl") 'grails/list-plugin)
  (global-set-key   (kbd "C-c ;pp") 'grails/package-plugin)
  (global-set-key   (kbd "C-c ;pu") 'grails/uninstall-plugin))

(eval-after-load "emacs-grails-mode-ext"
  '(progn (grails/contribute-keys) ))

(defun grails/contribute-menu ()
  "Add menu extensions"

  (define-key global-map [menu-bar grailmenu grails-cmd-1] '("Execute Command" . grails/icommand))
  (define-key global-map [menu-bar grailmenu grails-cmd-2] '("Create Domain" . grails/create-domain))
  (define-key global-map [menu-bar grailmenu grails-cmd-3] '("Create Controller" . grails/create-controller))
  (define-key global-map [menu-bar grailmenu grails-cmd-4] '("Create Service" . grails/create-service)) )

(add-hook 'menu-bar-update-hook 'grails/contribute-menu)
(grails/contribute-menu)

(provide 'emacs-grails-mode-ext)
