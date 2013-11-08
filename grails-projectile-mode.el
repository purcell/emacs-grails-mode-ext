;; ================================
;; Extensions for grails-mode
;; Author : Rimero Solutions
;; Created : 11-07-2013
;;
;; License: GNU GPL v3 (http://www.gnu.org/licenses/gpl-3.0.txt)
;; Sypnosis: Emacs Grails mode with Projectile for project-management.
;;    - You can run pre-defined or arbitrary Grails commans for a project
;;    - You can browse documentation (wiki, guide, apidocs)
;;    - Menubar contributions in Grails mode
;;    - Grails and JVM options specific to a given project via a .grails-projectile file.
;; ================================
(require 'projectile)

(defvar grails-executable-suffix
  (if (eq system-type 'windows-nt)
      ".bat"
    ""))

(defcustom use-grails-wrapper-when-possible t
  "Use the Grails wrapper whenever available"
  :type 'boolean
  :group 'grails)

(defcustom grails-cmd-opts
  "--non-interactive --stacktrace"
  "Grails command line options"
  :type 'string
  :group 'grails)

(defcustom grails-wrapper-filename
  "grailsw"
  "Grails Wrapper file name"
  :type 'string
  :group 'grails)

(defcustom grails-projectile-filename
  ".grails-projectile"
  "Grails Wrapper file name"
  :type 'string
  :group 'grails)

(defcustom grails-jvm-opts
  "-DXmx1g"
  "Grails command line options"
  :type 'string
  :group 'grails)

(defcustom grails-executable
  "grails"
  "Path to Grails executable.
  By default, it's assumed that grails is in your PATH variable."
  :type 'string
  :group 'grails)

(defcustom grails-url-wikidocs "http://grails.org/Documentation"
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

  ( let ((default-directory (expand-file-name (projectile-project-root)))
         (grails-args (concat grails-jvm-opts " " grails-cmd-opts))
         (grails-cmd-line (concat grails-executable grails-executable-suffix)))

    (when use-grails-wrapper-when-possible
      (when (file-exists-p (concat default-directory grails-wrapper-filename grails-executable-suffix))
        (setq grails-cmd-line (concat default-directory grails-wrapper-filename grails-executable-suffix))))

    (when (file-exists-p (concat default-directory grails-projectile-filename))
      (setq grails-args (ers-get-string-from-file (concat default-directory grails-projectile-filename))))

    (let (( grails-command-line (concat grails-cmd-line " " grails-args " " str)))
      ;; runs the grails command from the project directory
      (async-shell-command grails-command-line "*Grails*"))))


(defun grails/read-param-and-run (input-hint grails-command)
  "Read an input parameter and invoke a given Grails command"

  (let (grails-command-argument)
    (setq grails-command-argument (read-from-minibuffer input-hint))
    (grails/command (concat grails-command " " grails-command-argument))))

;; --------------------------------
;; General functions
;; --------------------------------
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
(defun grails/list-plugins ()
  "List Grails installed plugins"

  (interactive)
  (grails/command "list-plugins -installed"))

(defun grails/package-plugin ()
  "Package a Grails plugin"

  (interactive)
  (grails/command "package-plugin"))

(defun grails/publish-plugin ()
  "Publish a Grails plugin"

  (interactive)
  (grails/command "publish-plugin"))

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
  "Browse the Wiki Documentation"

  (interactive)
  (if (boundp 'grails-url-wikidocs)
      (browse-url grails-url-wikidocs)
    (message "No Grails Wikidocs set. Customize the 'grails' group")))

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

(defvar grails-projectile-mode-map
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

(easy-menu-define grails-projectile-mode-menu grails-projectile-mode-map
  "Emacs Grails Project Mode Menu."
  '("Grails"
    ["Execute Command"      grails/icommand t]
    ["Create Domain Class"  grails/create-domain t]
    ["Create Controller"    grails/create-controller t]
    ["Create Service"       grails/create-service t]))

;;;###autoload
(define-minor-mode grails-projectile-mode
  "Emacs Grails Project Mode Extensions"
  :lighter " Grails"
  :keymap 'grails-projectile-mode-map
  :group  'grails
  :global t
  (easy-menu-add grails-projectile-mode-menu))

(provide 'grails-projectile-mode)
