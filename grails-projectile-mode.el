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
;;
;; Add the folder containing grails-projectile-mode.el in your load-path
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;;
;; (require 'grails-projectile-mode)
;; (grails-projectile-mode t)
;;
;; All the commands start with 'grails/'
;; example, from a projectile managed buffer run
;; M-x grails/compile [RET]
;;
;; To list keybindings press C-h b and search for grails-projectile-mode.
;;
;; ================================
(require 'projectile)

(defcustom grails-projectile-keymap-prefix (kbd "C-c ;")
  "Grails Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(defcustom grails-projectile-mode-line " Grails"
  "Grails projectile modeline."
  :type 'string
  :group 'grails)

(defvar grails-executable-suffix
  (if (eq system-type 'windows-nt)
      ".bat" ""))

(defcustom grails-compilation-buffer-name "*Grails*"
  "Buffer name for Grails commands."
  :type 'string
  :group 'grails)

(defcustom use-grails-wrapper-when-possible t
  "Use the Grails wrapper whenever available."
  :type 'boolean
  :group 'grails)

(defcustom grails-output-opts ""
  "Output options such as --plain-output."
  :type 'string
  :group 'grails)

(defcustom grails-cmd-opts "--non-interactive --stacktrace"
  "Grails command line options."
  :type 'string
  :group 'grails)

(defcustom grails-wrapper-filename "grailsw"
  "Grails Wrapper file name."
  :type 'string
  :group 'grails)

(defcustom grails-projectile-filename ".grails-projectile"
  "Project file to define custom grails command and JVM options.
   The contents of this file override both grails-cmd-opts and grails-jvm-opts.
   Everything must hold within a single line, no newline at the end of the file."
  :type 'string
  :group 'grails)

(defcustom grails-jvm-opts "-DXmx1g"
  "Grails command line options"
  :type 'string
  :group 'grails)

(defcustom grails-executable "grails"
  "Path to Grails executable.
  By default, it's assumed that grails is in your PATH variable."
  :type 'string
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

(defun grails/--join-lines (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(defun grails/--read-grails-options-projectile-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (mark-whole-buffer)
    (grails/--join-lines (point-min)(point-max))
    (buffer-string)))

(defun grails/wizard-new-app ()
  "Create a new application project."
  (interactive)
  (grails/--wizard-new-app-or-plugin "create-app"))

(defun grails/wizard-new-plugin ()
  "Create a new plugin project."
  (interactive)
  (grails/--wizard-new-app-or-plugin "create-plugin"))

(defun grails/--wizard-new-app-or-plugin (cmd)
  "Create a new application or plugin."

  (let ((insert-default-directory  t))
    (let ((grails-project-folder (read-directory-name "Directory: " default-directory))
          (app-name (read-from-minibuffer "Application Name: ")))

      (let ((default-directory (file-name-as-directory grails-project-folder))
            (grails-command (concat grails-executable grails-executable-suffix))
            (grails-arguments (concat cmd " --inplace " app-name)))

        (unless (file-exists-p default-directory)
          (make-directory default-directory t))

        (grails/create-grails-projectile-file default-directory)

        (let ((grails-command-line (concat grails-command " " grails-arguments)))
          (compilation-start grails-command-line 'compilation-mode 'get-grails-compilation-buffer-name))))))

(defun grails/create-grails-projectile-file (dir)
  (with-temp-file (concat dir ".projectile")
    (insert "-/target")))

;; --------------------------------
;; Main functions
;; --------------------------------
(defun grails/command (str)
  "Run a Grails command (Non interactive)"

  (let ((default-directory (expand-file-name (projectile-project-root)))
        (grails-args (concat grails-jvm-opts " " grails-cmd-opts))
        (grails-cmd-line (concat grails-executable grails-executable-suffix)))

    (when use-grails-wrapper-when-possible
      (when (file-exists-p (concat default-directory grails-wrapper-filename grails-executable-suffix))
        (setq grails-cmd-line (concat default-directory grails-wrapper-filename grails-executable-suffix))))

    (when (file-exists-p (concat default-directory grails-projectile-filename))
      (setq grails-args (grails/--read-grails-options-projectile-file (concat default-directory grails-projectile-filename))))

    (let (( grails-command-line (concat grails-cmd-line " " grails-output-opts " " grails-args " " str)))
      ;; runs the grails command from the project directory
      (compilation-start grails-command-line 'compilation-mode 'get-grails-compilation-buffer-name))))

(defun get-grails-compilation-buffer-name (mode)
  "The buffer name to use for Grails Commands."
  grails-compilation-buffer-name)

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
(defun grails/plugins-list-installed ()
  "List Grails installed plugins"

  (interactive)
  (grails/command "list-plugins -installed"))

(defun grails/plugins-package-plugin ()
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

(defun grails/clean ()
  "Clean"

  (interactive)
  (grails/command "clean"))

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

;;; Minor mode
(defvar grails-projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map   (kbd "r d") 'grails/refresh-dependencies)
      (define-key prefix-map   (kbd "c p") 'grails/compile)
      (define-key prefix-map   (kbd "c l") 'grails/clean)
      (define-key prefix-map   (kbd "e")   'grails/icommand)
      (define-key prefix-map   (kbd "c d") 'grails/create-domain)
      (define-key prefix-map   (kbd "n a") 'grails/wizard-new-app)
      (define-key prefix-map   (kbd "n p") 'grails/wizard-new-plugin)
      (define-key prefix-map   (kbd "c c") 'grails/create-controller)
      (define-key prefix-map   (kbd "c s") 'grails/create-service)
      (define-key prefix-map   (kbd "p l") 'grails/plugins-list-installed)
      (define-key prefix-map   (kbd "p p") 'grails/plugins-package-plugin)

      (define-key map grails-projectile-keymap-prefix prefix-map))
    map)
  "Keymap for Grails Projectile mode.")

(easy-menu-define grails-projectile-mode-menu grails-projectile-mode-map
  "Emacs Grails Project Mode Menu."
  '("Grails"
    ["Execute Command"      grails/icommand               t]
    ["Compile"              grails/compile                t]
    ["Clean"                grails/clean                  t]
    ["--"                   'ignore                        ]
    ["Create Domain Class"  grails/create-domain          t]
    ["Create Controller"    grails/create-controller      t]
    ["Create Service"       grails/create-service         t]
    ["--"                   'ignore                        ]
    ["Installed Plugins"    grails/plugins-list-installed t]
    ["Package Plugin"       grails/plugins-package-plugin t]))

;;;###autoload
(define-minor-mode grails-projectile-mode
  "Grails Projectile Mode.

  \\{grails-projectile-mode-map}"
  :lighter grails-projectile-mode-line
  :keymap  'grails-projectile-mode-map
  :group   'grails
  :require 'grails-projectile-mode
  (easy-menu-add grails-projectile-mode-menu))

;;;###autoload
(define-globalized-minor-mode grails-projectile-global-mode
  grails-projectile-mode
  grails-projectile-on)

(defun grails-projectile-on ()
  "Enable Grails Projectile minor mode."
  (grails-projectile-mode 1))

(defun grails-projectile-off ()
  "Disable Grails Projectile minor mode."
  (grails-projectile-mode -1))

(defun grails-projectile-global-on ()
  "Enable Grails Projectile global minor mode."
  (grails-projectile-global-mode +1))

(defun grails-projectile-global-off ()
  "Disable Grails Projectile global minor mode."
  (grails-projectile-global-mode -1))

(provide 'grails-projectile-mode)
