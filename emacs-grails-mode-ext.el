;;; emacs-grails-mode-ext.el --- Extensions for emacs-grails-mode.
;;
;; Author: Yves Zoundi <rimerosolutions@gmail.com>
;; Maintainer: Yves Zoundi
;; Version: 20140128.062131
;; X-Original-Version: 1.0.0
;; Package-Requires: ((grails-mode "0.1") (emacs "24"))
;; Contributors: The internet and people who surf it.
;; Last updated: 2014-01-28
;;
;; This file is not part of GNU Emacs.
;;
;; License: GNU GPL v3 (http://www.gnu.org/licenses/gpl-3.0.txt)
;; Sypnosis:
;;    - You can run pre-defined or arbitrary Grails commands for a project.
;;    - You can browse documentation (wiki, guide, apidocs).
;;    - Menubar contributions in Grails mode.
;;
;; To list keybindings press C-h b and search for grails-mode.

(require 'grails-mode)
(grails-mode +1)

(defcustom grails-wrapper-filename "grailsw"
  "Grails Wrapper file name."
  :type 'string
  :group 'grails)

(defcustom grails-output-opts "--plain-output"
  "Output without weird characters."
  :type 'string
  :group 'grails)

(defcustom grails-jvm-opts "-DXmx512m"
  "Grails command line options"
  :type 'string
  :group 'grails)

(defconst grails-cmp-buffer-name "*Grails*"
  "Grails commands buffer name")

(defcustom grails-use-wrapper-when-possible t
  "Use the Grails wrapper whenever available."
  :type 'boolean
  :group 'grails)

(defcustom grails-executable-suffix
  (if (eq system-type 'windows-nt)
      ".bat" "")
  "Suffix for the Grails executable file.")

(defcustom grails-executable-path (executable-find "grails")
  "Path to the Grails executable."
  :type 'string
  :group 'grails)

(defcustom grails-url-wikidocs "http://grails.org/Documentation"
  "URL to the Grails wiki documentation."
  :type 'string
  :group 'grails)

(defcustom grails-url-guide "http://grails.org/doc/latest/guide/single.html"
  "URL to the Grails latest guide."
  :type 'string
  :group 'grails)

(defcustom grails-url-apidocs "http://grails.org/doc/latest/api/"
  "URL to the Grails latest apidocs."
  :type 'string
  :group 'grails)

;; --------------------------------
;; Main functions
;; --------------------------------
(defun grails--command (str)
  "Run a Grails command."
  (project-ensure-current)
  (let ((default-directory (project-default-directory (project-current)))
        (grails-executable grails-executable-path)
        (grails-wrapper-path (concat default-directory grails-wrapper-filename grails-executable-suffix)))

    (when (and grails-use-wrapper-when-possible
               (file-exists-p grails-wrapper-path))
      (setq grails-executable grails-wrapper-path))

    (let ((grails-command-line (concat grails-executable
                                       " "
                                       grails-output-opts
                                       " "
                                       str)))
      (compilation-start grails-command-line 'compilation-mode 'grails--get-compilation-buffer-name))))

(defun grails--get-compilation-buffer-name (mode)
  "Get the grails compilation buffer name."
  grails-cmp-buffer-name)

(defun grails--read-param-and-run (input-hint grails-command)
  "Read an input parameter and invoke a given Grails command."
  (let ((grails-command-argument (read-from-minibuffer input-hint)))
    (grails--command (concat grails-command " " grails-command-argument))))

;; --------------------------------
;; General functions
;; --------------------------------
(defun grails-icommand ()
  "Enter a Grails command (Interactive)."
  (interactive)
  (grails--read-param-and-run "Goal:" ""))

(defun grails-create-domain ()
  "Create a Grails Domain Class."
  (interactive)
  (grails--read-param-and-run "Domain class:" "create-domain-class"))

(defun grails-create-controller ()
  "Create a Grails Controller."
  (interactive)
  (grails--read-param-and-run "Controller Domain class:" "create-controller"))

(defun grails-create-service ()
  "Create a Grails Service."
  (interactive)
  (grails--read-param-and-run "Service Domain class:" "create-service"))

(defun grails-create-taglib ()
  "Create a Grails Taglib."
  (interactive)
  (grails--read-param-and-run "TagLib Name:" "create-tag-lib"))

;; --------------------------------
;; Plugin functions
;; --------------------------------
(defun grails-install-plugin ()
  "Install a Grails plugin."
  (interactive)
  (grails--read-param-and-run "name optionalversion:" "install-plugin"))

(defun grails-uninstall-plugin ()
  "Uninstall a Grails plugin."
  (interactive)
  (grails--read-param-and-run "Plugin Name:" "uninstall-plugin"))

(defun grails-package-plugin ()
  "Package a Grails plugin."
  (interactive)
  (grails--command "package-plugin"))

(defun grails-refresh-dependencies ()
  "Refresh Grails Dependencies."
  (interactive)
  (grails--command "refresh-dependencies"))

;; --------------------------------
;; Browse docs (api, wiki, guide)
;; --------------------------------
(defun grails-browse-wiki-docs ()
  "Browse the Wiki Documentation."
  (interactive)
  (browse-url grails-url-wikidocs))

(defun grails-browse-api-docs ()
  "Browse the API Documentation."
  (interactive)
  (browse-url grails-url-apidocs))

(defun grails-browse-latest-guide ()
  "Browse the official Grails Guide."
  (interactive)
  (browse-url grails-url-guide))

(defun grails-contribute-keys()
  "Add keybindings"
  (global-set-key (kbd "C-c ;e")  'grails-icommand)
  (global-set-key (kbd "C-c ;cd") 'grails-create-domain)
  (global-set-key (kbd "C-c ;cc") 'grails-create-controller)
  (global-set-key (kbd "C-c ;cs") 'grails-create-service) )

(eval-after-load "emacs-grails-mode-ext"
  '(progn (grails-contribute-keys) ))

(defun grails-contribute-menu()
  "Add menu extensions."
  (define-key global-map [menu-bar grailmenu grails-cmd-1] '("Execute Command"   . grails-icommand))
  (define-key global-map [menu-bar grailmenu grails-cmd-2] '("Create Domain"     . grails-create-domain))
  (define-key global-map [menu-bar grailmenu grails-cmd-3] '("Create Controller" . grails-create-controller))
  (define-key global-map [menu-bar grailmenu grails-cmd-4] '("Create Service"    . grails-create-service)))

(add-hook 'menu-bar-update-hook 'grails-contribute-menu)
(grails-contribute-menu)

(provide 'emacs-grails-mode-ext)

;;; emacs-grails-mode-ext.el ends here
