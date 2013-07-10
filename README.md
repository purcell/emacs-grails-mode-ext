# Emacs Grails Mode Extensions

Simple contribution to the existing Emacs [grails-mode](https://github.com/kurtharriger/emacs-grails-mode).

## Features
* You can run pre-defined or arbitrary Grails commands for a project with easy shortcuts.
* You can browse documentation (wiki, guide, apidocs).
* Custom JVM options to pass to the Grails command are supported at a global level.
* You can use the [Grails wrapper](http://grails.org/doc/2.1.0/ref/Command%20Line/wrapper.html) by default whenever available for a project.
* Customization group called __grails__ that is accessible via /*M-x customize-group*/.
* Menubar contributions for the original Emacs Grails mode.

Note: Not all commands are added to the menubar. 
All available functions start with *grails/* (grails followed by a slash).

## Dependencies

This project depends on [grails-mode](https://github.com/kurtharriger/emacs-grails-mode).

## Configuration

Append the following lines to your emacs startup file:

```lisp
;; Assuming that emacs-grails-mode-ext.el is in ~/.emacs.d/vendor
(setq load-path (cons (expand-file-name "~/.emacs.d/vendor") load-path))
(require 'emacs-grails-mode-ext)
```

Additional setup for file associations (requires [groovy-mode](https://github.com/timvisher/emacs-groovy-mode-mirror)) :

```lisp
(autoload 'groovy-mode "groovy-mode" "Mode for editing Groovy source files")
(autoload 'nxml-mode "nxml-mode" "Mode for editing GSP pages")

(add-to-list 'auto-mode-alist '("\.gsp$" . nxml-mode)) 
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode)) 
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode)) 
```

The Grails mode version available at [https://github.com/timvisher/emacs-groovy-mode-mirror](https://github.com/timvisher/emacs-groovy-mode-mirror) is old, I'm using [kurtharriger's](https://github.com/kurtharriger/emacs-grails-mode) version.
You only need the Groovy mode related files.

You may want to filter the output of Grails commands in the shell as described [here](http://www.redtoad.ca/ataylor/2011/09/grails-2-0-and-emacs-shell-mode/).

## License

Copyright © 2012-2013 Rimero Solutions

Licensed under the GPL V3. (http://www.gnu.org/licenses/gpl-3.0.txt)
