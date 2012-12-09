# Emacs Grails Mode Extensions

Simple(one-time) contribution to the existing Emacs [grails-mode](https://github.com/kurtharriger/emacs-grails-mode).

## Features
* You can run pre-defined or arbitrary Grails commands for a project
* You can browse documentation (wiki, guide, apidocs)
* Menubar contributions in Grails mode

Note: Not all commands are added to the menubar. 
All available functions start with *grails/* (grails followed by slash).

## Dependencies

This project depends on [grails-mode](https://github.com/kurtharriger/emacs-grails-mode).

## Configuration

Append the following lines to your emacs startup file:

```lisp
;; Assuming that emacs-grails-mode-ext.el is in ~/.emacs.d/vendor
(setq load-path (cons (expand-file-name "~/.emacs.d/vendor") load-path))
(require 'emacs-grails-mode-ext)
```

## License

Copyright © 2012 Yves Zoundi

Licensed under the GPL V3. (http://www.gnu.org/licenses/gpl-3.0.txt)
