;;;; Troels Henriksen's .emacs configuration file.
;;;;
;;;; init.el just loads the various other files in this directory.
;;;;
;;;;   First written on November 9th 2014, to replace an old and very
;;;;   crufty .emacs.

;; I need Common Lisp functionality to work.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

;; Some nice things that make it easier to write conditional
;; configuration.
(defun noerr-require (feature)
  "`require' FEATURE, but don't invoke any Lisp errors.
If FEATURE cannot be loaded, this function will print an error
message through `message' and return nil. It otherwise behaves
exactly as `require'."
  (ignore-errors
    (require feature (symbol-name feature) t)))

(defmacro with-feature (feature &rest body)
  "Require FEATURE and execute BODY.
If FEATURE can't be loaded, don't execute BODY."
  (when (noerr-require (car feature))
    (push 'progn body)))

(defmacro with-features (features &rest body)
  "Require FEATURES and execute BODY.
If any of FEATURES cannot be loaded, don't execute BODY."
  (if features
      `(with-feature (,(first features))
         (with-features ,(cdr features)
           ,@body))
    `(progn ,@body)))

;; I put my personal hacks in the lisp subdirectory.
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Stuff I find on the Internet goes here.
(add-to-list 'load-path "~/emacs/")

;; I don't use .custom much, but it goes in here.
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; My preferences are stored in files with a my- prefix so I hopefully
;; will not have conflicts.
(load-library "my-prefs")
(load-library "my-keys")
(load-library "my-modes")
(with-feature
 (erc)
 (load-library "my-erc"))
(load-library "my-aux")
