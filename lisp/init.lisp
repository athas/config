;;;; Common Initialization Routines for all Lisp implementations.

(in-package :cl-user)

(pushnew #p"/usr/share/common-lisp/systems/"
         asdf:*central-registry*)
(pushnew #P"/home/athas/.asdf-install-dir/systems/"
         asdf:*central-registry*)
(pushnew #P"/home/athas/.cl-systems/"
         asdf:*central-registry*)
#+openmcl(pushnew #P"/home/athas/code/clx_0.7.3-openmcl-060101/"
              asdf:*central-registry*)

(asdf:oos 'asdf:load-op :asdf-binary-locations)

(setf asdf:*centralize-lisp-binaries* t)
(setf asdf:*source-to-target-mappings*
      '((#p"/home/athas/code/cvsrepos/sbcl/" nil)
        (#p"/usr/lib/sbcl/" nil)
        (#p"/usr/lib64/sbcl/" nil)))

(asdf:oos 'asdf:load-op :asdf-install)

(defun loadsys (system)
  "Load SYSTEM via ASDF."
  (asdf:operate 'asdf:load-op system))

(loadsys :split-sequence)
(loadsys :iterate)

(defun check-pkg (pkg-designator)
  "Report on all symbols exported from the package that do not
  designate a value, a class, a macro or a function."
  (do-external-symbols (symbol (find-package pkg-designator))
    (unless (or (fboundp symbol)
                (ignore-errors (symbol-value symbol) t)
                (ignore-errors (symbol-function symbol) t)
                (ignore-errors (find-class symbol) t))
      (print symbol))))

(in-package #:asdf)

(defmethod operation-done-p ((o operation) (c component))
  (let ((out-files (output-files o c))
	(in-files (input-files o c)))
    (cond ((and (not in-files) (not out-files))
	   ;; arbitrary decision: an operation that uses nothing to
	   ;; produce nothing probably isn't doing much 
	   t)
	  ((not out-files) 
	   (let ((op-done
		  (gethash (type-of o)
			   (component-operation-times c))))
	     (and op-done
		  (>= op-done
		      (or  0)))))
	  ((not in-files) nil)
	  (t
	   (and
	    (every #'probe-file out-files)
	    (> (apply #'min (mapcar #'file-write-date out-files))
	       (apply #'max (mapcar #'(lambda (file)
                                        ;; If no file-write-date can
                                        ;; be found, assume 0. This is
                                        ;; a hacky fix.
                                        (or (file-write-date file)
                                            0))
                                    in-files))))))))

(in-package :cl-user)

;;#+sbcl (load  (compile-file "/home/athas/code/sbcl-threadevents.lisp"))

(defun value-to-string (value)
  "Convert VALUE to string. 
This function will automatically identify the type of VALUE, and invoke
the appropiate conversion function"
  (cond ((symbolp value)
         (symbol-name value))
        (t
         (format nil "~a" value))))

;;; If the fasl was stale, try to recompile and load (once). Since only SBCL
;;; has a separate condition for bogus fasls we retry on any old error
;;; on other lisps. Actually, Allegro has a similar condition, but it's 
;;; unexported.  Works nicely for the ACL7 upgrade, though.
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl 
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #-(or sbcl allegro lispworks) error ()
     (asdf:perform (make-instance 'asdf:compile-op) c)
     (call-next-method))))

(defun load-aserve ()
  "Load Portable AllegroServe."
  (load "/home/athas/code/cvsrepos/portableaserve/INSTALL.lisp")
  (load "/home/athas/code/practicals-1.0.3/Chapter08/packages.lisp")
  (load "/home/athas/code/practicals-1.0.3/Chapter08/macro-utilities.lisp")
  (load "/home/athas/code/practicals-1.0.3/Chapter31/packages.lisp")
  (load "/home/athas/code/practicals-1.0.3/Chapter31/html.lisp"))

(defun run-clim-desktop ()
  (load "/home/athas/code/run-clim-desktop.lisp"))
