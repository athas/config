;;;; Dumping ground for random mode configurations, mostly for
;;;; different languages.

(add-to-list 'load-path "~/emacs/gnu-apl-mode")
(noerr-require 'gnu-apl-mode)

(with-feature
 (flycheck-mode)
 (global-flycheck-mode))

(with-feature
 (c-mode)
 (setq c-basic-offset 2)
 (add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode)))

(with-feature
 (moo-code)
 (add-to-list
  'auto-mode-alist
  (cons "\\.moo" 'moo-code-mode))
 )

;; Always autofill if I ever end up editing a ChangeLog again.
(add-hook 'change-log-mode-hook
          (lambda ()
            (auto-fill-mode)))

(with-feature
 (paredit)
 (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1))))

;;; Darcs - a revision control system.

;; I might accidentaly open a darcs-file, in that case, warn.
(add-hook 'find-file-hook 'warn-if-darcs-file)

(defun warn-if-darcs-file()
  (let ((f (buffer-file-name (current-buffer))))
    (and f (string-match "_darcs" f)
         (if (y-or-n-p "This is a _darcs file, open the real file? ")
             (jump-to-real-file-from-darcs)
           (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
                 mode-line-buffer-identification)))))

(defun jump-to-real-file-from-darcs()
  (interactive)
  (let* ((f (buffer-file-name (current-buffer)))
         (match (string-match "_darcs/current" f)))
    (and f match
         (find-alternate-file
          (concat (substring f 0 (match-beginning 0))
                  (substring f (match-end 0)))))))

;;; SLIME and generic Common Lisp.

(with-features
 (slime paredit)
 ;;(setq inferior-lisp-program "/usr/bin/lisp")
 (setq inferior-lisp-program "sbcl"
       slime-multiprocessing t
       slime-startup-animation nil)
 (slime-setup '(slime-fancy slime-asdf))
 (define-key paredit-mode-map (kbd "RET") nil)
 (define-key lisp-mode-shared-map (kbd "RET") 'paredit-newline)
 (define-key slime-repl-mode-map (kbd "C-M-d") 'down-list)
 (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
 (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
 (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
 (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
 (add-to-list 'auto-mode-alist '("\\.cmucl-init$" . lisp-mode))
 (add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

 (setq slime-complete-symbol-function 'slime-complete-symbol*)

 ;; I have a local copy of the Common Lisp HyperSpec
 (setq common-lisp-hyperspec-root "file:/home/athas/docs/HyperSpec/")

 (defvar hyperspec-browser-function 'w3m-browse-url
   "Function to display the relevant entry of the HyperSpec in a WWW browser.
This is used by the command `trh-hyperspec-lookup'.")

 (define-key slime-mode-map "\C-c\C-dh"
   (lambda ()
     (interactive)
     (let ((browse-url-browser-function hyperspec-browser-function))
       (call-interactively 'slime-hyperspec-lookup))))

 (global-set-key "\C-c\C-dh"
                 '(lambda ()
                    (interactive)
                    (let ((browse-url-browser-function hyperspec-browser-function))
                      (call-interactively 'slime-hyperspec-lookup))))

 (setq lisp-simple-loop-indentation 1
       lisp-loop-keyword-indentation 6
       lisp-loop-forms-indentation 6)

 )

;;; Erlang setup.

(add-to-list 'load-path "~/emacs/erlang")
(noerr-require 'erlang-start)

;;; Scheme setup.

(setq scheme-program-name "mzscheme")
(with-feature
 (quack)


 (set-face-foreground quack-pltish-defn-face "green")

 (define-key scheme-mode-map "\C-c\C-c" 'scheme-send-last-sexp)
 (define-key scheme-mode-map "\C-c\C-e" 'scheme-compile-definition-and-go)
 )

;;; Agda setup
(ignore-errors
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (add-to-list 'agda2-include-dirs
               "/home/athas/oregon-summer-school/agda-prelude/src/"))

(with-features
 (haskell-mode haskell-interactive-mode haskell-process)

 (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
 )

;;; SML setup.
(with-feature
 (sml-mode)

 ;; Use MOSML for DIKU.
 (setf sml-program-name "mosml")
 (setf sml-default-arg "-P full")

 (add-to-list 'auto-mode-alist
              '("\\.sml$" . sml-mode))
 (add-to-list 'auto-mode-alist
              '("\\.ML$"  . sml-mode))
 (add-to-list 'auto-mode-alist
              '("\\.sig$" . sml-mode)))

;;; AUCTeX setup.
(with-feature
 (latex)
 (setq TeX-auto-save nil)
 (setq TeX-parse-self t)
 (setq-default TeX-master nil)
 (setq-default TeX-engine 'default)
 (TeX-global-PDF-mode t)
 (setq TeX-electric-sub-and-superscript t)
; (add-to-list 'TeX-view-program-selection '(output-pdf "Evince"))
 (add-to-list 'LaTeX-indent-environment-list
              '("lstlisting" current-indentation))
 (add-to-list 'LaTeX-verbatim-environments
              "lstlisting") )

;;; Eshell:
(with-feature
 (eshell)
 (noerr-require 'esh-mode)
 (noerr-require 'em-cmpl)

  (setq eshell-history-size 16000)
  (add-to-list 'eshell-output-filter-functions 'eshell-handle-control-codes)
  )

(with-feature
 (futhark-mode)

 (add-hook 'futhark-mode-hook (lambda () (flycheck-mode)))
 )

(with-feature
 (cc-mode)

 (add-to-list 'auto-mode-alist
              '("\\.cu$" . c++-mode))
 (add-to-list 'auto-mode-alist
              '("\\.cl$" . c-mode)))

(with-feature
 (org)
 (define-key org-mode-map (kbd "<C-tab>") nil))

(with-feature
 (fsharp-mode)

 (setq fsharp-ac-intellisense-enabled nil))

;; Do not indent C++ namespace contents.
(defun my-c-setup ()
   (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'my-c-setup)
