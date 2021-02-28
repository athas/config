;;;; Changing Emacs defaults that are dumb, and other basic
;;;; initialisation.
;;;;
;;;; Keybindings don't go here, but in keys.el, and mode-specific
;;;; keybindings elsewhere.

;; Backup-files in the working directory sucks; put them in ~/backup
;; instead.
(add-to-list 'backup-directory-alist
             (cons ".*" "~/backup"))

;; Lots of variables...
;; Do not use The Evil Tab, use Holy Spaces instead.
(setq-default indent-tabs-mode nil
	      case-fold-search t
              show-trailing-whitespace t)

(setf pop-up-windows t
      pop-up-frames nil
      european-calendar-style t         ; Use european date format.
      delete-auto-save-files t   ; Delete unnecessary auto-save files.
      default-major-mode 'fundamental-mode ; At least this mode won't do anything stupid.
      scroll-step 1                   ; Only move in small increments.
      scroll-conservatively 10000
      ;; Personal information.
      user-mail-address "athas@sigkill.dk"
      user-full-name "Troels Henriksen"
      user-company-name "Church of Emacs"
      mail-user-agent 'gnus-user-agent
      visible-bell t
      fill-column 70
      dired-recursive-copies t
      enable-local-variables :safe
      undo-strong-limit 3000000
      tab-width 2
      auto-revert-verbose nil
      tab-always-indent t ;; Always run the indent-function on <Tab>.
      inhibit-startup-screen t
      vc-follow-symlinks t
      )

;; More useful frame title
(setq frame-title-format
      '("%b - Emacs " emacs-version))

;; Don't show me the region.
(transient-mark-mode 0)

;; Highlight current line.
(if window-system
    (global-hl-line-mode 1))

;; Enable syntax-highlighting.
(global-font-lock-mode t)

;; Enable emacsclient.
(server-start)

;; Show column number on the mode-line.
(column-number-mode 1)

;; Show line number on the mode-line.
(line-number-mode 1)

;; Paren-matching.
(show-paren-mode 1)

;; Don't auto-fill by default.
(auto-fill-mode -1)

;; ido is a really great way to switch between everything.
(with-feature
 (ido)
 (ido-mode 1)
 (setq ido-enable-flex-matching t)
 (defvar ido-enable-replace-completing-read nil
   "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

 ;; Replace completing-read wherever possible, unless directed otherwise
 (defadvice completing-read
   (around use-ido-when-possible activate)
   (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
           (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
       ad-do-it
     (let ((allcomp (all-completions "" collection predicate)))
       (if allcomp
           (setq ad-return-value
                 (ido-completing-read prompt
                                      allcomp
                                      nil require-match initial-input hist def))
         ad-do-it))))
 (add-hook 'ido-define-mode-map-hook 'ido-my-keys)
 (defun ido-my-keys ()
   "Add my keybindings for ido."
   (define-key ido-mode-map (kbd "C-w") 'backward-kill-word))
 ;; Display ido results vertically, rather than horizontally
 (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                         " [No match]" " [Matched]" " [Not readable]"
                         " [Too big]" " [Confirm]"))
 (defun ido-disable-line-trucation ()
   (set (make-local-variable 'truncate-lines) nil))
 (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation))

(with-feature
 (ispell)
 (setq ispell-program-name "aspell")
 (ispell-change-dictionary "british" t)
 )

;; When opening files with identical names, name the buffers after
;; their subdirectories.
(with-feature
 (uniquify)
 (setq uniquify-buffer-name-style 'forward))

;; Make Emacs a bitch to close (C-x C-c is sooo easy to hit):
(add-to-list 'kill-emacs-query-functions
             (lambda () (y-or-n-p "Last chance, your work would be lost. ")))
(add-to-list 'kill-emacs-query-functions
             (lambda () (y-or-n-p "Are you ABSOLUTELY certain that Emacs should close? ")))
(add-to-list 'kill-emacs-query-functions
             (lambda () (y-or-n-p "Should Emacs really close? ")))

;; Load the Emacs package system.
(with-feature
 (package)
 (add-to-list 'package-archives
              '("melpa" . "http://melpa.org/packages/"))
 (package-initialize))

;;; Disable X-fluff and remove stuff:
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;;; Set my preferred browser.
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

(when (eq system-type 'darwin)
  (setq browse-url-browser-function (quote browse-url-generic))
  (setq browse-url-generic-program "open")
  (set-frame-font "Menlo 15" nil t))

;; Put autosave files in /tmp.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(with-feature
 (epa)
 (setq epg-gpg-home-directory "~/.gnupg"))

;; Do not auto-revert files bigger than 1MiB.
(defun my-not-too-big (&rest _)
  (or (not buffer-file-name)
      (let ((size (nth 7 (file-attributes buffer-file-name))))
        (< size 1000000))))

(when (>= (string-to-number emacs-version) 25)
  (add-function :after-while buffer-stale-function
                #'my-not-too-big))

;; From https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
