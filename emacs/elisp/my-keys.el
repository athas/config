;;;; My fundamental key shortcut definitions

;; C-z is for the window manager!
(global-unset-key (kbd "C-z"))

(defmacro global-set-keys (&rest keycommands)
  "Register keys to commands.
Analyze KEYCOMMANDS in pairs, and maps the corresponding keys
to the corresponding functions."
  (let ((setkey-list nil))
    (while keycommands
      (let ((key (car keycommands))
            (command (cadr keycommands)))
        (push `(global-set-key (kbd ,key)
                               ,command)
              setkey-list))
      (setq keycommands (cddr keycommands)))
    (push 'progn setkey-list)
    setkey-list))

(defmacro set-keybinding-for-maps (key command &rest keymaps)
  "Register keys to commands in a nuber of keymaps.
Maps KEY to COMMAND in the keymaps listed in KEYMAPS."
  (let ((defkey-list nil))
    (while keymaps
      (let ((current-map (first keymaps)))
        (push `(define-key
                 ,current-map
                 (kbd ,key)
                 ,command)
              defkey-list))
      (setq keymaps (rest keymaps)))
    (push 'progn defkey-list)
    defkey-list))

(defmacro define-keys (keymap &rest args)
  `(progn
     ,@(let (defs)
         (while args
           (let ((key (first args))
                 (def (second args)))
             (push `(define-key ,keymap ,key ,def) defs))
           (setf args (cddr args)))
         defs)))

(global-set-keys

 ;; M-x strains my fingers.
 "\C-x\C-m" 'execute-extended-command
 "\C-c\C-m" 'execute-extended-command

 ;; Backspace is far away, making backward-kill-word hard to perform.
 "\C-w"     'backward-kill-word
 "\C-x\C-k" 'kill-region
 "\C-c\C-k" 'kill-region

 "M-p"      'backward-paragraph
 "M-n"      'forward-paragraph

 "\C-xw"    'goto-line
 "\C-x\C-b" 'buffer-menu
 "\C-cn"    'bs-cycle-next
 "\C-cp"    'bs-cycle-previous
 "\C-ce"    'eshell
 "\C-c\C-e" 'eshell
 "\C-ck"    'compile
 "\C-x!"    'shell-command
 "\C- "     'set-mark-command
 "\C-hg"    'apropos
 "<f5>"      (lambda ()
               (interactive)
               (find-file "~/.notes"))
 "<f6>"      (lambda ()
               (interactive)
               (find-file "~/.todo"))

 "<f1>"     'other-window
 "C-x c s"  'slime-selector
 "C-x a r"  'align-regexp
 "C-S-n"    'next-error
 "C-S-p"    'previous-error
)

(global-set-key (kbd "C-c TAB") 'lisp-complete-symbol)
(global-set-key  (kbd "<C-tab>") 'other-window)

;;; A lot of major modes do not allow quick exit, but they should.
(defvar maps-for-quick-exit nil
  "List of keymaps that should have a key for quick exit defined.")

(with-feature (help-mode)
              (push help-mode-map maps-for-quick-exit))

(with-feature (grep)
              (push grep-mode-map maps-for-quick-exit))

(push completion-list-mode-map maps-for-quick-exit)

(dolist (map maps-for-quick-exit)
  (set-keybinding-for-maps  "q" 'kill-this-buffer map))

