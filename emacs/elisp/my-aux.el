;;;; My own random and small hacks.

;;; I always end up with spurious whitespaces, this will remove them.
(defun trh-remove-whitespace-around-point ()
  "Removes whitespace from point up to, anc back to, the next non-whitespace.
Will affect linebreaks, tabs and spaces"
  (interactive)
  (save-excursion
    (let ((orig-pos (point)))
      (delete-region
       (progn
         (skip-chars-forward " \t\n")
         (constrain-to-field nil orig-pos t))
       (progn
         (skip-chars-backward " \t\n")
         (constrain-to-field nil orig-pos))))))

(global-set-key "\C-cr" 'trh-remove-whitespace-around-point)

(defun trh-indent-whole-buffer ()
  "Indents the whole buffer.
Uses ``indent-region'' to indent the whole buffer."
  (interactive)
  (when (y-or-n-p "Are you sure you want to indent the entire buffer? ")
    (indent-region (point-min) (point-max) nil)))

(global-set-key "\C-ci" 'trh-indent-whole-buffer)

(defun trh-visit-all-buffers ()
  "Will switch to, and close, all currently active buffers.
This is designed to be used in conjuction with ERC."
  (interactive)
  (save-window-excursion
    (let ((print-list)
          (obuffer (current-buffer)))
      (dolist (buffer (buffer-list))
        (switch-to-buffer buffer)))))

(defun trh-switch-to-gnus (&optional arg)
  "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*
    Use a prefix argument to start Gnus if no candidate exists."
  (interactive "P")
  (let (candidate
        (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                 ("^\\*Group")
                 ("^\\*Summary")
                 ("^\\*Article" nil (lambda ()
                                      (buffer-live-p gnus-article-current-summary))))))
    (catch 'none-found
      (dolist (item alist)
        (let (last
              (regexp (nth 0 item))
              (optional (nth 1 item))
              (test (nth 2 item)))
          (dolist (buf (buffer-list))
            (when (and (string-match regexp (buffer-name buf))
                       (> (buffer-size buf) 0))
              (setq last buf)))
          (cond ((and last (or (not test) (funcall test)))
                 (setq candidate last))
                (optional
                 nil)
                (t
                 (throw 'none-found t))))))
    (cond (candidate
           (switch-to-buffer candidate))
          (arg
           (gnus))
          (t
           (error "Gnus does not appear to be running,")))))
(global-set-key (kbd "\C-cf") 'trh-switch-to-gnus)

(defun trh-insert-shell-command-output (command)
  "Inserts the output of COMMAND at point.
This function will garble the contents of *Shell Command Output*
if the buffer already exists."
  (interactive "MCommand: ")
  (shell-command command)      ; Puts output in *Shell Command Output*
  (insert-string
   (save-excursion
     (save-window-excursion
       (set-buffer "*Shell Command Output*")
       (buffer-substring
        (point-min)
        (point-max))))))

(global-set-key "\C-c!" 'trh-insert-shell-command-output)


(defun revert-all-file-buffers ()
  "Refresh all open buffers from their respective files."
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (let ((filename (buffer-file-name buffer)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when (and filename
                   (not (buffer-modified-p buffer)))
          (if (file-exists-p filename)
              ;; If the file exists, revert the buffer.
              (with-current-buffer buffer
                (revert-buffer :ignore-auto :noconfirm :preserve-modes))
            ;; If the file doesn't exist, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buffer)
              (message "Killed non-existing file buffer: %s" filename)))))
      (setq buffer (pop list)))
    (message "Finished reverting buffers containing unmodified files.")))

(defun untabify-buffer ()
  "Call `untabify' with the entire buffer as region."
  (interactive)
  (untabify (point-min) (point-max)))

;;; What to do when starting computer:
(defun on-start ()
  (irc)
  (gnus))

(defun six-windows ()
  "Split the Emacs frame into a three-by-two window grid."
  (interactive)
  (delete-other-windows)
  (let* ((tl (selected-window))
         (tm (split-window tl nil 'right))
         (tr (split-window tm nil 'right))
         (bl (split-window tl nil 'below))
         (bm (split-window tm nil 'below))
         (br (split-window tr nil 'below)))
    (balance-windows)))

(defun three-windows ()
  "Split the Emacs frame into a three-by-one window grid."
  (interactive)
  (delete-other-windows)
  (let* ((tl (selected-window))
         (tm (split-window tl nil 'right))
         (tr (split-window tm nil 'right)))
    (balance-windows)))

(defun four-windows ()
  "Split the Emacs frame into a four-by-one window grid."
  (interactive)
  (delete-other-windows)
  (let* ((a (selected-window))
         (b (split-window a nil 'right))
         (c (split-window b nil 'right))
         (d (split-window c nil 'right)))
    (balance-windows)))

(with-feature
 (loadhist)
 (defun reload-feature (feature)
   "Uses `unload-feature' to get rid of a feature, then reload it
with `require'."
   (interactive
    (list
     (read-feature "Reload feature: " t)))
   (unless (featurep feature)
     (error "%s is not a currently loaded feature" (symbol-name feature)))
   (unload-feature feature t)
   (require feature)))

;;; BBDB setup

(with-feature
 (bbdb)
 ;;If you don't live in Northern America, you should disable the
 ;;syntax check for telephone numbers by saying
 (setq bbdb-north-american-phone-numbers-p nil)
 ;;Tell bbdb about your email address:
 (setq bbdb-user-mail-names
       (regexp-opt '("athas@sigkill.dk"
                     "mzd885@alumni.ku.dk")))
 ;;cycling while completing email addresses
 (setq bbdb-complete-name-allow-cycling t)
 ;;No popup-buffers
 (setq bbdb-use-pop-up nil))

(defun termbin ()
  "Send the current region to termbin.com, and put the URL into the kill ring."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end)
   "nc termbin.com 9999"
   t
   nil))

