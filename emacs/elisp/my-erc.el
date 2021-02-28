;;;; Setup for ERC, the best IRC client.

(noerr-require 'erc-auto)
(noerr-require 'erc-autojoin)
(noerr-require 'erc-match)

;; Fix a potentially broken charset:
(setq erc-default-coding-system '(utf-8 . undecided)
      erc-join-buffer 'bury)

;; Some stupid channels require obsolete charsets.
(push '("#udvikleren.dk" . (iso-latin-1 . undecided))
      erc-encoding-coding-alist)

(push '("#linuxin.dk" . (iso-latin-1 . undecided))
      erc-encoding-coding-alist)

(push '("#piratgruppen.org" . (iso-latin-1 . undecided))
      erc-encoding-coding-alist)

;; Display private messages in new buffers.
(setq erc-auto-query 'buffer)

(setq erc-paranoid t ; Show CTCP-messages.
      erc-email-userid "athas@@sigkill.dk"
      erc-user-full-name "Troels Henriksen"
      erc-prompt (lambda () (concat (buffer-name) ">"))
      erc-track-visibility 't)

;; Read-only:
(add-hook 'erc-insert-post-hook 'erc-make-read-only)
(add-hook 'erc-send-post-hook 'erc-make-read-only)

;; Spellchecking.
(with-feature
 (flyspell)
 (erc-spelling-mode 0)

 (setq erc-spelling-dictionaries '(("irc.quakenet.org" "dansk")
                                   ("#eggsml" "dansk")
                                   ("#diku" "dansk")
                                   ("localhost" "dansk"))))

;; Hooks, put stuff here:
(add-hook 'erc-mode-hook
          (lambda ()
            ;; Use emacs-mule as coding system for all erc-buffers (for easy logging).
            (set (make-variable-buffer-local 'coding-system-for-write) 'emacs-mule)))

;; /hop command ala mIRC.

;; Generic slap.
(defun erc-cmd-SLAPWITH (&rest rest)
  (erc-send-action
   (erc-default-target)
   (concat "slaps "
           (car rest)
           " around a bit with"
           (let (thing)
             (setq rest (cdr rest))
             (while (not (equal (car rest) nil))
               (setq thing (concat thing " " (car rest)))
               (setq rest (cdr rest)))
             thing)
           ".")))

;; So, I don't use Winamp, but I can at least print my Emacs-state.
(defun erc-cmd-EMACS ()
  (erc-send-action
   (erc-default-target)
   (apply 'concat "is editing "
          (nconc
           (mapcar (lambda (buffer)
                     (concat "\"" (buffer-name buffer) "\" ["
                             (save-excursion (set-buffer buffer)
                                             (symbol-name major-mode))
                             "], "))
                   (remove-if-not 'buffer-file-name (buffer-list)))
           (list "and that's it.")))))

;; Badass slap!
(defun erc-cmd-SLAP (&rest nick)
  (erc-cmd-SLAPWITH (car nick) "a small 50lb Unix-manual"))

;; Yucky slap. :-(
(defun erc-cmd-SLAPPENIS (&rest nick)
  (erc-cmd-SLAPWITH (car nick) (concat
                                "his "
                                (find-epenis)
                                "cm long ePenis")))

(defun erc-cmd-EPENIS ()
  "Calculates the size of your ePenis and writes it in `erc-default-target'"
  (erc-send-message
   (get-epenis)))

(defun penisdyst (process response)
  (let ((user (car (erc-parse-user (erc-response.sender response)))))
    (when (and (string= "!penisdyst" (erc-response.contents response))
               (or (string-equal "KasperTSW" user)
                   (string-equal "Athas" user)
                   (string-equal "WinAthas" user)))
      (save-excursion
        (set-buffer (first (erc-response.command-args response)))
        (erc-send-message (get-epenis)))))
  nil)

(add-hook 'erc-server-PRIVMSG-functions 'penisdyst)

(defun get-epenis ()
  "Returns a string describing your ePenis."
  (let ((penis (find-epenis)))
    (concat
     "ePenis: "
     (graph-epenis (string-to-number penis) 4)
     " ("
     penis
     "cm).")))

(defun graph-epenis (length fraction)
  "Return ASCII image of epenis of length LENGTH, with one \"piece\" for
each FRACTION in LENGTH."
  (concat
   "o"
   (make-string (floor (/ length fraction)) ?=)
   "8"))

(defun find-epenis ()
  "Return a string containing size of electronic penis.
Returns the value in centimeters."
  (shell-command "cat /proc/uptime") ; Puts output in *Shell Command Output*
  (save-excursion
    (set-buffer "*Shell Command Output*")
    (goto-char (point-min))
    (format "%.3f"
            (* 2.427
               (log
                (string-to-number
                 (buffer-substring
                  (point-min)
                  (search-forward "." (point-max)))))))))

(defvar irc-nickname "Athas"
  "Standard nickname for use on IRC")

(defvar irc-port 6667
  "Standard port to connect to IRC servers with.")

(defvar irc-realname "Troels Henriksen"
  "Standard real name on IRC.")

;;; ERC really doesn't like connecting to the same IRC-server
;;; twice. Therefore, I set up a guard, to error out early, before any
;;; network connections are made, if ERC is already connected to the
;;; specified server.

(defvar irc-connected-hosts nil
  "List of servers ERC is connected to.")

(defun irc-connect (server &optional port)
  "Connect to SERVER via ERC, using default values for nickname, realname and
port. Port can be specified as optional parameter PORT. The default values are
specified in the variables `irc-nickname', `irc-port' and `irc-realname'."
  (when (find server irc-connected-hosts)
    (error "Already connected to server."))
  (erc :server server :port (or port irc-port) :nick irc-nickname :full-name irc-realname)
  (push server irc-connected-hosts))

(defun irc-bitlbee ()
  (interactive)
  (irc-connect "localhost" 6667))

(defun irc-quakenet ()
  "Connect to the QuakeNet IRC-network using ERC."
  (interactive)
  (irc-connect "irc.quakenet.org"))

(defun irc-freenode ()
  "Connect to the Freenode IRC-network using ERC."
  (interactive)
  (irc-connect "irc.freenode.net" 8001))

(defun irc-efnet ()
  "Connect to the EFNet IRC-network using ERC."
  (interactive)
  (irc-connect "irc.inet.tele.dk" 6661))

(defun irc-chatsociety ()
  "Connect to the ChatSociety IRC-network using ERC."
  (interactive)
  (irc-connect "irc.chatsociety.net"))

(defun irc-synirc ()
  "Connect to the SynIRC IRC-network using ERC."
  (interactive)
  (irc-connect "irc.synirc.net"))

(defun irc-zybourne ()
  "Connect to the Zybourne IRC-network using ERC."
  (interactive)
  (irc-connect "irc.zybourne.net"))

(defun irc-oftc ()
  (interactive)
  (irc-connect "irc.oftc.net"))

(defun irc-cleanup ()
  "Kills all IRC buffers.
For now, it kills all buffers in ERC-mode."
  (interactive)
  (save-window-excursion
    (let ((print-list)
          (kill-buffer-query-functions ())
          (obuffer (current-buffer)))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (eq major-mode 'erc-mode)
          (set-buffer buffer)
          (progn
            (kill-buffer buffer))))
      (setq irc-connected-hosts nil))))

(add-to-list 'kill-emacs-query-functions
             (lambda () (progn
                          'irc-cleanup
                          t)))         ; Just to clean up properly.

;; Wrapper function to connect.
(defun irc ()
  "Automatically connects to the IRC-servers irc.quakenet.org
and irc.freenode.net using ERC."
  (interactive)
  ;; (irc-quakenet)
  ;; (irc-freenode)
  ;; (irc-oftc)
  ;; (irc-bitlbee)
  (znc-erc 'freenode)
  (znc-erc 'bitlbee)
  (znc-erc 'synirc)
  ; (znc-erc 'efnet)
  )

;; Set up highlight-options:
(setq erc-keywords '("Athas" "YuleAthas" "futhark" "Futhark" "accelerate" "Accelerate"))

(erc-match-mode 1)

;; Make my prompt reflect the current channel:
(setq erc-prompt (lambda ()
                   (if (and (boundp 'erc-default-recipients) (erc-default-target))
                       (erc-propertize (concat (erc-default-target) ">") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
                     (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t 'front-nonsticky t))))

;; Auto-join of channels is a nice thing to have.
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#diku" "#haskell" "#eggsml" "#proglangdesign" "#haskell-offtopic")
        ("quakenet.org" "#udvikleren.dk")
        ("zybourne.net" "#cobol")
        ))

(setq erc-truncate-buffer-on-save nil)

(defun reset-erc-track-mode ()
  "Clear the list of channels with activity."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(global-set-key "\C-cg" 'reset-erc-track-mode)

;; Timestamps:
(erc-timestamp-mode t)

(setq erc-hide-timestamps nil
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%R:%S  "
      erc-fill-prefix "          "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)


;;; Logging

;; Set up logging:
(setq erc-log-insert-log-on-open t)
(setq erc-log-channels nil)
(setq erc-enable-logging t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-generate-log-file-name-function 'erc-generate-log-file-name)

(defun erc-generate-log-file-name (buffer target nick server port)
  "This function computes a somewhat short log file name.
In fact, it only uses the target and server name, so
you can affect that using `rename-buffer' and the-like."
  (let ((file (concat target "@" server ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(setq erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

;; When exiting emacs, save all logs without confirmation
(defadvice save-buffers-kill-emacs
    (before save-logs (&rest args) activate)
  (save-some-buffers
   t
   (lambda ()
     (when (and (eq major-mode 'erc-mode)
                (not (null buffer-file-name))) t))))
