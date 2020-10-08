(require 'gnus-sum)
(require 'smime)
(require 'epa)

(setq fastmail '(nnimap "Fastmail"
                        (nnimap-address "imap.fastmail.com")
                        (nnimap-server-port 993)
                        (nnimap-authenticator login)
                        (nnimap-stream ssl)
                        (nnimap-inbox "INBOX")
                        (nnimap-split-methods fancy)
                        (nnimap-split-fancy
                         (| (any "tech@openbsd.org" "openbsd-tech")
                            (any "misc@openbsd.org" "openbsd-misc")
                            (any "announce@openbsd.org" "openbsd-announce")
                            (any "haskell-cafe@haskell.org" "haskell-cafe")
                            (any "ghc-commits@haskell.org" "ghc-commits")
                            (any "ghc-devs@haskell.org" "ghc-devs")
                            (any "revy@dikurevy.dk" "dikurevy")
                            (from "buildbot@futhark-lang.org" "futhark")
                            (from "builds@travis-ci.org" "travis")
                            (to ".*noreply.github.com.*" "github")
                            (any ".*@spiltirsdag.dk" "spiltirsdag")
                            ("subject" ".*tophemmelig.*" "hemmeligheder")))))

(with-feature
 (bbdb)
 (bbdb-initialize 'gnus 'message)

 (add-hook 'message-mode-hook
           (function (lambda()
                       (local-set-key (kbd "<tab>") 'bbdb-complete-mail))))

 (setq bbdb-mua-auto-update-p 'query)

 (setq bbdb-mua-update-interactive-p '(query . create)))

(setq gnus-select-method '(nnnil "news.tele.dk")
      gnus-use-full-window nil
      gnus-suppress-duplicates t
      gnus-summary-ignore-duplicates t
      gnus-treat-display-smileys nil
      mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-attachment-override-types '("image/.*")
      mm-url-use-external t
      mm-url-program "surf"
      gnus-message-setup-hook 'nil
      mail-user-agent gnus-user-agent
      gnus-buttonized-mime-types '("multipart/signed")
      mail-sources '()
      gnus-secondary-select-methods
      `((nnml "")
        (nnimap "KU-Mail"
                (nnimap-address "exchange.ku.dk")
                (nnimap-server-port 993)
                (nnimap-authenticator login)
                (nnimap-stream ssl)
                (nnimap-inbox "INBOX")
                (nnimap-split-methods fancy)
                (nnimap-split-fancy
                 (| ("subject" ".*tophemmelig.*" "hemmeligheder"))))
        ,fastmail)
      gnus-message-archive-method fastmail
      gnus-message-archive-group "Sent"
      gnus-gcc-mark-as-read t
      gnus-message-replysign t
      gnus-message-replyencrypt t
      gnus-message-replysignencrypted t
      shr-color-visible-luminance-min 80 ; fix grey-on-grey problems.
      mm-keep-viewer-alive-types '(".*")
      gnutls-min-prime-bits 1024
      )

(when (eq system-type 'darwin)
  (setq mailcap-mime-data '(("application"
                             ("pdf"
                              (viewer . "/usr/bin/open %s")
                              (type . "application/pdf")))
                            ("application"
                             (".*"
                              (viewer . "/usr/bin/open %s")
                              (type . "application/octet-stream"))))))

(add-to-list 'gnus-message-setup-hook (lambda ()
                                        (auto-fill-mode 't)))

;;; I don't have a local SMTP-server running, so Emacs will have to
;;; emulate it.
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "localhost")
(setq smtpmail-smtp-service 1337)

(setf nnmail-split-methods nil)

(push '("mail.misc" "")
      nnmail-split-methods)

(defun add-mailing-list-filter (name &optional fulladdr)
  "Add filters to handle mail from mailing list NAME properly. If
FULLADDR is non-NIL, filter by FULLADDR."
  (cl-flet ((match-builder (header)
                           (list (concat "mail."
                                         name)
                                 (concat "^"
                                         header
                                         ":.*"
                                         (or fulladdr name)))))
    (push (match-builder "To")
          nnmail-split-methods)
    (push (match-builder "Cc")
          nnmail-split-methods)))

(defun add-mailing-lists (&rest mailing-lists)
  "Add filter rules for MAILING-LISTS."
  (dolist (entry mailing-lists)
    (let ((foldername (if (listp entry)
                          (first entry)
                        (if (string-match ".+@.+" entry)
                            (substring entry 0 (string-match "@" entry))
                          entry))))
      (dolist (addr (if (listp entry)
                        (rest entry)
                      (list entry)))
        (add-mailing-list-filter foldername addr)))))

(push '("mail.junk" "Subject:.*\\*\\*\\*SPAM\\*\\*\\*.*")
      nnmail-split-methods)

(add-mailing-lists "climacs-devel"
                   "climacs-cvs"
                   "climacs-announce"
                   "gsharp-devel"
                   "stumpwm-devel"
                   "stumpwm-cvs"
                   "gardeners"
                   "mcclim-devel"
                   "mcclim-cvs"
                   "clim-desktop-devel"
                   "clim-desktop-cvs"
                   "dkclug"
                   "sslug-announce"
                   "sslug-emacs"
                   "sslug-forening"
                   "debian-user-danish"
                   "danish-lispniks"
                   "sbcl-devel"
                   "beirc-devel"
                   "beirc-cvs"
                   '("revy" "revy@diku.dk" "drift@dikurevy.dk" "revyteknik@diku" "revy@dikurevy.dk" "materiale@dikurevy.dk")
                   '("texnik" "teknik@dikurevy.dk")
                   "Revy40"
                   "satyrrevy"
                   '("revyvideo" "video@dikurevy.dk")
                   "closure-devel"
                   "closure-cvs"
                   "summeroflisp-discuss"
                   "haskell-cafe"
                   "glasgow-haskell-users"
                   "cvs-ghc"
                   "spiltirsdag"
                   "twelf"
                   "rkg"
                   '("hackers@suckless.org" "hackers@suckless.org")
                   '("dev@suckless.org" "dev@suckless.org" "dwm@suckless.org")
                   "xmonad"
                   "meddelelse@nybro.dk"
                   '("fagraad" "fagraad@dikumail.dk")
                   "dikutal"
                   "reboot"
                   "ip11"
                   "ip12"
                   "topDatamat"
                   "ghc-devs"
                   "ghc-commit"
                   "ghc-builds"
                   "intro"
                   "TOPPS"
                   "APL-group"
                   "Swpat"
                   "oplss14"
                   "tarsnap"
                   "xstg-all"
                   "xstg-cluster"
                   "hotties"
                   "ov14"
                   "ov15"
                   "osm16"
                   '("openbsd-announce" "announce@openbsd.org")
                   '("openbsd-misc" "misc@openbsd.org" "misc@lists.openbsd.org")
                   '("openbsd-tech" "tech@openbsd.org" "tech@lists.openbsd.org")
                   "da-dk"
                   "ocr-user"
                   "compsys17")

;; Add the remaining rules.

(push '("mail.gardeners" "Subject:.*\\[Gardeners\\].*")
      nnmail-split-methods)

(push '("mail.sigkill" "Subject: sigkill.dk")
      nnmail-split-methods)

(push '("mail.junk" "From: La Lettre de Paix Liturgique")
      nnmail-split-methods)

(push '("mail.facebook" "From:.*@facebookmail\\.com.*")
      nnmail-split-methods)

(push '("mail.facebook" "From:.*@plus\\.google\\.com.*")
      nnmail-split-methods)

(push '("mail.okcupid" "From:.*OkCupid!*")
      nnmail-split-methods)

(push '("mail.diku-qa" "From: DIKU Q&A.*")
      nnmail-split-methods)

(push '("mail.doodle" "From: Doodle .*")
      nnmail-split-methods)

(push '("mail.prog-lang" "Subject:.*\\[Prog-lang\\].*")
      nnmail-split-methods)

(push '("mail.dikunix" "Subject:.*\\[DIKUNIX\\].*")
      nnmail-split-methods)

(push '("mail.futhark-errors" "Subject: Futhark integration error")
      nnmail-split-methods)

(push '("mail.github" "From:.*@github.com")
      nnmail-split-methods)

;; (push '("mail.acm" "From:.*@acm.org")
;;       nnmail-split-methods)

(push '("mail.acm" "From:.*@hq.acm.org")
      nnmail-split-methods)

(push '("mail.acm" "From:.*@ACM.ORG")
      nnmail-split-methods)

(push '("mail.meetup" "From:.*@meetup.com")
      nnmail-split-methods)

(push '("mail.politiken" "From:.*@pol.dk")
      nnmail-split-methods)

(push '("mail.tarsnap" "From:.*@tarsnap.com")
      nnmail-split-methods)

(push '("mail.keybase" "From:.*@keybase.io")
      nnmail-split-methods)

(push '("mail.udacity" "From:.*@udacity.com")
      nnmail-split-methods)

(push '("mail.trello" "From:.*@trello.com")
      nnmail-split-methods)

(push '("mail.paypal" "From:.*@paypal.com")
      nnmail-split-methods)

(push '("mail.piazza" "From:.*@piazza.com")
      nnmail-split-methods)

(push '("mail.travis-builds" "From:.*builds@travis-ci.org")
      nnmail-split-methods)

(push '("mail.futhark-builds" "From: buildbot@futhark-lang.org")
      nnmail-split-methods)

(push '("mail.appveyor" "From:.*no-reply@appveyor.com")
      nnmail-split-methods)

(push '("mail.cron" "From: (Cron Daemon)")
      nnmail-split-methods)

(push '("mail.haskell" "To: haskell@haskell.org")
      nnmail-split-methods)

(defun trh-move-processable-as-spam ()
  (interactive)
  (gnus-summary-move-article nil "nnml:mail.junk"))

(define-key gnus-summary-mode-map
  "$"
  'trh-move-processable-as-spam)
