(in-package :beirc)

(setf *default-nick* "beathas")
(setf *default-realname* "Troels Henriksen")

(setf *auto-join-alist* '(("irc.freenode.net" . ("#lisp" "#diku"))
                          ("irc-quakenet.org" . ("#udvikleren.dk" "#athas"))))

(define-beirc-command (com-quote :name t)
  ((what 'mumble :prompt "Text"))
  (let ((stream (irc:output-stream (current-connection *application-frame*))))
    (write-sequence what stream)
    (terpri stream)
    (force-output stream)))
