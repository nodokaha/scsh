;;; This file is meant for developing.  Use the example startup
;;; scripts to start the webserver,
;;; e.g. $SCSH_LIB_DIRS/sunet/web-server/start-surflet-server

;;; Reads package descriptions in the right order.  In the end, the
;;; server can be started via SERVER. Assumes scsh has been started with
;;; SSAX loaded: scsh -lel SSAX/load.scm (otherwise surflets won't work)
;;; and it is called with cwd=sunet/scheme/httpd/surflets/

(batch 'on)
(config `(load "../../packages.scm"))
(config `(load "packages.scm"))
(config `(load "../../../web-server/start-surflet-server"))
(user)
(open 'surflet-server)
(batch 'off)
(in 'scsh '(run (display "type (server) to start the server\n")))

