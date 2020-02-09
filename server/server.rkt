#lang web-server

(require web-server/servlet-env)
;(require reloadable)

(require "signals.rkt")
(require "app.rkt")

(define (main)
;  (define request-handler
;    (reloadable-entry-point->procedure
;     (make-reloadable-entry-point 'app-dispatch "app.rkt")))
;
;  (reload!)
;
;  (start-restart-signal-watcher)

  (define request-handler app-dispatch)

  (serve/servlet request-handler
               #:stateless? #t
               #:servlet-regexp #rx""
               #:server-root-path "."
               #:servlet-current-directory "."
               #:port 9090))

(main)
