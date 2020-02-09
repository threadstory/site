(import :std/net/httpd
	:std/text/json
	:std/text/utf8
	:std/sugar
	:gerbil/gambit/threads)

(export main)

;; (define vec1 #f)

;; (hash-map cons (hash-get vec1 'info))

(define contact-handler
  (lambda (req res)
    
    (define (hash->record h)
      (hash-map cons (hash-get h 'info)))
    
    (let ((contact-record  (hash->record (string->json-object (utf8->string
								(http-request-body req))))))
      (displayln "contact record:" contact-record)
      (with-output-to-file (list path: "contacts.scm" append: #t)
	(lambda () (displayln contact-record)))
      (http-response-write res
			   200
			   '(("Content-Type" . "application/json"))
			   (json-object->string (hash ("msg" "query recorded")))))))

(define address "127.0.0.1:8000")

(define (start-server) (start-http-server! address))

(define (setup httpd)
  (set-httpd-max-token-length! 2048)
  (http-register-handler httpd "/contact-us" contact-handler)
  (thread-join! httpd))

(define (stop-server httpd)
  (stop-http-server! httpd))

;; (define httpd (start-server))
;; (setup httpd)

(define (main . args)
  (setup (start-server)))