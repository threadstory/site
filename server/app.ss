(import :std/net/httpd
	:std/text/json
	:std/text/utf8
	:std/sugar
	:gerbil/gambit/threads)

(export main)

;; (define vec1 #f)

;; (hash-map cons (hash-get vec1 'info))

(define options-response-headers '(("Access-Control-Allow-Origin" . "*")
				   ("Access-Control-Allow-Methods" . "GET, POST, OPTIONS")
				   ("Access-Control-Allow-headers" . "DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range")
				   ("Access-Control-Max-Age" . "1728000")
				   ("Content-Type" . "text/plain; charset=utf-8")
				   ("Content-Length" . "0")))

(define post-response-headers '(("Content-Type" . "application/json")
				("Access-Control-Allow-Origin" . "*")
				("Access-Control-Allow-Methods" . "GET, POST, OPTIONS")
				("Access-Control-Allow-Headers" . "Content-Length, Content-Range")))

(define contact-handler
  (lambda (req res)
    
    (define (hash->record h)
      (hash-map (lambda (x y)
		  (displayln x y (string-empty? y) (if y 1 2))
		  
		  (cond
		   ((and x y (or (not (string? y))
				 (not (string-empty? y))))
		    (cons x (if (string? y) (string-append "\"" y "\"" ) y)))
		   
		   (else #f))) (hash-get h 'info)))
    
    (case (http-request-method req)
      ((OPTIONS) (http-response-write res
				      204
				      options-response-headers
				      #f))
      ((POST)
       (let (contact-record  (hash->record (string->json-object (utf8->string
								 (http-request-body req)))))
	 (displayln "contact record:" contact-record)
	 (with-output-to-file (list path: "contacts.scm" append: #t)
	   (lambda () (displayln contact-record)))
	 (http-response-write res
			      200
			      post-response-headers
			      (json-object->string (hash ("msg" "query recorded")))))))))

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