#lang racket

(provide app-dispatch)

(require web-server/servlet
         web-server/servlet-env
         web-server/http
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/private/url-param)

(require web-server/dispatch)

(require json)

(define a "abcd")

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


(define (list->headers headers)
  (map (λ (h) (make-header (string->bytes/utf-8 (car h))
                           (string->bytes/utf-8 (cdr h)))) headers))

(define post-headers (list->headers post-response-headers))
(define options-headers (list->headers options-response-headers))

(define (options req)
  (response 204
            #"OK"
            (current-seconds)
            #"text/plain; charset=utf-8"
            options-headers
            void))

(define (contact-us req)

  (define (hash->record h)
    (hash-map (hash-ref h 'info)
              (lambda (x y)	  
                (cond
                  ((and x y (or (not (string? y))
                                (non-empty-string? y)))
                   (cons x (if (string? y) (string-append "\"" y "\"" ) y)))
		   
                  (else #f)))))
  
  (let ([data (hash->record (string->jsexpr (bytes->string/utf-8 (request-post-data/raw req))))])

    (with-output-to-file "contacts.scm"
      (λ () (displayln data))
      #:exists 'append)
    
    (response 200
              #"OK"
              (current-seconds)
              #"application/json; charset=utf-8"
              post-headers
              (λ (op) (write-json #hasheq((msg . "query recorded")) op)))))

(define (json-response req)
  (response 200
            #"OK"
            (current-seconds)
            #"application/json; charset=utf-8"
            empty
            (λ (op) (write-json #hasheq((waffle . (1 2 3))) op))))


(define-values (app-dispatch vcr-url)
  (dispatch-rules
   [("contact-us") #:method "options" options]
   [("contact-us") #:method "post" contact-us]
   [("hello") json-response]))


;(define a1 (with-input-from-file "contacts.scm"
;             (lambda ()
;               (let lp ((ch (read))
;                        (xs (list)))
;                 (cond
;                   ((eof-object? ch) (reverse xs))
;                   (else (lp (read) (cons ch xs))))))))
;
;a1

