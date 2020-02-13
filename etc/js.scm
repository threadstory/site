;;;;;;;;;;;;;;;;
;; js helpers ;;
;;;;;;;;;;;;;;;;

(define pass
  (lambda (expr env)

    (define lookup (lambda (var env)
		     (displayln "looking up " var " in env: " env)
		     (let ((syms (environment-symbols env)))
		       
		       ;; check both scheme-environment and interaction-environment

		       (displayln "me;" (top-level-bound? var)  (top-level-syntax? var))
		       (if (and (member var syms)
			      (not (top-level-bound? var)))
			   (top-level-value var env)
			   var))))
    
    (displayln "expr is " expr)

    (let ((expr-datum (syntax->datum expr)))
      
      (cond
       ((null? expr) (list))
       
       ((symbol? expr-datum) (lookup expr-datum env))

       ((list? expr-datum) (cons (pass (car expr-datum) env)
				 (map (lambda (x) (pass x env)) (cdr expr-datum))))

       ((list? expr)
	(cons (pass (car expr) env)
	      (map (lambda (x) (pass x env)) (cdr expr))))

       (else expr)))))

(trace-define-syntax define-client-script!
  (lambda (stx)    
    (syntax-case stx ()
      ((_ js-thunk ...)
       
       (with-syntax (((js ...) (pass  #'(js-thunk ...) (interaction-environment))))
	 #'(begin js ...))))))
