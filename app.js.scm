;; can be  compiled to js with chicken-spock


(define (get-element-by-id id)
  (%inline "document.getElementById" id))

(define (query-selector-all sel)
  (%inline "document.querySelectorAll" sel))

(define-syntax-rule (add-class e classes ...)
  (%inline ".classList.add" e classes ...))

(define-syntax-rule (remove-class e classes ...)
  (%inline ".classList.remove" e classes ...))

(define-syntax-rule (contains-class? e class)
  (%inline ".classList.contains" e class))

(define header (get-element-by-id "header"))
(define navcontent (get-element-by-id "nav-content"))
(define navaction (get-element-by-id "navAction"))
(define brandname (get-element-by-id "brandname"))
(define to-toggle (query-selector-all ".toggleColour"))

;; this may not work when we include client side routing
(define active-nav (get-element-by-id "active-nav"))
;; (define navs (query-selector-all ".nav"))

(define nav-menu (get-element-by-id "nav-toggle"))

(define (setup-navbar)
  
  ;; todo cleanup this function
  (define (change-navbar-background background-white?)
    (print "bg white " background-white?)
    (if background-white?
	(begin 
	  (add-class header "bg-white" "shadow")

	  (add-class navaction "gradient" "text-white")
	  (remove-class navaction "bg-white" "text-gray-800")

	  (%inline ".forEach"
		   to-toggle
		   (callback (lambda (e)
			       (remove-class e "text-white")
			       (add-class e "text-black" "hover:text-green-800"))))

	  (when (and (not (null? active-nav)) (contains-class? active-nav "text-black"))
	    (remove-class active-nav "text-black")
	    (add-class active-nav "text-green-800"))
	  
	  (add-class navcontent "bg-white")
	  (remove-class navcontent "bg-gray-100")

	  (remove-class nav-menu "text-black")
	  (add-class nav-menu "text-green-800"))
	(begin
	  (remove-class header "bg-white" "shadow")

	  (add-class navaction "bg-white" "text-gray-800")
	  (remove-class navaction "gradient" "text-white")

	  (%inline ".forEach" to-toggle (callback
					 (lambda (e)
					   (remove-class e "text-black" "hover:text-green-800")
					   (add-class e "text-white"))))

	  (remove-class active-nav "text-green-800")
	  (add-class active-nav "text-black")
	  
	  (remove-class navcontent "bg-white")
	  (add-class navcontent "bg-gray-100")

	  (remove-class nav-menu "text-green-800")
	  (add-class nav-menu "text-black"))))

  (when (not (void? (%host-ref "window.bgWhite"))) (change-navbar-background #t))
  
  (%inline "document.addEventListener"
	   "scroll"
	   (callback (lambda ()
		       (let ((scroll-pos (%host-ref "window.scrollY")))
			 (change-navbar-background (or (> scroll-pos 10)
						      (not (void? (%host-ref "window.bgWhite"))))))))))


(define (setup-mobile-navbar)
  (define nav-menu-div (get-element-by-id "nav-content"))
  (define nav-menu (get-element-by-id "nav-toggle"))

  (define *drawer-open* #f)

  (define nav-callback
    (lambda (e)
      (define to-toggle (query-selector-all ".toggleColour"))
      (if *drawer-open*
	  (add-class nav-menu-div "hidden")
      	  
	  (begin
	    (remove-class nav-menu-div "hidden")

	    (%inline ".forEach"
		     to-toggle
		     (callback (lambda (e)
				 (remove-class e "text-white" "hover:text-black")
				 (add-class e "text-black" "hover:text-green-800"))))

	    (remove-class active-nav "text-white")
	    (add-class active-nav "text-black")))
      (set! *drawer-open* (not *drawer-open*))))

  (%property-set! .onclick nav-menu (callback nav-callback)))


(define (setup)
  (setup-navbar)
  (setup-mobile-navbar))

(set! window.onload (callback setup))
