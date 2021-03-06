(library-directories '("./thunderchez" "."))

(import (sxml to-html)
	
	(srfi s1 lists)
	(srfi s13 strings)

	(prelude))

(define *threadstory-email* "contact@threadstory.in")
(define *threadstory-phone* "+91-9090701366")

(define *build-environment* 'production)
(define *build-environment* 'dev)

(define dev-environment?
  (lambda () (equal? *build-environment* 'dev)))

(define prod-environment?
  (lambda () (equal? *build-environment* 'production)))

(meta-cond
 ((dev-environment?)
  (define +tailwind-css-url+ "https://unpkg.com/tailwindcss/dist/tailwind.min.css"))
 (else
  (define +tailwind-css-url+ "/base.css")))

(define +font-url+ "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700")

(define *dist-directory* "dist")

(define (ensure-directory dir)
  (let ((full-dist-path (string-append (current-directory)
				       (string (directory-separator))
				       dir)))
    (when (not (file-directory? full-dist-path))
      (mkdir full-dist-path))))


(define (html-template body)
  `(html (@ (lang "el"))
	 (head (meta (@ (charset "UTF-8")))
	       (meta (@ (name "viewport")
			(content "width=device-width, initial-scale=1.0")))
	       (meta (@ (http-equiv "X_UA_Compatible")
			(content "ie=edge")))
	       (meta (@ (description "Custom merchandise")
			(keywords "custom t-shirts, custom hoodies, standees")
			(author "Threadstory")))
	       
	       ;; tailwind css
	       (link (@ (rel "stylesheet")
			(href ,+tailwind-css-url+)) " ")
	       
	       (link (@ (rel "stylesheet")
			(href ,+font-url+)) " ")
	       
	       (style "
.gradient {
   background: linear-gradient(90deg, #5659da 0%, #6a0dad 100%);
}
")
	       ,body)))



(define (classes cs)
  `(class ,(string-join (map (lambda (stx)
			       (symbol->string (syntax->datum stx)))
			     cs)
			" ")))


;;;;;;;;;;;;
;; navbar ;;
;;;;;;;;;;;;


(define navbar
  (lambda (active-item items background-white?)

    (define (make-nav-item item-info active?)
      (let* ((item-name (if (pair? item-info) (car item-info) item-info))
	     (item-href (if (pair? item-info)
			    (cdr item-info)
			    (string-append "/" (symbol->string item-name) ".html")))
	     (inactive-classes (if background-white?
				   (list 'text-black 'text-black)
				   (list 'text-white 'text-white)))
	     (color (cond
		     ((and active? background-white?)
		      (list 'text-green-800 'font-bold))
		     
		     (active? (list 'text-white 'font-bold))

		     (else (cons 'toggleColour inactive-classes))))
	     (css-classes `(inline-block py-2 px-4 ,@color
					 no-underline hover:font-bold
					 hover:text-underline )))
	`(li (@ (class "mr-3"))
	     (a (@ ,(classes css-classes)
		   (href ,item-href)
		   (id ,(if active? "active-nav" "nav-item")))
		,(string-titlecase (symbol->string item-name))))))

    (define (navbar-items)
      (map (lambda (i)
	     (make-nav-item i (equal? (if (pair? i) (car i) i)
				      active-item)))  items))
    
    `(nav (@ (id "header")
	     (class "w-full fixed z-30 top-0 text-white"))
	  
	  (div (@ ,(classes '(w-full container mx-auto flex flex-wrap items-center
				     justify-between mt-0 py-2)))
	       (div (@ (class "pl-4 flex items-center"))
		    (a (@ ,(classes '(toggleColour text-white no-underline
						   hover:no-underline font-bold text-2xl
						   lg:text-4xl))
			  (href "/"))
		       (img (@ (class "h-8 fill-current inline px-4")
			       (src "/pictures/ts-white.svg")))
		       "Threadstory"))

	       ;; hamburger
	       (div (@ (class "block lg:hidden pr-4 text-white")
		       (id "nav-toggle"))
		    (button (@ ,(classes '(flex items-center p-1)))
			    (svg (@ (class "fill-current h-6 w-6")
				    (viewBox "0 0 20 20")
				    (xmlns "http://www.w3.org/2000/svg"))
				 (title "Menu")
				 (path (@ (d "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"))))))

	       (div (@ ,(classes '(w-full flex-grow lg:flex lg:items-center lg:w-auto
					  hidden lg:block mt-2 lg:mt-0 bg-white
					  lg:bg-transparent text-black p-4 lg:p-0 z-20))
		       (id "nav-content"))
		    (ul (@ ,(classes '(list-reset lg:flex justify-end flex-1
						  items-center)))

			,(navbar-items)

			(a (@ (href "/contact-us.html"))
			   (button (@ (id "navAction")
				      ,(classes '(mx-auto lg:mx-0 hover:underline bg-white
							  text-gray-800 font-bold rounded-full
							  mt-4 lg:mt-0 py-4 px-8 shadow
							  opacity-75)))
				   "Contact us"))))))))


;; (navbar 'home '((home . "/") products about))

;;;;;;;;;;;;;;;;;;;
;; form elements ;;
;;;;;;;;;;;;;;;;;;;

(define assget
  (lambda (k xs)
    (let ((val (assoc k xs)))
      (if val (cadr val) #f))))

(trace-define-syntax define-record-with-defaults
  (lambda (stx)
    (syntax-case stx ()
      ((_ record-name (field-info ...))
       (with-syntax (((field-names ...) (map (lambda (field-value)
					     (let ((val (syntax->datum field-value)))
					       (cond
						((symbol? val) (datum->syntax #'record-name val))
						((pair? val) (datum->syntax #'record-name (car val)))
						(else (syntax-error "either a symbol or pair supported")))))
					   #'(field-info ...)))
		     (default-constructor (construct-name #'record-name "make-" #'record-name)))
	 #'(begin (define-record-type record-name (fields field-names ...))
		  
		  (trace-define-syntax record-name
		    (lambda (stx)
		      
		      ;; given a list of symbols and a list of pairs
		      ;; returns list of values associated with the symbols
		      (define order
			(lambda (fields field-values-info)
			  (let ((default-vals (filter (lambda (f) (not (symbol? f))) fields)))
			    (map (lambda (field)
				   (cond
				    ((symbol? field) (assget field field-values-info))
				    ((pair? field) (or (assget (car field) field-values-info)
						      (assget (car field) default-vals)))
				    (else (error "wrong value passed to constructor" field))))
				 fields))))
		      
		      (syntax-case stx ()
			((_ (opts (... ...)))
			 (with-syntax (((vals (... ...))
					(map (lambda (v) (datum->syntax #'record-name v))
					     (order '(field-info ...)
						    (map syntax->datum #'(opts (... ...)))))))
			   #'(default-constructor vals (... ...)))))))))))))


;; constructor takes a plist of values
;; genrates all the usual record defs
(define-record-with-defaults input-options
  (input-name
   [type "text"]
   [label-class "w-1/3"]
   [input-class "w-2/3"]
   [container-class "flex items-center mb-6"]))

(define input
  (lambda (options)
    (let ((input-name (input-options-input-name options)))
      `(div (@ (class ,(input-options-container-class options)))
	    (div (@ (class ,(input-options-label-class options)))
		 (label
		  (@ ,(classes '(block text-gray-500 font-bold md:text-center
				       mb-1 md:mb-0 pr-4 ))
		     (for ,input-name))
		  ,(string-titlecase input-name)))
	    (div (@ (class ,(input-options-input-class options)))
		 (input
		  (@ ,(classes
		       '(bg-gray-200 appearance-none border-2
				     border-gray-200 rounded
				     w-full py-2 px-4 text-gray-700
				     leading-tight focus:outline-none
				     focus:bg-white focus:border-purple-500))
		     (required "true")
		     (type ,(input-options-type options))
		     (id ,(string-downcase input-name))) ""))))))

(define-syntax text-input
  (syntax-rules ()
    ((_ name)
     (input (input-options ((input-name name)))))))


(define (select-box input-name options)
  `(div (@ (class "flex items-center mb-6"))
	(div (@ (class "w-1/3"))
	     (label
	      (@ ,(classes '(block text-gray-500 font-bold md:text-center
				   mb-1 md:mb-0 pr-4 ))
		 (for ,input-name))
	      ,(string-append "Select " (string-titlecase input-name))))
	
	(div (@ (class "w-2/3"))
	     (div (@ (class "relative"))
		  (select (@ (class "block appearance-none w-full bg-gray-200 border border-gray-200 text-gray-700 py-3 px-4 pr-8 rounded leading-tight focus:outline-none focus:bg-white focus:border-gray-500")
			     (id ,input-name))
			  ,@(map (lambda (o) `(option ,o)) options))
		  (div (@ (class "pointer-events-none absolute inset-y-0 right-0 flex items-center px-2 text-gray-700"))
		       (svg (@ (class "fill-current h-4 w-4")
			       (xmlns "http://www.w3.org/2000/svg")
			       (viewBox "0 0 20 20"))
			    (path (@ (d "M9.293 12.95l.707.707L15.657 8l-1.414-1.414L10 10.828 5.757 6.586 4.343 8z")))))))))


;;;;;;;;;;;
;; hero  ;;
;;;;;;;;;;;

(define hero
  `(div (@ (class "pt-24"))
	(div (@ ,(classes '(container px-3 mx-auto flex flex-wrap flex-col md:flex-row
				      items-center)))

	     ;; left column
	     (div (@ ,(classes '(flex flex-col w-full md:w-2/5 justify-center items-center
				      text-center md:text-left)))
		  (p (@ (class "uppercase tracking-loose w-full"))
		     "Custom Merchandise made easy")
		  (h1 (@ (class "my-4 text-5xl font-bold leading-tight"))
		      "One stop solution for your branding needs!")
		  (p (@ (class "leading-normal text-2xl mb-8"))
		     "We work hard to get you the best quality available in the market.")

		  (a (@ (href "/contact-us.html")
			(class "items-center"))
		     (button (@ ,(classes '(mx-auto lg:mx-0 hover:underline bg-white
						    text-gray-800 font-bold rounded-full my-6
						    py-4 px-8 shadow-lg)))
			     "Contact Us")))

	     ;; right column
	     (div (@ (class "w-full md:w-3/5 py-6 text-center"))
		  (img (@ (class "w-full md:w-4/5 z-50")
			  (src "pictures/brand.svg")))))))

(define wavy-svg
  `(div (@ (class "relative"))
	(svg (@ (viewBox "0 0 1428 174")
		(version "1.1")
		(xmlns "http://www.w3.org/2000/svg")
		(xmlns:xlink "http://www.w3.org/1999/xlink"))
	     (g (@ (stroke "none")
		   (stroke-width "1")
		   (fill "none")
		   (fill-rule "evenodd"))
		(g (@ (transform "translate(-2.000000, 44.000000)")
		      (fill "#ffffff")
		      (fill-rule "nonzero"))
		   (path (@ (d "M0,0 C90.7283404,0.927527913 147.912752,27.187927 291.910178,59.9119003 C387.908462,81.7278826 543.605069,89.334785 759,82.7326078 C469.336065,156.254352 216.336065,153.6679 0,74.9732496")
			    (opacity "0.100000001")))

		   (path (@ (d "M100,104.708498 C277.413333,72.2345949 426.147877,52.5246657 546.203633,45.5787101 C666.259389,38.6327546 810.524845,41.7979068 979,55.0741668 C931.069965,56.122511 810.303266,74.8455141 616.699903,111.243176 C423.096539,147.640838 250.863238,145.462612 100,104.708498 Z")
			    (opacity "0.100000001")))

		   (path (@ (d "M1046,51.6521276 C1130.83045,29.328812 1279.08318,17.607883 1439,40.1656806 L1439,120 C1271.17211,77.9435312 1140.17211,55.1609071 1046,51.6521276 Z")
			    (id "Path-4")
			    (opacity "0.200000003"))))
		(g (@ (transform "translate(-4.000000, 76.000000)")
		      (fill "#ffffff")
		      (fill-rule "nonzero"))
		   (path (@ (d "M0.457,34.035 C57.086,53.198 98.208,65.809 123.822,71.865 C181.454,85.495 234.295,90.29 272.033,93.459 C311.355,96.759 396.635,95.801 461.025,91.663 C486.76,90.01 518.727,86.372 556.926,80.752 C595.747,74.596 622.372,70.008 636.799,66.991 C663.913,61.324 712.501,49.503 727.605,46.128 C780.47,34.317 818.839,22.532 856.324,15.904 C922.689,4.169 955.676,2.522 1011.185,0.432 C1060.705,1.477 1097.39,3.129 1121.236,5.387 C1161.703,9.219 1208.621,17.821 1235.4,22.304 C1285.855,30.748 1354.351,47.432 1440.886,72.354 L1441.191,104.352 L1.121,104.031 L0.457,34.035 Z"))))))))


(define section-underline
  '(div (@ (class "w-full mb-4"))
	(div (@ (class "h-1 mx-auto gradient w-64 opacity-25 my-0 py-0 rounded-t")))))

(define section-title
  (lambda (txt)
    `((h1 (@ ,(classes '(w-full my-2 text-5xl font-bold leading-tight text-center
				text-gray-800)))
	  ,txt)
      ,section-underline)))


(define title
  (lambda (txt)
    `(h3 (@ (class "text-3xl text-gray-800 font-bold leading-none mb-3"))
	 ,txt)))



(define promise-section
  `(section (@ (class "bg-white border-b py-8"))
	    (div (@ (class "container max-w-5xl mx-auto m-8"))
		 
		 ,@(section-title "Our Promise")
		 
		 
		 (div (@ (class "flex flex-wrap items-center"))
		      
		      (div (@ (class "w-5/6 sm:w-1/2 p-6 container mx-auto"))
			   ,(title "Express your brand")
			   (p (@ (class "text-gray-600 mb-8"))
			      "Your brand should be on the finest of product."))

		      (div (@ (class "w-full sm:w-1/2 p-6"))
			   (img (@ (class "w-full sm:h-64 mx-auto")
				   (src "pictures/branding.svg")))))

		 (div (@ (class "flex flex-wrap flex-col-reverse sm:flex-row"))

		      (div (@ (class "w-full sm:w-1/2 p-6 mt-6 container mx-auto"))
			   (img (@ (class "w-full sm:h-64 mx-auto")
				   (src "pictures/materials.svg"))))
		      
		      (div (@ (class "w-full sm:w-1/2 p-6 mt-6"))
			   (div (@ (class "align-middle"))
				,(title "High quality materials"))
			   (p (@ (class "text-gray-600 mb-8"))
			      "Your brand should be on the finest of product."))))))

;;;;;;;;;;;
;; cards ;;
;;;;;;;;;;;

(define-record-type card (fields name description image))

(define (cards->sxml cards)
  (define (card->sxml card)
    `(a (@ ,(classes '(w-full md:w-1/3 p-6 flex flex-col flex-grow flex-shrink))
	   (href ,(string-append "products/" (string-downcase (card-name card)) ".html")))
	(div (@ (class "max-w-sm rounded overflow-hidden shadow-lg"))
	     (img (@ (class "w-full")
		     (src ,(card-image card))
		     (alt ,(card-name card))))
	     (div (@ (class "px-6 py-4"))
		  (div (@ (class "font-bold text-xl text-black mb-2"))
		       ,(card-name card))
		  (p (@ (class "text-gray-700 text-base"))
		     ,(card-description card))))))
  
  `(div (@ (class "flex-wrap flex max-w-6xl mx-auto"))
	,@(map card->sxml cards)))

;;;;;;;;;;;;;;
;; products ;;
;;;;;;;;;;;;;;

(define product-list (list (make-card "Hoodies"
				      "Comfortable pullovers for your winters"
				      "/pictures/hoodie.webp")
			   
			   (make-card "Round neck t-shirts"
				      "Comfortable cotton t-shirts"
				      "/pictures/tshirt.webp")

			   (make-card "Caps"
				      "Caps for summers"
				      "/pictures/caps.webp")

			   (make-card "Flex Banner"
				      "Banners for powerful messages"
				      "/pictures/flex-banner.webp")

			   (make-card "Posters"
				      "Posters for your room"
				      "/pictures/poster.webp")

			   (make-card "Visiting cards"
				      "Visiting cards for business"
				      "/pictures/card.webp")))

(define product-grid
  (lambda (only-popular?)
    `(section (@ (class "bg-white border-p py-12"))

	      (div (@ (class "container mx-auto flex flex-wrap pt-4 pb-12 py-8"))

		   ,@(if only-popular?
			 (section-title "Popular Products")
			 (list))

		   ,(cards->sxml (if only-popular?
				     (take product-list 3)
				     product-list))

		   (img (@ (class "wave-top")
			   (src "pictures/wave-top.svg")))))))


;;;;;;;;;;;;;;;;;;;;;
;; product details ;;
;;;;;;;;;;;;;;;;;;;;;

(define svg-circle
  (lambda (attrs cx cy r)
    `(svg ,attrs
	  (circle (@ (cx ,(number->string cx))
		     (cy ,(number->string cy))
		     (r ,(number->string r)))))))

(define (color-circles . colors)
  (map (lambda (c)
	 (let ((color (string->symbol (string-append "text-" c "-500"))))
	   (svg-circle `(@ ,(classes `(,color fill-current inline-block h-12 w-12))
			   (id ,c))
		       30 30 12)))
       colors))

(define product-detail
  (lambda (product)
    (define size-selector-style '(bg-transparent bg-blue-500 text-white font-semibold
						 hover:text-black py-2 px-4 border border-blue-500
						 hover:border-transparent rounded))


    (define order-button-style '(bg-blue-500 hover:bg-blue-400 text-white font-bold py-2 px-4
					     border-b-4 border-blue-700 hover:border-blue-500
					     rounded))


    (define colors (quote ((active . (text-black hover:text-white))
			   (inactive . (text-white hover:text-black)))))

    (define-syntax form-input
      (syntax-rules ()
	((_ label input-type)
	 (input (input-options ((input-name label)
				(type input-type)
				(container-class "flex-initial flex-row text-center py-2")
				(label-class "flex ")
				(input-class "flex w-full h-12 appearance-none leading-tight")))))))    

    (define (size-selector . sizes)
      (map (lambda (s)
	     (let ((id (string-append "size-"  (string-downcase s))))
	       `(div (@ (class "w-1/3 p-1 text-center"))
		     (div (@ ,(classes size-selector-style)
			     (id ,id)) ,s))))
	   sizes))

    (define label
      (lambda (txt)
	`(div (@ (class "text-xl text-black py-3 px-2 bold")) ,txt)))
    `(section (@ (class "bg-white border-b w-screen py-4 align-middle sm:container sm:mx-auto"))
	      (div (@ (class "flex flex-row flex-wrap container mx-auto py-8 "))

		   (img (@ (class "flex-1 bg-fixed py-8 md:mb-48 ")
			   (src ,(card-image product))))
		   
		   (div (@ (class "flex-1 flex-col w-screen shadow-2xl bg-white px-4 py-8 z-3"))
			
			
			(div (@ (class "font-extrabold text-4xl text-black")
				(id "product"))
			     ,(string-append "Custom " (card-name product)))

			(div (@ (class "flex flex-row sm:mr-3"))

			     ,(label "Choose color: ")
			     
			     ,@(color-circles "purple" "black" "blue" "red" "green")
			     ;; ,(svg-circle '(@ (class "fill-current text-purple-500 inline-block h-20 w-16")
			     ;; 		      (stroke-width "3")
			     ;; 		      (stroke "red"))
			     ;; 		  30 30 18)
			     )
			
			(div (@ (class "flex flex-row w-auto"))

			     ,(label "Choose size:")
			     
			     (div (@ (class "flex flex-row "))
				  ,(size-selector "Small" "Medium" "Large")))

			;; (div (@ (class "flex flex-col"))
			;;      ,(label "Upload design:")
			;;      (div (@ (class "overflow-hidden relative text-center"))
			;; 	  (button (@ ,(classes (quote (py-2 px-4 w-full inline-flex items-center
			;; 					    bg-gray-200 text-gray-700 border
			;; 					    border-gray-200 rounded
			;; 					    hover:border-gray-500 h-32
			;; 					    text-center))))
			;; 		  (span
			;; 		   (@ (class "container mx-auto w-full"))
			;; 		   "Browse Files")
			;; 		  (input (@ (class "cursor-pointer absolute block opacity-0 pin-r pin-t")
			;; 			    (type "file"))))))

			(div (@ (class "flex flex-col"))
			     ,(label "Description:")
			     (div (@ (class "text-lg text-black px-2"))
				  "Handcrafted quality, best of class materials. Presenting you the 
very best of merchandise in India."))
			
			(div (@ (class "text-lg text-black px-2 py-4"))
			     "Enter your contact details below to order samples.")
			(div (@ (class "w-full md:px-8 text-center container mx-auto py-2"))
			     ,(form-input "Name" "text")
			     ,(form-input "Phone" "tel")
			     ,(form-input "Pincode" "text"))


			(div (@ (class "container mx-auto"))
			     (div (@ (class "bg-teal-100 border-t-4 border-teal-500 rounded-b text-teal-900 px-4 py-3 shadow-md hidden")
				     (role "alert")
				     (id "success-message"))
				  (div (@ (class "flex"))
				       (div (@ (class "py-1"))
					    (svg (@ (class "fill-current h-6 w-6 text-teal-500 mr-4")
						    (xmlns "http://www.w3.org/2000/svg")
						    (viewBox "0 0 20 20"))
						 (path (@ (d "M2.93 17.07A10 10 0 1 1 17.07 2.93 10 10 0 0 1 2.93 17.07zm12.73-1.41A8 8 0 1 0 4.34 4.34a8 8 0 0 0 11.32 11.32zM9 11V9h2v6H9v-4zm0-6h2v2H9V5z")))))
				       (div (p (@ (class "font-bold"))
					       "Your query has been recorded.")
					    (p (@ (class "text-sm"))
					       "We will get back to you in 12 hours"))))

			     (div (@ (role "alert")
				     (class "hidden")
				     (id "error-message"))
				  (div (@ (class "bg-red-500 text-white font-bold rounded-t px-4 py-2"))
				       "Error")
				  (div (@ (class "border border-t-0 border-red-400 rounded-b bg-red-100 px-4 py-3 text-red-700")
					  (id "error-text"))
				       ,(string-append
					 "Could not record your query. Please call us @"
					 *threadstory-phone*))))
			

			(div (@ (class "flex flex-col px-2 py-8")
				(id "send-enquiry"))
			     (button (@ ,(classes order-button-style))
				     "Order samples"))
			)))))


;;;;;;;;;;;;;;;;;;;;;
;; contact us page ;;
;;;;;;;;;;;;;;;;;;;;;

(define contact
  `(section (@ (class "container mx-auto text-center py-6 mb-12"))

	    (h1 (@ ,(classes (quote (w-full my-2 text-5xl font-bold leading-tight
					    text-center text-white))))
		"Contact Today")

	    (div (@ (class "w-full mb-4"))
		 (div (@ (class "h-1 mx-auto bg-white w-1/6 opacity-25 my-0 py-0 rounded-t"))))

	    (h3 (@ (class "my-4 text-3xl leading-tight"))
		"get the early bird discount!")

	    (a (@ (href "/contact-us.html"))
	       (button (@ (class "mx-auto lg:mx-0 hover:underline bg-white text-gray-800 font-bold rounded-full my-6 py-4 px-8 shadow-lg"))
		       "Contact us!"))))


;;;;;;;;;;;;;
;; footers ;;
;;;;;;;;;;;;;

(define-record-type footer-item (fields icon label href))
(define-record-type footer-list (fields title items))



(define footer-items
  (list
   (make-footer-list "Contact Info"
		     (list (make-footer-item "/pictures/mail.svg"
					     *threadstory-email*
					     (string-append "mailto:" *threadstory-email*))
			   (make-footer-item " /pictures/phone.svg"
					     *threadstory-phone*
					     (string-append "tel:" *threadstory-phone*)) 
			   (make-footer-item "/pictures/whatsapp.svg"
					     *threadstory-phone*
					     (string-append "https://api.whatsapp.com/send?phone=" *threadstory-phone*))))
   (make-footer-list "Legal" '(terms privacy))
   (make-footer-list "Social" '((facebook . "https://www.facebook.com/threadstoryin/")
				(linkedin . "https://www.linkedin.com/company/threadstory")
				(twitter . "https://twitter.com/threadstoryin")))
   ;; blog about 
   (make-footer-list "Company" '((contact . "/contact-us.html")))))

(define footer-list->sxml
  (lambda (footer-list)
    ;; any formatting to be done on symbols before they are set as footer items
    ;; string items are taken as it is and is not subject to this function
    (define symbol->footer-heading
      (lambda (sym)
	(string-titlecase (symbol->string sym))))
    
    `(div (@ (class "flex-1"))
	  (p (@ (class "uppercase text-white md:mb-6"))
	     ,(footer-list-title footer-list)
	     (ul (@ (class "list-reset mb-6 text-black"))
		 ,@(map (lambda (i)
			  (let ((icon (if (footer-item? i)
					  `((img (@ (src ,(footer-item-icon i))
						    (classes "fill-black"))))
					  (list)))
				(label (let ((label (cond
						     ((footer-item? i) (footer-item-label i))
						     ((pair? i) (car i))
						     (else i))))
					 (if (symbol? label)
					     (symbol->footer-heading label)
					     label)))
				(href (cond
				       ((footer-item? i) (footer-item-href i))
				       ;; if item pair then car is label cdr is link
				       ((pair? i) (cdr i))
				       (else (string-append "/"
							    (if (symbol? i) (symbol->string i) i)
							    ".html")))))
			    `(li (@ (class "mt-2 inline-block mr-2 md:block md:mr-0"))
				 (div (@ (class "flex flex-column"))
				      ,@icon
				      (a (@ (href ,href)
					    ,(classes '(no-underline hover:underline text-white px-2)))
					 ,label)))))
			(footer-list-items footer-list)))))))

(define footer
  `(footer 
    (div (@ (class "container mx-auto px-8 text-white"))

	 (div (@ (class "w-full flex flex-col md:flex-row py-6"))

	      (div (@ (class "flex-1 mb-6"))
		   
		   (a (@ ,(classes '(text-white no-underline font-bold text-2xl
						lg:text-4xl))
			 (href "#"))
		      (img (@ (class "h-6 fill-current inline mr-2")
			      (src "/pictures/ts-white.svg")))
		      "Threadstory"))

	      ,@(map footer-list->sxml footer-items)))))


;;;;;;;;;;;;;;;;;
;; macro utils ;;
;;;;;;;;;;;;;;;;;

(meta define construct-name
      (lambda (template-identifier . args)
	(datum->syntax template-identifier
		       (string->symbol
			(apply string-append (map (lambda (x)
						    (if (string? x)
							x
							(symbol->string (syntax->datum x))))
						  args))))))

(meta define construct-string
      (lambda (template-identifier . args)
	(datum->syntax template-identifier
		       (apply string-append (map (lambda (x)
						   (if (string? x)
						       x
						       (symbol->string (syntax->datum x))))
						 args)))))

(define-syntax define-page
  (lambda (stx)
    (syntax-case stx ()
      ((_ page-name content)
       (with-syntax ((gen-lambda (construct-name #'page-name "generate-" #'page-name "-page"))
		     (dist-page (construct-string #'page-name "/" #'page-name ".html")))
	 #'(begin (define gen-lambda
		    (lambda ()
		      (with-output-to-file (string-append *dist-directory* dist-page)
			(lambda ()
			  (SXML->HTML (html-template content)))
			'replace)))
		  (gen-lambda)))))))


;;;;;;;;;;;;;;;;
;; Site macro ;;
;;;;;;;;;;;;;;;;

;; macro with site template

(define-syntax define-threadstory-page
  (syntax-rules ()
    ((_ page-name content active-page background-white?)
     (define-page page-name
       `(body (@ (class "leading-normal tracking-normal text-white gradient")
		 (style "font-family: 'Source Sans Pro', sans-serif;"))
	      
	      ,(navbar active-page '((home . "/") products) background-white?)

	      ,@content

	      ,footer

	      (script (@ (type "application/javascript")
			 (src "/scripts/app.js")) " "))))))


(define-threadstory-page index (list hero
				     wavy-svg
				     promise-section
				     (product-grid #t)
				     ;; contact
				     ) 'home #f)


(define-threadstory-page products `(,(product-grid #f)
				    (script "window.bgWhite = true")) 'products #t)

(trace-define-syntax define-pages
  (lambda (stx)
    (syntax-case stx ()
      ((_ prefix)
       (with-syntax ((((name . (product-prop ...)) ...)
		      (map (lambda (p)
			     (cons (construct-name #'prefix
						   #'prefix
						   "/"
						   (string->symbol (string-downcase (card-name p))))
				   (map (lambda (x)
					  (datum->syntax #'prefix x))
					(list (card-name p)
					      (card-description p)
					      (card-image p)))))
			   product-list)))
	 #'(begin (define-threadstory-page name
		    `(,(product-detail (make-card product-prop ...)) 
		      (script "window.bgWhite = true")
		      (script (@ (src "/scripts/order.js")
				 (type "application/javascript"))
			      "")) 'products #t) ...))))))

(define-pages products)

(define-threadstory-page about
  `((section (@ (class "bg-white border-b py-12"))
	     (div (@ (class "container max-w-5xl mx-auto m-8 py-8"))

		  ,@(section-title "About Us")

		  (div (@ (class "w-full text-center text-black py-8"))
		       "Group of people highly comitted to bring quality craftmanship 
easier to access.")))
    (script "window.bgWhite = true")) 'about #t)


(define-threadstory-page contact-us
  `((section (@ (class "bg-white border-b py-12"))
	     (div (@ (class "container max-w-5xl mx-auto m-8 py-8"))

		  ,@(section-title "Contact us")

		  (div (@ (class "text-center py-8 px-2"))
		       (p (@ (class "text-black"))
			  "Call us at +91-7004282702 or fill this form and we will get back to you."))

		  (form (@ (class "w-full max-w-sm container mx-auto px-8 text-center"))

			,(text-input "Name")

			,(input (input-options ((input-name "phone")
						(type  "tel"))))

			,(input (input-options ((input-name "email")
						(type "email"))))

			,(select-box "product" (map card-name product-list))

			,(input (input-options ((input-name "quantity")
						(type "number"))))

			(div (@ (class "flex items-center mb-6"))
			     (div (@ (class "w-1/3")) "")
			     (div (@ (class "w-2/3"))
				  (button (@ (class "bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-4 border border-blue-500 hover:border-transparent rounded")
					     (id "send-enquiry"))
					  "Send Enquiry"))))


		  (div (@ (class "container mx-auto"))
		       (div (@ (class "bg-teal-100 border-t-4 border-teal-500 rounded-b text-teal-900 px-4 py-3 shadow-md hidden")
			       (role "alert")
			       (id "success-message"))
			    (div (@ (class "flex"))
				 (div (@ (class "py-1"))
				      (svg (@ (class "fill-current h-6 w-6 text-teal-500 mr-4")
					      (xmlns "http://www.w3.org/2000/svg")
					      (viewBox "0 0 20 20"))
					   (path (@ (d "M2.93 17.07A10 10 0 1 1 17.07 2.93 10 10 0 0 1 2.93 17.07zm12.73-1.41A8 8 0 1 0 4.34 4.34a8 8 0 0 0 11.32 11.32zM9 11V9h2v6H9v-4zm0-6h2v2H9V5z")))))
				 (div (p (@ (class "font-bold"))
					 "Your query has been recorded.")
				      (p (@ (class "text-sm"))
					 "We will get back to you in 12 hours"))))

		       (div (@ (role "alert")
			       (class "hidden")
			       (id "error-message"))
			    (div (@ (class "bg-red-500 text-white font-bold rounded-t px-4 py-2"))
				 "Error")
			    (div (@ (class "border border-t-0 border-red-400 rounded-b bg-red-100 px-4 py-3 text-red-700")
				    (id "error-text"))
				 ,(string-append
				   "Could not record your query. Please call us @"
				   *threadstory-phone*))))))

    (script "window.bgWhite = true")
    (script (@ (type "application/javascript")
	       (src "/scripts/contact-us.js")) ""))

  'contact-us

  #t)
#!eof


(define build (fork-thread
	       (lambda ()
		 (let lp ((run #t))
		   (load "web.scm")
		   (load "build.scm")
		   (generate-site)

		   (with-output-to-file "build.log"
		     (lambda ()
		       (display "build done")
		       (display (current-date))
		       (newline))
		     'append)
		   
		   (lp (sleep (make-time 'time-duration 10 3)))))))

;; read contacts
;;

(with-input-from-file "server/contacts.scm"
  (lambda ()
    (let lp ((ch (read))
	     (xs (list)))
      (cond
       ((eof-object? ch) (reverse xs))
       (else (lp (read) (cons ch xs)))))))

