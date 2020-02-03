(library-directories '("./thunderchez" "."))

(import (sxml to-html)
	
	(srfi s1 lists)
	(srfi s13 strings))

(define +tailwind-css-url+ "https://unpkg.com/tailwindcss/dist/tailwind.min.css")
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
			(href ,+tailwind-css-url+)))
	       
	       (line (@ (rel "stylesheet")
			(href ,+font-url+)))
	       
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


(define navbar
  (lambda (active-item items)

    (define (make-nav-item item-info active?)
      (let* ((item-name (if (pair? item-info) (car item-info) item-info))
	     (item-href (if (pair? item-info)
			    (cdr item-info)
			    (string-append "/" (symbol->string item-name) ".html")))
	     (color (if active?
			(list 'text-black)
			(list 'text-white 'toggleColour)))
	     (css-classes `(inline-block py-2 px-4 ,@color
					 no-underline hover:text-black
					 hover:text-underline)))
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
		       (img (@ (class "h-8 fill-current inline")
			       (src "https://threadstory.in/image/ts-logo.svg")))
		       "Threadstory"))

	       ;; hamburger
	       (div (@ (class "block lg:hidden pr-4 text-black")
		       (id "nav-toggle"))
		    (button (@ ,(classes '(flex items-center p-1)))
			    (svg (@ (class "fill-current h-6 w-6")
				    (viewBox "0 0 20 20")
				    (xmlns "http://www.w3.org/2000/svg"))
				 (title "Menu")
				 (path (@ (d "M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z"))))))

	       (div (@ ,(classes '(w-full flex-grow lg:flex lg:items-center lg:w-auto
					  hidden lg:block mt-2 lg:mt-0 bg-blue-400
					  lg:bg-transparent text-black p-4 lg:p-0 z-20))
		       (id "nav-content"))
		    (ul (@ ,(classes '(list-reset lg:flex justify-end flex-1
						  items-center)))

			,(navbar-items)

			(button (@ (id "navAction")
				   ,(classes '(mx-auto lg:mx-0 hover:underline bg-white
						       text-gray-800 font-bold rounded-full
						       mt-4 lg:mt-0 py-4 px-8 shadow
						       opacity-75)))
				"Contact us")))))))


;; (navbar 'home '((home . "/") products about))

(define hero
  `(div (@ (class "pt-24"))
	(div (@ ,(classes '(container px-3 mx-auto flex flex-wrap flex-col md:flex-row
				      items-center)))

	     ;; left column
	     (div (@ ,(classes '(flex flex-col w-full md:w-2/5 justify-center items-start
				      text-center md:text-left)))
		  (p (@ (class "uppercase tracking-loose w-full"))
		     "Custom Merchandise made easy")
		  (h1 (@ (class "my-4 text-5xl font-bold leading-tight"))
		      "One stop shop for all your branding needs!")
		  (p (@ (class "leading-normal text-2xl mb-8"))
		     "We work hard to get you the best quality available in the marker.")

		  (a (@ (href "/products.html"))
		     (button (@ ,(classes '(mx-auto lg:mx-0 hover:underline bg-white
						    text-gray-800 font-bold rounded-full my-6
						    py-4 px-8 shadow-lg)))
			     "View Products")))

	     ;; right column
	     (div (@ (class "w-full md:w-3/5 py-6 text-center"))
		  (img (@ (class "w-full md:w-4/5 z-50")
			  (src "pictures/hero.png")))))))

(define wavy-svg
  `(div (@ (class "relative -mt-12 lg:-mt-24"))
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
		 
		 
		 (div (@ (class "flex flex-wrap"))
		      
		      (div (@ (class "w-5/6 sm:w-1/2 p-6"))
			   ,(title "Express your brand")
			   (p (@ (class "text-gray-600 mb-8"))
			      "Your brand should be on the finest of product."))

		      (div (@ (class "w-full sm:w-1/2 p-6"))
			   (img (@ (class "w-full sm:h-64 mx-auto")
				   (src "pictures/brand.webp")))))

		 (div (@ (class "flex flex-wrap flex-col-reverse sm:flex-row"))

		      (div (@ (class "w-full sm:w-1/2 p-6 mt-6"))
			   (img (@ (class "w-full sm:h-64 mx-auto")
				   (src "pictures/brand.webp"))))
		      
		      (div (@ (class "w-full sm:w-1/2 p-6 mt-6"))
			   (div (@ (class "align-middle"))
				,(title "High quality materials"))
			   (p (@ (class "text-gray-600 mb-8"))
			      "Your brand should be on the finest of product."))))))

(define-record-type card (fields name description image))


(define (cards->sxml cards)
  (define (card->sxml card)
    `(div (@ ,(classes '(w-full md:w-1/3 p-6 flex flex-col flex-grow flex-shrink)))
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



(define products (list (make-card "Hoodies"
				  "Comfortable pullovers for your winters"
				  "pictures/hoodie.webp")
		       
		       (make-card "Round neck t-shirts"
				  "Comfortable cotton t-shirts"
				  "pictures/tshirt.webp")

		       (make-card "Caps"
				  "Caps for summers"
				  "pictures/caps.webp")

		       (make-card "Flex Banner"
				  "Banners for powerful messages"
				  "pictures/flex-banner.webp")

		       (make-card "Posters"
				  "Posters for your room"
				  "pictures/poster.webp")

		       (make-card "Visiting cards"
				  "Visiting cards for business"
				  "pictures/card.webp")))

(define product-grid
  (lambda (only-popular?)
    `(section (@ (class "bg-white border-p py-8"))

	      (div (@ (class "container mx-auto flex flex-wrap pt-4 pb-12"))

		   ,@(if only-popular?
			 (section-title "Popular Products")
			 (list))

		   ,(cards->sxml (if only-popular?
				     (take products 3)
				     products))

		   (img (@ (class "wave-top")
			   (src "pictures/wave-top.svg")))))))


(define contact
  `(section (@ (class "container mx-auto text-center py-6 mb-12"))

	    (h1 (@ ,(classes (quote (w-full my-2 text-5xl font-bold leading-tight
					    text-center text-white))))
		"Contact Today")

	    (div (@ (class "w-full mb-4"))
		 (div (@ (class "h-1 mx-auto bg-white w-1/6 opacity-25 my-0 py-0 rounded-t"))))

	    (h3 (@ (class "my-4 text-3xl leading-tight"))
		"get the early bird discount!")

	    (button (@ (class "mx-auto lg:mx-0 hover:underline bg-white text-gray-800 font-bold rounded-full my-6 py-4 px-8 shadow-lg"))
		    "Contact us!")))

(define-record-type footer-list (fields title items))

(define footer-items (list (make-footer-list "Links" '(faq help support))
			   (make-footer-list "Legal" '(terms privacy))
			   (make-footer-list "Social" '(facebook linkedin twitter))
			   (make-footer-list "Company" '(blog about contact))))

(define footer-list->sxml
  (lambda (footer-list)
    `(div (@ (class "flex-1"))
	  (p (@ (class "uppercase text-gray-500 md:mb-6"))
	     ,(footer-list-title footer-list)
	     (ul (@ (class "list-reset mb-6"))
		 ,@(map (lambda (i)
			  `(li (@ (class "mt-2 inline-block mr-2 md:block md:mr-0"))
			       (a (@ (href "#")
				     ,(classes '(no-underline hover:underline text-gray-800
							      hover:text-orange-500)))
				  ,(string-titlecase (symbol->string i)))))
			(footer-list-items footer-list)))))))

(define footer
  `(footer (@ (class "bg-white"))
	   (div (@ (class "container mx-auto px-8"))

		(div (@ (class "w-full flex flex-col md:flex-row py-6"))

		     (div (@ (class "flex-1 mb-6"))

			  (a (@ ,(classes '(text-black no-underline font-bold text-2xl
						       lg:text-4xl))
				(href "#"))
			     (img (@ (class "h-8 fill-current inline")
				     (src "https://threadstory.in/image/ts-logo.svg")))
			     "Threadstory"))

		     ,@(map footer-list->sxml footer-items)))))


(define generate-index-page
  (lambda ()
    (with-output-to-file (string-append *dist-directory* "/index.html")
      (lambda ()
	(SXML->HTML
	 (html-template `(body (@ (class "leading-normal tracking-normal text-white gradient")
				  (style "font-family: 'Source Sans Pro', sans-serif;"))
			       
			       ,(navbar 'home '((home . "/") products about))

			       ,hero

			       ,wavy-svg

			       ,promise-section

			       ,(product-grid #t)

			       ,contact

			       ,footer

			       (script (@ (type "application/javascript")
					  (src "scripts/scm.js")))))))
      'replace)))


(define generate-products-page
  (lambda ()
    (with-output-to-file (string-append *dist-directory* "/products.html")
      (lambda ()
	(SXML->HTML
	 (html-template `(body (@ (class "leading-normal tracking-normal text-white gradient")
				  (style "font-family: 'Source Sans Pro', sans-serif;"))
			       
			       ,(navbar 'products '((home . "/") products about))


			       ,(product-grid #f)

			       ,footer

			       (script (@ (type "application/javascript")
					  (src "scripts/scm.js")))))))
      'replace)))


#!eof

(load "web.scm")

(begin )
