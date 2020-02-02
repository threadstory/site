(library-directories '("./thunderchez" "."))

(import (sxml to-html))

(define +tailwind-css-url+ "https://unpkg.com/tailwindcss/dist/tailwind.min.css")
(define +font-url+ "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700")

(define *dist-directory* "dist")

(define (ensure-dist-directory)
  (let ((full-dist-path (string-append (current-directory)
				       (string (directory-separator))
				       *dist-directory*)))
    (when (not (file-directory? full-dist-path))
      (mkdir full-dist-path))))

(ensure-dist-directory)


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

    (define (make-nav-item item-name active?)
      (let* ((color (if active? 'text-black 'text-gray-100))
	     (css-classes `(inline-block py-2 px-4 ,color
					 no-underline hover:text-black
					 hover:text-underline)))
	`(li (@ (class "mr-3"))
	     (a (@ ,(classes css-classes)
		   (href "#"))
		,(string-titlecase (symbol->string item-name))))))

    (define (navbar-items)
      (map (lambda (i)
	     (make-nav-item i (equal? i active-item)))  items))
    
    `(nav (@ (id "header")
	     (class "fixed w-full z-30 top-0 text-white"))
	  
	  (div (@ ,(classes '(w-full container mx-auto flex flex-wrap items-center
				     justify-between mt-0 py-2)))
	       (div (@ (class "pl-4 flex items-center"))
		    (a (@ ,(classes '(toggleColour text-white no-underline
						   hover:no-underline font-bold text-2xl
						   lg:text-4xl))
			  (href "#"))
		       (img (@ (class "h-8 fill-current inline")
			       (src "https://threadstory.in/image/ts-logo.svg")))
		       "Threadstory"))

	       ;; hamburger
	       (div (@ (class "block lg:hidden pr-4"))
		    (button (@ (id "nav-toggle")
			       ,(classes '(flex items-center p-1 text-orange-800
						hover:text-gray-900)))
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

			(button (@ (id "navAction")
				   ,(classes '(mx-auto lg:mx-0 hover:underline bg-white
						       text-gray-800 font-bold rounded-full
						       mt-4 lg:mt-0 py-4 px-8 shadow
						       opacity-75)))
				"Contact us")))))))

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

(define generate-index-page
  (lambda ()
    (with-output-to-file "dist/index.html"
      (lambda ()
	(SXML->HTML
	 (html-template `(body (@ (class "leading-normal tracking-normal text-white gradient")
				  (style "font-family: 'Source Sans Pro', sans-serif;"))
			       
			       ,(navbar 'home '(home products about))

			       ,hero))))
      'replace)))




;; (load "web.scm")
;; (generate-index-page)
