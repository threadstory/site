(include "web.scm")


(define generate-site-pages
  (lambda ()
    (generate-index-page)
    (generate-products-page)
    (generate-about-page)
    (generate-contact-us-page)))


(define copy-binary-file
  (lambda (src dest)
    (printf "copying file from: ~s to: ~s \n" src dest)
    (let ((ip (open-file-input-port src))
	  (op (open-file-output-port dest (file-options no-fail))))
      
      (let lp ((ch (get-u8 ip)))
	(cond
	 ((eof-object? ch) #t)
	 (else (begin (put-u8 op ch)
		      (lp (get-u8 ip))))))
      
      (flush-output-port op)
      (close-output-port op)
      (close-input-port ip))))

;; (copy-binary-file "pictures/hero.png" "dist/pictures/hero.png" )


(define (construct-paths prefix . segments)
  (string-append prefix
		 (string-join segments (string (directory-separator)) 'prefix)))


(define (copy-folder folder-name)
  (let ((dest-folder (construct-paths *dist-directory* folder-name))
	(src-folder folder-name))
    (ensure-directory dest-folder)
    (map (lambda (filename)
	   (copy-binary-file (construct-paths src-folder filename)
			     (construct-paths dest-folder filename)))
	 (directory-list src-folder))))


(define (purge-css)
  (displayln "purgin css")
  (system (string-append "postcss -o dist/base.css css/base.css")))


(define (compile-javascript . files)
  (let ((dest-folder (construct-paths *dist-directory* "scripts")))
    (ensure-directory dest-folder)
    (map (lambda (file)
	   (let ((dest-js (construct-paths dest-folder
					   (string-drop-right file 4)))) 
	     (printf "compiling ~s to ~s \n" file dest-js)
	     (system (string-append "chicken-spock -runtime " file " -o " dest-js))))
	 files)))


(define (compress-js . js-files)
  (displayln "compressing js")
  (for-each (lambda (file)
	      (system (string-append "uglifyjs dist/scripts/" file " -o dist/scripts/" file)))
	    js-files))

(define (compress-pictures)
  (displayln "compressing pictures")
  (system "imagemin --plugin.webp.quality=50 pictures/* --out-dir=dist/pictures/")
  (system "~/apps/pingo -strip dist/pictures/"))

(define (package-html)
  (displayln "packaging static site")
  (system "tar -cvf server.tar.gz dist/"))

(define (generate-site)
  (ensure-directory *dist-directory*)
  (ensure-directory (string-append *dist-directory* "/products"))
  (generate-site-pages)
  (copy-folder "pictures")
  (copy-binary-file "contact-us.js" "dist/scripts/contact-us.js")
  (copy-binary-file "order.js" "dist/scripts/order.js")
  (compile-javascript "app.js.scm" )
  (when (prod-environment?)
    (begin (purge-css)
	   ;; (compress-js "app.js" "contact-us.js"  "order.js")
	   (compress-pictures)
	   (package-html))))

#!eof

(load "web.scm")
(load "build.scm")
(generate-site)

