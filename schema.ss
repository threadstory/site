
;;;;;;;;;;;;;;;;;;;;;
;; Database schema ;;
;;;;;;;;;;;;;;;;;;;;;



;; user management

(define-model address
  (id . auto)
  (pincode . (int 6))
  (state . string)
  (city . string)
  (address . text))

(define-model user
  (id . auto)
  (email . email)
  (password . string)
  (name . string)
  (photo . picture-url)
  (phone-number . phone))

;; Product listing

(define-model product-category
  (id . auto)
  (photo . picture-url)
  (name . string))

(define-model product
  (id . auto)
  (category . (product-category id))
  (photo . picture-url))


(define-model product-variant
  (id . auto)
  (product . (product id))
  ;; label could be sizes / or sth else
  (label . string)
  (photo . picture-url))


;; order management

(define-model order-tracking
  (id . auto)
  (status . string))

(define-model user-order
  (id . auto)
  (user . (user id))
  (quantity . int)
  (product . ((product-variant id)))
  (time . timestamp)
  (status . (order-tracking id))
  (designs . (list picture-url))
  (payment-status . bool)
  (payment-amount . currency))
