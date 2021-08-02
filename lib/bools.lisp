(load lib/basic-macros)
(load lib/lists)

(define-macro* (and-2 a b)
  '(cond (,a ,b) ('true 'false)))

(define (and* values)
  (foldr values ''true
    (lambda (x xs) '(and-2 ,x ,xs))))

(define-macro* (and & values)
  (and* values))

(define true 'true)
(define false 'false)

(define (not x)
  (if (same-atom? x true)
    false
    true))