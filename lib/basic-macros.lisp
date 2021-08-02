(define-macro* (if pred then else)
 '(cond (,pred ,then)
        ('true ,else)))

(define* basic-macros/cadr
  (lambda (x) (car (cdr x))))

(define* basic-macros/map
  (label map
    (lambda (f xs)
      (if (nil? xs)
        xs
        (cons (f (car xs)) (map f (cdr xs)))))))

(define-macro* (let binds & body)
  (cons
    '(lambda ,(basic-macros/map car binds) ,(cons 'do body))
    (basic-macros/map basic-macros/cadr binds)))

(define-macro* (define sign & body)
  (let ((body (cons 'do body)))
    (if (atom? sign)
     '(define* ,sign ,body)
      (let ((name (car sign))
            (args (cdr sign)))
       '(define* ,name (label ,name (lambda ,args ,body)))))))
