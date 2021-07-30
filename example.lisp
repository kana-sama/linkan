(define-macro* (if pred then else)
 '(cond (,pred ,then)
        ('true ,else)))

(define* .
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define* map
  (label map
    (lambda (f xs)
      (if (nil? xs)
        xs
        (cons (f (car xs)) (map f (cdr xs)))))))

(define-macro* (let binds & body)
  (cons
   '(lambda ,(map car binds) ,(cons 'do body))
    (map (. car cdr) binds)))

(define-macro* (define sign & body)
  (let ((body (cons 'do body)))
    (if (atom? sign)
     '(define* ,sign ,body)
      (let ((name (car sign))
            (args (cdr sign)))
       '(define* ,name
          (label ,name
            (lambda ,(cdr sign) ,body)))))))

(define zero '(zero))
(define (succ x) (cons 'succ x))

(define (length xs)
  (if (nil? xs)
    zero
    (succ (length (cdr xs)))))

(let ((xs '(hello world ,(atom? 'my) long list)))
  (print xs)
  (print (length xs)))