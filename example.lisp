(define-macro* (if c then else)
  '(cond (,c ,then)
         ('true ,else)))

(define* cadr
  (lambda (x) (car (cdr x))))

(define* map
  (label map
    (lambda (f xs)
      (if (atom? xs)
        xs
        (cons (f (car xs)) (map f (cdr xs)))))))

(define-macro* (let binds & body)
  (cons
    '(lambda ,(map car binds) ,(cons 'do body))
    (map cadr binds)))

(let ((a 'hello)
      (b 'world))
  (print a)
  (print b)
  (print (cons a b)))

(define-macro* (define sign & body)
  (if (atom? sign)
   '(define* ,sign ,(cons 'do body))
    (let ((name (car sign))
          (args (cdr sign)))
      (print 'HERE)
     '(define* ,name
        (label ,name
          (lambda ,(cdr sign)
           ,(cons 'do body)))))))


# (define zero 'zero)
# (define (succ x) (cons 'succ ,x))
# (print succ)


# (define-macro* (q a b & rest)
#   ''((unquote a) (unquote b) (unquote rest)))
# 
# (print (q hello world my long list))