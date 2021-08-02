(load lib/basic-macros)

(define (map xs f)
  (if (nil? xs)
    xs
    (cons (f (car xs)) (map (cdr xs) f))))

(define (foldr xs on-nil on-cons)
  (if (nil? xs)
    on-nil
    (on-cons (car xs) (foldr (cdr xs) on-nil on-cons))))

(define (length xs)
  (if (nil? xs)
    0
    (+ 1 (length (cdr xs)))))

(define nil '())
(define (list & vals) vals)

(define (nth n list)
  (if (same-number? 0 n)
    (car list)
    (nth (+ n -1) (cdr list))))

(define (concat-2 xs ys)
  (if (nil? xs)
    ys
    (cons (car xs) (concat-2 (cdr xs) ys))))

(define (concat & lists)
  (foldr lists nil concat-2))

(define (lists/indexed-go i xs)
  (if (nil? xs)
    nil
    (cons '(,i ,(car xs)) (lists/indexed-go (+ 1 i) (cdr xs)))))

(define (indexed xs)
  (lists/indexed-go 0 xs))

(define (lists/reverse-go result xs)
  (if (nil? xs)
    result
    (lists/reverse-go (cons (car xs) result) (cdr xs))))

(define (reverse xs)
  (lists/reverse-go nil xs))
