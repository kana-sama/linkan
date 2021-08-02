(load lib/basic-macros)
(load lib/bools)
(load lib/lists)

(define (cons? xs)
  (and (list? xs) (not (nil? xs))))

(define-macro* (-> x & fs)
  (foldr (reverse fs) x (lambda (f x) (cons (car f) (cons x (cdr f))))))

(define (id x) x)

(define (merge-cond-pairs p1 p2)
  '(,(concat (nth 0 p1) (nth 0 p2))
    ,(concat (nth 1 p1) (nth 1 p2))))

(define (list-of? n l)
  (and (list? l) (number? n) (same-number? n (length l))))

(define (is-atom? a b)
  (and (atom? a) (atom? b) (same-atom? a b)))

(define (match->cond-pair value pattern)
  (cond
    ; 42 -> value should be `42`
    ((number? pattern)
     '(((number? ,value) (same-number? ,value ,pattern)) ()))
    
    ; nil -> value is `'()`
    ((is-atom? 'nil pattern)
     '(((list? ,value) (nil? ,value)) ()))

    ; _ -> value can be any
    ((is-atom? '_ pattern)
     '(() ()))

    ; a -> value can be any and will be bound as `a`
    ((atom? pattern)
     '(() ((,pattern ,value))))

    ; (as a b) -> value should match pattern `b` and will be bound as `a`
    ((and (list-of? 3 pattern)
          (is-atom? (nth 0 pattern) 'as)
          (atom? (nth 1 pattern)))
     (merge-cond-pairs
       (match->cond-pair value (nth 2 pattern))
       '(() ((,(nth 1 pattern) ,value)))))

    ; (atom a) -> value should be atom and will be bound as `a`
    ((and (list-of? 2 pattern)
          (is-atom? (nth 0 pattern) 'atom)
          (atom? (nth 1 pattern)))
     '(((atom? ,value))
       ((,(nth 1 pattern) ,value))))

    ; (number a) -> value should be number and will be bound as `a`
    ((and (list-of? 2 pattern)
          (is-atom? (nth 0 pattern) 'number)
          (atom? (nth 1 pattern)))
     '(((number? ,value))
       ((,(nth 1 pattern) ,value))))

    ; 'a -> value should be `a`
    ((and (list-of? 2 pattern)
          (is-atom? (nth 0 pattern) 'quote)
          (atom? (nth 1 pattern)))
     '(((is-atom? ,value (quote ,(nth 1 pattern)))) ()))

    ; (cons a b) -> value should be non-empty list, `a` and `b` can be any patterns for head and tail
    ((and (list-of? 3 pattern)
          (is-atom? (car pattern) 'cons))
     (merge-cond-pairs
       '(((list? ,value) (not (nil? ,value))) ())
       (merge-cond-pairs
         (match->cond-pair '(car ,value) (nth 1 pattern))
         (match->cond-pair '(cdr ,value) (nth 2 pattern)))))

    ; (list a_1 a_2 ... a_n) -> value should be a list with n elements, a_i - pattern for the i-th element
    ((and (list? pattern)
          (not (nil? pattern))
          (is-atom? (car pattern) 'list))
     (merge-cond-pairs
      '(((list? ,value)
         (same-number? ,(length pattern) (+ 1 (length ,value))))
        ())
       (-> (cdr pattern)
         (indexed)
         (map (lambda (index-pattern)
                (match->cond-pair '(nth ,(nth 0 index-pattern) ,value) (nth 1 index-pattern))))
         (foldr '(() ()) merge-cond-pairs))))
  
    (true '(() ()))))

(define-macro* (match value & branches)
  '(let ((matched* ,value))
     ,(cons 'cond
        (map branches (lambda (branch)
                        (let ((cond-pair (match->cond-pair 'matched* (nth 0 branch))))
                          '(,(cons 'and (nth 0 cond-pair))
                            (let ,(nth 1 cond-pair)
                              (do ,(nth 1 branch))))))))))

(define (my-length list)
  (match list
    (nil 0)
    ((cons _ xs) (+ 1 (my-length xs)))))

(print (my-length (list 1 2 3 4)))
(print (my-length (list)))

(define (foo x)
  (match x
    ('bar 'it-is-bar)
    ((list 1 2 3) 'it-is-1-2-3)
    ((as l (list a _ 'c)) '(it-is-list-of-3-with-last-c ,l))
    ((cons _ _) 'it-is-nonempty-list)
    (nil 'it-is-empty-list)
    (0 'it-is-zero)
    ((number x) '(it-is-nonzero-number ,x))
    ((atom x) '(it-is-atom ,x))
    (_ 'something-other)))

(print (foo 'bar)) ; it-is-bar
(print (foo '(1 2 3))) ; it-is-1-2-3
(print (foo '(a 0 c))) ; (it-is-list-of-3-with-last-c (a 0 c))
(print (foo '(a 0 d))) ; it-is-nonempty-list
(print (foo '())) ; it-is-empty-list
(print (foo 0)) ; it-is-zero
(print (foo 42)) ; (it-is-nonzero-number 42)
(print (foo 'hello)) ; (it-is-atom hello)
(print (foo (lambda (x) x))) ; something-other
