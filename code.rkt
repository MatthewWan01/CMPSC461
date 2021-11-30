#lang racket

;p1 
(define (funPower f n)
 (if(<= n 0) ;check if n >= 0
    (lambda (x) x)
    (lambda (x) (f ((funPower f (- n 1)) x ))))) ;recursive call function f until n = 0

((funPower sqrt 2) 16) ;return 2

(define (encode n)
  (lambda (f) (lambda (z) ((funPower f n) z)))) ;recursive return f n times

(encode 2)

(define (decode n)
  ((n (lambda (x) (+ x 1))) 0 )) ;recursive add 1

(decode(encode 2)) ;return 2

(define (MULT n1 n2)
  (encode (* (decode n1) (decode n2)))) ;decode then multiply then encode the result

(decode (MULT (encode 2) (encode 3))) ;return 6

;p2
(define (merge x y)
  (cond
    [(null? x) y] ;check if x is empty
    [(null? y) x] ;check if y is empty
    [(<= (car x) (car y)) ;if head element of x <= y
     (cons (car x) (merge (cdr x) y))] ;return head of x, recurse call merge with next element in x
    [(< (car y) (car x)) ;if head element of y < x
     (cons (car y) (merge x (cdr y)))])) ;return head of y, recurse call merge with next element in y

(merge '(2 7) '()) ; returns (2 7)
(merge '(1 1 2) '(1 3 5)) ; returns (1 1 1 2 3 5)
(merge '(1 1 6 8) '(2 7)) ; returns (1 1 2 6 7 8)

;p3
(define (findMax lst) ;use flatten to remove paren
  (cond
    [(null? lst) 0] ;empty list return 0
    [(null? (cdr (flatten lst))) (car (flatten lst))] ;single element return element
    [(>= (car (flatten lst)) (cadr (flatten lst))) (findMax (cons (car (flatten lst)) (cddr (flatten lst))))] ; if x(i) >= x(i+1), findMax new lst without x(i+1)
    [(< (car (flatten lst)) (cadr (flatten lst))) (findMax (cdr (flatten lst)))])) ;if x(i) < x(i+1)    

(findMax '(4 5 1)) ; returns 5
(findMax '(5 1 (3 (4 8)))) ; returns 8
(findMax '(1 3 (3 3) () 6 (1))) ; returns 6

;p4
(define (lstEvenSum lst)
  (cond
    [(null? lst) 0] ;empty list
    [(even? (car lst)) (+ (car lst) (lstEvenSum (cdr lst)))] ;number is even, sum number + lstEvenSum next number
    (else (lstEvenSum (cdr lst))))) ;not even, lstEvenSum next number

(lstEvenSum '(1)) ;return 0
(lstEvenSum '(1 2 3)) ;return 2
(lstEvenSum '(1 2 3 4)) ;return 6

;p5

(define (trunc a b lst)
  (map (lambda (x) ;map result of condition to all number in list
    (cond
      [(< x a) a] ;element x < a set x = a
      [(> x b) b] ;element x > b set x = b
      (else x))) lst)) ;otherwise dont change

(trunc 0 1 '(-2 -1 0 1 2)) ; returns ’(0 0 0 1 1)
(trunc 0.5 1.5 '(-2 -1 0 1 2)) ; returns ’(0.5 0.5 0.5 1 1.5)
(trunc 1 1 '(-2 -1 0 1 2)) ; returns ’(1 1 1 1 1)

(define (lstOR lst)
  (foldl (lambda (a b) (or a b)) #f lst)) ;check list for any #t, if none return false

(lstOR '(#t #f)) ; returns #t
(lstOR '(#f #f)) ; returns #f
(lstOR '()) ; returns #f

(define (geq lst1 lst2)
  (cond
    [(null? lst1) #t]) ;if list empty return true
  (foldl (lambda (a b) (and a b)) #t ;make sure all is #t
         (map (lambda (pair) ;create pairs to compare
           (>= (car pair) (cadr pair))) ;lst1[i] >= lst2[i]
              (car (foldl (lambda (y x) 
                            (list (cons (list y (caadr x)) (car x)) ;create list insert boolean value if inequality is true or false
                                  (cdadr x)))
                          (list '() lst2) lst1)))))

(geq  '(5 4 3) '(2 3 3)) ; returns #t
(geq  '(1 0) '(0 1)) ; returns #f
(geq  '() '()) ; returns #t