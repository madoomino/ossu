
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low
            (sequence (+ low stride) high stride))))

(define (string-append-map lst suffix)
  (map (lambda (x) (if (string? x) (string-append x suffix) x)) lst))

(define (list-nth-mod lst n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? lst) (error "list-nth-mod: empty list")]
        [#t (car (list-tail lst (remainder n (length lst))))]))
              
(define (stream-for-n-steps s n)
  (letrec ([f (λ (s counter)
                (let ([p (s)])
                  (if (> counter n)
                    null
                    (cons (car p) (f (cdr p) (+ counter 1))))))])
    (f s 1)))

(define funny-number-stream
  (letrec ([f (λ (x)
                (if (= (remainder x 5) 0)
                    (cons (- x) (λ () (f (+ x 1))))
                    (cons x (λ () (f (+ x 1))))))])
    (λ () (f 1))))


(define dan-then-dog
  (letrec ([f (λ (i)
                (if (= i 0)
                    (cons "dan.jpg" (λ () (f 1)))
                    (cons "dog.jpg" (λ () (f 0)))))])
    (λ () (f 0))))


(define (stream-add-zero s)
  (let ([pr (s)])
    (λ () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))
  

(define (cycle-lists lst1 lst2)
  (letrec ([f (λ (xs ys)
                (cond [(null? xs) (f lst1 ys)]
                      [(null? ys) (f xs lst2)]
                      [else (cons (cons (car xs) (car ys))
                                  (λ () (f (cdr xs) (cdr ys))))]))])
    (λ () (f lst1 lst2))))


(define (vector-assoc v vec)
  (if (= (vector-length vec) 0)
      #f
      (if (and (pair? (vector-ref vec 0))
               (equal? (car (vector-ref vec 0)) v))
          (vector-ref vec 0)
          (vector-assoc v (vector-take-right vec (- (vector-length vec) 1))))))
; Another way of doing it is by implementing a recursive fn
; that processes one element at a time.
; with a counter for the tracking the index.
; it would be a better-time-complexity solution,
; but I'm too lazy to implement it :)
    






