#lang racket

(define (p a b c d n)
  (define (cons-x v) (λ [l] (cons v l)))
  (cond
    [(< n 0) '()]
    [(= n 0) '(())]
    [else (append (map (cons-x a) (p a b c d (- n a)))
                  (map (cons-x b) (p a b c d (- n b)))
                  (map (cons-x c) (p a b c d (- n c)))
                  (map (cons-x d) (p a b c d (- n d))))]))

(length (p 2 3 5 7 8)) ; -> 6