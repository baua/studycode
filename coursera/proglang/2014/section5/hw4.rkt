
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(define ones (lambda () (cons 1 ones)))
;; put your code below
;; problem 1
(define sequence
  (lambda (low high stride)
    (cond 
      [(< low high) (cons low (sequence (+ low stride) high stride))]
      [(> low high) null]
      [(or (= low high) (< (- high low) stride)) (cons high null)])))

;; problem 2
(define string-append-map
  (lambda (xs suffix)
    (map (lambda (s) (string-append s suffix)) xs)))

;; problem 3
(define list-nth-mod
  (lambda (xs n)
    (cond
      [(< n 0) (error "list-nth-mod: negtive number")]
      [(null? xs) (error "list-nth-mod: empty list")]
      [#t (let 
             ([pos (remainder n (length xs))]) 
             (car (list-tail xs pos)))])))

;; problem 4
(define (stream-for-n-steps stream n)
    (letrec ([f (lambda (stream n)
                  (if (= n 0) null (cons (stream) (f stream (- n 1)))))])
      (f stream n)))
