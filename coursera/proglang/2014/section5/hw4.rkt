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
                  (let ([pr (stream)])
                    (if (= n 0) null (cons (car pr) (f (cdr pr) (- n 1))))))])
      (f stream n)))

;; problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([g (lambda () (f (+ x 1)))])
                       (if (= 0 (modulo x 5))
                           (cons (* x -1) g)
                           (cons x g))))])
    (lambda () (f 1))))

;; problem 6
(define dan-then-dog
  (letrec ([f (lambda (b)
                (if b
                    (cons "dan.jpg" (lambda () (f #f)))
                    (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

;; problem 7
(define (stream-add-zero stream)
  (define (thunk x)
    (cons (cons 0 (car (x))) (lambda () (thunk (cdr (x))))))
  (lambda () (thunk stream)))

;; problem 8
(define (cycle-lists xs ys)
  (define (thunk c)
    (cons (cons (list-nth-mod xs c) (list-nth-mod ys c)) (lambda () (thunk (+ c 1)))))
  (lambda () (thunk 0)))

;; problem 9
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (pos)
                (if (= pos l)
                    #f
                    (let ([vpos (vector-ref vec pos)])
                      (if (not (pair? vpos))
                          (f (+ pos 1))
                          (if (equal? v (car vpos))
                              vpos
                              (f (+ pos 1)))))))])
    (f 0)))

;; problem 10
;;(define (cached-assoc xs n)

