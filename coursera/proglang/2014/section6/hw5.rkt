;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1
;; ( a )
(define (racketlist->mupllist rl)
   (if (null? rl) 
       (aunit)
       (apair (car rl) (racketlist->mupllist(cdr rl)))))
          
;; ( b )
(define (mupllist->racketlist ml)
  (if (aunit? ml) 
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))
         
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(or 
          (int? e) 
          (aunit? e) 
          (closure? e)) e]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied on non-number")))]
        [(apair? e) 
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e) 
         (let [(v (eval-under-env (fst-e e) env))]
           (if (apair? v) 
               (apair-e1 v) 
               (error "fst applied to a non-pair")))]
        [(snd? e) 
         (let [(v (eval-under-env (snd-e e) env))]
           (if (apair? v) 
               (apair-e2 v) 
               (error "second applied to a non-pair")))]
        [(mlet? e) 
         (let* ([v (eval-under-env (mlet-e e) env)]
               [envv (cons (cons (mlet-var e) v) env)])
               (eval-under-env (mlet-body e) envv))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(fun? e) 
         (closure env e)]
        [(call? e) 
         (let ([fe (eval-under-env (call-funexp e) env)]
               [act (eval-under-env (call-actual e) env)])
           (if (closure? fe)
               (let* ([name (fun-nameopt (closure-fun fe))]
                      [formal (fun-formal (closure-fun fe))]
                      [body (fun-body (closure-fun fe))]
                      [envx (closure-env fe)]
                      [envbinding (cons (cons formal act) envx)]
                      [envname (if (eq? name #f) 
                                   envbinding
                                   (cons (cons name fe) envbinding))])
                 (eval-under-env body envname))
               (error "MUPL call funexp is not a closure")))]
        #|
        (struct call (funexp actual)       #:transparent) ;; function call
        (struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
        |#
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
'' ( a )
(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3)) 

;; (b) 
(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

;; (c)
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "v1" e1) 
               (cons "v2" e2))
         (ifgreater (var "v1") (var "v2") 
                    e4
                    (ifgreater (var "v2") (var "v1")
                               e4
                               e3))))
  
  
;; Problem 4
;; ( a )
(define mupl-map
  (fun #f "first"
       (fun "second" "l" (ifaunit (var "l") (aunit) 
                                  (apair 
                                   (call (var "first") (fst (var "l")))
                                   (call (var "second") (snd (var "l"))))))))

;; ( b )
(define mupl-mapAddN
  (mlet "first" mupl-map
        (fun #f "x"
             (call (var "first")
                   (fun #f "y"
                        (add (var "x") (var "y")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
