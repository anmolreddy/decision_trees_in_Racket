#lang racket

(require "decision_functions.rkt")
(require 2htdp/batch-io)
;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

(provide titanicout)
(define titanicout "../output/titanic-decision-tree.dot")

(provide mushroomout)
(define mushroomout "../output/mushroom-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings

(provide toy-raw)
(define toy-raw
  (cdr (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw
  (cdr (map (lambda(x) (cddr x)) (read-csv-file titanictrain))))

(provide mushroom-raw)
(define mushroom-raw
  (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data); this data is not the final data
  (define (stringlist->numlist lst) (map (lambda(x) (string->number x)) lst))
  (cons (cdr (stringlist->numlist data)) (car (stringlist->numlist data))) )

;list of (features . result)
(define abstract2;takes raw output as input
  (lambda(lst-of-lsts)
    (map(lambda(x) (format x)) lst-of-lsts)))

(provide toy)
(define toy
  (abstract2 toy-raw))

(provide titanic)
(define titanic
  (abstract2 titanic-raw))

(provide mushroom)
(define mushroom
  (abstract2 mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================
;data is the mushroom, titanic and toy defined earlier.
;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data);toy is the input
  (/ (foldr + 0 (map (lambda(x) (cdr x)) data)) (length data)))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (define (one-term p) (* -1 (log p 2) p))
  (let([P (get-leaf-prob data)])
    (if(or (= P 0) (= P 1)) 0 
       (+ (one-term P) (one-term (- 1 P))))))
  
;find the difference in entropy achieved=initial-final entropy -> positive
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
    (- (get-entropy data)
       (/
        (foldr (lambda(sub-data ans-rest) (+ (* (length sub-data) (get-entropy sub-data)) ans-rest))
                                 0
                                 (group-by (lambda(x) ((cdr f) (car x))) data))
        (length data))))

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
  (argmax (lambda(candidate) (entropy-diff candidate data)) candidates))

(provide DTree)
(struct DTree (desc func kids) #:transparent); 

;build a decision tree (depth limited) from the candidate decision functions and data
;leaf has (prob."leaf") fun-gives-"leaf" '()
;DTree in general has (prob.(list of fun of each classification in kids)) fun kids
(provide build-tree)
(define (build-tree candidates data depth)
  (cond
    [(or (= depth 0)
         (= (get-leaf-prob data) 1)
         (= (get-leaf-prob data) 0))
     (DTree (cons (get-leaf-prob data) "leaf") "leaf" '())]
    [else (let* ([best-fun (choose-f candidates data)]
                 [new-candidates (remove best-fun candidates)]
                 [list-of-sub-data (group-by (lambda(x) ((cdr best-fun) (car x))) data)])
            (DTree (cons (get-leaf-prob data) (cons (car best-fun) (map (lambda(x) ((cdr best-fun) (caar x))) list-of-sub-data)))
                   best-fun
                   (map (lambda(sub-data) (build-tree new-candidates sub-data (- depth 1))) list-of-sub-data)))]))

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test);test is a list of features
  (define (choose-a-kid value-of-testcase-with-f lst list-of-kids)
    (let([i (index-of lst value-of-testcase-with-f)])
      (if(equal? #f i)
         (DTree (cons 0 "leaf") "leaf" '())
         (list-ref list-of-kids i))))
  (let ([Dkids (DTree-kids tree)]
        [Dfunc (DTree-func tree)]
        [Ddesc (DTree-desc tree)])
    (cond
      [(null? Dkids) (car Ddesc)]
      [else (make-decision (choose-a-kid ((cdr Dfunc) test) (cddr Ddesc) Dkids) test)])))

  ;============================================================================================================
  ;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\""
                   d;d is the desc
                   "\"];"
                   "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;;============================================================================================================
;;============================================================================================================
;;============================================================================================================
