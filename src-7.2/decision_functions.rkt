#lang racket
;record has all the details of each object but not final answer.
;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define y1 (cons "feature1" (lambda(record) (car record)))) ; returns the value of feature 1 for a given test sample
(define y2 (cons "feature2" (lambda(record) (cadr record))))
(define y3 (cons "feature3" (lambda(record) (caddr record))))
(define y4>62 (cons "feature4>62" (lambda(record) (if(> (cadddr record) 62) 1 0)))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define pclass (cons "pclass" (lambda(record) (first record)))) ; returns the value of pclass for a given test sample
(define sex (cons "sex" (lambda(record) (second record))))
(define age>25 (cons "age>25" (lambda(record) (if(> (third record) 25) 1 0))))
(define sibsp (cons "sibsp" (lambda(record) (fourth record))))
(define parch (cons "parch" (lambda(record) (fifth record))))
(define fare>50 (cons "fare>50" (lambda(record) (if(> (sixth record) 50) 1 0))))
(define emb (cons "emb" (lambda(record) (seventh record))))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)

(define cshape (cons "cshape" (lambda(record) (first record))))
(define csurf (cons "csurf" (lambda(record) (second record))))
(define bruise (cons "bruise" (lambda(record) (third record))))
(define odor (cons "odor" (lambda(record) (fourth record))))
(define gatch (cons "gatch" (lambda(record) (fifth record))))
(define gspace (cons "gspace" (lambda(record) (sixth record))))
(define gsize (cons "gsize" (lambda(record) (seventh record))))
(define sshape (cons "sshape" (lambda(record) (eighth record))))
(define nring (cons "nring"  (lambda(record) (ninth record))))
(define pop (cons "pop" (lambda(record) (tenth record))))
(define hab (cons "hab" (lambda(record) (tenth (cdr record)))))




