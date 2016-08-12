#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first symbol-exp?)
   (rest s-list?)))

(define-datatype symbol-exp symbol-exp?
  (symbol-symbol-exp
   (data symbol?))
  (s-list-symbol-exp
   (data s-list?)))

;; Exercise 2.3
(define vector-of
  (lambda (pred)
    (lambda (val)
      (letrec ((check-vector
                (lambda (start end)
                  (if (= start end)
                      #t
                      (and (pred (vector-ref val start))
                           (check-vector (+ start 1)
                                         end))))))
        (check-vector 0 (vector-length val))))))

(define vector-of-tail-rec
  (lambda (pred)
    (lambda (val)
      (letrec ((check-vector
                (lambda (start end b)
                  (if  (or (= start end)
                           (not b))
                      b
                      (check-vector (+ start 1) end
                                    (and (pred (vector-ref val start))
                                         b))))))
        (check-vector 0 (vector-length val) #t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tree-a (interior-node 'a (leaf-node 2) (leaf-node 3)))
(define tree-b (interior-node 'b (leaf-node -1) tree-a))
(define tree-c (interior-node 'c tree-b (leaf-node 1)))

(define leaf-sum
  (lambda (tree)
    (cases bintree tree
        (leaf-node (datum) datum)
        (interior-node (key left right)
            (+ (leaf-sum left) (leaf-sum right))))))

;; Exercise 2.4
(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
        (leaf-node (datum) (list 'leaf-node datum))
        (interior-node (key left right)
            (list 'interior-node key (bintree-to-list left)
                                     (bintree-to-list right))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lambda-exp
   (id symbol?)
   (body expression?))
  (app-exp
   (rator expression?)
   (rand expression?)))

(define occurs-free?
  (lambda (var exp)
    (cases expression exp
           (var-exp (id) (eqv? id var))
           (lambda-exp (id body)
                       (and (not (eqv? id var))
                            (occurs-free? var body)))
           (app-exp (rator rand)
                    (or (occurs-free? var rator)
                        (occurs-free? var rand))))))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
           (var-exp (id) id)
           (lambda-exp (id body)
                       (list 'lambda (list id)
                             (unparse-expression body)))
           (app-exp (rator rand)
                    (list (unparse-expression rator)
                          (unparse-expression rand))))))

(define parse-expression
  (lambda (datum)
    (cond
     ((symbol? datum) (var-exp datum))
     ((pair? datum)
      (if (eqv? (car datum) 'lambda)
          (lambda-exp (caadr datum)
                      (parse-expression (caddr datum)))
          (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum))))
      (else (eopl:error 'parse-expression
                        "Invalid concrete syntax ~s" datum))))))
