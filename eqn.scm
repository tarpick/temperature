; m1*tf + m2*tf = m1*t1 + m2*t2

(define (contains term)
  (lambda (terms)
    (member term terms)))

(define (does-not-contain term)
  (lambda (terms)
    (not ((contains term) terms))))

(define (filter p seq)
  (cond ((null? seq) '())
        ((p (car seq)) (cons (car seq) (filter p (cdr seq))))
        (else (filter p (cdr seq)))))

(define (make-expr op . params) (cons op params))
(define op-expr car)
(define params-expr cdr)

(define (find-inverse-op op)
  (case op
    ((+) '-)
    ((-) '+)
    ((*) '/)
    ((/) '*)))

(define (make-inverse-expr op params)
  (let ((inverse-op (find-inverse-op op)))
    (make-expr inverse-op params)))

(define (find-subexpressions match? expr)
  (let ((op (car expr)) (rest (cdr expr)))
    (if (pair? (car rest))
        (filter pair?
                (map (lambda (r) (find-subexpressions match? r))
                     rest))
        (if (match? rest) expr '()))))

(define (remove-sums expr match?)
  (let ((op (op-expr expr))
        (terms (find-subexpressions match? expr)))
    (if (null? terms)
        '()
        (make-inverse-expr op terms))))

(define (trace expr)
  (print "TRACE EXPR: " expr)
  expr)

(define (remove-term term expr)
  (cond ((null? expr)
         expr)
        ((equal? (car expr) term)
         (remove-term term (cdr expr)))
        (else
         (cons (car expr) (remove-term term (cdr expr))))))

(define (simplify expression)
  (let ((num-elements (length expression)))
    (cond ((= num-elements 2)
            (cadr expression))
          ((and (= num-elements 1) (pair? expression))
            (car expression))
          (else
            expression))))

(define (remove-terms terms expr)
  (if (null? terms)
      (simplify expr)
      (remove-terms (cdr terms) (remove-term (car terms) expr))))

(define (remove-product expr term)
  (let ((op (op-expr expr))
        (params (params-expr expr)))
    (make-inverse-expr op (remove-term term params))))

(define (apply-operations ops expr)
  (if (null? ops)
      expr
      (let ((op (car ops))
            (terms (cadr ops)))
        (case op
          ((- /)
          (let ((res (remove-terms terms expr)))
            (if (equal? res expr)  ;  no change
                (apply make-expr op res terms)
                res)))
          (else terms)))))

(define (flatmap f seq)
  (foldl append '() (map f seq)))

(define (get-terms-except term)
  (lambda (expr)
    (let ((op (op-expr expr))
          (params (params-expr expr)))
      (filter (lambda (t) (not (eq? t term))) params))))

(define (group-term term expr)
  (define getter (get-terms-except term))
  (let ((op (op-expr expr))
        (terms (params-expr expr)))
    (let ((new-terms
           (if (pair? (car terms))
               (flatmap getter terms)
               (getter expr))))
      (if (= (length new-terms) 1) ; lowest form, so don't factor
          expr
          (make-expr '* term (apply make-expr op new-terms))))))

(define (group-left term eqn)
  (let ((lhs (lhs-equation eqn))
        (rhs (rhs-equation eqn)))
    (make-equation (group-term term lhs) rhs)))

(define (make-equation lhs rhs) (cons lhs rhs))
(define lhs-equation car)
(define rhs-equation cdr)

(define heat-eqn
  (make-equation
    (make-expr '+ (make-expr '* 'm1 't1) (make-expr '* 'm2 't2))
    (make-expr '+ (make-expr '* 'm1 'tf) (make-expr '* 'm2 'tf))))

(define (print-eqn eqn)
  (print (lhs-equation eqn) " = " (rhs-equation eqn)))

(define (infix-expr expr)
  (cond ((null? expr)
         '())
        ((not (pair? expr))
         expr)
        ((= (length expr) 2)
         (infix-expr (cadr expr)))
        (else
         (list (infix-expr (cadr expr)) (car expr) (infix-expr (cons (car expr) (cddr expr)))))))

(define (factorize-eqn-left term eqn)
  (let ((eq-last (group-left term eqn)))
    (let ((rm-prod (remove-product (lhs-equation eq-last) term))
          (left (lhs-equation eq-last))
          (right (rhs-equation eq-last)))
      (make-equation
        (apply-operations rm-prod left)
        (apply-operations rm-prod right)))))

(define (solve-heat-eqn term)
  (define left-oriented-eqn
    (let ((terms (params-expr (lhs-equation heat-eqn))))
      (if (null? (filter (contains term) terms))
          (make-equation (rhs-equation heat-eqn) (lhs-equation heat-eqn))
          heat-eqn)))
  (let ((l (lhs-equation left-oriented-eqn))
        (r (rhs-equation left-oriented-eqn)))
    (let ((lhs-ops (remove-sums l (does-not-contain term)))
          (rhs-ops (remove-sums r (contains term))))
      (factorize-eqn-left term
        (make-equation
          (apply-operations rhs-ops (apply-operations lhs-ops l))
          (apply-operations lhs-ops (apply-operations rhs-ops r)))))))

(define (t)
  (solve-heat-eqn 'm1))

(define r '(- (* m2 tf) (* m2 t2)))
(define (t1) (apply-operations '(/ ((- t1 tf))) r))

(define (t2) (solve-heat-eqn 't1))
