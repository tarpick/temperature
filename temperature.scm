(define (to-fahrenheit c)
  (+ (* (/ 9.0 5.0) c) 32.0))

(define (to-celsius f)
  (* (/ 5.0 9.0) (- f 32.0)))

(define degC 'C)
(define degF 'F)

(define (make-temperature value c-or-f)
  (cond ((eq? c-or-f degC)
         (cons (* 1.0 value) (to-fahrenheit value)))
        ((eq? c-or-f degF)
         (cons (to-celsius value) (* 1.0 value)))
        (else (error "unknown specifier" c-or-f))))

(define temp make-temperature)

(define c-temp car)
(define f-temp cdr)

(define (mixed-fluid-temperature mass1 temp1 mass2 temp2)
  (let ((tc1 (c-temp temp1))
        (tc2 (c-temp temp2)))
    (make-temperature (/ (+ (* mass1 tc1) (* mass2 tc2)) (+ mass1 mass2)) degC)))

; m1 * (x - t1) = m2 * (t2 - x)
; m1*tf - m1*t1 = m2*t2 - m2*tf
;
; (m1 + m2)*x = m1*t1 + m2*t2
;
; (m1 + m2)*x - m1*t1 - m2*t2 = 0
; m1*tf + m2*tf - m1*t1 - m2*t2 = 0
; S*x - m1*t1 - m2*t2 = 0

(define ice-temp  (make-temperature   0.0 degC))
(define boil-temp (make-temperature 100.0 degC))
(define cup-mass 226.8)

(define m1 'm1)
(define m2 'm2)
(define t1 't1)
(define t2 't2)
(define tf 'tf)

(define valid-vars (list m1 m2 t1 t2 tf))

(define (make-var-binding var value)
  (if (memq var valid-vars)
      (cons var value)
      (error "undefined variable name" var)))

(define var-binding car)
(define val-binding cdr)

(define (filter p seq)
  (cond ((null? seq)
          '())
        ((p (car seq))
          (cons (car seq) (filter p (cdr seq))))
        (else
          (filter p (cdr seq)))))

; (equate (add (mult m1 t1) (mult m2 t2)) (add (mult m1 tf) (mult m2 tf)))

; m1*t1 + m2*t2 = tf*(m1 + m2)
; m1*t1 + m2*t2 = m1*tf + m2*tf

; tf = (m1*t1 + m2*t2) / (m1 + m2)
; m1 = m2 * (dt2 / dt1)
; m2 = m1 * (dt1 / dt2)
; t1 = ((m1 + m2)*tf - m2*t2) / m1
; t2 = ((m1 + m2)*tf - m1*t1) / m2

(define (find-missing-vars bindings)
  (let ((vars (map var-binding bindings)))
    (filter (lambda (e) (not (memq e vars))) valid-vars)))

(define (solve-heat-eqn bindings)
  (define (get var) (cdr (assoc var bindings)))
  (define (get-t var) (c-temp (cdr (assoc var bindings))))
  (let ((missing (find-missing-vars bindings)))
    (if (not (= (length missing) 1))
        (error "too many unknown variables" missing)
        (let ((v (car missing)))
          (cons
            v
            (case v
              ((tf)
               (make-temperature
                (/ (+ (* (get m1) (get-t t1))
                      (* (get m2) (get-t t2)))
                    (+ (get m1) (get m2)))
                degC))
              ((m1)
               (* (get m2)
                  (/ (abs (- (get-t t2) (get-t tf)))
                     (abs (- (get-t t1) (get-t tf))))))
              ((m2)
               (* (get m1)
                  (/ (abs (- (get-t t1) (get-t tf)))
                     (abs (- (get-t t2) (get-t tf))))))
              ((t1)
               (make-temperature
                (/ (- (* (get-t tf)
                         (+ (get m1) (get m2)))
                      (* (get m2)
                         (get-t t2)))
                   (get m1))
                degC))
              ((t2)
               (make-temperature
                (/ (- (* (get-t tf)
                         (+ (get m1) (get m2)))
                      (* (get m1)
                         (get-t t1)))
                   (get m2))
                degC))
              (else (error "do not know how to solve for" v))))))))

(define (ice-with-water-temperature ice-mass water-mass water-temperature)
  (let ((res (/ (- (* water-mass (c-temp water-temperature))
                   (* 79.7 ice-mass))
                (+ water-mass ice-mass))))
    (make-temperature res degC)))

(define she solve-heat-eqn)

(define (bind . pairs)
  (if (null? pairs)
      '()
      (let ((k (car pairs))
            (v (cadr pairs))
            (rest (cddr pairs)))
        (cons (make-var-binding k v) (apply bind rest)))))
