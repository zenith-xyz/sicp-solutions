#lang sicp
; 1.1
10    ; 10
(+ 5 3 4)    ; 12
(- 9 1)    ; 8
(/ 6 2)    ; 3
(+ (* 2 4) (- 4 6))    ; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))    ; 19
(= a b)    ; #f
(if (and (> b a) (< b (* a b)))
    b
    a)    ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))    ;16
(+ 2 (if (> b a) b a))    ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))    ; 16


; 1.2
(/ (+ 5 4
   (- 2 (- 3
   (+ 6
   (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


; 1.3
(define (sum-of-squares a b) (+ (* a a) (* b b)))

(define (tri-num a b c)
  (cond ((and (>= (* 2 a) (+ b c)) (>= (* 2 b) (+ a c)))
        (sum-of-squares a b))
        ((and (>= (* 2 b) (+ a c)) (>= (* 2 c) (+ a b)))
        (sum-of-squares b c))
        (else (sum-of-squares a c))))

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; If b is larger than zero, print the sum of a and b, otherwise
; print their difference.


; 1.5
; Under eager evaluation (applicative-order), both arguments will
; be evaluated first before they are applied to the function.
; Evaluating 0 is simply 0, but evaluating the function (p)
; results in an infinite loop of (p) calling itself.
;
; Under lazy evaluation (normal-order), arguments are evaluated
; one-by-one based on whether they're needed in the body of the
; function. In this case, when 0 is first evaluated (as 0) and
; and then immeidately applied to test, the second arg y (as (p))
; is not evaluated because applying 0 to test will already yield
; 0 as output. Arg y is only needed if x evaluated as anything
; different to zero.
;
; It is worth noting that if you flipped the order of the args,
; such as:
; (test (p) 0)
; It will evaluate (p) first, resulting in an infinite loop,
; exactly like in the case of eager evaluation.


; 1.6
; In Alyssa's 'new-if' function, due to Scheme's eager evaluation,
; both the 'then' and 'else' clauses are evaluated before the
; function itslef. This means that even if the 'good-enough?'
; predicate is satisfied, the 'else' clause (which contains
; the recursive call to 'sqrt-iter', will be evaluated.
;
; This results in an infinite loop because 'sqrt-iter' will keep
; trying to improve the guess, even when it's already good enough.
; Even though 'good-enough?' predicate returns 'True' at some
; point, the 'new-if' function doesn't "see" this because it's
; too busy going deeper and deeper into the recursion started by
; the 'else' clause. The program simply never stops to 'realise'
; that the 'then' clause has evaluated as 'True' already, because
; it never finishes evaluating the 'else' clause.


; 1.7
; Small numbers scenario: For numbers whose square - x is smaller
; than the threshold (0.001 in our case), the approximation
; improvement steps will stop way too prematurely.
; Let's say we're evaluating 2.5e-7.
; The correct answer here is 0.0005, however our program won't
; even come close in its evaluation of an approximate guess.
; (x^2 - 2.5e-7) < 0.001
; x < 0.0316
; Thus, our approximation steps will end for any 'guess' smaller
; than 0.0316, even though we might still be far from the true
; answer.
; Large number scenario: Given that our decimal numbers go up
; to 16 significant figures, the threshold calculation would
; fail for any number with 14 digits before the decimal point,
; i.e. >10000000000000.00, given that the threshold requires
; 3 decimal places.
;
; My code using alternative method suggested:
(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (/ (square guess) x) 1.0)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))


; 1.8
(define (cube x) (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (/ (cube guess) x) 1.0)) 0.001))

(define (quotient-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (improve-cube guess x)
  (average guess (quotient-cube guess x)))

(define (cubert-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cubert-iter (improve-cube guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))