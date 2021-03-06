(define (square x) (* x x))
(square 3)

(define (triangles max-perimenter)
  (define (next-triangle a b c) 1)
  (cond ((> (sum a b c) max-perimenter))))


;; 1.8
;; Lexical scoping:
;; When resolving free variables in a procedure, the interpreter looks in
;; the context that defined the procedure.



;; tree recursion
;; 1.2.2
(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 1) 1)
	((= kind-of-coins 2) 5)
	((= kind-of-coins 3) 10)
	((= kind-of-coins 4) 25)
	((= kind-of-coins 5) 50)))

(define (cc amount kind-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kind-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kind-of-coins 1))
		 (cc (- amount (first-denomination kind-of-coins))
		     kind-of-coins)))))

(define (count-change amount)
  (cc amount 5))

;; ex 1.11


;; three-recursive
(define (ex1.11 x)
  (cond ((< x 3) x)
	(else (+ (ex1.11 (- x 1))
		 (* 2 (ex1.11 (- x 2)))
		 (* 3 (ex1.11 (- x 3)))))))

;; iterative
(define (ex1.11b x)
  (define (next-term n-1 n-2 n-3)
    (+ n-1 (* 2 n-2) (* 3 n-3)))
  (define (iter n n-1 n-2 n-3)
    (cond ((= n x) n-1)
	  (else (iter (+ 1 n) (next-term n-1 n-2 n-3) n-1 n-2))))
  (cond ((< x 3) x)
	(else (iter 2 2 1 0))))

;; Finds elements in Pascals triangle
(define (ex1.12 row col)
  (cond ((or (= col 1) (= row col)) 1)
	((or (<= col 0) (> col row)) 0)
	(else (+ (ex1.12 (- row 1) col) (ex1.12 (- row 1) (- col 1))))))

(define (c x)
  (define (c-iter x counter)
    (cond ((< x 0.1) counter)
	  (else (c-iter (/ x 3) (+ 1 counter)))))
  (c-iter x 1))

(define (exp x y)
  (cond ((= y 0) 1 )
	(else (* x (exp (- y 1))))))

(define (exp-lin x y)
  (define (exp-lin-iter y acc)
    (cond ((= 0 y) acc)
	  (else (exp-lin-iter (- y 1) (* acc x)))))
  (exp-lin-iter y 1))


(define (smallest-divider n)
  (find-divider n 2))

(define (find-divider n test-divider)
  (cond ((> (square test-divider) n) n)
	((divides? n test-divider) test-divider)
	(else (find-divider n (1+ test-divider)))))

(define (-1+ x) (- x 1))
(define (1+ x) (+ x 1))
(define square (lambda (x) (* x x)))
(define divides? (lambda (x y) (= 0 (remainder x y))))
(define is-prime? (lambda (x) (= (smallest-divider x) x)))


(define 1- (lambda (x) (- x 1)))
(define 1+ (lambda (x) (+ x 1)))
(define square (lambda (x) (* x x)))

(define pow (lambda (x y)
	      (cond ((<= y 0) 1)
		    ((even? y) (square (pow x (/ y 2))))
		    (else (* x (pow x (1- y )))))))

(define expmod (lambda (x y m) (remainder (pow x y) m)))

(define (divides? a b )
  (= 0 (remainder a b)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? n test-divisor) test-divisor)
	(else (find-divisor n (1+ test-divisor)))))

(define smallest-divisor (lambda (n) (find-divisor n 2)))

(define (fermat-test n)
  (let ((a (1+ (random (1- n)))))
    (= a (expmod a n n))))

(define (fast-prime? n times)
  (cond ((= times 0) false)
	(else (or (fermat-test n)
		  (fast-prime? n (1- times))))))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (next-prime start)
  (if (prime? start) start
      (next-prime (1+ start))))

(define (find-next-n-primes start n)
  (if (> n 0)
      (next-prime start)
      :done))

;; 1.3 Formulating Abstraction with higher order procedures
;; We want a construct to name reoccuring patterns, and use them
;; by that name. That is what procedures are
;;
;; Procedures that take procedures as arguments,or return them as results
;; are called higher-order procedures
(define inc (lambda (x) (+ 1 x)))

(define (sum-of-integers a b)
  (if (> a b) 0
      (+ a (sum-of-integers (inc a) b))))

(define cube (lambda (x) (* x x x)))

(define (sum-of-cubes a b)
  (if (> a b) 0
      (+ (cube a) (sum-of-cubes (+ 1 a) b))))

(define (sum-pi a b)
  (if (> a b) 0
      (+ (sum-pi (+ a 4) b) (/ 1 (* a (+ a 2))))))

;; thre three functions share following pattern:
;; (define <name> a b
;;   (if (> a b) 0
;;     (+ (<term> a)
;;     (<name> (<next> a) b)))
;; Called 'summation'
;;
;; Allows us to talk about the concept of summation, in stead of talking
;; about specifi series.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-of-cubes a b)
  (sum cube a inc b ))

(define (pi-sum a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (mysum pi-term pi-next a b))

;; Greatest common divisor
(define (gcd a b)
  (if (= 0 (remainder a b))
      b
      (gcd b (remainder a b))))


;; Integral for a function f between a and b is aprox. the product of dx
;; and the sum of f(a + dx/2 + dx * n) unit a + dx/2 + dx * n > b
;; it is dividing the surface in small pilars (dx wide, and f(x + dx/2) wide
;; The extra +dx/2 makes it more accurate (takes the middle point of the interval;; i.s.o. the max
;; so
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b )))


;; Ex 1.30 rewrite sum to be iterative in stead of linear recursive
(define (pi-sum a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (mysum pi-term pi-next a b))

;; Integral for a function f between a and b is aprox. the product of dx
;; and the sum of f(a + dx/2 + dx * n) unit a + dx/2 + dx * n > b
;; it is dividing the surface in small pilars (dx wide, and f(x + dx/2) wide
;; The extra +dx/2 makes it more accurate (takes the middle point of the interval;; i.s.o. the max
;; so
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b )))


;; Ex 1.30 rewrite sum to be iterative in stead of linear recursive
(define (pi-sum a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (mysum pi-term pi-next a b))

;; Integral for a function f between a and b is aprox. the product of dx
;; and the sum of f(a + dx/2 + dx * n) unit a + dx/2 + dx * n > b
;; it is dividing the surface in small pilars (dx wide, and f(x + dx/2) wide
;; The extra +dx/2 makes it more accurate (takes the middle point of the interval;; i.s.o. the max
;; so
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b )))


;; Ex 1.30 rewrite sum to be iterative in stead of linear recursive
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)) )))
  (iter a 0))

;; Ex 1.31 write generic product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;; Approx pi with pi/8 = 4*4*6*6*8*8... / 3*3*5*5*7*7...
(define (approx-pi p)
  (define (next n)
    (+ n 2))
  (define (term x)
    (/ (* 4 x x)
       (*
	(- (* 2 x) 1)
	(- (* 2 x) 1))))
  (product term 2 next p))

;; 1.3.2 : Constructing Procedures with Lambda
;; (lambda (<formal-parameters>) <body>)

(define (plus5 x ) (+ x 5))
;; is the same as
(define plus5 (lambda (x) (+ 5 x)))

;; This is the same, but not associated to any name in this environment
(lambda (x) (+ 5 x))

;; let can create very local bindings
(let ((a 1)
      (b (+ 1 1)))
  (* a b))

;; let is just syntax sugar for lambda
((lambda (a b)
   (* a b))
   1
   (+ 1 1))

(let ((x 1))
  (let ((y (+ 1 x)))
    (+ x y)))

(define (f g)
  (g 2))

(f square)
;; (f f)


;; Ex 1.32
;;
;; Accumulate
;; A more genereci form of sum and product
;;
(define  (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner
				     null-value term (next a)
				     next b))))

;; accumulate recursive
;;
(define (accumulate-rec combiner null-value term a next b)
  (define (f n acc)
    (if (> n b)
	acc
	(f (inc n)
	   (combiner acc (term n)))))
  (f a null-value))

(define (filtered-accumulate predicate combiner
			     null-value term a next b)
  (define (fn n acc)
    (if (> n b)
	acc
	(if (predicate n)
	    (fn (next n) (combiner acc (term n)))
	    (fn (next n) acc))))
  (fn a null-value))

;; ex 1.33 1
(define (sum-of-square-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; ex 1.33 2
(define (prod-pos-rel-primes n)
  (define (rel-prime d)
    (= 1 (gcd n d)))
  (filtered-accumulate rel-prime * 1 identity 1 inc n))

;; 1.3.2 : Constructing procedures using Lambda
;; Define function, without assigning a symbol
;; (lambda (parameters) body)
((lambda (x y)
   (* x y)) 2 5 )
;; => 10: defining and executing (* 2 5)
;; lambda can be used in any context where we can use a procedure name
;;
;; lambda can be used for defining temp variables
;;
;; with lambda
(define (gcd-with-lambda a b)
  (lambda (r)
    (if (= 0 r)
	b
	(gcd-with-lambda b r)))
  (remainder a b))
;;
;; with let
(let ((a 1)
      (b 2))
  (+ a b))

;; more general (let ((var1 expr1) (var2 exp2)...) body)
;; let allows for local binding
;; values are calculated outside let binding!
;;
;; 1.3.3 Procedures as General Methods
;; 2 examples of higher order programming: roots of equation and fixed points
(define (search-half-interval f x y)
  (define (average x y)
    (/ (+ x y) 2))
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average x y)))
    (if (close-enough? x y)
	midpoint
	(let ((midpoint-value (f midpoint)))
	  (cond
	   ((positive? midpoint-value)
	    (search-half-interval f x midpoint))
	   ((negative? midpoint-value) (search-half-interval f midpoint y))
	   (else midpoint))))))

(define (half-interval-method f a b)
  (let ((value-a (f a))
	(value-b (f b)))
    (cond
     ((and (positive? value-a)
	   (negative? value-b))
      (search-half-interval f b a))
     ((and (positive? value-b)
	   (negative? value-a))
      (search-half-interval f a b))
     (else
      (error "Values are not of the oposite sign" a b value-a value-b)))))

;; chciken specific: tracing
(require-library trace)
(import (prefix trace t:))

(define tolerance 0.0001)

(define (fixed-point f initial-guess)
  (define (close-enough? x y)
    (> tolerance (abs (- x y))))
  (define (helper guess)
   (print guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
	  next
	  (helper next))))
  (helper initial-guess))

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 0.001))

(define golden-ration
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; ex 1.36
(define (log-fn-average)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))


;; ex 1.37
(define (cont-frac n d k from)
  (if (> from k)
      (/ (n from) (d from))
      (/ (n from)
	 (+ (d from)
	    
	    (cont-frac n d k (+ from 1))))))


;; Procedures as return values
(define (damp f)
  (lambda (x)
   (average x (f x))))

(define (sqrt x)
  (fixed-point
   (damp
    (lambda (y) (/ x y))) 1))



;; Newtons method

;; Step 1: derivatives
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; Newtons transformation
(define (newton-transform f)
  (lambda (x) (- x (/ (f x)
		 ((deriv f) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f)
	       guess))


(define sqrt
  (lambda (x)
    (newton-method
     (lambda (y) (- (square y) x))
     1.0)))

;; ex 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


;; ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5) ;; 21



;; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; ex 1.43
(define dec (lambda (x) (- x 1)))

(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (dec n)))))

;; ex 1.44
(define (smoothed f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))


;; ex 1.45
(define (cube x)
  (fixed-point
   (damp (lambda (y)  (/ x (* y y)))) 1))

(define (forth-root x)
  (fixed-point
   ((repeated damp 2)
    (lambda (y) (/ x (* y y y )))) 1.0))

(define (fifth-root x)
  (fixed-point
   ((repeated damp 2)
    (lambda (y) (/ x (* y y y y )))) 1.0))

(define (nth-root x n)
  (let ((d (floor (/ (log x) (log 2)))))
    (fixed-point
     ((repeated damp d)
      (lambda (y) (/ x (expt y (dec n))))) 1.0)))

;; ex 1.46
(define (iter-impr good-enuf? improve)
  (define (iter guess)
    (if (good-enuf? guess)
	guess
	(iter (improve guess))))
  iter)

(define (sqrt x)
  (iter-impr (lambda (y) (< (abs (- (* y y) x)) tolerance))
	     (damp (lambda (y) (/ x y)))))

(define (fixed-point f)
  (iter-impr (lambda (y) (< (abs (- (f y) y)) tolerance))
	     (lambda (y) (f y))))
