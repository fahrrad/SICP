;; Chapter 2: Building Abstractions with data
;; chapter 1 was about procedures and processes, ch2 is about the data they operate upon

;; goal is again to elevate the conceptual level we cn reason and talk about
;; programs
;; ex 2.1

(define (flip-sign n)
  (* -1 n))

(define (gcd a b)
  (let ((r (remainder a b)))
    (if (= 0 r)
	b
	(gcd b r))))

(define (make-rat n d)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons
       (/ n g)
       (/ d g))))
  (cond
   ((= 0 d) (error "Cannot make a rational with denominator 0"))
   ((> d 0) (make-rat n d))
   ((make-rat (flip-sign n) (flip-sign d)))))


;; Ex 2.2
(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (make-segment point-a point-b)
  (cons point-a point-b))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
		       (x-point (end-segment segment)))
	      (average (y-point (start-segment segment))
		       (y-point (end-segment segment)))))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-rectangle point-a point-b)
  (cons point-a point b))


;; pairs can be implemented as a function
(define (cons x y)
  (lambda (z)
    (cond ((= z 1) y)
	  ((= z 0) x)
	  (else (error "argument has to be 1 or 0")))))

(define cdr
  (lambda (pair)
    (pair 1)))

(define car
  (lambda (pair)
    (pair 0)))
;; ex 2.4
(define cons
  (lambda (a b)
    (lambda (f) (f a b))))

(define car
  (lambda (pair)
    (pair (lambda (x y) x))))

(define cdr
  (lambda (pair)
    (pair (lambda (x y) y))))

;; ex 2.5
(define cons
  (lambda (x y)
    (* (expt 2 x) (expt 3 y))))


;; Church Numerals
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(add-1 (lambda (x) x))

;;
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
		    (lower-bound y))
		 (- (upper-bound x)
		    (upper-bound y))))



(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (car interval))

(define (lower-bound interval)
  (cdr interval))

;; Hierarchical data structures
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 4 9 16 25 36))

(define (length-rec items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) 
                     (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1) list2))))


(define last-pair
  (lambda (list)
    (if (null? (cdr list))
	(car list)
	(last-pair (cdr list)))))




;; extended example : interval arithmatic
(define (p r1 r2)
  (/ 1 (+ (/ 1 r1) (/ 1 r2))))

(define (make-interval from to)
  ((cons from to)))

(define (from interval)
  (car interval))

(define (to interval)
  (cdr interval))

(define add-interval
  (lambda (i1 i2)
    (make-interval
     (+ (from i1) (from i2))
     (+ (to i1) (to i2)))))


(define (mul-interval i1 i2)
  (let ((p1 (* (from i1) (from i2)))
	(p2 (* (from i1) (to i2)))
	(p3 (* (to i1) (from i2)))
	(p4 (* (to i1) (to i2))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval i1 i2)
  (let ((r-i2
	 (make-interval (/ 1.0 (from i2))
			(/ 1.0 (to i2)))))
    (mul-interval i1 r-i2)))

(define (sub-interval i1 i2)
  (make-interval (- (from i1) (from i2))))


(define (width i)
  (let ((mx (max (from i) (to i)))
	(mn (min (from i) (to i))))
    (/ (- mx mn) 2.0)))

(define (sign x)
  (cond ((< x 0) -1)
	((> x 0) 1)
	(else 0)))

(define (make-interval-with-center center tolerance)
  (let ((half-tolerance (/ tolerance 2))
	 (l-bound (- center half-tolerance))
	 (h-bound (+ center half-tolerance)))
    (make-interval l-bound h-bound)))

;; 2.2 Hierachical data and closure property
;;

;; List
(define l1
  (cons 1
       (cons 2
	     (cons 3 '()))))

;; ===
(define l2
  (list 1 2 3))

(define (list-ref l i)
  (if (= i 0)
      (car l)
      (list-ref (cdr l) (- i 1))))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (length l)
  (define (length-iter items count)
    (if (null? items)
	count
	(length-iter (cdr items) (+ 1 count))))
  (length-iter l 0))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
	    (append (cdr l1)
		    l2))))

;; excersice 2.18 : Last pair
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;; excersise 2.19: reverse
(define (reverse l)
  (if (null? (cdr l))
      (car l)
      (cons (reverse l) (last-pair l))))


;; ex 2.20
(define same-parity
  (lambda numbers
    (define iter (lambda (numbers parity))
      (let ((n (car numbers)))
	(if (= parity
	       (n . remainder . 2))
	    (cons n (iter (cdr numbers) parity))
	    (iter (cdr numbers) parity))))))


;; ex
