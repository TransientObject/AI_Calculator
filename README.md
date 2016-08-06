# AI_Calculator
Basic Calculator with simple arithmetic operations using LISP

README


;-----------------------------------------------------------------------------------------------------------------
;	PROJECT COMPONENETS
;-----------------------------------------------------------------------------------------------------------------

* Derive unary and binary expressions involving 
	* addition
	* subtraction
	* multiplication
	* division
	* unary minus
	* exponential (a ^ b)
	* eulers function (e ^ x)
	* natural logarithms
	* sin function
	* cosine function
	* tan function
	* sqrt function

* For the same set of Functions, output can be evaluated on a particular value for x

* simplification of the equations
	* x + 0 = x
	* x * 0 = 0
	* x * 1 = x
	* x - 0 = x
	* x / 1 = x
	* x ^ 0 = 1
	* x ^ 1 = x
		x can be a atom or a list. still the above equations will hold true.

* Also, division by zero is handled in case of evaluating a differential

* Date: Sep 16 2013

* Programming Language: LISP

* Compiler Used: GNU Common LISP

* IDE used: VIM

* Environment: Ubuntu 13.04 (Debian)




;---------------------------------------------------------------------------------------------------------------
;	HOW TO PROCEED
;---------------------------------------------------------------------------------------------------------------


The Source Code is Present in One file name calculator.lsp

Steps to load and execute the file are

a) run gcl in ubuntu machine by typing "gcl" from the terminal

b) Once in the terminal, type (load "src\calculator.lsp") to the load the functions into the compiler

c) To deriv a function, type in (deriv '(<function to be derived>) 'x). This function takes 3 arguments. the function to be derived has to be an atom or a list. Deriv uses splus, sminus, smult, sdiv and sexpt to simplify the output

d) To derive and evaluate a function, call (deriv-eval '(<function to be derived>) 'x <value>). Again the function can either be an atom or a list

e) To simplify an equation, call simplify('(<function to be derived>) 'x). The function can be an atom or a list. simplifies trivial cases in multiplication, negation, division, addition, subtraction, exponents
	


;---------------------------------------------------------------------------------------------------------------
;	Examples and Outputs for the aforementioned functions 
;---------------------------------------------------------------------------------------------------------------


	1) (deriv '(/ (* (sin x) 100) (+ (+ (/ 80 (+ x (tan x))) 20) (* 0.7213 (log x)))) 'x)

	Output - 
		(/ (- (* (+ (+ (/ 80 (+ X (TAN X))) 20) (* 0.7213 (LOG X)))
		         (* (COS X) 100))
		      (* (* (SIN X) 100)
		         (+ (/ (- 0 (* 80 (+ 1 (+ 1 (* (TAN X) (TAN X))))))
		               (* (+ X (TAN X)) (+ X (TAN X))))
		            (* (/ 1 X) 0.7213))))
		   (* (+ (+ (/ 80 (+ X (TAN X))) 20) (* 0.7213 (LOG X)))
		      (+ (+ (/ 80 (+ X (TAN X))) 20) (* 0.7213 (LOG X)))))


    2) (deriv-eval '(/ (sin x) (- (+ (* (cos x) (tan x)) (log x)) (* (sqrt x) x))) 'x 5)

    Output -  -0.05176398571365319

    3) (deriv '(* (* (exp x) (log x)) (+ (expt x 1) (/ (- 1 x) (sqrt x)))) 'x)

    Output -
		(+ (* (+ 1
		         (/ (- (* (SQRT X) -1) (* (- 1 X) (/ 1 (* 2 (SQRT X)))))
		            (* (SQRT X) (SQRT X))))
		      (* (EXP X) (LOG X)))
		   (* (+ (* (/ 1 X) (EXP X)) (* (EXP X) (LOG X)))
		      (+ (EXPT X 1) (/ (- 1 X) (SQRT X)))))

	4) (deriv-eval '(+ (* x (log (expt x 2)))  (* (exp x) (/ (sin x) (sqrt x)))) 'x 200)

	Output -  -1.961685290748993E85


	5) (simplify '(+ (+ (* (* x (log (+ x 0))) 0) (sqrt x)) (/ x 1)) 'x)

	Output - (+ (SQRT X) X)

	6) (simplify '(- (+ (tan (expt x 0)) (/ (cos (* x 0)) (sqrt x))) 0) 'x)

	Output - (+ (TAN 1) (/ (COS 0) (SQRT X)))

	Note that this is just a simplification and hence need not solve the equations.

	7) (deriv-eval '(+ (cos (sin (log (exp x)))) (/ 1 x)) 'x 76)

	Output -  -0.4423040514988672

	8) (simplify '(+(*(*(* (* x (expt x 0)) (- x 0)) (+ (* x 5) 1)) (+ (- x x) 1)) 1) 'x)

	Output - (simplify '(+(*(*(* (* x (expt x 0)) (- x 0)) (+ (* x 5) 1)) (+ (- x x) 1)) 1) 'x)

	9) (deriv '(+ (* (* (/ x (sqrt x)) (/ (sin x) (cos x))) (/ 1 (tan x))) (* x (exp x))) 'x)

	Output - 
		(+ (+ (* (/ (- 0 (+ 1 (* (TAN X) (TAN X)))) (* (TAN X) (TAN X)))
		         (* (/ X (SQRT X)) (/ (SIN X) (COS X))))
		      (* (+ (* (/ (- (* (COS X) (COS X)) (* (SIN X) (- (SIN X))))
		                  (* (COS X) (COS X)))
		               (/ X (SQRT X)))
		            (* (/ (- (SQRT X) (* X (/ 1 (* 2 (SQRT X)))))
		                  (* (SQRT X) (SQRT X)))
		               (/ (SIN X) (COS X))))
		         (/ 1 (TAN X))))
		   (+ (* (EXP X) X) (EXP X)))

	10) (deriv-eval '(- (+ (* (* (- x (log x)) (/ (sin x) x)) (expt (tan x) 1))) (* x (exp x))) 'x 46)

	output - -4.4631761276831505E21
