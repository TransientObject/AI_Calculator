;----------------------------------------
; 	deriv - main derive function
;
;   Owner - Priya Narayana Subramanian
;----------------------------------------
(defun deriv (expr var)
	(if (atom expr)
    	(if (equal expr var) 1 0)      
    	(cond                       
    		((eq '+ (first expr))								; PLUS 
            	(derivplus expr var))
			((eq '- (first expr))								; MINUS
				(if (eq (length expr) 2)
					(if (eq (second expr) var) (list '- 1) 0)	; UNARY MINUS
					(derivsub expr var)							; BINARY MINUS
				))
        	((eq '* (first expr))	    						; MULT
            	(derivmult expr var))
			((eq '/ (first expr))								; DIV
				(derivdiv expr var))
			((eq 'expt (first expr))							; EXPONENT
				(derivexponents expr var))
			((eq 'exp (first expr))								; e ^ f(g)
				(deriveuler expr var))
			((eq 'sin (first expr))								; sin
				(derivsin expr var))
			((eq 'cos (first expr))								; cos
				(derivcos expr var))
			((eq 'tan (first expr))								; tan
				(derivtan expr var))
			((eq 'sqrt (first expr))							; sqrt
				(derivsqrt expr var))
			((eq 'log (first expr))								; log
				(derivlog expr var))
        	(t													; Invalid
            	(error "Invalid Expression!"))
     	)
  	)
)

;---------------------------------------------
;	deriv plus - derive logic for addition
;---------------------------------------------
(defun derivplus (expr var)
    (splus
          (deriv (second expr) var)
          (deriv (third expr) var)
    )
)

;----------------------------------------------
;	deriv sub - derive logic for subtraction
;----------------------------------------------
(defun derivsub (expr var)
    (sminus
          (deriv (second expr) var)
          (deriv (third expr) var)
    )
)


;------------------------------------------------------
;   deriv mult - derive logic for multiplication
;------------------------------------------------------
(defun derivmult(expr var)
	(splus
			(smult (deriv(third expr) var) (second expr))
			(smult (deriv(second expr) var) (third expr))	
	)
)

;-----------------------------------------------------
;   deriv division - derive logic for division
;-----------------------------------------------------
(defun derivdiv(expr var)
	(sdiv
		(sminus (smult (third expr) (deriv(second expr) var))
			(smult (second expr) (deriv(third expr) var))
		)
		(smult (third expr) (third expr))
	)
)


;----------------------------------------
;   deriv exponents - derives a ^ b
;----------------------------------------
(defun derivexponents(expr var)
	(smult
		(smult (third expr) (sexpt (second expr) (sminus (third expr) '1)))
		(deriv(second expr) var)
	)
)

;----------------------------------------
;   deriv euler - derives e ^ x
;----------------------------------------
(defun deriveuler(expr var)
	(smult
		(list 'exp (second expr))
		(deriv(second expr) var)
	)
)

;----------------------------------------
;   deriv sin - sine of a radian
;----------------------------------------
(defun derivsin(expr var)
	(smult
		(list 'cos (second expr))
		(deriv(second expr) var)
	)
)

;----------------------------------------
;   deriv cos - cosine of a radian
;----------------------------------------
(defun derivcos(expr var)
	(smult
		(sunaryminus (list 'sin (second expr)))
		(deriv(second expr) var)
	)
)

;-------------------------------------------------------
;   deriv tan - derives the tangent to a radian
;-------------------------------------------------------
(defun derivtan(expr var)
	(smult
		(splus 1 
			(smult (list 'tan (second expr)) (list 'tan (second expr)))
		)
		(deriv(second expr) var)
	)
)

;----------------------------------------
;   deriv sqrt - derives sqrt to any f(x)
;----------------------------------------
(defun derivsqrt(expr var)
	(smult
		(sdiv 1 
			(smult '2 (list 'sqrt (second expr)))
		)
		(deriv(second expr) var)
	)
)

;----------------------------------------
;   deriv log - logarithms
;----------------------------------------
(defun derivlog(expr var)
	(smult
		(sdiv 1 (second expr))
		(deriv(second expr) var)
	)
)

;----------------------------------------
;	splus - simplify addition
;----------------------------------------
(defun splus (x y)
    (if (numberp x)
        (if (numberp y)
            (+ x y)  										;	case where both arguments are numbers  
            (if (zerop x)									;	case where only x is an number 
                y 											;	case where x is zero	
                (list '+ x y)								;	case where x is a non zero number and y is not
            )
        )
        (if (and (numberp y) (zerop y))						;	case where x is not a number
            x												;	case where y is zero
            (list '+ x y)									;	case where it does not matter what y is
        )
    )
)

;----------------------------------------
;	sminus - simplify subtraction
;----------------------------------------
(defun sminus (x y)
    (if (numberp y)
        (if (numberp x)
            (- x y)  										;	case where both arguments are numbers  
            (if (zerop y)									;	case where only x is not an number and y is 
                x 											;	case where y is zero. anything - 0 = anything	
                (list '- x y)								;	case where x is a non zero number and y is not zero.
            )
        )
        (list '- x y)										;	case where it does not matter what x is, since y is not a number
    )
)


;----------------------------------------
;	smult - simplify multiplication
;----------------------------------------
(defun smult (x y)
	(if (or (and (numberp x) (zerop x)) (and (numberp y)(zerop y)) ) 
		0
		(if (and (numberp x) (numberp y))
			(* x y)
			(if (and (numberp x) (eq x 1))
				y
				(if (and (numberp y) (eq y 1))
					x
					(list '* x y)				
				)
			)		
		)
	)
)

;----------------------------------------
;	sdiv - simplify division
;----------------------------------------
(defun sdiv (x y)
	(if (and (numberp y) (eq y 1))
		x
		(list '/ x y)
	)
)

;----------------------------------------
;	simplify exponential
;----------------------------------------
(defun sexpt (x y)
	(if (and (numberp x) (numberp y))
		(expt x y)
		(if (and (numberp y) (eq y 0))
			1
			(if (and (numberp y) (eq y 1))
				x
				(list 'expt x y)		
			)
		)
	)
)

;---------------------------------------------------------------------------------------
;	deriv eval - derive a f(x) and then evaluate on the given value - main function
;---------------------------------------------------------------------------------------

(defun deriv-eval(expr var value)
	(setq y (deriv expr var))
	(substitute_val y var value)
)

;--------------------------------------------------------------
;	function that sets the value of the expression recursively
;--------------------------------------------------------------

(defun substitute_val(expr var value)
	(if (atom expr)
		(number_or_var expr value)
		(cond                       
    		((eq '+ (first expr))								; PLUS 
            	(evalplus expr var value))
            ((eq '- (first expr))								; MINUS
				(if (eq (length expr) 2)
					(evalunaryminus expr var value)				; UNARY MINUS
					(evalminus expr var value)					; BINARY MINUS
			))
	        ((eq '* (first expr))	    						; MULT
	            (evalmult expr var value))
			((eq '/ (first expr))								; DIV
				(evaldiv expr var value))
			((eq 'expt (first expr))							; EXPONENT
				(evalexpt expr var value))
			((eq 'exp (first expr))								; e ^ f(g)
				(evalexp expr var value))
			((eq 'sin (first expr))								; sin
				(evalsin expr var value))
			((eq 'cos (first expr))								; cos
				(evalcos expr var value))
			((eq 'tan (first expr))								; tan
				(evaltan expr var value))
			((eq 'sqrt (first expr))							; sqrt
				(evalsqrt expr var value))
			((eq 'log (first expr))								; log
				(evallog expr var value))
	    	(t													; Invalid
            	(error "Invalid not defined yet!"))
 		)
	)
)


;-------------------------------------------------------------------------------------------
;	chooses between a integer constant and a vairable constant that just got assigned
;-------------------------------------------------------------------------------------------
(defun number_or_var(var value)
	(if (numberp var) var value)
)

;-------------------------------------------------------------------------------------------
;	evaluates plus signs - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalplus(expr var value)
	(if (atom (second expr))
		(if(atom (third expr))
			(+ (number_or_var (second expr) value) (number_or_var (third expr) value))
			(+ (number_or_var (second expr) value) (substitute_val (third expr) var value))
		)
		(if(atom (third expr))
			(+ (substitute_val (second expr) var value) (number_or_var (third expr) value))
			(+ (substitute_val (second expr) var value) (substitute_val (third expr) var value)))
		)
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates unary minus signs - does it recursively (basically negates the value)
;-------------------------------------------------------------------------------------------
(defun evalunaryminus(expr var value)
	(if (atom (second expr))
		(- (number_or_var (second expr) value))
		(- (substitute_val (second expr) var value))
	)
)


;-------------------------------------------------------------------------------------------
;	evaluates minus signs - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalminus(expr var value)
	(if (atom (second expr))
		(if(atom (third expr))
			(- (number_or_var (second expr) value) (number_or_var (third expr) value))
			(- (number_or_var (second expr) value) (substitute_val (third expr) var value))
		)
		(if(atom (third expr))
			(- (substitute_val (second expr) var value) (number_or_var (third expr) value))
			(- (substitute_val (second expr) var value) (substitute_val (third expr) var value)))
		)
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates multiplication signs - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalmult(expr var value)
	(if (atom (second expr))
		(if(atom (third expr))
			(* (number_or_var (second expr) value) (number_or_var (third expr) value))
			(* (number_or_var (second expr) value) (substitute_val (third expr) var value))
		)
		(if(atom (third expr))
			(* (substitute_val (second expr) var value) (number_or_var (third expr) value))
			(* (substitute_val (second expr) var value) (substitute_val (third expr) var value)))
		)
	)
)


;-------------------------------------------------------------------------------------------
;	evaluates division signs - does it recursively
;-------------------------------------------------------------------------------------------
(defun evaldiv(expr var value)
	(if (eq (number_or_var (third expr) value) 0)
		(print "error. Cannot divide by zero")
		(if (atom (second expr))
			(if(atom (third expr))
				(/ (number_or_var (second expr) value) (number_or_var (third expr) value))
				(/ (number_or_var (second expr) value) (substitute_val (third expr) var value))
			)
			(if(atom (third expr))
				(/ (substitute_val (second expr) var value) (number_or_var (third expr) value))
				(/ (substitute_val (second expr) var value) (substitute_val (third expr) var value)))
			)
		)
	)
)


;-------------------------------------------------------------------------------------------
;	evaluates a ^ b signs - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalexpt(expr var value)
	(if (atom (second expr))
		(if(atom (third expr))
			(expt (number_or_var (second expr) value) (number_or_var (third expr) value))
			(expt (number_or_var (second expr) value) (substitute_val (third expr) var value))
		)
		(if(atom (third expr))
			(expt (substitute_val (second expr) var value) (number_or_var (third expr) value))
			(expt (substitute_val (second expr) var value) (substitute_val (third expr) var value)))
		)
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates euler function - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalexp(expr var value)
	(if (atom (second expr))
		(exp (number_or_var (second expr) value))
		(exp (substitute_val (second expr) var value))
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates sin of a radian - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalsin(expr var value)
	(if (atom (second expr))
		(sin (number_or_var (second expr) value))
		(sin (substitute_val (second expr) var value))
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates cosine of a radian - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalcos(expr var value)
	(if (atom (second expr))
		(cos (number_or_var (second expr) value))
		(cos (substitute_val (second expr) var value))
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates tan of a radian - does it recursively
;-------------------------------------------------------------------------------------------
(defun evaltan(expr var value)
	(if (atom (second expr))
		(tan (number_or_var (second expr) value))
		(tan (substitute_val (second expr) var value))
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates sqrt of a f(x) - does it recursively
;-------------------------------------------------------------------------------------------
(defun evalsqrt(expr var value)
	(if (atom (second expr))
		(sqrt (number_or_var (second expr) value))
		(sqrt (substitute_val (second expr) var value))
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates log of a f(x) - does it recursively
;-------------------------------------------------------------------------------------------
(defun evallog(expr var value)
	(if (atom (second expr))
		(log (number_or_var (second expr) value))
		(log (substitute_val (second expr) var value))
	)
)

;-------------------------------------
;	simplify routine definition
;-------------------------------------

(defun simplify(expr var)
	(if (atom expr)
		expr
		(cond
     		((eq '+ (first expr))
     			(splus (simplify (second expr) var) (simplify (third expr) var))
     		)
     		((eq '/ (first expr))
     			(sdiv (simplify (second expr) var) (simplify (third expr) var))
     		)
     		((eq '* (first expr))
     			(smult (simplify (second expr) var) (simplify (third expr) var))
     		)
     		((eq 'expt (first expr))
     			(sexpt (simplify (second expr) var) (simplify (third expr) var))
     		)
     		((eq '- (first expr))
     			(if (eq (length expr) 2)
     				(sunaryminus (simplify (second expr) var))
     				(sminus (simplify (second expr) var) (simplify (third expr) var))
     			)
     		)
     		((eq 'exp (first expr))								; e ^ f(g)
				(list 'exp (simplify (second expr) var)))
			((eq 'sin (first expr))								; sin
				(list 'sin (simplify (second expr) var)))
			((eq 'cos (first expr))								; cos
				(list 'cos (simplify (second expr) var)))
			((eq 'tan (first expr))								; tan
				(list 'tan (simplify (second expr) var)))
			((eq 'sqrt (first expr))							; sqrt
				(list 'sqrt (simplify (second expr) var)))
			((eq 'log (first expr))								; log
				(list 'log (simplify (second expr) var)))
        	(t													; Invalid
            	(error "Invalid Expression!"))
	    )
	)
)

;-------------------------------------------------------------------------------------------
;	evaluates simplifies unary minus
;-------------------------------------------------------------------------------------------
(defun sunaryminus(x)
	(if(numberp x)
		(- x)
		(list '- x)
	)
)