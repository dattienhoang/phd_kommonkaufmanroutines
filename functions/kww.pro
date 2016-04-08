;This function returns the value of the Kohlrausch-Williams-Watt (KWW)
;stretched exponential.  It is meant to be used with the MPFIT
;routines for fitting our experimental autocorrelation functions

;Input parameters:
; X     - The array of independent variable values
; p     -A three element array specifying the prefactor, time constant,
;                         and stretching exponent replectively.


FUNCTION kww, x, p

f = p[0] * Exp(-(x/p[1])^p[2])

RETURN, f


END
