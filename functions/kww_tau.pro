;This function calulates the effective time constant for a KWW fit, given
;the fitting parameters Tau(fit) and Beta(fit)

FUNCTION kww_tau, tau, beta

Return, (tau/beta)* Gamma(1/beta)

END
