# Get the pertinent summary data for negative binomial regression
# Requires a 'glm.nb' object from the 'MASS' library
# Parameters:
# 	los.nb = glm.nb object
# 	round_digits = number of digits for the output to be rounded to (default = 5)
nb_summary <- function(los.nb, round_digits=5) {
	est.nb <- cbind(IRR = coef(los.nb), confint(los.nb))
	round(cbind(exp(est.nb), 'p-value' = coef(summary(los.nb))[,4]), round_digits)
}
