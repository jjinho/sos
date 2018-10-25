# Get the pertinent summary data for conditional logistic regression
# Requires a 'clogit' object from the 'survival' library
# Parameters:
# 	s.out = summary of a clogit object (e.g. summary(clogit(...)))
# 	round_digits = number of digits for the output to be rounded to (default = 5)
clr_summary <- function(s.out, round_digits=5) {
	round(cbind(OR = s.out$conf.int[,1], '2.5 %' = s.out$conf.int[,3], '97.5 %' = s.out$conf.int[,4], 'p-value' = coef(s.out)[,5]), round_digits)
}
