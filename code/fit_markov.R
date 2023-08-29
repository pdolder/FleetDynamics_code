
fit_markov <- function(train.data) {

## fit with multinom from nnet
library(nnet)

	print(m <- multinom(state ~ state.tminus1:month + state.tminus1:I(month^2) + ToHome + FromHome, data = train.data, maxit = 1e3))

	return(list(fit = m, sum.fit = coef(m)))

}
