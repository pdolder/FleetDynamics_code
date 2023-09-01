fit_rum <- function(train.data, reparam = FALSE) {

	## fit with mlogit

	if(!reparam) {

#	print(m <- mlogit::mlogit(choice ~ profit | month + I(month^2), data = train.data, print.level = 5))
        print(m <- mlogit::mlogit(choice ~ val + costs | month + I(month^2), data = train.data, print.level = 5))

	
	} else {
	
	print(m <- mlogit::mlogit(choice ~ logOddsval + logOddscosts | month + I(month^2), data = train.data, print.level = 5))
	
	}

	return(list(fit = m, sum.fit = summary(m)))

}
