predict_gravity <- function(train.data = train.data, Closure = Closure) {

		# N = CPUE/sum(CPUE) - we use profit per unit effort
			
		if(!Closure) {

		res <- train.data %>% group_by(month, state) %>%
			summarise(prop = profit.mean/profit.tot) %>%
		pivot_wider(names_from = month, values_from = prop) %>% 
		as.data.frame() 

		}


		if(Closure) {
		
		## If there's a closure, we first need to exclude the closure
		## areas - set profit from these to zero and recalc total
		## profit
		train.data[train.data$state %in% c("Closure_1", "Closure_2"), "profit.mean"] <- 0
	
		res  <- train.data %>% group_by(month) %>% mutate(profit.tot = sum(profit.mean)) %>%
			group_by(month, state) %>% summarise(prop = profit.mean / profit.tot) %>% 
			pivot_wider(names_from = month, values_from = prop) %>% 
		as.data.frame() 
		
		}

return(res)

}
