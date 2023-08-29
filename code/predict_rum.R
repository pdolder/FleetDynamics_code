predict_rum <- function(train.data = train.data, model_fit = model_fit, Closure = Closure) {

		if(!Closure) {

		LD <- train.data

		LD.predictions <- cbind(data.frame(month = LD[LD$choice == TRUE, "month"]),
					mlogit:::predict.mlogit(model_fit, newdata = LD))
		
		LD.predictions <- LD.predictions %>% pivot_longer(cols = A:H) %>%
			group_by(month, name) %>% summarise(prop = sum(value)/n())

		LD.predictions$month <- as.numeric(LD.predictions$month)

		res  <- pivot_wider(LD.predictions, names_from = month, 
					   values_from = prop, values_fill = list(prop = 0)) %>%
				as.data.frame()
	
		}

	
		if(Closure) {

		LD.closure <- train.data 
		LD.closure[grep("Closure", rownames(LD.closure)),c("profit")] <- -1e9 ## very big loss, essentially don't go here!
		LD.closure[grep("Closure", rownames(LD.closure)),c("logOddscosts")] <- 1e10 ## very big loss, essentially don't go here!
	        LD.closure[grep("Closure", rownames(LD.closure)),c("logOddsval")]   <- 1e-10 ## very big loss, essentially don't go here!

		close.predictions <- cbind(data.frame(month = LD.closure[LD.closure$choice == TRUE, "month"]),
					mlogit:::predict.mlogit(model_fit, newdata = LD.closure))
		
		close.predictions <- close.predictions %>% pivot_longer(cols = A:H) %>%
			group_by(month, name) %>% summarise(prop = sum(value)/n())

		close.predictions$month <- as.numeric(close.predictions$month)

		res <- pivot_wider(close.predictions, names_from = month, 
					   values_from = prop, values_fill = list(prop = 0)) %>%
				as.data.frame()
	
		}

return(res)

}
