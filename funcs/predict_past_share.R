predict_past_share <- function(train.data = train.data, Closure = Closure) {


if(!Closure) {

	res <- train.data %>% group_by(month, state) %>%
			summarise(N = sum(ObsN)) %>%
			mutate(prop = N / sum(N)) %>% select(-N) %>%
			pivot_wider(names_from = month, values_from = prop) %>%
			as.data.frame()
	}

	
if(Closure) {
	
	 res  <- train.data %>% filter(! state %in% c("Closure_1", "Closure_2")) %>%
		 group_by(month, state) %>%
			summarise(N = sum(ObsN)) %>%
			mutate(prop = N / sum(N)) %>% select(-N) %>%
			pivot_wider(names_from = month, values_from = prop) %>%
			as.data.frame()

	}

	return(res)

}
