####################################################
## Function to make predictions of the % of effort
## in each state
##
## Returns a matrix with the % of effort per state from the prediction
####################################################

make_predictions <- function(train.data = NULL, model_fit = NULL, test.yrs = NULL, train.yrs = NULL, 
			     type = NULL, Closure = c(FALSE,FALSE), model_predictions = NULL, DSVM_optim = FALSE, Intercept = TRUE) {

	library(tidyverse)
        	
	ny <- length(test.yrs)
	predictions <- list()

	if(type == "past_share") {

	for(y in seq_len(ny)) {

	predictions[[y]]  <- predict_past_share(train.data = train.data, Closure = Closure[y]) 

	}

	return(predictions)

	}


	if(type == "Gravity") {

		for(y in seq_len(ny)) {

       predictions[[y]] <- predict_gravity(train.data = train.data, Closure = Closure[y])

	
		}

	return(predictions)

	
	}

	if(type == "Markov") {
		
	
for(y in seq_len(ny)) {

	predictions[[y]] <- predict_markov(train.data = train.data, model_fit = model_fit, Closure = Closure[y])

		}

	return(predictions)

	}

if(type %in% c("RUM", "RUM_reparam")) {

for(y in seq_len(ny)) {

	predictions[[y]] <- predict_rum(train.data = train.data, model_fit = model_fit, Closure = Closure[y])

		      }

		return(predictions)

}


if(type == "DSVM") {
	  
for(y in seq_len(ny)) {

	if(DSVM_optim) {

	  opt_val <- optimize(f = optim_dsvm, interval = c(1,800), train.data = train.data, Closure = Closure[y])
          SIGMA   <- round(opt_val$minimum,0)
 	  predictions[[y]] <- predict_DSVM(train.data = train.data, Closure = Closure[y], SIGMA = SIGMA)

	} else {
	
	SIGMA <- 340
	predictions[[y]] <- predict_DSVM(train.data = train.data, Closure = Closure[y], SIGMA = SIGMA)
	  
	}
	  }

	  return(predictions)

	}
	
	
if(type %in% c("Ensemble_lm","Ensemble_dirichlet")) {

  
	 for(y in seq_len(ny)) {

	predictions[[y]] <- predict_ensemble(train.data = train.data, model_predictions = model_predictions, type = type, train.yrs = train.yrs, test.yrs =test.yrs[y], Closure = Closure[y], y = y, Intercept = Intercept)

	    
	 }
	   
	return(predictions)

	}
	


}  # end function

