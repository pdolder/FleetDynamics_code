##############################################################
## Function to fit the model type to the training data
##
## Returns the fitted model 
## For the process-based models, returns the 
##############################################################

fit_model <- function(train.data = NULL, type = NULL) {

	if(type == "Gravity") {
	
	res <- print("Nothing to do")
	
	}

	if(type == "Markov") {

		res <- fit_markov(train.data)

		}

	if(type == "RUM") {

		res <- fit_rum(train.data, reparam = FALSE)
	
		}

	if(type == "RUM_reparam") {
	
	   	res <- fit_rum(train.data, reparam = TRUE)
	
	}


	if(type == "DSVM") {

	res <- print("Nothing to do")
	
	
	}
  
  return(res)

  }
