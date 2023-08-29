#####################################################
## Function to format data for the model fitting
## procedures
#####################################################

prepare_data <- function(logs = NULL, flt = 3, train.yrs = NULL, test.yrs = NULL, type = NULL) {


  library(tidyverse)
  train.dat <- filter(logs, fleet == flt, year %in% train.yrs)
  test.dat  <- filter(logs, fleet == flt, year %in% test.yrs)
  
  
  if(type == "Markov") {
   
	  res <- prepare_markov(train.dat = train.dat, test.dat = test.dat)
       
  }
  
  if(type %in% c("RUM","RUM_reparam")) {
    
	  res <- prepare_rum(train.dat = train.dat, test.dat = test.dat)
    
  }
  
  
  if(type == "Gravity") {

	  res <- prepare_gravity(train.dat = train.dat, test.dat = test.dat)
    
   
  }
  
  if(type == "DSVM") {
  
	  res <- prepare_dsvm(train.dat = train.dat, test.dat = test.dat)
    
  }
 
 if(type == "Ensemble") {

	train.dat <- filter(train.dat, state != "home")
	res <- list(train.dat = train.dat, test.dat = test.dat) 

 }

 return(res)

}
