###############################
## Predictions from all fleet 
## dynamics models
###############################

# load the simulation data
load('../Simulation_data_for_models.RData')


## Load ALL the functions
lapply(list.files("../funcs/", full.names = T), source)

range(logs$year)

first.yr <- 20
last.yr  <- 39
  
  
runs <- lapply(first.yr:(last.yr-4), function(x) {
 
  trn.yrs  <- x:(x+2)
  tst.yrs <- (x+3):(x+4)

  close <- ifelse(tst.yrs >= 30, TRUE, FALSE)
 
  print(paste("training on", paste(trn.yrs, collapse = ","), sep = " "))
  print(paste("testing on", paste(tst.yrs, collapse = ","), sep = " "))

  ## Prepare data
 
  # DSVM
  DSVM_data <- prepare_data(logs = logs, flt = 3, train.yrs = trn.yrs, test.yrs = tst.yrs, type = "DSVM")
  
 
  # DSVM
  fit_model(train.data = DSVM_data[["train.dat"]], type = "DSVM")
  
  
 # DSVM
  DSVM_predictions <- make_predictions(train.data = DSVM_data[["train.dat"]], test.yrs = tst.yrs, type = "DSVM", 
                                       Closure = close, DSVM_optim = TRUE)

#  opt_val <- optimize(f = optim_dsvm, interval = c(1,500), train.data = DSVM_data[["train.dat"]], Closure = FALSE)
#  round(opt_val$minimum,0)
 
 
  
  return(list(DSVM_predictions = DSVM_predictions))
  
  
  })

runs2 <- runs

save(runs2, file = "DSVM_Runs.RData")

