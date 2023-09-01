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
 
  ## past share
  ## Nothing to do

  # Gravity
  Gravity_data <- prepare_data(logs = logs, flt = 3, train.yrs = trn.yrs, test.yrs = tst.yrs, type = "Gravity")
  
  # RUM
  RUM_data <- prepare_data(logs = logs, flt = 3, train.yrs = trn.yrs, test.yrs = tst.yrs, type = "RUM")

  # RUM reparameterised
  RUMReparam_data <- RUM_data  ## now is same data input to save time

  # Markov
  Markov_data <- prepare_data(logs = logs, flt = 3, train.yrs = trn.yrs, test.yrs = tst.yrs, type = "Markov")
  
  # DSVM
  DSVM_data <- prepare_data(logs = logs, flt = 3, train.yrs = trn.yrs, test.yrs = tst.yrs, type = "DSVM")
  
  # Ensemble 
  Ensemble_data <- prepare_data(logs = logs, flt = 3, train.yrs = trn.yrs, test.yrs = tst.yrs, type = "Ensemble")
  
  
  ## Fit models

  ## Past share 
  ## Nothing to do
  
  # Gravity
  fit_model(train.data = Gravity_data[["train.dat"]], type = "Gravity")
  
  # RUM
  RUM_fit <- fit_model(train.data = RUM_data[["LD"]], type = "RUM")
  #RUM_fit[["sum.fit"]]
  
  RUMReparam_fit <- fit_model(train.data = RUMReparam_data[["LD"]], type = "RUM_reparam")
  #RUM_fit[["sum.fit"]]
  
  # Markov
  Markov_fit <- fit_model(train.data = Markov_data[["train.dat"]], type = "Markov")
  
  
  # DSVM
  fit_model(train.data = DSVM_data[["train.dat"]], type = "DSVM")
  
  
  ## make predictions 
 
  # past share
  PastShare_predictions <- make_predictions(train.data = Gravity_data[["train.dat"]], 
                                          test.yrs = tst.yrs, model_fit = NULL, type = "past_share", Closure = close)

  # Gravity
  Gravity_predictions <- make_predictions(train.data = Gravity_data[["train.dat"]], 
                                          test.yrs = tst.yrs, model_fit = NULL, type = "Gravity", Closure = close)
  
  # RUM
  RUM_predictions <- make_predictions(train.data = RUM_data[["LD"]], test.yrs =
                                        tst.yrs, model_fit = RUM_fit[["fit"]], type
                                      = "RUM", Closure = close)
  
  
  # RUM reparameterised
  RUMReparam_predictions <- make_predictions(train.data = RUMReparam_data[["LD"]], test.yrs =
                                        tst.yrs, model_fit = RUMReparam_fit[["fit"]], type
                                      = "RUM_reparam", Closure = close)
  
  # Markov
  Markov_predictions <- make_predictions(train.data = Markov_data[["train.dat"]], model_fit = Markov_fit[["fit"]],
                                         test.yrs = tst.yrs, type = "Markov", Closure = close)
  # DSVM
  DSVM_predictions <- make_predictions(train.data = DSVM_data[["train.dat"]], test.yrs = tst.yrs, type = "DSVM", 
                                       Closure = close)
  
  # Ensemble - lm
  lm_predictions <- make_predictions(train.data = Ensemble_data[["train.dat"]], test.yrs = tst.yrs, 
                                           train.yrs = trn.yrs, type = "Ensemble_lm", Closure = close, 
                                           model_predictions = list(Gravity_predictions = Gravity_predictions,
                                                                    RUM_predictions = RUM_predictions,
                                                                    RUMReparam_predictions = RUMReparam_predictions,
                                                                    Markov_predictions = Markov_predictions,
                                                                    DSVM_predictions = DSVM_predictions))
  
  # Ensemble - dirichlet
  dirichlet_predictions <- make_predictions(train.data = Ensemble_data[["train.dat"]], test.yrs = tst.yrs, 
                                           train.yrs = trn.yrs, type = "Ensemble_dirichlet", Closure = close, 
                                           model_predictions = list(Gravity_predictions = Gravity_predictions,
                                                                    RUM_predictions = RUM_predictions,
                                                                    RUMReparam_predictions = RUMReparam_predictions,
                                                                    Markov_predictions = Markov_predictions,
                                                                    DSVM_predictions = DSVM_predictions))
  
  
  ## The observations
  ## We should ensure all states are captured, even if not 
  ## visited

  Obs <- lapply(1:2, function(i) { 
    res <- Gravity_data[["test.dat"]] %>% filter(year == tst.yrs[i]) %>%
    group_by(month, state) %>%
    summarise(N = n()) %>% arrange(state) %>% 
    pivot_wider(names_from = month, values_from = N) %>%
    as.data.frame() 
  return(res)
  })
  
  Obs <- lapply(1:2, function(i) {
    
    Obs2 <- Obs[[i]]
    
    Obs2[is.na(Obs2)] <- 0
    Obs2[,2:13] <- apply(Obs2[,2:13], 2,  function(x) { x/sum(x) }) 
    return(Obs2)
  })
  
  
  
  return(list(PastShare_predictions = PastShare_predictions,
	      Gravity_predictions = Gravity_predictions,
              RUM_predictions = RUM_predictions,
	            RUMReparam_predictions = RUMReparam_predictions,
              Markov_predictions = Markov_predictions,
              DSVM_predictions = DSVM_predictions,
              lm_predictions = lm_predictions,
	            dirichlet_predictions = dirichlet_predictions,
              Observations = Obs
              ))
  
  
  })
       
save(runs, file = "Model_Runs.RData")

