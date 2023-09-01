predict_ensemble <- function(train.data = train.data, model_predictions = model_preditions, type = "ensemble_dirichlet", train.yrs = train.yrs, test.yrs = test.yrs, Closure = Closure, y = y, Intercept = FALSE) {

ny <- length(test.yrs)

## proportion per state in training data
train.obs <-filter(train.data, state != "home") %>%
group_by(year, month, state) %>%
summarise(N = n()) %>% group_by(year, month) %>%
mutate(sumN = sum(N)) %>% mutate(prop = N/sumN) %>%
as.data.frame()
	  
## by year
train_obs_yr <- lapply(1:3, function(x) {
res <- filter(train.obs, year == train.yrs[x]) %>%
select(month, state, prop)
res <- pivot_wider(res, names_from = month , values_from = prop) %>%
select(state:13) %>% arrange(state) %>% as.data.frame()
res[is.na(res)] <- 0
return(res)
	  })
	  
# mean across years
train_obs_mean <- train.obs %>%
select(month, state, prop) %>% 
pivot_wider(names_from = month, values_from = prop, 
values_fn = list(prop = mean, na.rm = TRUE)) %>%
select(state:13) %>% arrange(state) %>% as.data.frame()
train_obs_mean[is.na(train_obs_mean)] <- 0
	

## Prepare the data... we need to ensure that all areas are covered
## in the observations and the individual model predictions

areas <- c("A", "B", "D", "E", "Elsewhere", "G", "H", "F", "Closure_1", "Closure_2")

## Observations
if(any(!areas %in% train_obs_mean[,"state"])) {
  miss_areas <- areas[!areas %in% train_obs_mean[,"state"]]
  miss_df    <- data.frame(state = miss_areas, "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                           "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
  )
  colnames(miss_df) <- c("state", 1:12)
  train_obs_mean <- rbind(train_obs_mean, miss_df)
  train_obs_mean <- train_obs_mean[order(train_obs_mean[,"state"]),]
} 

model_predictions <- lapply(model_predictions, function(m) {

for(i in 1:2) {
colnames(m[[i]])[1] <- "state"	
if(any(!areas %in% m[[i]][,"state"])) {
  miss_areas <- areas[!areas %in% m[[i]][,"state"]]
  miss_df    <- data.frame(state = miss_areas, "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                           "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
  )
  colnames(miss_df) <- c("state", 1:12)
  m[[i]] <- rbind(m[[i]], miss_df)
  m[[i]] <- m[[i]][order(m[[i]][,"state"]),]
}
}
return(m)
  })

Gravity_pred <- model_predictions[["Gravity_predictions"]]
RUM_pred     <- model_predictions[["RUM_predictions"]]
RUMR_pred    <- model_predictions[["RUMReparam_predictions"]]
Markov_pred  <- model_predictions[["Markov_predictions"]]
DSVM_pred    <- model_predictions[["DSVM_predictions"]]

 
	  
if(type == "Ensemble_lm") {
 
## Get the predictions from the other models
Gr  <- lapply(seq_len(ny), function(x)  { cbind(data.frame(year = test.yrs[x], reshape2::melt(Gravity_pred[[x]]))) })
Gr  <- bind_rows(Gr)

Ma  <- lapply(seq_len(ny), function(x)  { cbind(data.frame(year = test.yrs[x], reshape2::melt(Markov_pred[[x]]) )) })
Ma  <- bind_rows(Ma)

ru  <- lapply(seq_len(ny), function(x)  { cbind(data.frame(year = test.yrs[x], reshape2::melt(RUM_pred[[x]]))) })
ru  <- bind_rows(ru)
	  
rr  <- lapply(seq_len(ny), function(x)  { cbind(data.frame(year = test.yrs[x], reshape2::melt(RUMR_pred[[x]]))) })
rr  <- bind_rows(rr)
	  
ds  <- lapply(seq_len(ny), function(x)  { cbind(data.frame(year = test.yrs[x], reshape2::melt(DSVM_pred[[x]]))) })
ds <- bind_rows(ds)
	  
## Now format for ensemble 
colnames(Gr)[3] <- colnames(Ma)[3] <- colnames(ru)[3] <- colnames(rr)[3] <- colnames(ds) <-  "month"
colnames(Ma)[2] <- colnames(ru)[2] <- colnames(rr)[2] <-  "state"
colnames(ds) <- c("year", "state", "month", "value")
	  
ens_data <- rbind(cbind(type = "Gravity", Gr),
cbind(type = "Markov", Ma),
cbind(type = "RUM", ru),
cbind(type = "RUMR", rr),
cbind(type = "DSVM", ds)
)

## Add the mean obs in the training data to the ensemble data
train_obs_mean_df <- reshape2::melt(train_obs_mean, id = c("state"), variable.name = "month") 

## prepare ensemble data
ens_data <- reshape2::dcast(ens_data, year + month + state ~ type, value.var = "value")
ens_data[is.na(ens_data)] <- 0
	  
ens_data$trainObs <- train_obs_mean_df$value[match(paste0(ens_data$month, ens_data$state),
paste0(train_obs_mean_df$month, train_obs_mean_df$state))]

ens_data_full <- ens_data


if(!Closure) {
	      
	     ens_data <- filter(ens_data_full, year == test.yrs)
	     superensemble  <- lm(trainObs ~ (Gravity + Markov + RUM + RUMR + DSVM):state, data = ens_data)
	    
	     ens_data$ensemble <- predict(superensemble)
	     ens_data$ensemble[ens_data$ensemble < 0] <- 0  ## negative proportions
	     
	     # needs to sum to 1
	     ens_data <- ens_data %>% group_by(month) %>% 
	       mutate(ensemble_adjusted = ensemble/sum(ensemble)) %>% 
	       as.data.frame() 
	     
	     # Now to format
	     ens_pred <- ens_data %>% select(month, state, ensemble_adjusted)
	     ens_pred <- pivot_wider(ens_pred, names_from = "month", values_from = ensemble_adjusted) %>%
	       as.data.frame()
	     
	     res <- ens_pred

	      
	      }
	    
	    
if(Closure) {
	      
	    ens_data <- filter(ens_data_full, year == test.yrs)
	    ens_data$trainObs <- ifelse(ens_data$state %in% c("Closure_1","Closure_2"), 0, ens_data$trainObs)
	    
	    ## Now need to make sure observations share sum to one
	    ens_data <- ens_data %>% group_by(year, month) %>%
	      mutate(Gravity = Gravity/sum(Gravity), Markov = Markov/sum(Markov),
	             RUM = RUM/sum(RUM), RUMR = RUMR/sum(RUMR),
	             DSVM = DSVM/sum(DSVM), 
	             trainObs = trainObs/sum(trainObs)) %>%
	      as.data.frame()
	   
	     superensemble  <- lm(trainObs ~ (Gravity + Markov + RUM + RUMR + DSVM):state, data = ens_data)
	     
	     ens_data$ensemble <- predict(superensemble)
	     ens_data$ensemble[ens_data$ensemble < 0] <- 0  ## negative proportions
	     
	     # needs to sum to 1
	     ens_data <- ens_data %>% group_by(month) %>% 
	       mutate(ensemble_adjusted = ensemble/sum(ensemble)) %>% 
	       as.data.frame() 
	     
	     # Now to format
	     ens_pred <- ens_data %>% select(month, state, ensemble_adjusted)
	     ens_pred <- pivot_wider(ens_pred, names_from = "month", values_from = ensemble_adjusted) %>%
	       as.data.frame()
	     
	     res <- ens_pred
	      
	    }

return(res)

}


if(type == "Ensemble_dirichlet") {

library(DirichletReg)

## negative log-likelihood for a Dirichlet choice-specific model
## i.e., a single parameter per model and an intercept

if(Intercept) {
nll <- function(theta) {
Xb    <- theta[1] + sapply(2:6, function(i){X[,,i-1] * theta[i]}, simplify = "array")
alpha <- exp(apply(Xb, c(1,2), sum))

## Treatment of 0s and 1s - dirichlet can't handle
Y[Y==0] <- 1e-09
Y[Y==1] <- 1-1e-09

if(any(alpha==0)) {return(1e6)} else {
ll    <- ddirichlet(Y, alpha, log = TRUE, sum.up = TRUE)
return(-ll)
}
}
} else {

nll <- function(theta) {
Xb    <- sapply(1:5, function(i){X[,,i] * theta[i]}, simplify = "array")
alpha <- exp(apply(Xb, c(1,2), sum))

## Treatment of 0s and 1s - dirichlet can't handle
Y[Y==0] <- 1e-09
Y[Y==1] <- 1-1e-09

if(any(alpha==0)) {return(1e6)} else {
ll    <- ddirichlet(Y, alpha, log = TRUE, sum.up = TRUE)
return(-ll)
}
}
}


## prediction function
if(Intercept) {
predict_dirichlet <- function(params, model_predictions) {
	Xb    <- params[1] + sapply(2:6, function(i){X[,,i-1] * params[i]}, simplify = "array")
	alpha <- exp(apply(Xb, c(1,2), sum))
	mu    <- alpha / rowSums(alpha)
	return(mu) } } else {
predict_dirichlet <- function(params, model_predictions) {
	Xb    <- sapply(1:5, function(i){X[,,i] * params[i]}, simplify = "array")
	alpha <- exp(apply(Xb, c(1,2), sum))
	mu    <- alpha / rowSums(alpha)
	return(mu)
}
}

## prepare model predictions 
X <- array(NA, dim = c(12,10,5))  # months, areas, models

X[,,1] <- t(as.matrix(Gravity_pred[[y]][,2:13]))
X[,,2] <- t(as.matrix(Markov_pred[[y]][,2:13]))
X[,,3] <- t(as.matrix(RUM_pred[[y]][,2:13]))
X[,,4] <- t(as.matrix(RUMR_pred[[y]][,2:13]))
X[,,5] <- t(as.matrix(DSVM_pred[[y]][,2:13]))

   if(!Closure) {

	   ## The observations
	   Y <- t(as.matrix(train_obs_mean[,2:13]))

	   if(Intercept) {
	   mod <- optim(par = rep(0, 6), fn = nll, method = "BFGS", hessian = TRUE)
	   } else {
	   mod <- optim(par = rep(0, 5), fn = nll, method = "BFGS", hessian = TRUE)
	   }
	   print(mod$par)
	    res <- predict_dirichlet(params = mod$par, model_predictions = X)
	    res_t <- t(res)
	    res_df <- cbind(train_obs_mean[,1], as.data.frame(res_t))
	    colnames(res_df) <- c("state", 1:12)
	    return(res_df)
	    }
	    
	    
if(Closure) {

	    ## The observations
	    Y <- train_obs_mean
	     
	    ## Observations for the closure areas should be zero
	    ## This won't work as a matrix! 
	    Y[Y$state %in% c("Closure_1", "Closure_2"),2:13] <- 0
   	    Y[!Y$state %in% c("Closure_1", "Closure_2"),2:13] <- apply(Y[!Y$state %in% c("Closure_1", "Closure_2"),2:13], 2, function(x) {x/sum(x)}) 
	    
            Y <- t(as.matrix(Y[,2:13]))

	    if(Intercept) {
	    mod <- optim(par = rep(0, 6), fn = nll, method = "BFGS", hessian = TRUE)
	    } else {
	    mod <- optim(par = rep(0, 5), fn = nll, method = "BFGS", hessian = TRUE)
	    }
	    print(mod$par)
	    res <- predict_dirichlet(params = mod$par, model_predictions = X)
	    res_t <- t(res)
	    res_df <- cbind(train_obs_mean[,1], as.data.frame(res_t))
	    colnames(res_df) <- c("state", 1:12)
	    return(res_df)

}

   return(res)

}

}
