predict_ensemble_with_mod <- function(train.data = train.data, model_predictions = model_preditions, type = "ensemble_dirichlet", train.yrs = train.yrs, test.yrs = test.yrs, Closure = Closure, Simple = TRUE) {

ny <- length(test.yrs)

Gravity_pred <- model_predictions[["Gravity_predictions"]]
RUM_pred     <- model_predictions[["RUM_predictions"]]
RUMR_pred    <- model_predictions[["RUMReparam_predictions"]]
Markov_pred  <- model_predictions[["Markov_predictions"]]
DSVM_pred    <- model_predictions[["DSVM_predictions"]]

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
pivot_wider(names_from = month , values_from = prop, 
values_fn = list(prop = mean, na.rm = TRUE)) %>%
select(state:13) %>% arrange(state) %>% as.data.frame()
train_obs_mean[is.na(train_obs_mean)] <- 0
	  
	  
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

if(type == "Ensemble_lm") {
  
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
	     
	     res <- superensemble 
	      
	      
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

      	     if(Simple) {
	     superensemble  <- lm(trainObs ~ Gravity + Markov + RUM + RUMR + DSVM, data = ens_data)
	     }  else {
	     superensemble  <- lm(trainObs ~ (Gravity + Markov + RUM + RUMR + DSVM):state, data = ens_data)
	     }


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
	     
	     res <- superensemble 
	     

	      
	    }



return(res)

}


if(type == "Ensemble_dirichlet") {

library(DirichletReg)

trainObs <- data.frame(type = "Observations", year = rep(test.yrs, each = length(unique(train_obs_mean$state)) * 12),
	               state = train_obs_mean_df$state, month = train_obs_mean_df$month, value = train_obs_mean_df$value)
	  
	  ens_data <- rbind(ens_data, trainObs)
	  
	  ## prepare ensemble data
	  ens_data <- reshape2::dcast(ens_data, year + month + state ~ type, value.var = "value")
	  ens_data[is.na(ens_data)] <- 0

	  ens_data_full <- ens_data


   if(!Closure) {
	      
	      ens_data <- filter(ens_data_full, year == test.yrs)
	      
	      ens_data <- reshape2::melt(ens_data, id = c("year", "month", "state"))
	      colnames(ens_data)[4] <- "model"
	      
	      
	      ## Now format so states across top, everything else in rows
	      ens_data <- reshape2::dcast(ens_data, year + month ~ model + state, value.var = "value")
	      
	      Y <- DR_data(ens_data[,grep("Observations", colnames(ens_data))])
	      
	      ## explanatory data
	      
	      ddat <- cbind(ens_data[,grep("Gravity", colnames(ens_data))],
	                    ens_data[,grep("Markov", colnames(ens_data))],
	                    ens_data[,grep("DSVM", colnames(ens_data))],
	                    ens_data[,grep("RUM_", colnames(ens_data))],
	                    ens_data[,grep("RUMR", colnames(ens_data))]
	      )
	      
	      mod <- DirichReg(Y ~ Gravity_A + Markov_A + DSVM_A + RUM_A + RUMR_A | 
	                          Gravity_B + Markov_B + DSVM_B + RUM_B + RUMR_B | 
	                          Gravity_Closure_1 + Markov_Closure_1 + DSVM_Closure_1 + RUM_Closure_1 + RUMR_Closure_1 | 
	                          Gravity_Closure_2 + Markov_Closure_2 + DSVM_Closure_2 + RUM_Closure_2 + RUMR_Closure_2 | 
	                          Gravity_D + Markov_D + DSVM_D + RUM_D + RUMR_D | 
	                          Gravity_E + Markov_E + DSVM_E + RUM_E + RUMR_E|
	                          Gravity_Elsewhere + Markov_Elsewhere + DSVM_Elsewhere + RUM_Elsewhere + RUMR_Elsewhere | 
	                          Gravity_F + Markov_F + DSVM_F + RUM_F + RUMR_F | 
	                          Gravity_G + Markov_G + DSVM_G + RUM_G + RUMR_G | 
	                          Gravity_H + Markov_H + DSVM_H + RUM_H + RUMR_H, 
	                        data = ddat) 
	      
	      
	      ens_pred <- predict(mod, type = "probs")
	      
	      # Now to format
	      ens_pred <- reshape2::melt(ens_pred)
	      colnames(ens_pred) <- c("month", "state", "value")
	      ens_pred$state <- gsub("Observations_", "", ens_pred$state)
	      
	      ens_pred <- ens_pred %>% pivot_wider(names_from = "month", values_from = "value") %>% as.data.frame()
	      
	      res <- mod 
	      
	      
	    }
	    
	    
	    if(Closure) {
	      
	      ens_data <- filter(ens_data_full, year == test.yrs)
	      
	      ens_data$Observations <- ifelse(ens_data$state %in% c("Closure_1", "Closure_2"), 0, ens_data$Observations)
	      
	      ## Now need to make sure observations share sum to one
	      ens_data <- ens_data %>% group_by(year, month) %>%
	        mutate(Gravity = Gravity/sum(Gravity), Markov = Markov/sum(Markov),
	                  RUM = RUM/sum(RUM), RUMR = RUMR/sum(RUMR),
	               DSVM = DSVM/sum(DSVM), Observations = Observations/sum(Observations)) %>%
	      filter(!state %in% c("Closure_1", "Closure_2")) %>%
	        as.data.frame()
	      
	      
	      ens_data <- reshape2::melt(ens_data, id = c("year", "month", "state"))
	      colnames(ens_data)[4] <- "model"
	      
	      
	      ## Now format so states across top, everything else in rows
	      ens_data <- reshape2::dcast(ens_data, year + month ~ model + state, value.var = "value")
	      
	      Y <- DR_data(ens_data[,grep("Observations", colnames(ens_data))])
	      
	      ## explanatory data
	      
	      ddat <- cbind(ens_data[,grep("Gravity", colnames(ens_data))],
	                    ens_data[,grep("Markov", colnames(ens_data))],
	                    ens_data[,grep("DSVM", colnames(ens_data))],
	                    ens_data[,grep("RUM_", colnames(ens_data))],
	                    ens_data[,grep("RUMR", colnames(ens_data))]
	      )
	      
	      mod <- DirichReg(Y ~ Gravity_A + Markov_A + DSVM_A + RUM_A + RUMR_A | 
	                         Gravity_B + Markov_B + DSVM_B + RUM_B + RUMR_B | 
	                         Gravity_D + Markov_D + DSVM_D + RUM_D + RUMR_D | 
	                         Gravity_E + Markov_E + DSVM_E + RUM_E + RUMR_E |
	                         Gravity_Elsewhere + Markov_Elsewhere + DSVM_Elsewhere + RUM_Elsewhere + RUMR_Elsewhere | 
	                         Gravity_F + Markov_F + DSVM_F + RUM_F + RUMR_F | 
	                         Gravity_G + Markov_G + DSVM_G + RUM_G + RUMR_G | 
	                         Gravity_H + Markov_H + DSVM_H + RUM_H + RUMR_H, 
	                       data = ddat) 
	      
	      
	      ens_pred <- predict(mod, type = "probs")
	      
	      # Now to format
	      ens_pred <- reshape2::melt(ens_pred)
	      colnames(ens_pred) <- c("month", "state", "value")
	      ens_pred$state <- gsub("Observations_", "", ens_pred$state)
	      
	      ens_pred <- ens_pred %>% pivot_wider(names_from = "month", values_from = "value") %>% as.data.frame()
	      
	      res <- mod

	    }

   return(res)

}


}
