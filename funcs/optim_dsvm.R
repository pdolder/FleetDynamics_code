optim_dsvm <- function(train.data = train.data, Closure = Closure, SIGMA = SIGMA) {

## Fit the model

## Give the observations

## Compare

pred <- predict_DSVM(train.data = train.data, Closure = Closure, SIGMA = SIGMA)

obs <- train.data %>%
    group_by(month, state) %>%
    summarise(N = sum(ObsN)) %>% arrange(state) %>% 
    pivot_wider(names_from = month, values_from = N) %>%
    as.data.frame() 

# proportions
obs[,2:ncol(obs)] <- apply(obs[,2:ncol(obs)], 2, function(x) x/sum(x))

## Need to expand obs and pred to have the same number of states!!

 if(any(!c("Closure_1", "Closure_2") %in% obs$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(obs,
                   tmp)
      obs <- tmp[order(tmp$state),]
    }

if(any(!c("Closure_1", "Closure_2") %in% pred$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(pred,
                   tmp)
      pred<- tmp[order(tmp$state),]
    }




RMSE <- function(predictions, observations) {
  sqrt(mean(as.matrix(((predictions[,2:13] - observations[,2:13])^2))))
}

return(RMSE(pred, obs))


}
