prepare_dsvm <- function(train.dat = train.dat, test.dat = test.dat) {

  ## We know the time spent at home port, so we can exclude these from the
    ## dataset
    train.dat <- filter(train.dat, state != "home")
    test.dat  <- filter(test.dat, state != "home")
    
    # Now let's summarise the data with the number of tows per month and average
    # catch rates
    
    train.dat$state <- as.factor(train.dat$state)
    train.dat$month <- as.factor(train.dat$month)
    
    train.dat.mean <- train.dat %>% group_by(month, state) %>%
      summarise(ObsN = n(), ## number of tows per month, state
                spp1.mean = mean(spp1), spp2.mean = mean(spp2),
                spp3.mean = mean(spp3), spp4.mean = mean(spp4),
                val.mean = mean(val), costs.mean = mean(costs),
                profit.mean = mean(profit),
                # also the sds
                spp1.sd = sd(spp1), spp2.sd = sd(spp2),
                spp3.sd = sd(spp3), spp4.sd = sd(spp4),
                val.sd = sd(val), costs.sd = sd(costs),
                profit.sd = sd(profit)) %>% ungroup() %>% 
      complete(month, state) %>% as.data.frame()
    
    ## If n is 1, sd is zero
    train.dat.mean[is.na(train.dat.mean)] <- 0
    
    return(list(train.dat = train.dat.mean, test.dat = test.dat))
   

}
