prepare_gravity <- function(train.dat = train.dat, test.dat = test.dat) {

    train.dat <- filter(train.dat, state != "home")
    test.dat  <- filter(test.dat, state != "home")
    
    # Summarise the training data with the number of tows per week and average
    # catch rates
    
    train.dat.mean <- train.dat %>% group_by(month, state) %>%
      summarise(ObsN = n(), ## number of tows per week, state
                spp1.mean = mean(spp1), spp2.mean = mean(spp2),
                spp3.mean = mean(spp3), spp4.mean = mean(spp4),
                val.mean = mean(val), costs.mean = mean(costs),
                profit.mean = mean(profit),
                # also the sds
                spp1.sd = sd(spp1), spp2.sd = sd(spp2),
                spp3.sd = sd(spp3), spp4.sd = sd(spp4),
                val.sd = sd(val), costs.sd = sd(costs),
                profit.sd = sd(profit)
      ) %>% ungroup %>% complete(month, state) %>%
      as.data.frame()
    
    ## If n is 1, sd is zero
    train.dat.mean[is.na(train.dat.mean)] <- 0
    
    ## The predicted number of tows at a location should just be proportionate to
    ## the value i.e. N_tow(j) = N_tow * (cpue_(j)/sum(cpue(J))
    ## or profit
    
    ## Add to the data.frame the total value and profit of tows in all sites
    train.dat.tot <- train.dat.mean %>% group_by(month) %>%
      summarise(val.tot = sum(val.mean), profit.tot = sum(profit.mean)) %>% 
      as.data.frame()
    
    train.dat.mean$val.tot <- train.dat.tot$val.tot[match(train.dat.mean$month,
                                                          train.dat.tot$month)]
    
    train.dat.mean$profit.tot <- train.dat.tot$profit.tot[match(train.dat.mean$month,
                                                          train.dat.tot$month)]
    
    return(list(train.dat = train.dat.mean, test.dat = test.dat))
 

}
