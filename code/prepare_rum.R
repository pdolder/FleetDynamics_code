prepare_rum <- function(train.dat = train.dat, test.dat = test.dat) {

    library(mlogit)
    library(parallel)
 
    ## Calculate the alternative options
    
    ## This is month specific 
    expected_from_alt <- train.dat %>% 
      group_by(fleet, month, state) %>% summarise(val = mean(val),
                                                  spp1 = mean(spp1),
                                                  spp2 = mean(spp2),
                                                  spp3 = mean(spp3),
                                                  spp4 = mean(spp4)) %>% as.data.frame()
    
    # This is general, for missing options for some months 
    expected_from_alt_gen <- train.dat %>% 
      group_by(fleet, state) %>% summarise(val = mean(val),
                                           spp1 = mean(spp1),
                                           spp2 = mean(spp2),
                                           spp3 = mean(spp3),
                                           spp4 = mean(spp4)) %>% as.data.frame()
    
    
    # We need to create a dummy row to add to each trip, detailing the move to the home port. We'll create the rows and fold them into the logs
    
    home_trip <- expand.grid(fleet = unique(train.dat$fleet), vessel = unique(train.dat$vessel), x = 0, y = 0, stepD = NA, angles = NA, day  = NA, tow = NA,  trip = unique(train.dat$trip),
                             week = NA, month = NA, year = unique(train.dat$year), spp1 = 0, spp2 = 0, spp3 = 0, spp4 = 0, allspp = 0, val = 0, meanval = NA, sdval = NA, costs = NA, profit = NA, state = "home", state.tminus1 = NA) 
    
    
    train.dat$state <- as.character(train.dat$state)
    
    logs2 <- rbind(train.dat, home_trip)
    
    ##na.last = F in order to get NA from the week at the top of each week
    logs2 <- logs2[order(logs2$fleet, logs2$vessel, logs2$year, logs2$trip, logs2$week, logs2$month, logs2$year, logs2$tow, na.last = F),]
    
    ## Change to state and state.tminus 1
    colnames(logs2)[ncol(logs2)] <- "state"
    
    logs3 <- split(logs2, logs2[,c("fleet", "vessel")])
    logs4 <- lapply(logs3, function(x) cbind(x, data.frame(state.tminus1 = c(NA, x$state[-nrow(x)]))))
    logs5 <- do.call(rbind, logs4)
    
    logs <- logs5
    rm(logs2, logs3, logs4)
    
    ## Add as a new data frame
    alt.data <- expand.grid(fleet = unique(logs$fleet), month = unique(logs$month), state = unique(logs$state), state.tminus1 = unique(logs$state)) 
    
    alt.data$state <- as.character(alt.data$state)
    alt.data <- left_join(alt.data, expected_from_alt, by = c("fleet", "month","state"))
    
    ## Add the non fleet-specific data
    
    # We need to split up the data, those with vals and NAs
    
    alt.data.vals <- alt.data[!is.na(alt.data$val),]
    alt.data.novals <- alt.data[is.na(alt.data$val),]
    
    alt.data.novals <- left_join(alt.data.novals[,c("fleet","state","state.tminus1")],
                                 expected_from_alt_gen, by = c("fleet","state"))
    
    ## Travel to/from home is not an option 
    alt.data.novals <- alt.data.novals[!(alt.data.novals$state == "home" & alt.data.novals$state.tminus1 == "home"),]
    ## Need to add a month column on novals
    alt.data.novals.month <- lapply(1:12, function(x) {
      alt.data.novals$month <- x
      return(alt.data.novals)	  
    })
    alt.data.novals <- bind_rows(alt.data.novals.month)
    
    ## And to/from home has no value
    alt.data.novals[(alt.data.novals$state == "home" | alt.data.novals$state.tminus1 == "home"),c(4:8)]  <- 0
    
    alt.data <- alt.data.vals
    # alt.data <- rbind(alt.data.vals, alt.data.novals)
    rm(alt.data.vals, alt.data.novals)
    
    ## add travel costs
    alt.data$costs <- NA
    alt.data$state.tminus1 <- as.character(alt.data$state.tminus1)
    
    fuelC <- c("1" = 3, "2" = 2, "3" = 5, "4" = 2, "5" = 1)
    
    alt.data$costs <- sapply(1:nrow(alt.data), function(x) {
      if(alt.data[x,"state"] == "Elsewhere" | alt.data[x,"state.tminus1"] == "Elsewhere") { 50} else {
        fuelC[[alt.data[x,"fleet"]]] * 
          dist_centroids[alt.data[x,"state"],alt.data[x,"state.tminus1"]] }})
    
    ## estimate expected profit
    alt.data$profit <- (alt.data$val - alt.data$costs)
    
    ## Some extra columns
    alt.data$allspp  <- rowSums(alt.data[,c(6:9)])
    
    train.dat <- filter(train.dat, state != "home") ## not a choice as such   
    
    ## Stitch together the choice set
    
    cl <- makeCluster(getOption("cl.cores", detectCores()))
    clusterExport(cl = cl, varlist = c("alt.data", "train.dat"), envir = environment())
    
    choices <- parLapply(cl, seq_len(nrow(train.dat)), function(x) {
      
      ## Alternative set
      tmp <- alt.data[which(alt.data$fleet  == train.dat[x,"fleet"] &
                              alt.data$month == train.dat[x,"month"] &
                              alt.data$state.tminus1 == train.dat[x,"state.tminus1"] &
                              alt.data$state != train.dat[x,"state"]),]
      
      # combine with missing variables in logs
      tmp2 <- cbind(train.dat[x,c(1:2,7:12)],
                    tmp[,c(6:9,5,12,10,11,3:4)])
      tmp3 <- rbind(cbind(train.dat[x,c(1:2,7:18,21:24)],data.frame("choice" = 1)),
                    cbind(tmp2, data.frame("choice" = 0)))
      
      return(tmp3)
      
    })
    
    stopCluster(cl)
    
    choices <- bind_rows(choices)
    #      choices$choice <- as.factor(choices$choice)
    
    ## Remove home as as choice
    choices <- filter(choices, state != "home")
    choices$state <- as.factor(choices$state)
    choices$state.tminus1 <- as.factor(choices$state.tminus1)
    
    ## To fit the model
    
    ## Add an index required for mlogit
    choices$ind <- paste(choices$fleet, choices$vessel, choices$tow, sep=".")
    
    choices2 <- choices[!(is.na(choices$tow)),]
    
    train.dat <- choices2
    
    ## Only estimate between states actually observed....
    #      train.dat <- filter(train.dat, state %in% unique(dplyr::filter(train.dat, choice == "yes")$state)) %>% droplevels()
    
    LD <- mlogit.data(train.dat, choice = "choice", shape = "long", 
                      chid.var = "ind", alt.var = "state", drop.index = TRUE)
    
    # Define the formula
    LD$fleet <- as.factor(LD$fleet)
    LD$vessel <- as.factor(LD$vessel)
    
    
    ## Some costs are 0, make these a very small amount so we can log-transform
    LD$costs[LD$costs==0] <- 0.0001
    LD$val[LD$val==0]     <- 0.0001

    ## Add the log-odds of costs and revenue
    ## Values from state A added to dataframe
    StateA <- LD[grep("A", rownames(LD)),]
    
    LD$A_val <- StateA$val[match(paste0(LD$fleet, LD$vessel, LD$day, LD$tow, LD$year, LD$month, LD$week, LD$trip),
                                 paste0(StateA$fleet, StateA$vessel, StateA$day, StateA$tow, StateA$year, StateA$month, StateA$week, StateA$trip))]
    
    LD$A_costs <- StateA$costs[match(paste0(LD$fleet, LD$vessel, LD$day, LD$tow, LD$year, LD$month, LD$week, LD$trip),
                                     paste0(StateA$fleet, StateA$vessel, StateA$day, StateA$tow, StateA$year, StateA$month, StateA$week, StateA$trip))]
    
    ## log-odds of costs and value
    LD$logOddscosts <- log(LD$costs/LD$A_costs)
    LD$logOddsval <- log(LD$val/LD$A_val)
    
    # Return the formated logit data
    return(list(LD = LD, train.dat = train.dat, test.dat = test.dat))
    
}



