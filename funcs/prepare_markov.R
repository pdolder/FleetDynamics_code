prepare_markov <- function(train.dat = train.dat, test.dat = test.dat) {

 train.dat$id <- with(train.dat, paste0(vessel, trip))
    keep <- c("fleet", "vessel", "id", "trip", "state", "state.tminus1", "tow")
    
    ## Need to estimate probabilities separately for the case of:
    #* To home
    #* From home
    
    train.dat$ToHome <- ifelse(train.dat$state == "home", TRUE, FALSE)
    train.dat$FromHome <- ifelse(train.dat$state.tminus1 == "home", TRUE, FALSE)
    
    test.dat$ToHome <- ifelse(test.dat$state == "home", TRUE, FALSE)
    test.dat$FromHome <- ifelse(test.dat$state.tminus1 == "home", TRUE, FALSE)
    
    return(list(train.dat = train.dat, test.dat = test.dat))


}
