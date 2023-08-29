predict_DSVM <- function(train.data = train.data, Closure = Closure, SIGMA = SIGMA) {

library(RDynState5NAsigmaseason6Age)

 ages   <- 1:4
 season <- 1:6
 areas  <-sort(unique(train.data$state))
 SPP1DSCSTEPS <- SPP2DSCSTEPS <- 0
 FLEETSIZE <- 1000 ## changed from 1740
	  
 sp1<- sp2 <- sp3 <- sp4 <- sp5 <- new("DynStateInput")


if(!Closure) {
	    
	    ## Loop
	    
	    batch <- matrix(1:12, ncol = 6, byrow = T)
	    
	    
	    res <- lapply(1:2, function(x) {
	      
	      mn <- batch[x,]
	      print(mn)
	      train.subset <- filter(train.data, month %in% mn)
	      
	      ## We need to sort the catch data by season to ensure entered correctly...
	      train.subset <- train.subset[order(train.subset$state, train.subset$month),]
	      
	      catchMean(sp1)  <- array(rep(train.subset$spp1.mean,each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp2)  <- array(rep(train.subset$spp2.mean, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp3)  <- array(rep(train.subset$spp3.mean, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp4)  <- array(rep(train.subset$spp4.mean, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp5)  <- array(0.01,                     dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"=season,"option" =areas))/max(ages)
	      
	      # sd of catch
	      catchSigma(sp1) <- array(rep(train.subset$spp1.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp2) <- array(rep(train.subset$spp2.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp3) <- array(rep(train.subset$spp3.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp4) <- array(rep(train.subset$spp4.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp5) <- array(0.001,                  dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option" =areas))/max(ages)
	      
	      ## Effort to travel to metier - we'll set this proportionate to costs as a guide
	      ## between 1 and 10. This means the maximum fuel cost is 50
	      effort <- matrix(train.subset$costs.mean, nrow = length(areas))
	      effort <- 10 * apply(effort, 2, function(x) x/max(x))
	      
	      effort <- array(effort, dim=c(length(areas), length(season)), dimnames=list(option =areas,season=as.character(season)))
	      
	      #prices are equal across seasons 
	      VPT <- c("spp1" = 100, 
	               "spp2" = 200, 
	               "spp3" = 350, 
	               "spp4" = 600)
	      
	      sp1Price <-  array(VPT[[1]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp2Price <-  array(VPT[[2]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp3Price <-  array(VPT[[3]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp4Price <-  array(VPT[[4]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp5Price <-  array(c(0),    dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      
	      # Fuel costs - this should differ per metier state ??
	      fuelC = list("fleet 1" = 3, "fleet 2" = 2, "fleet 3" = 5, "fleet 4" = 2, "fleet 5" = 1)
	      
	      control0     <- DynState.control(spp1LndQuota= 1e8,  spp2LndQuota= 1e8, spp1LndQuotaFine= 0, spp2LndQuotaFine= 0,
	                                       fuelUse = c(1), fuelPrice = fuelC[[3]], landingCosts= 0,gearMaintenance= 0, addNoFishing= FALSE, increments= 25,
	                                       spp1DiscardSteps= SPP1DSCSTEPS, spp2DiscardSteps= SPP2DSCSTEPS, sigma= SIGMA, simNumber= FLEETSIZE, numThreads= 4)
	      
	      
	      z <- DynState(sp1, sp2, sp3, sp4, sp5,
	                    sp1Price, sp2Price, sp3Price, sp4Price, sp5Price,
	                    effort, control0)
	      
	      return(z) 
	      
	    })
	    
	    ## Now extract and index all of the results, stitching together the monthly
	    ## blocks
	    results_all <- lapply(1:2, function(x) {
	      
	      res.tmp <- lapply(1:6, function(mn) cbind(data.frame("month" = batch[x,mn]), 
	                                                as.data.frame(table(res[[x]]@sim@choice[,,mn]))))
	      res.tmp <- do.call(rbind, res.tmp)
	      colnames(res.tmp) <- c("month", "state", "NPredict") 
	      return(res.tmp)
	    })
	    
	    results_all <- bind_rows(results_all)
	    
	    
	    predictions_out <- pivot_wider(results_all, names_from = "month", values_from = NPredict) %>%
	      as.data.frame()
	    predictions_out[,2:13]  <-  predictions_out[,2:13] / colSums(predictions_out[,2:13])
	    predictions_out[is.na(predictions_out)] <- 0 

	    res <- predictions_out
	    
}
	  
	  
if(Closure) {


	     ## Remove closure area options
	     train.data_close <- train.data[!train.data$state %in% c("Closure_1", "Closure_2"),]
	     areas <- areas[!areas %in% c("Closure_1", "Closure_2")]

	     ## Loop
	     batch <- matrix(1:12, ncol = 6, byrow = T)
	    
	    
	    res <- lapply(1:2, function(x) {
	      
	      mn <- batch[x,]
	      print(mn)
	      train.subset <- filter(train.data_close, month %in% mn)
	      
	      ## We need to sort the catch data by season to ensure entered correctly...
	      train.subset <- train.subset[order(train.subset$state, train.subset$month),]
	      
	      catchMean(sp1)  <- array(rep(train.subset$spp1.mean,each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp2)  <- array(rep(train.subset$spp2.mean, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp3)  <- array(rep(train.subset$spp3.mean, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp4)  <- array(rep(train.subset$spp4.mean, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchMean(sp5)  <- array(0.01,                     dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"=season,"option" =areas))/max(ages)
	      
	      # sd of catch
	      catchSigma(sp1) <- array(rep(train.subset$spp1.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp2) <- array(rep(train.subset$spp2.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp3) <- array(rep(train.subset$spp3.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp4) <- array(rep(train.subset$spp4.sd, each = max(ages)), dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option"=areas))/max(ages)
	      catchSigma(sp5) <- array(0.001,                  dim=c(length(ages), length(season),length(areas)),  dimnames=list("cat"=ages,"season"= season,"option" =areas))/max(ages)
	      
	      ## Effort to travel to metier - we'll set this proportionate to costs as a guide
	      ## between 1 and 10. This means the maximum fuel cost is 50
	      effort <- matrix(train.subset$costs.mean, nrow = length(areas))
	      effort <- 10 * apply(effort, 2, function(x) x/max(x))
	      
	      effort <- array(effort, dim=c(length(areas), length(season)), dimnames=list(option =areas,season=as.character(season)))
	      
	      #prices are equal across seasons 
	      VPT <- c("spp1" = 100, 
	               "spp2" = 200, 
	               "spp3" = 350, 
	               "spp4" = 600)
	      
	      sp1Price <-  array(VPT[[1]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp2Price <-  array(VPT[[2]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp3Price <-  array(VPT[[3]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp4Price <-  array(VPT[[4]], dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      sp5Price <-  array(c(0),    dim=c(length(ages),length(season)), dimnames=list(cat=ages,season=as.character(season)))
	      
	      # Fuel costs - this should differ per metier state ??
	      fuelC = list("fleet 1" = 3, "fleet 2" = 2, "fleet 3" = 5, "fleet 4" = 2, "fleet 5" = 1)
	      
	      control0     <- DynState.control(spp1LndQuota= 1e8,  spp2LndQuota= 1e8, spp1LndQuotaFine= 0, spp2LndQuotaFine= 0,
	                                       fuelUse = c(1), fuelPrice = fuelC[[3]], landingCosts= 0,gearMaintenance= 0, addNoFishing= FALSE, increments= 25,
	                                       spp1DiscardSteps= SPP1DSCSTEPS, spp2DiscardSteps= SPP2DSCSTEPS, sigma= SIGMA, simNumber= FLEETSIZE, numThreads= 4)
	      
	      
	      z <- DynState(sp1, sp2, sp3, sp4, sp5,
	                    sp1Price, sp2Price, sp3Price, sp4Price, sp5Price,
	                    effort, control0)
	      
	      return(z) 
	      
	    })
	    
	    ## Now extract and index all of the results, stitching together the monthly
	    ## blocks
	    results_all <- lapply(1:2, function(x) {
	      
	      res.tmp <- lapply(1:6, function(mn) cbind(data.frame("month" = batch[x,mn]), 
	                                                as.data.frame(table(res[[x]]@sim@choice[,,mn]))))
	      res.tmp <- do.call(rbind, res.tmp)
	      colnames(res.tmp) <- c("month", "state", "NPredict") 
	      return(res.tmp)
	    })
	    
	    results_all <- bind_rows(results_all)

	    ## add on the closure areas
	    results_all <- rbind(results_all, 
		  data.frame(month = rep(1:12, times = 2), state = rep(c("Closure_1", "Closure_2"), each = 12),
				      NPredict = 0)
		  )

	    results_all <- results_all[order(as.character(results_all$state)),]
	    
	    predictions_out <- pivot_wider(results_all, names_from = "month", values_from = NPredict) %>%
	      as.data.frame()
	    predictions_out[,2:13]  <-  predictions_out[,2:13] / colSums(predictions_out[,2:13]) 
	    predictions_out[is.na(predictions_out)] <- 0 

	    res <- predictions_out
	    
		
}


return(res)


}
