predict_markov <- function(train.data = train.data, model_fit = model_fit, Closure = Closure) {

	  states <- sort(unique(train.data[,"state"]))
		  
		  new.df.intrip <- expand.grid(month = 1:12,
		                               state = states[!states %in% "home"],
		                               state.tminus1 = states[!states %in% "home"], 
		                               FromHome = FALSE, ToHome = FALSE
		                               		  )
		  
		  new.df.fromhome <- expand.grid(month = 1:12,
		                                 state = states[!states %in% "home"],
		                                 state.tminus1 = "home", 
		                                 FromHome = TRUE, ToHome = FALSE
		                                		  )
		  
		  new.df <- rbind(new.df.intrip, new.df.fromhome)
		  
		  ## now get the probabilities
		  probs <- nnet:::predict.multinom(model_fit, newdata = new.df, type = "probs")
		  predictions.df <- cbind(new.df, probs)


		if(!Closure) {
		  
		 ## to simulate
		  
		  # number of events per vessel
		  events <-  nrow(filter(train.data, vessel == 1))/length(unique(train.data$year))

		  
		  # trans matrix
		  transitions <- matrix(0, nrow = events, ncol = 1 + length(states),
		                        dimnames = list(seq_len(events), c("month", states)))
		  transitions[,"month"]  <- rep(1:12, each = 91)  ## 91 tows per month
		  
		  for(i in seq_len(events)) { ## only want 1 years predictions
		    
		    if(i %in% seq(2, events, 21)) { ## from home
		      move_predict <- apply(filter(predictions.df, month == transitions[i,"month"], FromHome == TRUE, ToHome == FALSE) %>% select(A:H), 2, mean, na.rm = T)
		      
		      transitions[i,states[!states %in% "home"]]  <- 1 * move_predict
		      
		    }
		    
		    if(!i %in% c(seq(1, events, 21), seq(2, events, 21))) { ## in trip move
		      
		      for(st in states[!states %in% "home"]) {  ## for each previous state
		        
		        move_predict <- apply(filter(predictions.df, state.tminus1 == st, month == transitions[i,"month"], FromHome == FALSE, ToHome == FALSE) %>% select(A:H), 2, mean, na.rm = T)
		        
		        if(!all(is.na(move_predict))) {
		          transitions[i,states[!states %in% "home"]]  <- transitions[i,states[!states %in% "home"]] + 
		            c(move_predict * transitions[i-1,st])
		        }
		        
		      }
		      
		      
		    }
		    
		    
		    if(i %in% seq(1, events, 21)) { ## to home
		      transitions[i,"home"]  <- 1
		      transitions[i,! colnames(transitions) %in% c("month", "home")] <- 0
		    }
		    
		  }
		  
		  # transitions without home
		  
		  
		  trans_without_home <- transitions[-seq(1,events,21),]
		  
		  res <- trans_without_home %>% as.data.frame() %>% select(-home) %>%
		    pivot_longer(A:H) %>% group_by(month, name) %>%
		    summarise(prob = mean(value)) %>%
		    pivot_wider(names_from = "month", values_from = "prob") %>%
		    as.data.frame()
		  
 
		}


		if(Closure) {
		
		 ## to simulate
		
		 ## First we need to close off the closure areas
		 ## Reduce prob transitions to/from to zero
		 ## And raise all others

		 new_vals <- predictions.df %>% mutate(Closure_1 = 0, Closure_2 = 0) %>%
			 select(A:H)
		 new_vals <- new_vals/rowSums(new_vals)
		 predictions.df[,6:15]  <- new_vals

		  # number of events per vessel
		  events <-  nrow(filter(train.data, vessel == 1))/length(unique(train.data$year))
		  
		  # trans matrix
		  transitions <- matrix(0, nrow = events, ncol = 1 + length(states),
		                        dimnames = list(seq_len(events), c("month", states)))
		  transitions[,"month"]  <- rep(1:12, each = 91)  ## 91 tows per month
		  
		  for(i in seq_len(events)) { ## only want 1 years predictions
		    
		    if(i %in% seq(2, events, 21)) { ## from home
		      move_predict <- apply(filter(predictions.df, month == transitions[i,"month"], FromHome == TRUE, ToHome == FALSE) %>% select(A:H), 2, mean, na.rm = T)
		      
		      transitions[i,states[!states %in% "home"]]  <- 1 * move_predict
		      
		    }
		    
		    if(!i %in% c(seq(1, events, 21), seq(2, events, 21))) { ## in trip move
		      
		      for(st in states[!states %in% "home"]) {  ## for each previous state
		        
		        move_predict <- apply(filter(predictions.df, state.tminus1 == st, month == transitions[i,"month"], FromHome == FALSE, ToHome == FALSE) %>% select(A:H), 2, mean, na.rm = T)
		        
		        if(!all(is.na(move_predict))) {
		          transitions[i,states[!states %in% "home"]]  <- transitions[i,states[!states %in% "home"]] + 
		            c(move_predict * transitions[i-1,st])
		        }
		        
		      }
		      
		      
		    }
		    
		    
		    if(i %in% seq(1, events, 21)) { ## to home
		      transitions[i,"home"]  <- 1
		      transitions[i,! colnames(transitions) %in% c("month", "home")] <- 0
		    }
		    
		  }
		  
		  # transitions without home
		  
		  
		  trans_without_home <- transitions[-seq(1,events,21),]
		  
		  res  <- trans_without_home %>% as.data.frame() %>% select(-home) %>%
		    pivot_longer(A:H) %>% group_by(month, name) %>%
		    summarise(prob = mean(value)) %>%
		    pivot_wider(names_from = "month", values_from = "prob") %>%
		    as.data.frame()
		  
		}


return(res)

}
