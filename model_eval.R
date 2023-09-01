###########################################################

## Evaluate the runs

###########################################################

load("Model_Runs.RData")
load("DSVM_Runs.RData")  ## The revised DSVM runs
load("Ensemble_Runs.RData")  ## The revised Ensemble runs

library(tidyverse)     # data manip and plots
library(broom)         # for model coefficients
library(RColorBrewer)  # plotsi
library(units)
library(xtable)        # creating latex table of results
library(cowplot)
###########################################################

## Replace the old DSVM runs
for(i in seq_len(length(runs))) {
runs[[i]]$DSVM_predictions      <- runs2[[i]]$DSVM_predictions
runs[[i]]$lm_predictions        <- runs4[[i]]$lm_predictions
runs[[i]]$dirichlet_predictions <- runs4[[i]]$dirichlet_predictions
}


## Include the missing closures values for the observations (or others)

for(i in seq_len(length(runs))) {
  for(j in 1:2) {
    
    ## past share 
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$PastShare_predictions[[j]]$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$PastShare_predictions[[j]],
                   tmp)
      runs[[i]]$PastShare_predictions[[j]] <- tmp[order(tmp$state),]
    }
    
    ## Gravity
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$Gravity_predictions[[j]]$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$Gravity_predictions[[j]],
                   tmp)
      runs[[i]]$Gravity_predictions[[j]] <- tmp[order(tmp$state),]
    }
    # RUM
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$RUM_predictions[[j]]$name)) {
      tmp <-  data.frame(name = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$RUM_predictions[[j]],
                   tmp)
      runs[[i]]$RUM_predictions[[j]] <- tmp[order(tmp$name),]
    }
    # RUM reparam
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$RUMReparam_predictions[[j]]$name)) {
      tmp <-  data.frame(name = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$RUMReparam_predictions[[j]],
                   tmp)
      runs[[i]]$RUMReparam_predictions[[j]] <- tmp[order(tmp$name),]
    }
    
    
    # Markov
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$Markov_predictions[[j]]$name)) {
      tmp <-  data.frame(name = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$Markov_predictions[[j]],
                   tmp)
      runs[[i]]$Markov_predictions[[j]] <- tmp[order(tmp$name),]
    }
    
   # DSVM
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$DSVM_predictions[[j]]$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$DSVM_predictions[[j]],
                   tmp)
      runs[[i]]$DSVM_predictions[[j]] <- tmp[order(tmp$state),]
    }
    
    # lm Ensemble
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$lm_predictions[[j]]$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$lm_predictions[[j]],
                   tmp)
      runs[[i]]$lm_predictions[[j]] <- tmp[order(tmp$state),]
    }
    
    # Dirichlet Ensemble
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$dirichlet_predictions[[j]]$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$dirichlet_predictions[[j]],
                   tmp)
      runs[[i]]$dirichlet_predictions[[j]] <- tmp[order(tmp$state),]
    }
    
    
    # Observations
    if(any(!c("Closure_1", "Closure_2") %in% runs[[i]]$Observations[[j]]$state)) {
      tmp <-  data.frame(state = c("Closure_1", "Closure_2"), 
                         "1" = 0, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0,
                         "7" = 0, "8" = 0, "9" = 0, "10" = 0, "11" = 0, "12" = 0
      )
      colnames(tmp)[2:13] <- 1:12
      
      tmp <- rbind(runs[[i]]$Observations[[j]],
                   tmp)
      runs[[i]]$Observations[[j]] <- tmp[order(tmp$state),]
    }
    
    
    
    
  }
  
  
}

################
## Add a gravity/past share hybrid
################

past_share_pc <- 0.8

Gravity_combo <- lapply(runs, function(x) {
		   
		   lapply(1:2, function(i) {
				  
				 res <-  (x$Gravity_predictions[[i]][,2:13] * (1- past_share_pc)) +
			                 (x$PastShare_predictions[[i]][,2:13] * past_share_pc)
				 res <- cbind(data.frame(state = x$Gravity_predictions[[i]][,1], res))
				 colnames(res)[2:13] <- 1:12
				 return(res)

})
		   
		   })

for(i in 1:length(runs)) {

	runs[[i]]$GravityCombo_predictions <- Gravity_combo[[i]]

}



## Check they all sum to 1

# Do any of them not such to 1 ??

any(
lapply(runs, function(x) {

res1<- lapply(x, function(x1) {
res2 <- lapply(x1, function(x2) {
any(colSums(x2[,2:13]) == FALSE)

})

any(res2 == TRUE)

})

any(res1 == TRUE)
		   })

== TRUE)

## Should be FALSE

### Data frame of the predictions and observations

first.yr <- 20
last.yr  <- 39
trn.yrs <- (first.yr):(last.yr-2)
tst.yrs <- (first.yr+3):(last.yr)
nys <- length(tst.yrs)-1



## the year
results_out <- bind_rows(lapply(seq_len(nys), function(y) {
  
  mod <- names(runs[[y]])
  
  ## the model
  bind_rows(lapply(mod, function(m) {
    
    # the prediction year		       
    bind_rows(yr_res <- lapply(1:2, function(i) {
      
      res <- reshape2::melt(runs[[y]][[m]][[i]])
      colnames(res) <- c("state", "month", "data")
      res <- cbind(data.frame(type = m, 
                              train.yrs = paste(trn.yrs[y],trn.yrs[y+1], trn.yrs[y+2], sep=",", collapse = ","),
                              year = tst.yrs[y-1+i], 
                              yr_ahead = i), res)
      return(res)	  		  
    }))
    
    
  }))
  
}))


## Fix the states
#results_out$state[results_out$state == "H"] <- "C"
results_out$state[results_out$state == "Closure_1"] <- "C_1"
results_out$state[results_out$state == "Closure_2"] <- "C_2"
results_out$state[results_out$state == "Elsewhere"] <- "OTH"

results_out$state <- as.factor(results_out$state)

#results_out$state <- factor(results_out$state, levels(results_out$state)[c(1:3,6:7,9:10,4:5,8)]) 

## convert to 0 - 100
results_out$data <- results_out$data * 100

levels(results_out$type)[levels(results_out$type) == "dirichlet_predictions"] <- "Dirichlet_predictions"

## models_to_include

mods <- c("PastShare", "Gravity", "GravityCombo", "DSVM", "Markov", "RUM",
					  "RUMReparam")#, "lm", "Dirichlet")

models_full <- unique(grep(paste(mods, collapse = "|"),
			   results_out$type, value = TRUE))

obs  <- filter(results_out, type == "Observations")
pred <- filter(results_out, type %in% models_full)

pred <- left_join(pred, obs, by = c("year", "train.yrs","yr_ahead", "state", "month"))


#############################
###### Figures ##############
#############################

##########################
## Predictions vs observed
###########################

theme_set(theme_bw())
for(i in unique(pred$train.yrs)) {
  
  ggplot(filter(pred, train.yrs == i), aes(x = month, y = data.x)) +
    geom_point(aes(colour = type.x)) +
    geom_line(aes(colour = type.x, group = type.x)) +
    geom_point(aes(y = data.y), shape = "x", colour = "black") +
    facet_grid(state ~ year)
  ggsave(file.path("figures", paste0("Predictions", i, ".png")), width = 6, height = 10)
}

###############################
## manuscript figure - 
## before/after closure predictions
###############################

df <- filter(pred, train.yrs %in% c("26,27,28")) 
df$type.x <- gsub("_predictions", "", df$type.x)

df$yearmonth <- paste(df$year, df$month, sep = "_")

df$yearmonth <- factor(df$yearmonth)
df$yearmonth <- factor(df$yearmonth, levels(df$yearmonth)[c(1,5:12,2:4,13,17:24,14:16)])

df$type.x <- factor(df$type.x, levels = mods)

df$model_class <- ifelse(df$type.x %in% c("PastShare", "Gravity", "DSVM", "GravityCombo"), "Process models",
			 ifelse(df$type.x %in% c("RUM", "Markov", "RUMReparam"), "Statistical models", "Ensemble model"))

df$model_class <- as.factor(df$model_class)
df$model_class <- factor(df$model_class, levels = c("Process models", "Statistical models", "Ensemble model"))

ggplot(df, aes(x = yearmonth, y = data.x)) +
  geom_line(aes(colour = type.x, group = type.x)) + 
#  geom_point(aes(y = data.y), shape = "x", colour = "black") +
  geom_line(aes(y = data.y, group = type.y), colour = "black") + 
  facet_grid(state ~ model_class) + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0), 
   panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), legend.position = "top")  + 
  scale_colour_brewer("model", type = "qual", palette = 2) + 
  scale_x_discrete(labels = c("1 (year 29)", 2:12, "1 (year 30)", 2:12)) +
  geom_vline(aes(xintercept = 13), colour = "red", linetype = "dotted", alpha = 0.3) + 
  xlab("") + ylab("Proportion Effort")
ggsave(file.path("figures", "Predictions_manu.png"), width = 8, height = 12)

## write as table to file

df_obs <- filter(df, type.x == "PastShare")
df_tab <- reshape2::dcast(df, year + month + state ~ type.x, value.var = "data.x")
df_tab$observations <- df_obs$data.y[match(paste(df_obs$year, df_obs$month, df_obs$state),
				       paste(df_tab$year, df_tab$month, df_tab$state))]

print(xtable(df_tab, digits = 4, caption = "Predicted and Observed proportions in each state", label = "tab:1"),
      caption.placement = "top", file = file.path("Pred_and_Obs.tex"), include.rownames = FALSE,
      tabular.environment = "longtable", floating = FALSE)

###############################
## manuscript figure -
## observed v predicted R2
###############################

pred$type.x <- gsub("_predictions", "", pred$type.x)
pred$closure <- ifelse(pred$year <= 29, "Before Closure", "After Closure")
pred$closure <- ifelse(pred$year == 30, "During Closure", pred$closure)
pred$type.x <- factor(pred$type.x)
pred$closure <- factor(pred$closure)

pred$closure <- factor(pred$closure, levels(pred$closure)[c(2,3,1)])

r_sq <- pred %>% group_by(type.x, closure) %>%
	do(glance(lm(data.y ~ data.x, data = .))) %>%
	as.data.frame()
r_sq$r.squared <- round(r_sq$r.squared, 2)

spearmans <- pred %>% group_by(type.x, closure) %>%
	do(glance(cor.test(~ data.y + data.x, data = ., method = "spearman"))) %>%
	as.data.frame()

spearmans$estimate <- round(spearmans$estimate, 3)

## colours
cols <- brewer.pal(3, "Accent") 

## order
pred$type.x <- factor(pred$type.x, levels = mods)


pred$model_class <- ifelse(pred$type.x %in% c("PastShare", "Gravity", "DSVM", "GravityCombo"), "Process model",
			 ifelse(pred$type.x %in% c("RUM", "Markov", "RUMReparam"), "Statistical model", "Ensemble model"))

theme_set(theme_bw())

  ggplot(pred, aes(x = data.x, y = data.y, group = closure, colour = closure)) +
  geom_point(aes(colour = closure), alpha = 1, size = 1) +
  facet_grid(type.x ~ closure) + 
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(intercept = 0, slope = 1) + 
  xlab("Predicted proportions") + ylab("Observed proportions") + 
  scale_colour_brewer(type = "qual") +
  geom_text(data = filter(spearmans, closure == "Before Closure"), aes(x = 38, y = 5, label = paste(estimate)), size = 4, colour = cols[1]) +
  geom_text(data = filter(spearmans, closure == "During Closure"), aes(x = 38, y = 5, label = paste(estimate)), size = 4, colour = cols[2]) +
  geom_text(data = filter(spearmans, closure == "After Closure"), aes(x = 38, y = 5, label = paste(estimate)), size = 4, colour = cols[3])  + 
  theme(panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), legend.position = "none")
  
ggsave(file.path("figures", "spearmans_cor.png"), width = 10, height = 12)

print(xtable(spearmans, digits = 4, caption = "Spearmans correlation estimate for Predictions against Observationsunder different model formulations", label = "tab:2"),
      caption.placement = "top", file = file.path("Spearmans.tex"), include.rownames = FALSE,
      tabular.environment = "longtable", floating = FALSE)



################
## RMSE scores 
################

RMSE <- function(predictions, observations) {
  sqrt(mean(as.matrix(((predictions[,2:13] - observations[,2:13])^2))))
}

###############
## MAE scores
###############

MAE <- function(predictions, observations) {
mean(abs(predictions[,2:13] - observations[,2:13]))
}


pred$SE <- sqrt((pred$data.x - pred$data.y)^2)
pred$sign <- ifelse(pred$data.y > pred$data.x, "positive", "negative")

##################
## Bubble plots
###################


for(i in unique(pred$train.yrs)) {
  ggplot(filter(pred, train.yrs == i), aes(x = month, y = state)) +
    geom_point(aes(size = SE, fill = sign), colour = "black", shape = 21) +
    facet_grid(type.x ~ year) + 
    scale_size_area() +
    scale_fill_manual(values = c("white", "grey20"))
  ggsave(file.path("figures", paste0("BubblePlot", i, ".png")), width = 6, height = 10)
}

## For paper
i <- c("26,27,28")
bubble_paper <- filter(pred, train.yrs == i) 
bubble_paper$year <- paste("year", bubble_paper$year, sep = " ")


ggplot(filter(bubble_paper, SE != 0), aes(x = month, y = state)) +
    geom_point(aes(size = SE, fill = sign), colour = "black", shape = 21) +
    facet_grid(type.x ~ year) + 
    scale_size_area(name = "Residual") +
    scale_fill_manual(values = c("white", "grey20"), name = "")
  ggsave(file.path("figures", paste0("BubblePlot", i, ".png")), width = 6, height = 10)

########
# RMSE
########

## Add error bars after
# Jensen - Which method is more accurate - Or Errors have error bars

RMSE_df <- pred %>% group_by(type.x, year, yr_ahead) %>%
  summarise(RMSE = sqrt(mean(((data.x - data.y)^2))), N = n()) %>% 
  mutate(RMSE_lo = RMSE * (1 - sqrt(1-(1.96*sqrt(2)/sqrt(N - 1)))),
	 RMSE_up = RMSE * (sqrt(1+(1.96*sqrt(2)/sqrt(N - 1)))-1)) %>%
  as.data.frame()


RMSE_df$yr_ahead <- as.factor(RMSE_df$yr_ahead)


RMSE_df$type.x <- factor(RMSE_df$type.x, levels = mods)



ggplot(filter(RMSE_df,yr_ahead == 2), aes(x = year, y = RMSE, group = type.x)) +
  geom_ribbon(aes(fill = type.x, ymin = RMSE - RMSE_lo, ymax = RMSE + RMSE_up), alpha = 0.3) + 
  geom_line(aes(colour = type.x, group = type.x)) +
  geom_point(aes(colour = type.x)) + 
  geom_vline(aes(xintercept = 30), colour = "grey", linetype = "dashed") +
scale_colour_brewer("model", type = "qual", palette = 2) +
scale_fill_brewer("model", type = "qual", palette = 2) +
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
ggsave(file.path("figures", "RMSE.png"))

RMSE_df2 <- reshape2::dcast(RMSE_df, year + yr_ahead ~ type.x, value.var = "RMSE")

print(xtable(RMSE_df2, digits = 4, caption = "Root Mean Squared Error for each year predictions", label = "tab:3"),
      caption.placement = "top", file = file.path("RMSE.tex"), include.rownames = FALSE,
      tabular.environment = "longtable", floating = FALSE)


## MAE

MAE_df <- pred %>% group_by(type.x, year, yr_ahead) %>%
  summarise(MAE = mean(abs(data.x - data.y), na.rm = T)) %>% as.data.frame()
MAE_df$yr_ahead <- as.factor(MAE_df$yr_ahead)

MAE_df$type.x <- factor(MAE_df$type.x, levels = mods)


ggplot(filter(MAE_df, yr_ahead == 2), aes(x = year, y = MAE, group = type.x)) +
  geom_point(aes(colour = type.x)) + 
  geom_line(aes(colour = type.x)) +
  geom_vline(aes(xintercept = 30), linetype = "dashed", colour = "grey")  +
scale_colour_brewer("model", type = "qual", palette = 2)  +
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
 
 
ggsave(file.path("figures", "MAE.png"))

MAE_df2 <- reshape2::dcast(MAE_df, year + yr_ahead ~ type.x, value.var = "MAE")

print(xtable(MAE_df2, digits = 4, caption = "Mean Absolute Error for each year predictions", label = "tab:4"),
      caption.placement = "top", file = file.path("MAE.tex"), include.rownames = FALSE,
      tabular.environment = "longtable", floating = FALSE)




## MSE by state for each model

pred$error <- pred$data.x - pred$data.y

state_mse <- pred %>% group_by(type.x, year, state, yr_ahead) %>%
  summarise(RMSE = sqrt(mean(error^2))/100,
            n_above = length(error[error > 0]),
            n_below = length(error[error < 0]),
            same    = length(error[error == 0])) %>% as.data.frame()	

state_mse$time <- state_mse$year-23  ## to get each year on 0-1 scale

## the plot

## which colour should the points be ?
state_mse$colour <- state_mse$n_above - state_mse$n_below

## x is year
state_mse$y <- state_mse$time + state_mse$RMSE

library(RColorBrewer)

theme_set(theme_bw())
ggplot(state_mse, aes(x = state, y = year+RMSE)) +
  geom_point(aes(size = RMSE, colour = colour)) +
         geom_line(aes(group = year)) +
  facet_wrap(~type.x, ncol = 2) + scale_size(c(0,1)) +
  scale_colour_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
	panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
ggsave(file.path("figures", "MSE_lines.png"),width = 12, height = 18)


## As a heatmap

state_mse$RMSE[state_mse$RMSE == 0] <- NA

theme_set(theme_bw())
ggplot(state_mse, aes(x = state, y = year+RMSE)) +
  geom_tile(aes(size = RMSE, colour = colour)) +
  facet_wrap(~type.x, ncol = 2) + scale_size(c(0,0.5)) +
  scale_colour_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0),
	panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
 ylab("year") 
ggsave(file.path("figures", "MSE_tile.png"),width = 12, height = 18)


###################################
## Combined table
###################################

RMSE_df2 <- pred %>% group_by(type.x, closure) %>%
  summarise(RMSE = sqrt(mean((data.x - data.y)^2))) %>% as.data.frame()

MAE_df2 <- pred %>% group_by(type.x, closure) %>%
  summarise(MAE = mean(abs(data.x - data.y), na.rm = T)) %>% as.data.frame()

# combined table

combined_table           <- left_join(RMSE_df2, MAE_df2)
combined_table$spearmans <- spearmans$estimate[match(paste(combined_table$type.x, combined_table$closure),
						     paste(spearmans$type.x, spearmans$closure))]
combined_table$spearmans_pvalue <- spearmans$p.value[match(paste(combined_table$type.x, combined_table$closure),
						     paste(spearmans$type.x, spearmans$closure))]
combined_table$spearmans_pvalue <- format.pval(combined_table$spearmans_pvalue)

## remove NAs
combined_table <- combined_table[!is.na(combined_table$type.x),]
colnames(combined_table)[1:2] <- c("model", "closure_period")

combined_table$model_type <- ifelse(combined_table$model %in% c("PastShare", "Gravity", "DSVM", "GravityCombo"), "Process model",
			 ifelse(combined_table$model %in% c("RUM", "Markov", "RUMReparam"), "Statistical model", "Ensemble model"))

combined_table <- cbind(model_type = combined_table[,ncol(combined_table)], combined_table[,-ncol(combined_table)])


print(xtable(combined_table, digits = 4, caption = "Summary of model comparison metrics", label = "tab:4"),
      caption.placement = "top", file = file.path("combined_table.tex"), include.rownames = FALSE,
      tabular.environment = "longtable", floating = FALSE)

######################################################
## Ensemble predictions against the individual models
#####################################################

ens <- filter(pred, type.x == "Dirichlet")

pred$ens <- ens$data.x[match(paste(pred$train.yrs, pred$year, pred$yr_ahead, pred$state, pred$month),
			     paste(ens$train.yrs, ens$year, ens$yr_ahead, ens$state, ens$month))]

ggplot(filter(pred, !type.x %in% c("lm","GravityCombo", "Dirichlet", "PastShare")), aes(x = data.x, y = ens)) +
	geom_point() + geom_abline(intercept = 0, slope = 1) + 
	facet_wrap(~type.x) + ylab("Ensemble predictions") + xlab("Model predictions")
ggsave("Ensemble_Vs_Models.png")
