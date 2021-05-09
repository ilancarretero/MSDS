###################### PART 1: STATISTICAL TOOLS ###############################

## basic commands---------------------------------------------------------------
# Clear console:
# cat("\014") 

# Clear Workspace:
# rm(list=ls())

# Clear plots
# if(!is.null(dev.list())) dev.off()

# Run a script: go to the path and
# source("name_file.R", echo = TRUE)

##### load packages----------------------------------------------------------------
  if (!require(pacman))install.packages("pacman")
  library(pacman)
  pacman::p_load(rmdformats, tidyverse, caret, mice, stringr, VIM, lubridate, zoo, ggthemes, mgcv, MASS,
                 corrplot, rcompanion, ltm, olsrr, GGally, randomForest, FOCI, Boruta, RColorBrewer)

##### PREPROCESSING-------------------------------------------------------------

# Load database
  db <- read.csv(file = "https://raw.githubusercontent.com/ilancarretero/MSDS/main/advanced_regression_and_prediction/part_1_statistical_tools/Escovid19data_CCAA_07-05-2021.csv")

# We have to make the following tasks on the dataset:
  # which variables are useless --> v.1
  # join PCR and new_cases --> v.2
  # which variables have a lot of NAs so are useless too --> v.3
  # compute NAs --> v.4
  # divide into 3 datasets, each for the respective province
  # join the 3 provinces to have the Valencian community dataset
  # We must have: 4 clean datasets with their variables
  
# Useless variables: ccaa, source_name, source, comments
  db_v.1 <- db %>% dplyr::select(!c(ccaa, source_name, source, comments))

# Join PCR and new_cases --> this is the same, but in some periods was counted 
  # in a different way
  new_cases <- db_v.1$new_cases
  PCR <- db_v.1$PCR
  
# for loop to do the sum in a correct way
  n_cases <- rep(0,nrow(db_v.1))
  for(i in 1:length(n_cases)){
    
    if((is.na(new_cases[i]) | is.na(PCR[i])) == FALSE){
      n_cases[i] <- max(new_cases[i], PCR[i])
    } else if((is.na(new_cases[i]) & is.na(PCR[i])) == FALSE){
      n_cases[i] <- sum(new_cases[i], PCR[i], na.rm = TRUE)
    } else {
      n_cases[i] <- NA
    }
    
  }
  
# same for cases accumulated
  cases_accumulated <- db_v.1$cases_accumulated
  cases_accumulated_PCR <- db_v.1$cases_accumulated_PCR
  
  cases_acc <- rep(0,nrow(db_v.1))
  for(i in 1:length(cases_acc)){
    
    if((is.na(cases_accumulated[i]) | is.na(cases_accumulated_PCR[i])) == FALSE){
      cases_acc[i] <- max(cases_accumulated[i], cases_accumulated_PCR[i])
    } else if((is.na(cases_accumulated[i]) & is.na(cases_accumulated_PCR[i])) == FALSE){
      cases_acc[i] <- sum(cases_accumulated[i], cases_accumulated_PCR[i], na.rm = TRUE)
    } else {
      cases_acc[i] <- NA
    }
    
  }
  
# count the NAs
  n_cases_NAs <- sum(is.na(n_cases)) # 139
  cases_acc_NAs <- sum(is.na(cases_acc)) # 122
  
  # we need to compute these NAs
  
# replace new_cases and PCR with n_cases
  db_v.2 <- db_v.1 %>% dplyr::select(!c(new_cases, PCR, cases_accumulated, cases_accumulated_PCR))
  db_v.2 <- db_v.2 %>% dplyr::mutate(new_cases = n_cases, cases_accumulated = cases_acc)
  
# select Valencia
  valencia <- db_v.2 %>% dplyr::filter(str_detect(province, "V"))
 
# select the cases from july 1st 2020 to may 7th 2021 (less NA)
  first_data <- which(valencia$date == "2020-07-01")
  last_data <- which(valencia$date == "2021-05-07")
  db_valencia <- valencia[first_data:last_data,]
  
# Check NAs in db_valencia
  pMiss <- function(x){sum(is.na(x))/length(x)*100}
  how_many_nas_v <- apply(db_valencia,2,pMiss)

# delete variables with all NAs and the province variable
  db_valencia_v.1 <- db_valencia %>% dplyr::select(!c(TestAc, activos, hospitalized_accumulated, hospitalized_new, province))
  
# change names
  db_valencia_v.1 <- db_valencia_v.1 %>%
    rename(
      hosp = hospitalized,
      cases_acc = cases_accumulated,
      i_care = intensive_care,
      dead = deceased,
      rec = recovered,
      n_cases = new_cases
    )
  
# NA pattern
  md.pattern(db_valencia_v.1)

# NA visualization
  aggr_plot_val <- aggr(db_valencia_v.1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(db_valencia_v.1), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# imputing the missing data
  completed_valencia_data <- na.locf(db_valencia_v.1)
  #db_valencia_v.1_temp <- mice(db_valencia_v.1,m=1,maxit = 100, meth='pmm',seed=500)
  #summary(db_valencia_v.1_temp)

# check imputed data 
  #db_valencia_v.1_temp$imp$n_cases
  #db_valencia_v.1_temp$meth
  
# completed data
  #completed_valencia_data <- complete(db_valencia_v.1_temp,1)

# inspecting distribution of original and imputed data
  
  #xyplot(db_valencia_v.1_temp, n_cases ~  hosp + i_care + dead,pch=18,cex=1)
  #xyplot(db_valencia_v.1_temp, n_cases ~  rec + cases_acc,pch=18,cex=1)
  #densityplot(db_valencia_v.1_temp)
  
# final check of NA
  sum(is.na(completed_valencia_data)) #0

# delete cases_acc (redundant for new_cases)
  completed_valencia_data_v.1 <- completed_valencia_data %>% dplyr::select(!cases_acc)

# add year, month and day variable
  completed_valencia_data_v.1$date <- as.Date(completed_valencia_data_v.1$date, "%Y-%m-%d")
  completed_valencia_data_final <- completed_valencia_data_v.1 %>%
    mutate(year = as.factor(format(completed_valencia_data_v.1$date, format = "%Y")),
           month = as.factor(format(completed_valencia_data_v.1$date, format = "%m")),
           day = as.factor(format(completed_valencia_data_v.1$date, format = "%d")),)
  
  val_db <- completed_valencia_data_final

# add lag_1 to capture the dynamic
  val_db <- val_db %>% mutate(n_cases_lag1 = lag(n_cases)) %>% drop_na()
  
# training and test set
  last_day <- which(val_db$date == "2021-05-03")
  train <- val_db[1:last_day,]
  test <- val_db[-(1:last_day),]
    
# delete date variable
  #completed_valencia_data_final <- completed_valencia_data_final %>% dplyr::select(!date)

  
##### FIRST PART----------------------------------------------------------------
  
# Valencia COVID new cases trend
  ggplot(train, aes(x=date , y=n_cases)) + geom_line(color="red", size=1.2) + theme_economist()

# First simple model with the date information
  model.simple <- lm(train$n_cases ~ train$date)
  summary(model.simple)

# plot model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + 
    stat_smooth(method="lm", formula=y ~ x, se=FALSE, col="black") + theme_economist()
  # problem with the model because is not linear
  
# residuals
  ggplot(train) + aes(x=date, y=residuals(model.simple)) + geom_point() + theme_economist()
  #somehow cubic
  
# second-order polynomial
  model.quadratic <- lm(n_cases ~ poly(date,2), train)
  summary(model.quadratic)
  
# plot quadratic model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + 
    stat_smooth(method="lm", formula=y ~ poly(x,2), se=FALSE, col="black") + theme_economist()
  
# Residuals
  ggplot(train) + aes(x=date, y=residuals(model.quadratic)) + geom_point() + theme_economist()
  
# third-order polynomial
  model.4 <- lm(n_cases ~ poly(date,3), train)
  summary(model.4)
  
# plot cubic model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + 
  stat_smooth(method="lm", formula=y ~ poly(x,3), se=FALSE, col="black") + theme_economist()
  
# Residuals
  ggplot(train) + aes(x=date, y=residuals(model.4)) + geom_point() + theme_economist()
  
# Residuals as a function of the month
  ggplot(train) + aes(x=month, y=residuals(model.simple)) + geom_boxplot() + theme_economist()
  
# We could also guess this seasonality using original data
  ggplot(train, aes(x=month, y=n_cases)) + geom_boxplot(fill="green") + theme_minimal() + theme_economist()

# Incorporate the seasonal part
  model.season <- lm(n_cases ~  poly(date,3) + month, train)
  summary(model.season)
  
# Plot model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + geom_line(y=predict(model.season))  + theme_economist()
  
# Residuals
  ggplot(train) + aes(x=month, y=residuals(model.season)) + geom_boxplot() + theme_economist()

# Residuals
  ggplot(train) + aes(x=date, y=residuals(model.season)) + geom_point() + theme_economist()
  
# predict with lag
  model.season <- lm(n_cases ~  poly(date,3) + month + n_cases_lag1, train)
  summary(model.season)

# Plot model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + geom_line(y=predict(model.season))  + theme_economist()
  # much much better  
  
# Residuals
  ggplot(train) + aes(x=date, y=residuals(model.season)) + geom_point() + theme_economist()
  
# Prediction
  predict(model.season, newdata = test, interval = 'prediction')
# value --> 81 #so near
  

##### SECOND PART---------------------------------------------------------------
  
# non-linear regression
  train %>% ggplot(aes(x = date, y = n_cases, colour=month)) + geom_point() +
    geom_smooth(method = "loess", formula = y~x, size = 1) + theme_economist()
  
# Advanced model: with interactions, but we can add more non-linear terms
  model.adv <- lm(n_cases ~ n_cases_lag1 + date + month + month:n_cases_lag1, train)
  summary(model.adv)

# Plot model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + geom_line(y=predict(model.adv))  + theme_economist()
  # good model
  
# Residuals
  ggplot(train) + aes(x=date, y=residuals(model.adv)) + geom_point() + theme_economist()
  # bad model when we see a pattern here 
  
  ggplot(train) + aes(x=month, y=residuals(model.adv)) + geom_point() + theme_economist()
  # pattern in the months
  
  ggplot(train) + aes(x=n_cases_lag1, y=residuals(model.adv)) + geom_point() + theme_economist()
  # We have discounted auto-correlation  
  
## Robust regression
  
# prediction error of the linear model
# residuals per date
  ggplot(train) + aes(x=date, y=rstudent(model.adv)) + geom_point() + 
  geom_hline(yintercept=c(-2.33,2.33), color="blue") + theme_economist()
  
# residuals per month
  ggplot(train) + aes(x=month, y=rstudent(model.adv), fill = month) + geom_boxplot() + 
  geom_hline(yintercept=c(-2.33,2.33), color="blue") + theme_economist()
  
# There are some outliers
  train[which(abs(rstudent(model.adv)) > 2.33),c(1,8,10)]
# 2.33 standard deviations
  
# lets give less weights to the outliers --> robust regression
  
# Diagnostic plots: residuals, fitted values, Cook’s distance, and leverage
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  plot(model.adv, las = 1)
  # There are some points specially problematic
  
# Estimate now the model with robust regression
  linfit.huber <- rlm(n_cases ~ n_cases_lag1 + month + date, data=train, psi=psi.huber) # at the end you include the lost function psi.nameOfLossFunction
  summary(linfit.huber)

# residuals
  ggplot(train) + aes(x=date, y=rstudent(linfit.huber)) + geom_point() + 
  geom_hline(yintercept=c(-2.33,2.33), color="blue") + theme_economist()
  
# residuals
  ggplot(train) + aes(x=month, y=rstudent(linfit.huber)) + geom_point() + 
  geom_hline(yintercept=c(-2.33,2.33), color="blue") + theme_economist()
  
# There are some outliers
  train[which(abs(rstudent(linfit.huber)) > 2.33),c(1,8,10)]  
  # alot of outliers on january

# Let's sort the data set by Huber weights:
  cbind(train, residH = linfit.huber$resid, weightH = linfit.huber$w) %>% arrange(weightH) %>% head() # $w to get the weights
  # normal months --> weights 1
  
# Note in OLS regression, all observations have a weight of 1
  
# let’s run the same model, but using the bisquare weighting function:
  linfit.bi <- rlm(n_cases ~ n_cases_lag1 + date + month, data=train, psi=psi.bisquare) # other loss function --> bisquare
  summary(linfit.bi)
  cbind(train, residB = linfit.bi$resid, weightB = linfit.bi$w) %>% arrange(weightB) %>% head()
  # The weight given to the biggest outlier is lower, close to dissapear 
  
# Plot the models and the residuals of the models
  
  # Huber
  # Plot model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + geom_line(y=predict(linfit.huber))  + theme_economist()
  
  # Residuals
  ggplot(train) + aes(x=month, y=residuals(linfit.huber)) + geom_boxplot() + theme_economist()
  
  # Residuals
  ggplot(train) + aes(x=date, y=residuals(linfit.huber)) + geom_point() + theme_economist()
  
  # bisquare
  # Plot model
  ggplot(train, aes(x=date, y=n_cases)) + geom_line(color="red", size=1.2) + geom_line(y=predict(linfit.bi))  + theme_economist()
  
  # Residuals
  ggplot(train) + aes(x=month, y=residuals(linfit.bi)) + geom_boxplot() + theme_economist()
  
  # Residuals
  ggplot(train) + aes(x=date, y=residuals(linfit.bi)) + geom_point() + theme_economist()
  
  # in this case better robust regression --> less residual standard error
  

##### PART 3--------------------------------------------------------------------
  
# CORRELATIONS (numerical variables)
  # Which are the most correlated variables with price?
  corr_n_cases <- sort(cor(train[,c(2,3,4,5,6,10)])["n_cases",], decreasing = T)
  corr=data.frame(corr_n_cases)
  ggplot(corr,aes(x = row.names(corr), y = corr_n_cases)) + 
    geom_bar(stat = "identity", fill = "#E9B479") + 
    scale_x_discrete(limits= row.names(corr)) +
    labs(x = "Predictors", y = "Number of cases", title = "Correlations") + 
    theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
          axis.text.x = element_text(angle = 45, hjust = 1)) + theme_economist()
  # a lot of variables with very interesting correlation
  
  vec_quan <- c(2,3,4,5,6,10)
  quan_db <-cor(train[,vec_quan])
  corrplot::corrplot(quan_db, type="upper", order="hclust",
           col=RColorBrewer::brewer.pal(n=8, name="RdYlBu"))
  corrplot::corrplot(quan_db, method = "number")
  # correlations between numerical variables
  
  # n_cases vs dead
  ggplot(train, aes(x=dead, y=n_cases)) + ylab("number of cases") + 
    geom_point(alpha=0.6) + ggtitle("Number of cases vs deceased of COVID-19") + theme_economist()
  # Non-linear relation --> quadratic relation
  
  # n_cases vs recoveries
  ggplot(train, aes(x=rec, y=n_cases)) + ylab("number of cases") + xlab("recoveries") + 
    geom_point(alpha=0.6) + ggtitle("Number of cases vs recoveries of COVID-19") + theme_economist()
  # Non-linear relation --> quadratic relation 
  
  
# Simple regression: try first the most relevant predictor from previous analysis
  linFit <- lm(n_cases ~ hosp, data=train)
  summary(linFit)
  # hospitalization highly significant
  
# diagnosis
  par(mfrow=c(2,2))
  plot(linFit, pch=23 ,bg='#F8D0A3',cex=2)
  
# Simple regression:
  linFit <- lm(log(n_cases) ~ log(hosp), data=train)
  summary(linFit)
  
# diagnosis
  par(mfrow=c(2,2))
  plot(linFit, pch=23 ,bg='#F8D0A3',cex=2)
  
# Multiple regression:
  linFit <- lm(log(n_cases) ~ n_cases_lag1  + log(hosp) + poly(dead,2) + date + month + year + day, data=train)
  summary(linFit)
  # good enough
  
# Multiple regression with logs and most important variables:
  linFit <- lm(n_cases ~ n_cases_lag1 + hosp, data=train)
  summary(linFit)  
  
  linFit <- lm(log(n_cases) ~ log(n_cases_lag1) + log(hosp), data=train)
  summary(linFit)
  
  linFit <- lm(log(n_cases) ~ log(n_cases_lag1) + log(hosp) + poly(dead,2), data=train)
  summary(linFit)
  
  linFit <- lm(log(n_cases) ~ log(n_cases_lag1) + log(hosp) + poly(dead,2) + month, data=train)
  summary(linFit)
    
### Robust regression
  
# residuals
  ggplot(train) + aes(x=log(hosp), y=rstudent(linFit)) + geom_point() + 
    geom_hline(yintercept=c(-2.33,2.33), color="blue") + theme_economist()
  # some outliers
  
# Diagnostic plots: residuals, fitted values, Cook’s distance, and leverage
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  plot(linFit, las = 1)
  # some outliers

# huber function
  linfit.huber <- rlm(log(n_cases) ~ log(n_cases_lag1) + log(hosp), data=train)
  summary(linfit.huber)
  # a little bit improvement
  
# bisquare function
  linfit.bisquare <- rlm(log(n_cases) ~ log(n_cases_lag1) + log(hosp), data=train, psi = psi.bisquare)
  summary(linfit.bisquare)
  # huber function works better, bisquare little improvement respect to lm
  

# Predictions
  
  prLM <- predict(linFit, newdata=test)
  cor(log(test$n_cases), prLM)^2 #we only have 1 number and we need a vector
  mean(abs(log(test$n_cases)- prLM)/log(test$n_cases))

  prHuber <- predict(linfit.huber, newdata=test)
  cor(log(test$n_cases), prHuber)^2
  mean(abs(log(test$n_cases)- prHuber)/log(test$n_cases))

  prBi <- predict(linfit.bisquare, newdata=test)
  cor(log(test$n_cases), prBi)^2
  mean(abs(log(test$n_cases)- prBi)/log(test$n_cases))  
    
# the better the linear fit because it is more easy and interpretable

### Model selection

# Try a model selection tool to simplify the model:
# exhaustive method --> all possible combinations
  model = log(n_cases) ~ log(n_cases_lag1) + month + log(hosp) + poly(dead,2) + date
  linFit <- lm(model, data=train)
  ols_step_all_possible(linFit) # All possible subset regressions: the number is exponential with p 
  ols_step_best_subset(linFit) # The best subset regression for each p: still exponential with p 

# More practical methods when dimension (p/n) is high 
  ols_step_forward_p(linFit) # forward based on p-value
  plot(ols_step_forward_p(linFit))  # 3 variables
  
  ols_step_forward_aic(linFit) # forward based on AIC
    
  ols_step_backward_aic(linFit) # backward AIC  
  
  ols_step_both_aic(linFit) # stepwise AIC

# this model with 3 variables seems reasonable: it's simple and explains well the price variability
  linFit <- lm(log(n_cases) ~ month + log(n_cases_lag1) + log(hosp), data=train)
  summary(linFit)

### Prediction

# Take care: output is in logs
  predictions <- exp(predict(linFit, newdata=test))
  cor(test$n_cases, predictions)^2
  RMSE <- sqrt(mean((predictions - test$n_cases)^2))
  RMSE
  
# How do we know we are doing well?
  
# Always consider a benchmark model (or a reference)
# For instance, we can predict all the new home prices as the average price in the training set
  mean(train$n_cases)
# This is equivalent to
  benchFit <- lm(n_cases ~ 1, data=train) # beta0 is the mean so it is the same
  predictions <- predict(benchFit, newdata=test)
  cor(test$n_cases, predictions)^2 # variance 0 because it is constant the prediction
  RMSE <- sqrt(mean((predictions - test$n_cases)^2))
  RMSE
# always compare the model with a simple one and see what happens
  
### Advanced regression models

# Most models need calibration, estimation and validation

# Caret package highly recommended:
# to evaluate performance abd calibrate sensitive parameters
# to choose the best model across these parameters
# to estimate model performance from a training set
# Main function: train()
  
# Each model can be automatically tuned and evaluated 
# In this case, we are goint to use 4 repeats of 5-fold cross validation
  ctrl <- trainControl(method = "repeatedcv", 
                       number = 5, repeats = 4)

  modelS1 = n_cases ~ month + n_cases_lag1 + hosp    
  modelS2 = log(n_cases) ~ month + log(n_cases_lag1) + log(hosp)
  modelF = log(n_cases) ~ month*log(hosp) + log(n_cases_lag1) +  poly(dead,2) + date + year + day

# Finally, create a data frame with all the predictors (prices in logs)
  test_results <- data.frame(n_cases = log(test$n_cases))
  
# Linear regression
  lm_tune <- train(modelS1, data = train, 
                   method = "lm", 
                   preProc=c('scale', 'center'),
                   trControl = ctrl)
# cross validation in a linear model is useless,
# because we don't have hyper-parameters to optimize (we only have betas)
  
  lm_tune
  test_results$lm <- predict(lm_tune, test)
  postResample(pred = test_results$lm,  obs = test_results$n_cases)
  

  qplot(test_results$lm, test_results$n_cases) + 
    labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
    geom_abline(intercept = 2.7, slope = 0.05, colour = "blue") +
    theme_economist()
  # some  bias
  
  
  lm_tune_2 <- train(modelS2, data = train, 
                   method = "lm", 
                   preProc=c('scale', 'center'),
                   trControl = ctrl)
  # cross validation in a linear model is useless,
  # because we don't have hyper-parameters to optimize (we only have betas)
  importance <- varImp(lm_tune_2, scale=FALSE)
  plot(importance)
  
  
  lm_tune_2
  test_results$lm_2 <- predict(lm_tune_2, test)
  postResample(pred = test_results$lm_2,  obs = test_results$n_cases)
  
  
  qplot(test_results$lm_2, test_results$n_cases) + 
    labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
    geom_abline(intercept = 2, slope = 0.6, colour = "blue") +
    theme_economist()
  # some small bias
  

  
  
  alm_tune <- train(modelF, data = train, 
                    method = "lm", 
                    preProc=c('scale', 'center'),
                    trControl = ctrl)
  # as we have overfitting we have a lot of warnings
  test_results$alm <- predict(alm_tune, test)
  postResample(pred = test_results$alm,  obs = test_results$n_cases)
  # is overfitting dangerous to predict?
  # worst model but the prediction is better
  # it is not dangerous with the prediction
  # however to see the p-values is worst
  importance <- varImp(alm_tune, scale=FALSE)
  plot(importance)
  
  
  
  qplot(test_results$alm, test_results$n_cases) + 
    labs(title="Linear Regression Observed VS Predicted", x="Predicted", y="Observed") +
    geom_abline(intercept = 3.5, slope = 0.1, colour = "blue") +
    theme_economist()
  # some  bias
  
  
  # Forward regression
  for_tune <- train(modelF, data = train, 
                    method = "leapForward", 
                    preProc=c('scale', 'center'),
                    tuneGrid = expand.grid(nvmax = 1:20),
                    trControl = ctrl)
  
  for_tune
  plot(for_tune)
  #variables selected
  coef(for_tune$finalModel, for_tune$bestTune$nvmax)
  test_results$frw <- predict(for_tune, test)
  postResample(pred = test_results$frw,  obs = test_results$n_cases)
  
  qplot(test_results$frw, test_results$n_cases) + 
    labs(title="Forward Regression Observed VS Predicted", x="Predicted", y="Observed") +
    geom_abline(intercept = 3.5, slope = 0.2, colour = "blue") +
    theme_economist()
  # some bias, very similar to lm
  
  
  # Backward regression
  back_tune <- train(modelF, data = train, 
                     method = "leapBackward", 
                     preProc=c('scale', 'center'),
                     tuneGrid = expand.grid(nvmax = 1:20),
                     trControl = ctrl)
  
  back_tune
  plot(back_tune)
  # which variables are selected?
  coef(back_tune$finalModel, back_tune$bestTune$nvmax)
  test_results$bw <- predict(back_tune, test)
  postResample(pred = test_results$bw,  obs = test_results$n_cases)
  # good but not as good as the best model M2
  
  qplot(test_results$bw, test_results$n_cases) + 
    labs(title="Backward Regression Observed VS Predicted", x="Predicted", y="Observed") +
    geom_abline(intercept = 3.7, slope = 0.2, colour = "blue") +
    theme_economist()
  # some bias, very similar to lm
  
  
  # Stepwise regression
  step_tune <- train(modelF, data = train, 
                     method = "leapSeq", 
                     preProc=c('scale', 'center'),
                     tuneGrid = expand.grid(nvmax = 1:20),
                     trControl = ctrl)
  
  plot(step_tune)
  # which variables are selected?
  coef(step_tune$finalModel, step_tune$bestTune$nvmax)
  test_results$seq <- predict(step_tune, test)
  postResample(pred = test_results$seq,  obs = test_results$n_cases)
  # good method but still the best method is still a S2 method to predict
  
  # Ridge regression
  ridge_grid <- expand.grid(lambda = seq(0, .1, length = 20)) # define the grid
  ridge_tune <- train(modelF, data = train,
                      method='ridge',
                      preProc=c('scale','center'),
                      tuneGrid = ridge_grid,
                      trControl=ctrl)
  
  plot(ridge_tune)
  ridge_tune$bestTune
  test_results$ridge <- predict(ridge_tune, test)
  postResample(pred = test_results$ridge,  obs = test_results$n_cases)
  # a bit worse, but easier to use...
  # if ridge = 0 --> OLS is the best for that --> the computer
  # is seeing that is the best model
  # again the overfitted model is the better (approx 0.21)
  
  
  # The Lasso
  lasso_grid <- expand.grid(fraction = seq(.01, 1, length = 20))
  lasso_tune <- train(modelF, data = train,
                      method='lasso',
                      preProc=c('scale','center'),
                      tuneGrid = lasso_grid,
                      trControl=ctrl)
  
  plot(lasso_tune)
  # the ro we need the minimum
  lasso_tune$bestTune
  test_results$lasso <- predict(lasso_tune, test)
  postResample(pred = test_results$lasso,  obs = test_results$n_cases)
  # still better the overfitted method
  
  
  # Elastic net
  modelLookup('glmnet')
  elastic_grid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, 1, 0.1)) # 2 dimensional grid
  glmnet_tune <- train(modelF, data = train,
                       method='glmnet',
                       preProc=c('scale','center'),
                       tuneGrid = elastic_grid,
                       trControl=ctrl)
  
  
  importance <- varImp(glmnet_tune, scale=FALSE)
  plot(importance)
  
  
  plot(glmnet_tune)
  glmnet_tune$bestTune
  test_results$glmnet <- predict(glmnet_tune, test)
  postResample(pred = test_results$glmnet,  obs = test_results$n_cases)
  # best prediction
  
  # PCR
  # with Caret:
  pcr_tune <- train(modelF, data = train,
                    method='pcr',
                    preProc=c('scale','center'),
                    tuneGrid = expand.grid(ncomp = 1:15),
                    trControl=ctrl)
  
  # hyperparameter a discrete number
  plot(pcr_tune)
  test_results$pcr <- predict(pcr_tune, test)
  postResample(pred = test_results$pcr,  obs = test_results$n_cases)
  # one of the best
  
  # PLS
  # With Caret:
  pls_tune <- train(modelF, data = train,
                    method='pls',
                    preProc=c('scale','center'),
                    tuneGrid = expand.grid(ncomp = 1:15),
                    trControl=ctrl)
  
  
  importance <- varImp(pls_tune, scale=FALSE)
  plot(importance)
  
  
  plot(pls_tune)
  test_results$pls <- predict(pls_tune, test)
  postResample(pred = test_results$pls,  obs = test_results$n_cases)
  # not the best
  
  # Let's look again at the performance for all models:
  apply(test_results[-1], 2, function(x) cor(x,test_results$n_cases)^2)
  apply(test_results[-1], 2, function(x) mean(abs(x - test_results$n_cases)))
  
  # Why is it better to combine the best (let's say 3) models than choosing the best one?
  test_results %>%
    dplyr::select(-n_cases) %>%
    ggcorr(palette = "RdBu", label = TRUE) + labs(title = "Correlations between different models")
  # combination is going to improve results, but not too much...
  
  # Combination
  test_results$comb = (test_results$lm_2 + test_results$glmnet + test_results$pls)/3
  postResample(pred = test_results$comb,  obs = test_results$n_cases)
  # not improvement --> obviously, because of the results
  
  ### Prediction Intervals 
  
  # Sometimes it's convenient to use a custom loss function to evaluate the quality of predictions (instead of using R2, MAE, etc.)
  # Other times it's convenient to obtain confidence intervals for predictions
  
  # Take care: unlike confidence intervals from classical statistics (for a population parameter), 
  # prediction intervals are about individual predictions (conditional to variables)
  
  # Easy way:
  # take a simple but good lm:
  linFit <- lm(log(n_cases) ~ month + log(n_cases_lag1) + log(hosp), data=train)
  # obtain the prediction intervals directly:
  predictions <- exp(predict.lm(linFit, newdata=test, interval="prediction", level=0.90))
  head(predictions)
  predictions=as.data.frame(predictions)
  predictions$real = exp(test_results$n_cases)
  
  predictions = predictions %>% mutate(out=factor(if_else(real<lwr | real>upr,1,0)))
  # how many real observations are out of the intervals? --> 0 but a lot of variability
  mean(predictions$out==1)
  
  # visualization of the confidence intervals
  ggplot(predictions, aes(x=fit, y=real))+
    geom_point(aes(color=out)) + theme(legend.position="none") +
    geom_ribbon(data=predictions,aes(ymin=lwr,ymax=upr),alpha=0.3) +
    labs(title = "Prediction intervals", x = "prediction",y="real number of cases") + theme_economist()
  
  
  # But what to do if we want to use a model with better prediction performance?
  # For instance, use the combination model
  # Then we cannot use the predict() function...
  
  # Final predictions:
  yhat = exp(test_results$comb)
  head(yhat) # show the prediction for 6 home prices
  hist(yhat, col="#FBAF5A")
  # take care: asymmetric distribution
  
  y = exp(test_results$n_cases)
  error = y-yhat
  hist(error, col="#FBAF5A")
  # But the error is more symmetric --> pff
  
  # Let's use the first 2 number of cases in testing to compute the noise size
  noise = error[1:2]
  sd.noise = sd(noise)
  
  # Let's use the rest of the homes to validate the coverage
  # If noise is a normal variable, then a 90%-CI can be computed as
  lwr = yhat[3:length(yhat)] - 1.65*sd.noise
  upr = yhat[3:length(yhat)] + 1.65*sd.noise
  
  # Non-parametric way:
  lwr = yhat[3:length(yhat)] + quantile(noise,0.05, na.rm=T)
  upr = yhat[3:length(yhat)] + quantile(noise,0.95, na.rm=T)
  
  # Non-parametric way, robust against outliers, but with no confidence level:
  lwr = yhat[3:length(yhat)] + boxplot.stats(noise, coef=1.5)$stats[1]
  upr = yhat[3:length(yhat)] + boxplot.stats(noise, coef=1.5)$stats[5]
  # This is a useful approach for anomaly detection
  # points outside the intervals are outliers
  # these are extreme outliers
  lwr = yhat[3:length(yhat)] + boxplot.stats(noise, coef=3)$stats[1]
  upr = yhat[3:length(yhat)] + boxplot.stats(noise, coef=3)$stats[5]
  # Non-parametric way, even more robust against outliers
  lwr = yhat[3:length(yhat)] - 2.4*mad(noise) 
  upr = yhat[3:length(yhat)] + 2.4*mad(noise)
  # for normal data, sd=1.48*mad
  
  # For a symmetric distribution with zero mean, the mad is the 75th percentile of the distribution
  # That means, the following interval has confidence level 50% (if noise satisfies those assumptions):
  lwr = yhat[3:length(yhat)] - mad(noise) 
  upr = yhat[3:length(yhat)] + mad(noise)
  # it's called the probable error:  half of the values lies within the interval and half outside
  
  predictions = data.frame(real=y[3:length(y)], fit=yhat[3:length(yhat)], lwr=lwr, upr=upr)
  predictions = predictions %>% mutate(out=factor(if_else(real<lwr | real>upr,1,0)))
  
  # how many real observations are out of the intervals?
  mean(predictions$out==1)
  # the real coverage of parametric approach is around 15% and the nominal one is 10%, hence it's ok, 
  # but the intervals should be a bit wider
  
  # the real coverage of non-parametric approach is around 15% and the nominal one is 10%, hence it's ok, 
  # but the intervals should be a bit wider
  
  # the real coverage of robust approach is around 8%. Here, no nominal value
  
  ggplot(predictions, aes(x=fit, y=real))+
    geom_point(aes(color=out)) + theme(legend.position="none") +
    geom_ribbon(data=predictions,aes(ymin=lwr,ymax=upr),alpha=0.3) +
    labs(title = "Prediction intervals", x = "prediction",y="number of COVID-19 cases") + theme_economist()

  
##### FEATURE SELECTION---------------------------------------------------------  
## RECURSIVE FEATURE ELIMINATION
  modelRFE <- rfe(n_cases ~  ., data=train, sizes=c(1:10), rfeControl=rfeControl(functions=rfFuncs, method="cv", number=10))
  print(modelRFE)
  
  # list the chosen features
  predictors(modelRFE)
  
  # visualization
  ggplot(data = modelRFE, metric = "MAE") + theme_economist()
  
## RANDOM FOREST
  fitRF <- train(n_cases ~  ., data=train, method='rf', preProcess="scale", trControl=ctrl) 
  importance <- varImp(fitRF, scale=FALSE)
  plot(importance)
  which(importance$importance$Overall>mean(importance$importance$Overall))
  
## FOCI
  #foci(train[,6],train[,c(2,3,4,5,10)])$selectedVar$index
  
## BORUTA
  boruta <- Boruta(n_cases ~ ., data=train, doTrace=0)  
  plot(boruta, cex.axis=.7, las=2, xlab="", main="Variable Importance")
  
      