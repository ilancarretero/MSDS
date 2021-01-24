#--------------------------STATISTICAL LEANING----------------------------------

#--------------------------Part 1: Statistical Tools----------------------------

# PACKAGES
#-------------------------------------------------------------------------------
# Load Packages we a re going to use
if(!require(pacman))install.packages("pacman")
library(pacman)

pacman::p_load(caret, ggplot2, dplyr, ggthemes,
               xtable, MLeval, naivebayes, ggmosaic,
               corrplot,RColorBrewer, ltm, rcompanion, VGAM, glmnet,
               bestglm, naivebayes, lda, MASS)

# Options for xtable package
options(xtable.timestamp = "")
#-------------------------------------------------------------------------------

# FUNCTIONS
#-------------------------------------------------------------------------------
# CONFUSION MATRIX PLOT FUNCTION
#FUNCTION: draw_confusion_matrix
#INPUTS: cm: confusion matrix
#        title: title of the plot
#OUTPUT: Beautiful confusion matrix

draw_confusion_matrix <- function(cm, title) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n',
       yaxt='n')
  title(title, cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'DEAD', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'LIVE', cex=1.2)
  text(125, 370, 'Predicted class', cex=1.3, srt=90, font=2)
  text(245, 450, 'True class', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'DEAD', cex=1.2, srt=90)
  text(140, 335, 'LIVE', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="",
       main = "EVALUATION METRICS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

# Clear console:
# cat("\014") 

# Clear Workspace:
# rm(list=ls())

# Clear plots
# if(!is.null(dev.list())) dev.off()

# Run a script: go to the path and
# source("name_file.R", echo = TRUE)
#-------------------------------------------------------------------------------


# LOAD DATABASE
#-------------------------------------------------------------------------------
# Set working directory
setwd("C:\\Users\\carre\\Google Drive\\semester 2\\statistical_learning\\project\\part1")

# Load the heart_failure_clinical_records_dataset
hf_db <- read.csv("heart_failure_clinical_records_dataset.csv")
#-------------------------------------------------------------------------------


# ANALYSIS OF THE DATABASE
#-------------------------------------------------------------------------------

# Check type of variables
str(hf_db)
# QUANTITATIVE VARIABLES:
  # age
  # creatinine_phosphokinase
  # ejection_fraction
  # platelets
  # serum_creatinine
  # serum_sodium
  # time

# QUALITATIVE VARIABLES
  # anaemia
  # diabetes
  # high_blood_pressure
  # sex
  # smoking

# VARIABLE TO PREDICT
  # DEATH_EVENT
  # 1 --> Dead    0 --> Live
hf_db <- hf_db %>%
  mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT==1, "Dead")) %>%
  as.data.frame()

hf_db <- hf_db %>%
  mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT==0, "Live")) %>%
  as.data.frame()

  

# Check NA's
  NA_db <- any(is.na(hf_db)) #FALSE

# Check basic values of the variables
  summary_db <- summary(hf_db)

# Check distributions of the variables
  #Quantitative variables: kernel density
  
  # age
  hf_db %>% ggplot(aes(x = age)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
    
    
  # creatinine_phosphokinase
  hf_db %>% ggplot(aes(x = creatinine_phosphokinase)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
  
  
  # ejection_fraction
  hf_db %>% ggplot(aes(x = ejection_fraction)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
  
  
  # platelets
  hf_db %>% ggplot(aes(x = platelets)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
  
  
  # serum_creatinine
  hf_db %>% ggplot(aes(x = serum_creatinine)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
  
  # serum_sodium
  hf_db %>% ggplot(aes(x = serum_sodium)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
  
  # time
  hf_db %>% ggplot(aes(x = time)) +  
    geom_density(aes(group = DEATH_EVENT, 
                     colour = DEATH_EVENT, 
                     fill = DEATH_EVENT),
                 alpha = 0.2) + 
    theme_economist() +
    scale_fill_manual(values=c("#B4464B", "#4BB446")) +
    scale_color_manual(values=c("#B4464B", "#4BB446")) 
  
  
  #Qualitative variables: mosaic plot
  
    # anaemia
  ggplot(data=hf_db)+geom_mosaic(aes(x=product(DEATH_EVENT, anaemia), fill=DEATH_EVENT)) +
    ggtitle("Mosaic Plot of Anaemia and Death event") +
    theme_economist() +  xlab("Anaemia (NO, YES)") + ylab("Death event") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
          axis.title.x = element_text(face = "bold"), legend.position = "right",
          title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
    scale_fill_discrete(name = "Death event", labels = c("Dead", "Live")) 
  
    # diabetes
  ggplot(data=hf_db)+geom_mosaic(aes(x=product(DEATH_EVENT, diabetes), fill=DEATH_EVENT)) +
    ggtitle("Mosaic Plot of Diabetes and Death event") +
    theme_economist() +  xlab("Diabetes (NO, YES)") + ylab("Death event") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
          axis.title.x = element_text(face = "bold"), legend.position = "right",
          title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
    scale_fill_discrete(name = "Death event", labels = c("Dead", "Live"))
  
  
    # high_blood_pressure
  ggplot(data=hf_db)+geom_mosaic(aes(x=product(DEATH_EVENT, high_blood_pressure), fill=DEATH_EVENT)) +
    ggtitle("Mosaic Plot of High blood pressure and Death event") +
    theme_economist() +  xlab("High blood pressure (NO, YES)") + ylab("Death event") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
          axis.title.x = element_text(face = "bold"), legend.position = "right",
          title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
    scale_fill_discrete(name = "Death event", labels = c("Dead", "Live"))
  
  
    # sex
  ggplot(data=hf_db)+geom_mosaic(aes(x=product(DEATH_EVENT, sex), fill=DEATH_EVENT)) +
    ggtitle("Mosaic Plot of sex and Death event") +
    theme_economist() +  xlab("sex (WOMAN, MAN)") + ylab("Death event") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
          axis.title.x = element_text(face = "bold"), legend.position = "right",
          title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
    scale_fill_discrete(name = "Death event", labels = c("Dead", "Live"))
  
  
    # smoking
  ggplot(data=hf_db)+geom_mosaic(aes(x=product(DEATH_EVENT, smoking), fill=DEATH_EVENT)) +
    ggtitle("Mosaic Plot of Smoking and Death event") +
    theme_economist() +  xlab("smoking (NO, YES)") + ylab("Death event") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
          axis.title.x = element_text(face = "bold"), legend.position = "right",
          title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
    scale_fill_discrete(name = "Death event", labels = c("Dead", "Live"))
  
  
 # Check normality --> No distribution is Gaussian or more or less similar
    shapiro.test(hf_db$age) # p-value = 5.35e-05
    shapiro.test(hf_db$anaemia) # p-value < 2.2e-16
    shapiro.test(hf_db$creatinine_phosphokinase) # p-value < 2.2e-16
    shapiro.test(hf_db$diabetes) # p-value < 2.2e-16
    shapiro.test(hf_db$ejection_fraction) # p-value = 7.216e-09
    shapiro.test(hf_db$high_blood_pressure) # p-value < 2.2e-16
    shapiro.test(hf_db$platelets) # p-value = 2.883e-12
    shapiro.test(hf_db$serum_creatinine) # p-value < 2.2e-16
    shapiro.test(hf_db$serum_sodium) # p-value = 9.215e-10
    shapiro.test(hf_db$sex) # p-value < 2.2e-16
    shapiro.test(hf_db$smoking) # p-value < 2.2e-16
    shapiro.test(hf_db$time) # p-value = 6.285e-09
  

 # Check CORRELATIONS:
#-------------------------------------------------------------------------------    
    # Predictors:
    vec_quan <- c(1,3,5,7,8,9,12)
    vec_qual <- c(2,4,6,10,11)
     
    # Quantitative-quantitative: linear dependency pearson correlation
    quan_db <-cor(hf_db[,c(1,3,5,7,8,9,12)])
    corrplot(quan_db, type="upper", order="hclust",
             col=brewer.pal(n=8, name="RdYlBu"))
    # Practically no linear correlations
    
    # Lets do it for non_linear dependency: kendall correlation
    db_quan <- hf_db %>% dplyr::select(1,3,5,7,8,9,12)
    kendall_cor <- cor(db_quan, method = "kendall")
    # Values too small --> so no non-linear dependency
    
    
    # Quantitative-qualitative: point biserial correlation
    zeros_1 <- rep(0,35)
    point_biserial_matrix <- matrix(zeros_1, nrow = 5)
    
    for (i in seq(1:length(vec_qual))) {
      for (j in seq(1:length(vec_quan))) {
        point_biserial_matrix[i,j] = biserial.cor(hf_db[,vec_quan[j]], hf_db[,vec_qual[i]])
      }
    }
    # No differentation between the means of the classes
    
    
    # Qualitative-qualitative: Cramer's V
    db_qual <- hf_db %>% dplyr::select(anaemia,diabetes,high_blood_pressure,sex,smoking)
    db_qual$anaemia <- as.factor(db_qual$anaemia)
    db_qual$diabetes <- as.factor(db_qual$diabetes)
    db_qual$high_blood_pressure <- as.factor(db_qual$high_blood_pressure)
    db_qual$sex <- as.factor(db_qual$sex)
    db_qual$smoking <- as.factor(db_qual$smoking)
    
    zeros_2 <- rep(0,25)
    crammerV_matrix <- matrix(zeros_2, nrow = 5)
    
    for (i in seq(1:length(vec_qual))) {
      for (j in seq(1:length(vec_qual))) {
        crammerV_matrix[i,j] = cramerV(db_qual[,j], db_qual[,i])
      }
    }
    
    # Mosaic plot smoking and sex --> could be interesting
    
    # Predictors vs DEATH_EVENT --> point biserial correlation
    biserial_death_event <- rep(0,7)
    for (i in seq(1:length(vec_quan))) {
      biserial_death_event[i] = biserial.cor(hf_db[,vec_quan[i]], hf_db$DEATH_EVENT)
    }
    
    # Box plots in the most higher values of biserial correlation
    # age
    hf_db %>% ggplot(aes(x = age)) +  
      geom_boxplot(aes(group = DEATH_EVENT, 
                       #colour = DEATH_EVENT, 
                       fill = DEATH_EVENT),
                   ) + 
      theme_economist() +
      scale_fill_manual(values=c("#B4464B", "#4BB446")) +
      scale_color_manual(values=c("#B4464B", "#4BB446"))
    
    # ejection_fraction
    hf_db %>% ggplot(aes(x = ejection_fraction)) +  
      geom_boxplot(aes(group = DEATH_EVENT, 
                       #colour = DEATH_EVENT, 
                       fill = DEATH_EVENT),
      ) + 
      theme_economist() +
      scale_fill_manual(values=c("#B4464B", "#4BB446")) +
      scale_color_manual(values=c("#B4464B", "#4BB446"))
    
    
    # serum_creatinine
    hf_db %>% ggplot(aes(x = serum_creatinine)) +  
      geom_boxplot(aes(group = DEATH_EVENT, 
                       #colour = DEATH_EVENT, 
                       fill = DEATH_EVENT),
      ) + 
      theme_economist() +
      scale_fill_manual(values=c("#B4464B", "#4BB446")) +
      scale_color_manual(values=c("#B4464B", "#4BB446"))
    
    # time
    hf_db %>% ggplot(aes(x = time)) +  
      geom_boxplot(aes(group = DEATH_EVENT, 
                       #colour = DEATH_EVENT, 
                       fill = DEATH_EVENT),
      ) + 
      theme_economist() +
      scale_fill_manual(values=c("#B4464B", "#4BB446")) +
      scale_color_manual(values=c("#B4464B", "#4BB446"))
    
    
    
    # Predictors vs DEATH_EVENT --> crammerV
    crammerV_death_event <- rep(0,5)
    for (i in seq(1:length(vec_qual))) {
      crammerV_death_event[i] = cramerV(db_qual[,i], hf_db$DEATH_EVENT)
    }

#-------------------------------------------------------------------------------    
    
    # VARIABLE TO PREDICT
    # DEATH_EVENT
    # 1 --> Dead    0 --> Live
    hf_db <- hf_db %>%
      mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT=="Dead", 1)) %>%
      as.data.frame()
    
    hf_db <- hf_db %>%
      mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT=="Live", 0)) %>%
      as.data.frame()
    
    hf_db$DEATH_EVENT <- as.numeric(hf_db$DEATH_EVENT)
    
    
#-------------------------------------------------------------------------------
    
# CLASSIFICATION MODELING USING STATISTICAL TOOLS
    
    
    
# Split between training and test sets
    set.seed(10)
    spl <- createDataPartition(hf_db$DEATH_EVENT, p = 0.8, list = FALSE)
    
    heart_failure_train <- hf_db[spl,]
    heart_failure_test <- hf_db[-spl,]
    
    # Each model can be automatically tuned and evaluated 
    # In this case, we are goint to use 5 repeats of 10-fold cross validation
    ctrl <- trainControl(method = "repeatedcv", 
                         repeats = 5,
                         number = 10)
    
# Logistic Regression 
    
    # Best Logistic regression using best subset method
    lr_hf_train <- rename(heart_failure_train, y = DEATH_EVENT)
    
    best_lr_model <- bestglm::bestglm(lr_hf_train, IC = "BIC")
    summary(best_lr_model$BestModel) 
      # Most important variables:
        # ejection_fraction
        # serum_creatinine
        # time
        # age
        # sex
    
    # Comparison between models
    
    # Database
    heart_failure_train <- heart_failure_train %>%
      mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT==1, "Dead")) %>%
      as.data.frame()
    
    heart_failure_train <- heart_failure_train %>%
      mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT==0, "Live")) %>%
      as.data.frame()
    
    heart_failure_train$DEATH_EVENT <- as.factor(heart_failure_train$DEATH_EVENT) 
    
    
    heart_failure_test <- heart_failure_test %>%
      mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT==1, "Dead")) %>%
      as.data.frame()
    
    heart_failure_test <- heart_failure_test %>%
      mutate(DEATH_EVENT = replace(DEATH_EVENT, DEATH_EVENT==0, "Live")) %>%
      as.data.frame()
    
    heart_failure_test$DEATH_EVENT <- as.factor(heart_failure_test$DEATH_EVENT) 
    
    
    # Logistic regression with all the variables
    lr_fit <- train(DEATH_EVENT ~ ., method = "glm", data = heart_failure_train,
                    family = "binomial", trControl = ctrl)
    print(lr_fit)
    lr_pred = predict(lr_fit, heart_failure_test)
    confusionMatrix(lr_pred,heart_failure_test$DEATH_EVENT)
    
    # Logistic regression with the more important variables
    lr_fit_best <- train(DEATH_EVENT ~ time + serum_creatinine + ejection_fraction + age + sex, 
                         method = "glm", data = heart_failure_train,
                    family = "binomial", trControl = ctrl)
    print(lr_fit_best)
    lr_pred_best = predict(lr_fit_best, heart_failure_test)
    lr_cm <- confusionMatrix(lr_pred_best,heart_failure_test$DEATH_EVENT)
    draw_confusion_matrix(lr_cm, "Logistic Regression")
    
    
# Naive Bayes
    
    #We are going to use RFE (backward selection)
    # Using Naive Bayes and repeated CV
    ctrl_nb <- rfeControl(functions = nbFuncs, method = "repeatedcv", 
                          repeats = 5,  verbose = FALSE)
    
    train_x <- heart_failure_train[,-13] 
    train_y <- heart_failure_train[,13] 
    nb_profile <- rfe(x = train_x, y = train_y, 
                      rfeControl = ctrl_nb,
                      metric = "Accuracy")
    print(nb_profile)
    # The top 4 variables (out of 4):
    #time, serum_creatinine, ejection_fraction, serum_sodium
    
    # Train and test naive bayes
    set.seed(10)
    nb_fit <- train(DEATH_EVENT ~ time + serum_creatinine + ejection_fraction + serum_sodium,
                    data = heart_failure_train, method="naive_bayes", trControl=ctrl, tuneLength = 4)
    nb_fit
    nb_pred<- predict(nb_fit,heart_failure_test)
    cm_naive_bayes <- confusionMatrix(reference = heart_failure_test$DEATH_EVENT, data = nb_pred, mode = "everything", positive = "Dead")
    draw_confusion_matrix(cm_naive_bayes, "Naive Bayes")
    
    
# LDA
    
    #We are going to use RFE (backward selection)
    # Using LDA and repeated CV
    ctrl_lda <- rfeControl(functions = ldaFuncs, method = "repeatedcv", 
                          repeats = 5,  verbose = FALSE)
    
    lda_profile <- rfe(x = train_x, y = train_y, 
                      rfeControl = ctrl_lda,
                      metric = "Accuracy")
    print(lda_profile)
    # The top 5 variables (out of 12):
    # time, serum_creatinine, ejection_fraction, serum_sodium, age
    
    # Train and test LDA
    set.seed(10)
    lda_fit <- train(DEATH_EVENT ~ time + serum_creatinine + ejection_fraction + serum_sodium + age,
                    data = heart_failure_train, method="lda", preProcess = c("center", "scale"), trControl=ctrl)
    lda_fit
    lda_pred<- predict(lda_fit,heart_failure_test)
    cm_lda <- confusionMatrix(lda_pred,heart_failure_test$DEATH_EVENT)
    draw_confusion_matrix(cm_lda, "Linear Discriminant Analysis")
    
    
# QDA
    
    # We are going to run the model with all the predictors and
    # after that see the variable importance and try another model
    # with the most importants predictors for QDA
    
    # Model with all the variables
    qda_fit = train(DEATH_EVENT~., data=heart_failure_train, method="qda", 
                    trControl = ctrl, metric = "Accuracy")
    print(qda_fit)
    qda_pred <- predict(qda_fit,heart_failure_test)
    cm_qda <- confusionMatrix(qda_pred,heart_failure_test$DEATH_EVENT)
    draw_confusion_matrix(cm_qda, "Quadratic Discriminant Analysis")
    
    # Useful variables for Logistic Regression Model
    varimp_qda = varImp(qda_fit)
    plot(varimp_qda, main = "Variable Importance with QDA")
    # Important variables --> time, serum_creatinine, ejection_fraction, serum_sodium, age
    
    # Model with important variables
    qda_fit_imp = train(DEATH_EVENT~time + serum_creatinine + ejection_fraction + serum_sodium + age,
                      data=heart_failure_train, method="qda", 
                    trControl = ctrl, metric = "Accuracy")
    print(qda_fit_imp)
    qda_pred_imp <- predict(qda_fit_imp,heart_failure_test)
    cm_qda_imp <- confusionMatrix(qda_pred_imp,heart_failure_test$DEATH_EVENT)
    draw_confusion_matrix(cm_qda_imp, "Quadratic Discriminant Analysis with IP")
    # Worst accuracy but more sensibility --> we want that in this case
    

# COMPARISON OF THE TRAINING SAMPLES 
    
    #TRAIN
    # Compare model performances using resample()
    model_comparison_train = resamples(list(LR = lr_fit_best, NB = nb_fit,
                                      LDA = lda_fit, QDA = qda_fit_imp))
    
    # Summary of the performances (in training)
    summary(model_comparison_train)
    
    # Box plots to compare model performances
    scales = list(x = list(relation = "free"), y = list(relation = "free"))
    bwplot(model_comparison_train, scales = scales)
    

# COST MATRIX (selected by me)
    
    #              Reference
    # Prediction  Dead      Live
    #   Dead       0       1
    #   Live       4       0
    
# Final improvement of our model
    
    relative.cost <- c(0, 4, 1, 0.0) 
    
    CM <- confusionMatrix(lr_pred_best, heart_failure_test$DEATH_EVENT)$table
    sum(relative.cost*CM)
    
    cost.i = matrix(NA, nrow = 10, ncol = 10)
    # 100 replicates for training/testing sets for each of the 10 values of threshold
    
    j <- 0
    ctrl <- trainControl(method = "none")
    
    for (threshold in seq(0.4,0.85,0.05)){
      
      j <- j + 1
      cat(j)
      for(i in 1:10){
        
        #partition data intro training (80%) and testing sets (20%)
        d <- createDataPartition(heart_failure_train$DEATH_EVENT, p = 0.8, list = FALSE)
        # select training sample
        
        train <- heart_failure_train[d,]
        test <- heart_failure_train[-d,] 
        
        lr_fit_best_prob <- train(DEATH_EVENT ~ time + serum_creatinine + ejection_fraction + age + sex, 
                             method = "glm", data = heart_failure_train,
                             family = "binomial", trControl = ctrl)
       
        lr_prob <- predict(lr_fit_best_prob, test, type = "prob")
        
        lr_pred_best_prob <- rep("Dead", nrow(test))
        lr_pred_best_prob[which(lr_prob[,2] > threshold)] <- "Live"
        lr_pred_best_prob <- factor(lr_pred_best_prob)
        
        CM <- confusionMatrix(lr_pred_best_prob, test$DEATH_EVENT)$table
        
        cost.i[i,j] <- sum(relative.cost*CM)/nrow(test) # unitary cost
        
      }
      
    }
    # if(!is.null(dev.list())) dev.off()
    
    boxplot(cost.i, main = "Hyper-parameter selection",
            ylab = "cost",
            xlab = "threshold value", names = seq(0.4,0.85,0.05),col="royalblue2")
    
        
     # optimal threshold
    threshold <- 0.7
    
    # Final prediction
    lr_fit_best_prob <- train(DEATH_EVENT ~ time + serum_creatinine + ejection_fraction + age + sex, 
                              method = "glm", data = heart_failure_train,
                              family = "binomial", trControl = ctrl)
    
    lr_prob <- predict(lr_fit_best_prob, heart_failure_test, type = "prob")
    
    lr_pred_best_prob <- rep("Dead", nrow(heart_failure_test))
    lr_pred_best_prob[which(lr_prob[,2] > threshold)] <- "Live"
    lr_pred_best_prob <- factor(lr_pred_best_prob)
    
    confusion_matrix <- confusionMatrix(lr_pred_best_prob, heart_failure_test$DEATH_EVENT)
    CM <- confusionMatrix(lr_pred_best_prob, heart_failure_test$DEATH_EVENT)$table
    draw_confusion_matrix(confusion_matrix, "Final Prediction")
    sum(relative.cost*CM)/nrow(heart_failure_test) # unitary cost
    
    # IN THIS CASE MORE IMPORTANT FOR US THE SENSIBILITY!   
    
    
    
