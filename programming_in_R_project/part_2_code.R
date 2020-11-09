# CODE PART 2: ML and Ensemble models

# Add database and packages we are going to use
# ----------------PACKAGES----------------------------------------------------

# PACMAN: Install and load packages more easily
if(!require(pacman))install.packages("pacman")
library(pacman)

# DAAG: Data Analysis and Graphics Data and Functions, for the db
# caret: For ML
# ggplot2: plots
# lattice: elegant visualization
# skimr: Compact and Flexible Summaries of Data
# dplyr: tool for manipulate data frames
# RANN: Fast Nearest Neighbour Search 
# randomForest: Classification And Regression With Random Forest
# earth: Multivariate Adaptive Regression Splines
# fastAdaboost: Fast Implementation of Adaboost
# xgBoost: Extreme Gradient Boosting
# xtable: LATEX tables
# fastDUmmies: Fast Creation of Dummy (Binary) Columns and Rows from Categorical Variables
# VIM: Visualization and Imputation of Missing Values
# plyr: Tools for Splitting, Applying and Combining Data
# MLeval: Machine Learning Model Evaluation
# klaR: Classification and Visualization
# kernlab: Kernel-Based Machine Learning Lab
# naivebayes: High Performance Implementation of the Naive Bayes Algorithm
# deepboost: Deep Boosting Ensemble Modeling
# caTools: Moving Window Statistics, GIF, Base64, ROC AUC, etc
# caretEnsemble: Ensembles of Caret Models

pacman::p_load(DAAG, caret, ggplot2, lattice, skimr, dplyr,
       RANN, randomForest, earth, fastAdaboost, xgboost,
       xtable, fastDummies, VIM, plyr, MLeval, klaR, kernlab, naivebayes,
       deepboost, caTools, nnet, caretEnsemble)

# Options for xtable package
options(xtable.timestamp = "")

# ---------------------------------------------------------------------------

#--------------------FUNCTIONS----------------------------------------------

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
    #cat("\014") 

  # Clear Workspace:
    # rm(list=ls())

  # Clear plots
    # if(!is.null(dev.list())) dev.off()

  # Run a script: go to the path and
    # source("name_file.R", echo = TRUE)
    # source("part_2_code.R", echo = TRUE)
    

#---------------------------------------------------------------------------

# VISUALIZATION OF VBLES OF DB
    # Load the database
    data(mifem)
    
    # See the variables (nº observations and class of them)
    str(mifem)
    
    # Save initial visualization of the DB in latex table
    head(mifem)
    table_head <- xtable::xtable(head(mifem))
    table_summary <- xtable::xtable(summary(mifem))

# DESCRIPTIVE STATISTICS
    # Descriptive statistics by employing skimr package
    skimmed_1 <- skimr::skim(mifem)
    #Latex table
    table_skimmed_1 <- xtable::xtable(skimmed_1)
    
# PREPROCESSING DB    
    # identify vbles with nk values (not known values)
    vbles_nk <- names(which(colSums(mifem == "nk")>0))
    
    # Change nk  to NA
    mifem_NA <- mifem %>% dplyr::mutate_if(is.factor, list(~na_if(., "nk")))
    
    # Handling NA with k_Nearest Neighbors
    mifem_without_NA <- VIM::kNN(mifem_NA, variable = vbles_nk, k = 5)
    # Divide DB in DB with vbles without NA and 
    # DB with the position of NA (TRUE value)
    mifem_NA_position <- mifem_without_NA %>% dplyr::select_if(is.logical)
    mifem_NA_clean <- mifem_without_NA %>% dplyr::select(!names
                                                         (mifem_NA_position))
    # LATEX code showing both tables
    table_NA_position <- xtable::xtable(head(mifem_NA_position))
    table_NA_clean <- xtable::xtable(head(mifem_NA_clean))
    
    # Convert factor vbles in dummy vbles
    mifem_dummy <- fastDummies::dummy_cols(mifem_NA_clean)
    
    # Clean dummy vbles we don't want:
    # NOTE: YES <- 1 -------- NO <- 0 FOR ALL VBLES
    # Except SMSTAT vble in which we will have all dummy vbles
    mifem_ML <- mifem_dummy %>% dplyr::select(!ends_with("_n") &
                                              !ends_with("_nk"),
                                              smstat_n) %>%  
                                       dplyr::select(outcome_live, age, yronset, 
                                              ends_with("_y"),
                                              starts_with("sms")) %>% 
                                       dplyr::select(!smstat)
    # Skimmed of the df for ML
    skimmed_2 <- skimr::skim(mifem_ML)
    # Latex table
    table_skimmed_2 <- xtable::xtable(skimmed_2)
    
    
# PARTITION OF THE DATASET
    # seed to be reproducible
    set.seed(10)
    
    # Row numbers for the train data
    train_row_numbers <- createDataPartition(mifem_ML$outcome_live,
                                          p = 0.8, list = FALSE)
    
    # Create training set
    set_training <- mifem_ML[train_row_numbers, ]
    
    # Create test set
    set_test <- mifem_ML[-train_row_numbers, ]
    set_test <- mifem_ML %>% dplyr::mutate(outcome_live = as.factor(outcome_live)) %>%
      mutate(outcome_live = plyr::revalue(outcome_live, c("0" = "DEAD", "1" = "LIVE")))
    
    # Store x and y  
    train_x <- set_training %>% dplyr::select(!outcome_live) 
    train_y <- set_training %>% dplyr::select(outcome_live)
    train_y <- as.factor(as.matrix(train_y))
    train_y <- plyr::revalue(train_y, c("0" = "DEAD", "1" = "LIVE"))
      
                                                       
# PREPROCESS OF TRAINING DATA
    # Range preprocessing
    preProcess_range_model <- preProcess(train_x, method = "range")
    set_training_ranged <- predict(preProcess_range_model, newdata = train_x)
  
                                                       
# VISUALIZE IMPORTANT VARIABLES                                                
    # See visually the role of x variables to predict y
    # Through feature plot and boxplot
    feature_box_plot <- featurePlot(x = set_training_ranged, 
                                    y = factor(train_y), 
                                    plot = "box",
                strip = strip.custom(par.strip.text = list(cex = 0.7)),
                scales = list(x = list(relation = "free"),
                y = list(relation = "free")))
    
    # Through feature plot and density plot
    feature_density_plot <- featurePlot(x = set_training_ranged,
                                        y = factor(train_y),
                plot = "density",
                strip = strip.custom(par.strip.text = list(cex = 0.7)), 
                scales = list(x = list(relation = "free"),
                y = list(relation = "free")))
    

# RECURSIVE FEATURE ELIMINATION    
    # Using random forest and repeated CV
    # seed to be reproducible
    set.seed(10)
    options(warn = -1)
    # features that should be retained (all)
    subsets <- c(1:ncol(train_x))
    # RF and Repeated CV
    ctrl_rf <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5,
                      verbose = FALSE)
    # Apply all the features predetermined
    # You need Y = FACTOR to have accuracy metric
    rf_profile <- rfe(x = train_x, y = train_y, sizes = subsets,
                    rfeControl = ctrl_rf, metric = "Accuracy")
    rf_profile_plot <- plot(rf_profile, type=c("g", "o"))
    #store the importance of variables for the model
    importance_rf <- caret::varImp(rf_profile)
    table_imp_rf <- xtable::xtable(importance_rf)
    
    # Using Naive Bayes and again, repeated CV
    ctrl_nb <- rfeControl(functions = nbFuncs, method = "repeatedcv", 
                          repeats = 5,  verbose = FALSE)
    # Apply all the features predetermined
    # You need Y = FACTOR to have accuracy metric
    nb_profile <- rfe(x = train_x, y = train_y, 
                      sizes = subsets, rfeControl = ctrl_nb,
                      metric = "Accuracy")
    nb_profile_plot <- plot(nb_profile, type=c("g", "o"))
    #store the importance of variables for the model
    importance_nb <- caret::varImp(nb_profile)
    table_imp_nb <- xtable::xtable(importance_nb)

 
# ALGORITHMS TO MODEL AND PREDICT OUR DATA   
    #We are going to prove with differents metrics and only will
    # show the best
    
    # Control Parameters For Train
    ctrl= trainControl(method = "repeatedcv", number = 5, repeats = 5, p = 0.8, 
                       savePredictions = "final", classProbs = T, 
                       summaryFunction = twoClassSummary)
    
    # worst result
    # Random Forest --> metric = ROC
    # set.seed(10)
    # train_rf <- train(train_x, train_y, method = "rf", metric = "ROC", trControl = ctrl, tuneLength = 5)
    # train_rf
    # pred_rf <- predict(train_rf,set_test)
    # cm_rf_roc <- confusionMatrix(reference = set_test$outcome_live, data = pred_rf, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_rf_roc, "Confusion Matrix with ROC metric and RF")
    
    # worst result
    # Random Forest --> metric = Accuracy
    # train_rf <- train(train_x, train_y, method="rf",  metric='Accuracy', trControl=ctrl, tuneLength = 5)
    # train_rf
    # pred_rf <- predict(train_rf,set_test)
    # cm_rf_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_rf, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_rf_acc, "Confusion Matrix with accuracy metric and RF")
    
    # Random Forest --> metric = Specificity
    train_rf <- train(train_x, train_y, method="rf", metric='Sens', trControl=ctrl, tuneLength = 5)
    train_rf
    pred_rf <- predict(train_rf,set_test)
    cm_rf_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_rf, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_rf_spec, "Random Forest")
     
    # Useful variables for RF model
    varimp_rf = varImp(train_rf)
    plot(varimp_rf, main = "Variable Importance with RF")
    
    
    
    # Multivariate Adaptative Regression Spline (earth) --> metric = ROC
    set.seed(10)
    train_mars <- train(train_x, train_y, method="earth", metric="ROC", trControl=ctrl, tuneLength = 5)
    train_mars
    pred_mars <- predict(train_mars,set_test)
    cm_mars_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_mars, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_mars_acc, "Multivariate Adaptative Regression Spline")
    
    # same result
    # # Multivariate Adaptative Regression Spline (earth) --> metric = Specificity
    # set.seed(10)
    # train_mars <- train(train_x, train_y, method="earth", metric='Sens', trControl=ctrl, tuneLength = 5)
    # train_mars
    # pred_mars <- predict(train_mars,set_test)
    # cm_mars_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_mars, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_mars_spec, "Confusion Matrix with specificity metric and MARS")
    
    # Useful variables for MARS model
    varimp_mars = varImp(train_mars)
    plot(varimp_mars, main = "Variable Importance with MARS")
    
    
    # worst result
    # # K-Nearest Neighbors --> metric = ROC
    # set.seed(10)
    # train_knn <- train(train_x, train_y, method="knn", metric='Accuracy', trControl=ctrl, tuneLength = 5)
    # train_knn
    # pred_knn <- predict(train_knn,set_test)
    # cm_knn_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_knn, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_knn_acc, "Confusion Matrix with accuracy metric and KNN")
    
    # K-Nearest Neighbors --> metric = Specificity
    set.seed(10)
    train_knn <- train(train_x, train_y, method="knn", metric='Sens', trControl=ctrl, tuneLength = 5)
    train_knn
    pred_knn <- predict(train_knn,set_test)
    cm_knn_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_knn, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_knn_spec, "K-Nearest Neighbors")
    
    # Useful variables for KNN model
    varimp_knn = varImp(train_knn)
    plot(varimp_knn, main = "Variable Importance with KNN")
    
    # NOT WORK
    # Support Vector Machine --> metric = ROC
    # set.seed(10)
    # train_svm <- train(train_x, train_y, method="svmRadial", metric='Accuracy', trControl=ctrl, tuneLength = 5)
    # train_svm
    # pred_svm <- predict(train_svm,set_test)
    # cm_svm_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_svm, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_svm_acc, "Confusion Matrix with accuracy metric and SVM")
    # 
    # # Useful variables for SVM model
    # varimp_svm = varImp(train_svm)
    # plot(varimp_svm, main = "Variable Importance with SVM")
    # 
    # # Support Vector Machine --> metric = Specificity
    # set.seed(10)
    # train_svm <- train(train_x, train_y, method="svmRadial", metric='Sens', trControl=ctrl, tuneLength = 5)
    # train_svm
    # pred_svm <- predict(train_svm,set_test)
    # cm_svm_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_svm, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_svm_spec, "Confusion Matrix with specificity metric and SVM")

    
    
    # Adaboost --> metric = ROC
    set.seed(10)
    train_adaboost <- train(train_x, train_y, method="adaboost", metric="ROC", trControl=ctrl, tuneLength = 4)
    train_adaboost
    pred_adaboost <- predict(train_adaboost,set_test)
    cm_adaboost_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_adaboost, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_adaboost_acc, "Adaboost")
    
    # Useful variables for Adaboost model
    varimp_adaboost = varImp(train_adaboost)
    plot(varimp_adaboost, main = "Variable Importance with Adaboost")
    
    # NOT WORK
    # # Adaboost --> metric = Specificity --> NOT WORK
    # set.seed(10)
    # train_adaboost <- train(train_x, train_y, method="adaboost", metric='Sens', trControl=ctrl, tuneLength = 4)
    # train_adaboost
    # pred_adaboost <- predict(train_adaboost,set_test)
    # cm_adaboost_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_adaboost, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_svm_spec, "Confusion Matrix with specificity metric and Adaboost")

    
    
    # xgBoost DART --> metric = ROC
    set.seed(10)
    train_xgbDART <- train(train_x, train_y, method="xgbDART", metric="ROC", trControl=ctrl, tuneLength = 4)
    train_xgbDART
    pred_xgbDART <- predict(train_xgbDART,set_test)
    cm_xgbDART_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_xgbDART, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_xgbDART_acc, "xgBoost DART")
    
    # Useful variables for xgbDART model
    varimp_xgbDART = varImp(train_xgbDART)
    plot(varimp_xgbDART, main = "Variable Importance with xgbDART")
    
    # worst result
    # # xgbDART --> metric = Specificity --> 
    # set.seed(10)
    # train_xgbDART <- train(train_x, train_y, method="xgbDART", metric='Sens', trControl=ctrl, tuneLength = 4)
    # train_xgbDART
    # pred_xgbDART<- predict(train_xgbDART,set_test)
    # cm_xgbDART_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_xgbDART, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_xgbDART_spec, "Confusion Matrix with specificity metric and xgbDART")
    
    
    
    # Generalized Linear Model --> metric = ROC
    set.seed(10)
    train_glm <- train(train_x, train_y, method="glm", metric="ROC", trControl=ctrl, tuneLength = 4)
    train_glm
    pred_glm <- predict(train_glm,set_test)
    cm_glm_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_glm, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_glm_acc, "Generalized Linear Model")
    
    # Useful variables for Generalized Linear Model
    varimp_glm = varImp(train_glm)
    plot(varimp_glm, main = "Variable Importance with GLM")
    
    # same result
    # # Generalized Linear Model --> metric = Specificity  
    # set.seed(10)
    # train_glm <- train(train_x, train_y, method="glm", metric='Sens', trControl=ctrl, tuneLength = 4)
    # train_glm
    # pred_glm <- predict(train_glm,set_test)
    # cm_glm_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_glm, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_glm_spec, "Confusion Matrix with specificity metric and GLM")
    
    
    
    # bad result
    # Naive Bayes --> metric = ROC
    # set.seed(10)
    # train_naive_bayes	 <- train(train_x, train_y, method="naive_bayes", metric="Accuracy", trControl=ctrl, tuneLength = 4)
    # train_naive_bayes	
    # pred_naive_bayes <- predict(train_naive_bayes,set_test)
    # cm_naive_bayes_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_naive_bayes, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_naive_bayes_acc, "Confusion Matrix with accuracy metric and Naive Bayes")
    
    # Naive Bayes --> metric = Specificity  
    set.seed(10)
    train_naive_bayes <- train(train_x, train_y, method="naive_bayes", metric="Sens", trControl=ctrl, tuneLength = 4)
    train_naive_bayes
    pred_naive_bayes<- predict(train_naive_bayes,set_test)
    cm_naive_bayes_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_naive_bayes, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_naive_bayes_spec, "Naive Bayes")
    
    # Useful variables for Naive Bayes model
    varimp_naive_bayes = varImp(train_naive_bayes)
    plot(varimp_naive_bayes, main = "Variable Importance with Naive Bayes")
    

    
    # NOT WORK PROPERLY
    # DeepBoost --> metric = ROC
    # set.seed(10)
    # train_deepboost	 <- train(train_x, train_y, method="deepboost", metric="Accuracy", trControl=ctrl, tuneLength = 4)
    # train_deepboost	
    # pred_deepboost <- predict(train_deepboost,set_test)
    # cm_deepboost_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_deepboost, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_deepboost_acc, "Confusion Matrix with accuracy metric and DeepBoost")
    
    # DeepBoost --> metric = Specificity  
    # set.seed(10)
    # train_deepboost <- train(train_x, train_y, method="deepboost", metric='Sens', trControl=ctrl, tuneLength = 4)
    # train_deepboost
    # pred_deepboost <- predict(train_naive_bayes,set_test)
    # cm_deepboost_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_deepboost, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_deepboost_spec, "Confusion Matrix with specificity metric and DeepBoost")
    # 
    # # Useful variables for DeepBoost model
    # varimp_deepboost = varImp(train_deepboost)
    # plot(varimp_deepboost, main = "Variable Importance with DeepBoost")
    # 
    
    
    # Boosted Logistic Regression --> metric = ROC
    set.seed(10)
    train_LogitBoost <- train(train_x, train_y, method="LogitBoost", metric="Accuracy", trControl=ctrl, tuneLength = 4)
    train_LogitBoost	
    pred_LogitBoost <- predict(train_LogitBoost,set_test)
    cm_LogitBoost_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_LogitBoost, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_LogitBoost_acc, "Boosted Logistic Regression")
    
    # Useful variables for BLR model
    varimp_LogitBoost = varImp(train_LogitBoost)
    plot(varimp_LogitBoost, main = "Variable Importance with BLC")
    
    # same result
    # Boosted Logistic Regression --> metric = Specificity  
    # set.seed(10)
    # train_LogitBoost <- train(train_x, train_y, method="LogitBoost", metric='Sens', trControl=ctrl, tuneLength = 4)
    # train_LogitBoost
    # pred_LogitBoost <- predict(train_LogitBoost,set_test)
    # cm_LogitBoost_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_LogitBoost, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_LogitBoost_spec, "Confusion Matrix with specificity metric and BLC")
    
    
    
    # worst result
    # Neural Networks with Feature Extraction --> metric = ROC
    # set.seed(10)
    # train_pcaNNet	 <- train(train_x, train_y, method="pcaNNet", metric="Accuracy", trControl=ctrl, tuneLength = 4)
    # train_bartMachine	
    # pred_pcaNNet <- predict(train_pcaNNet,set_test)
    # cm_pcaNNet_acc <- confusionMatrix(reference = set_test$outcome_live, data = pred_pcaNNet, mode = "everything", positive = "LIVE")
    # draw_confusion_matrix(cm_pcaNNet_acc, "Confusion Matrix with accuracy metric and NNFE")
    
    # Neural Networks with Feature Extraction --> metric = Specificity  
    set.seed(10)
    train_pcaNNet <- train(train_x, train_y, method="pcaNNet", metric='Sens', trControl=ctrl, tuneLength = 4)
    train_pcaNNet
    pred_pcaNNet <- predict(train_pcaNNet,set_test)
    cm_pcaNNet_spec <- confusionMatrix(reference = set_test$outcome_live, data = pred_pcaNNet, mode = "everything", positive = "LIVE")
    draw_confusion_matrix(cm_pcaNNet_spec, "Neural Networks with Feature Extraction")
    
    # Useful variables for NNFE model
    varimp_pcaNNet = varImp(train_pcaNNet)
    plot(varimp_pcaNNet, main = "Variable Importance with NNFE")
    
    
    
# MODEL COMPARISON
    # Compare model performances using resample()
    model_comparison = resamples(list(RF = train_rf, MARS = train_mars,
                                      KNN = train_knn, ADABOOST = train_adaboost,
                                      XGBDART = train_xgbDART, GLM = train_glm,
                                      NAIVE_BAYES = train_naive_bayes,
                                      BLR = train_LogitBoost,
                                      NNFE = train_pcaNNet))
    
    # Summary of the performances (in training)
    summary(model_comparison)
    
    # Box plots to compare model performances
    scales = list(x = list(relation = "free"), y = list(relation = "free"))
    bwplot(model_comparison, scales = scales)
    
 
# EMSEMBLING THE PREDICTIONS
    
    #Define trainControl
    trainControl = trainControl(method = "repeatedcv", number = 10, repeats = 3,
                                savePredictions = TRUE, classProbs = TRUE)
    
    # Run multiple algorithms in one call deepboost out (doesnt work properly)
    algorithm_list = c("rf", "earth", "knn", "adaboost",  "xgbDART", "glm",
                      "naive_bayes", "LogitBoost", "pcaNNet")
    
    # Set seed to be reproducible
    set.seed(10)
    # Create the ensemble with all the models
    models <- caretList(train_x, train_y, trControl = trainControl,
                       methodList = algorithm_list)
    
    #Combine models to form a multiple prediction
    #Ensamble the prediction based on glm
    stack.glm <- caretStack(models, method = "glm")
    print(stack.glm)
    
    #Predict on testData
    stack_predicted <- predict(stack.glm, set_test)
    
    #Confusion matrix
    cm_all_ensemble <- confusionMatrix(reference = set_test$outcome_live, data = stack_predicted, mode = "everything",
                    positive = "LIVE")
    draw_confusion_matrix(cm_all_ensemble, "ENSEMBLE MODEL")
    
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    




