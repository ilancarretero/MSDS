# CODE PART 1: Descriptive study and ggplot

# Add database and packages we are going to use
# ----------------PACKAGES----------------------------------------------------

# PACMAN: Install and load packages more easily
if(!require(pacman))install.packages("pacman")
library(pacman)

# DAAG: Data Analysis and Graphics Data and Functions, for the db
# fdth: Frequency Distribution Tables, Histograms and Polygons
# moments: Moments, cumulants, skewness, kurtosis and related tests
# ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics
# dplyr: A Grammar of Data Manipulation
# magrittr: A Forward-Pipe Operator for R
# GGally: Extension to 'ggplot2'
# ggmosaic: Mosaic Plots in the 'ggplot2' Framework
# xtable: LATEX tables
# ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'
# gridExtra: Miscellaneous Functions for "Grid" Graphics
# ggpubr: 'ggplot2' Based Publication Ready Plots

pacman::p_load(DAAG, fdth, moments, ggplot2, dplyr, magrittr, GGally, ggmosaic,
               xtable, ggthemes, gridExtra, ggpubr)

# Options for xtable package
options(xtable.timestamp = "")

# ---------------------------------------------------------------------------

#--------------------FUNCTIONS----------------------------------------------

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

# DATABASE
  # Load the database
  data(mifem)
  
  # See the variables (nº observations and class of them)
  str(mifem)
  
  # Save initial visualization of the DB in latex table
  head(mifem)
  table_head <- xtable::xtable(head(mifem))
  table_summary <- xtable::xtable(summary(mifem))


# VARIABLES FREQUENCY TABLES  

  # AGE
    # frequency table
    tb_age <- fdt(mifem[[2]],
                start=35,
                end=70, 
                h=5, 
                col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_age <- summary(tb_age, 
                          col=c(1:2, 4, 6),
                          format=TRUE, 
                          pattern = '%d')
    table_age <- xtable::xtable(summary_age)
    
    #representation
    par(mfrow=c(2,2), oma = c(0, 0, 2, 0), mar=c(2,2,2,2))
    plot(tb_age, main = "Absolute frequency histogram", xlab = "Age intervals", col = "lightblue")
    plot(tb_age, type = 'rfh', main = "Relative frequency histogram", xlab = "Age intervals", col = "lightblue")
    plot(tb_age, type = 'cdh', main = "Cumulative density histogram", xlab = "Age intervals", col = "lightblue")
    plot(tb_age, type = 'cfh', main = "Cumulative frequency histogram", xlab = "Age intervals",col = "lightblue")
    mtext("Frequency histograms for age", outer = TRUE, cex = 1.2)
    
   
    
  # YRONSET
    # frequency table
    tb_yronset <- fdt(mifem[[3]],
                     start=84,
                     end=94, 
                     h=2, 
                     col=c(1:2, 4, 6))
    
    #summary of the information of the table and save in LATEX format
    summary_yronset <- summary(tb_yronset, 
                              col=c(1:2, 4, 6),
                              format=TRUE, 
                              pattern = '%d')
    table_yronset <- xtable::xtable(summary_yronset)
    
    #representation
    par(mfrow=c(2,2), oma = c(0, 0, 2, 0), mar=c(2,2,2,2))
    plot(tb_yronset, main = "Absolute frequency histogram",  xlab = "yronset intervals",col = "lightblue")
    plot(tb_yronset, type = 'rfh', main = "Relative frequency histogram", xlab = "yronset intervals", col = "lightblue")
    plot(tb_yronset, type = 'cdh', main = "Cumulative density histogram", xlab = "yronset intervals", col = "lightblue")
    plot(tb_yronset, type = 'cfh', main = "Cumulative frequency histogram",  xlab = "yronset intervals",col = "lightblue")
    mtext("Frequency histograms for the year of acquisition", outer = TRUE, cex = 1.2)
    
    
  # OUTCOME 
    # frequency table
    tb_outcome <- fdt_cat(mifem[[1]], 
                         sort=TRUE, 
                         decreasing = TRUE, 
                         col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_outcome <- summary(tb_outcome, 
                              col=c(1:2, 4, 6),
                              format=TRUE, 
                              pattern = '%d') 
    table_outcome <- xtable::xtable(summary_outcome)
    
    # representation
    p1 <- summary_outcome %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p2 <- summary_outcome %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_outcome <- ggarrange(p1, p2, ncol = 2)
    annotate_figure(figure_outcome, top = text_grob("OUTCOME", face = "bold", size = 20))
    
    # factor variable for OUTCOME
    outcome_factor_absolute_1 <- as.factor(mifem[[1]])
   
     
  # PREMI
    # frequency table
    tb_premi <- fdt_cat(mifem[[4]], 
                       sort=TRUE, 
                       decreasing = TRUE, 
                       col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_premi <- summary(tb_premi, 
                            col=c(1:2, 4, 6),
                            format=TRUE, 
                            pattern = '%d')
    table_premi <- xtable::xtable(summary_premi)
    
    # representation
    p3 <- summary_premi %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p4 <- summary_premi %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_premi <- ggarrange(p3, p4, ncol = 2)
    annotate_figure(figure_premi, top = text_grob("PREMI", face = "bold", size = 20))
    
    # factor variable for PREMI
    outcome_factor_absolute_4 = as.factor(mifem[[4]])
    
    
  # SMSTAT
    # frequency table
    tb_smstat <- fdt_cat(mifem[[5]], 
                        sort=TRUE, 
                        decreasing = TRUE, 
                        col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_smstat <- summary(tb_smstat, 
                             col=c(1:2, 4, 6),
                             format=TRUE, 
                             pattern = '%d')
    table_smstat <- xtable::xtable(summary_smstat)
    
    # representation
    p5 <- summary_smstat %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p6 <- summary_smstat %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_smstat <- ggarrange(p5, p6, ncol = 2)
    annotate_figure(figure_smstat, top = text_grob("SMSTAT", face = "bold", size = 20))
    
    # factor variable for SMSTAT
    outcome_factor_absolute_5 = as.factor(mifem[[5]])
    
    
  # DIABETES
    # frequency table
    tb_diabetes <- fdt_cat(mifem[[6]], 
                          sort=TRUE, 
                          decreasing = TRUE, 
                          col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_diabetes <- summary(tb_diabetes, 
                               col=c(1:2, 4, 6),
                               format=TRUE, 
                               pattern = '%d')
    table_diabetes <- xtable::xtable(summary_diabetes)
    
    # representation
    p7 <- summary_diabetes %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p8 <- summary_diabetes %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_diabetes <- ggarrange(p7, p8, ncol = 2)
    annotate_figure(figure_diabetes, top = text_grob("DIABETES", face = "bold", size = 20))
    
    # factor variable for DIABETES
    outcome_factor_absolute_6 = as.factor(mifem[[6]])
    
    
  # HIGHBP
    # frequency table
    tb_highbp <- fdt_cat(mifem[[7]], 
                        sort=TRUE, 
                        decreasing = TRUE, 
                        col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_highbp <- summary(tb_highbp, 
                             col=c(1:2, 4, 6),
                             format=TRUE, 
                             pattern = '%d')
    table_highbp <- xtable::xtable(summary_highbp)
    
    # representation
    p9 <- summary_highbp %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p10 <- summary_highbp %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_highbp <- ggarrange(p9, p10, ncol = 2)
    annotate_figure(figure_highbp, top = text_grob("HIGHBP", face = "bold", size = 20))
    
    # factor variable for DIABETES
    outcome_factor_absolute_7 = as.factor(mifem[[7]])
    
    
  # HIGHCOL
    # frequency table
    tb_highcol <- fdt_cat(mifem[[8]], 
                         sort=TRUE, 
                         decreasing = TRUE, 
                         col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_highcol <- summary(tb_highcol, 
                              col=c(1:2, 4, 6),
                              format=TRUE, 
                              pattern = '%d')
    table_highcol <- xtable::xtable(summary_highcol)
    
    # representation
    p11 <- summary_highcol %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p12 <- summary_highcol %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_highcol <- ggarrange(p11, p12, ncol = 2)
    annotate_figure(figure_highcol, top = text_grob("HIGHCOL", face = "bold", size = 20))
    
    # factor variable for DIABETES
    outcome_factor_absolute_8 = as.factor(mifem[[8]])
    
    
  # ANGINA
    # frequency table
    tb_angina <- fdt_cat(mifem[[9]], 
                        sort=TRUE, 
                        decreasing = TRUE, 
                        col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_angina <- summary(tb_angina, 
                            col=c(1:2, 4, 6),
                            format=TRUE, 
                            pattern = '%d')
    table_angina <- xtable::xtable(summary_angina)
    
    #representation
    p13 <- summary_angina %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p14 <- summary_angina %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_angina <- ggarrange(p13, p14, ncol = 2)
    annotate_figure(figure_angina, top = text_grob("ANGINA", face = "bold", size = 20))
    
    # factor variable for DIABETES
    outcome_factor_absolute_9 = as.factor(mifem[[9]])
    
    
  # STROKE
    #frequency table
    tb_stroke <- fdt_cat(mifem[[10]], 
                        sort=TRUE, 
                        decreasing = TRUE, 
                        col=c(1:2, 4, 6))
    
    # summary of the information of the table and save in LATEX format
    summary_stroke <- summary(tb_highbp, 
                             col=c(1:2, 4, 6),
                             format=TRUE, 
                             pattern = '%d')
    table_stroke <- xtable::xtable(summary_stroke)
    
    # representation
    p15 <- summary_stroke %>% ggplot(aes(Category, f, fill = Category, label = f)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ggtitle("Absolute frequency histogram") + xlab("") + ylab("women") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    p16 <- summary_stroke %>% rename(rf = "rf(%)") %>% ggplot(aes(Category, rf, fill = Category, label = rf)) + geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
      ggtitle("Relative frequency histogram") + xlab("") + ylab("women (%)") + theme_economist() +
      theme(legend.position = "none") + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"))
    
    figure_stroke <- ggarrange(p15, p16, ncol = 2)
    annotate_figure(figure_stroke, top = text_grob("STROKE", face = "bold", size = 20))
    
    # factor variable for STROKE
    outcome_factor_absolute_10 = as.factor(mifem[[10]])
    
    
    
# MEASURES OF CENTRALITY, VARIABILITY AND SHAPE (SKEWNESS AND KURTOSIS)
    
    # NORMAL PLOT
    #age
    n_plot_age <- ggplot(mifem, aes(sample = age)) + stat_qq() + stat_qq_line() +
      ggtitle("Normal Q-Q Plot for Age variable") + theme_economist() +  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"), legend.position = "right",
            title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)))
    
    #yronset
    n_plot_yronset <- ggplot(mifem, aes(sample = yronset)) + stat_qq() + stat_qq_line() +
      ggtitle("Normal Q-Q Plot for Yronset variable") + theme_economist() +  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"), legend.position = "right",
            title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)))
    
    # computation of the measures --> using APPLY function
    numerical_var <- cbind(mifem[[2]], mifem[[3]])
    mean <- apply(numerical_var, MARGIN=2, mean)
    variance <- apply(numerical_var, MARGIN=2, var)
    skewness <- apply(numerical_var, MARGIN=2, skewness)
    kurtosis <- apply(numerical_var, MARGIN=2, kurtosis)
    sd <- apply(numerical_var, MARGIN=2, sd)
    
    # store in a data frame object
    measures <- rbind(mean,variance,sd, skewness,kurtosis)
    colnames(measures)=c("age", "yronset")
    measures_df=data.frame(measures)
    
    # measures by groups
    mifem_live <- mifem %>% filter(outcome == "live")
    mifem_dead <- mifem %>% filter(outcome == "dead")
    
    # computation of the measures and store in data frame --> using APPLY function
    numerical_var <- cbind(mifem_live[[2]], mifem_live[[3]])
    mean <- apply(numerical_var, MARGIN=2, mean)
    variance <- apply(numerical_var, MARGIN=2, var)
    skewness <- apply(numerical_var, MARGIN=2, skewness)
    kurtosis <- apply(numerical_var, MARGIN=2, kurtosis)
    sd <- apply(numerical_var, MARGIN=2, sd)
    
    measures <- rbind(mean,variance,sd, skewness,kurtosis)
    colnames(measures)=c("age", "yronset")
    measures_live_df=data.frame(measures)
    
    numerical_var <- cbind(mifem_dead[[2]], mifem_dead[[3]])
    mean <- apply(numerical_var, MARGIN=2, mean)
    variance <- apply(numerical_var, MARGIN=2, var)
    skewness <- apply(numerical_var, MARGIN=2, skewness)
    kurtosis <- apply(numerical_var, MARGIN=2, kurtosis)
    sd <- apply(numerical_var, MARGIN=2, sd)
    
    measures <- rbind(mean,variance,sd, skewness,kurtosis)
    colnames(measures)=c("age", "yronset")
    measures_dead_df=data.frame(measures)
    
    # Save the measures in LATEX format
    table_measures <- xtable::xtable(measures_df)
    table_measures_live <- xtable::xtable(measures_live_df)
    table_measures_dead <- xtable::xtable(measures_dead_df)
    
    # representation of the distribution of each variable in general
    # age
    dplot1 <- mifem %>% ggplot(aes(age)) + geom_density(alpha = 0.2, bw = 0.75, fill = "#2536D5") +
    theme_economist() +  xlab("age") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"))
    
    # yronset
    dplot2 <- mifem %>% ggplot(aes(yronset)) + geom_density(alpha = 0.2, bw = 0.75, fill = "#2536D5") +
      theme_economist() +  xlab("yronset") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"))
    
    # representation of the live and dead distributions in the previous vbles
    #age
    dplot3 <- mifem %>% ggplot(aes(age, fill = outcome)) + geom_density(alpha = 0.2, bw = 0.75, color = "black") +
      theme_economist() +  xlab("age") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold")) + facet_grid(outcome ~.)
    
    dplot4 <- mifem %>% ggplot(aes(yronset, fill = outcome)) + geom_density(alpha = 0.2, bw = 0.75, color = "black") +
      theme_economist() +  xlab("yronset") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold")) + facet_grid(outcome ~.)
    
    # combine the plots in figures
    # age
    figure_age_dist <- ggarrange(dplot1, dplot3, ncol = 2)
    annotate_figure(figure_age_dist, top = text_grob("Age distribution", face = "bold", size = 20)) 
    
    # yronset
    figure_yronset_dist <- ggarrange(dplot2, dplot4, ncol = 2)
    annotate_figure(figure_yronset_dist, top = text_grob("Yronset distribution", face = "bold", size = 20)) 
    
    
    
# GGPLOT FIGURES
    
    #Bar plot outcome inside diabetes
    ggplot(data=mifem)+aes(x=diabetes, fill=outcome)+geom_bar(position=position_stack())+ggtitle("Diabetes as a function of outcome") +
      theme_economist() +  xlab("diabetes") + ylab("count") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"))
    
    # Box Plot age function of outcome
    qplot(outcome, age, data=mifem, geom="boxplot", fill=outcome)+ggtitle("Box plot of age as function of the outcome") +
      theme_economist() +  xlab("outcome") + ylab("age") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"))
    
    # Box Plot --> age as function of the angina and outcome
    qplot(angina, age, data=mifem, geom="boxplot", fill=outcome)+ggtitle("Box plot of age as function of the angina and outcome") +
      theme_economist() +  xlab("angina") + ylab("age") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"), legend.position = "right",
            title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) + 
      scale_fill_discrete(name = "Outcome", labels = c("Live", "Dead"))
    
    # Density plot of Age as a function of highbp
    qplot(age, data=mifem, geom="density", color=highbp )+ ggtitle("Age density as function of the high pressure") +
      theme_economist() +  xlab("age") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"), legend.position = "right") + 
      scale_color_discrete(name = "High pressure", labels = c("Yes", "No", "Not Known"))
    
    # Density plot of Age as a function of stroke
    qplot(age, data=mifem, geom="density", color=stroke)+ggtitle("Age density as function of stroke") +
      theme_economist() +  xlab("age") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"), legend.position = "right") + 
      scale_color_discrete(name = "Stroke", labels = c("Yes", "No", "Not Known"))
    
    # Density plot of Age as a function of stroke, separated by values of stroke
    qplot(age, data=mifem, geom="density", color=stroke, facets = stroke ~ .)+ggtitle("Age density as function of stroke") +
      theme_economist() +  xlab("age") + ylab("density") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
            axis.title.x = element_text(face = "bold"), legend.position = "right",
            title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
      scale_color_discrete(name = "Stroke", labels = c("Yes", "No", "Not Known"))
      

      
# TIME SERIES
      #Mean Age as function of year of onset and outcome
      mifem %>% mutate(ag = age) %>% group_by(outcome,yronset) %>% summarize(MeanAge = mean(ag)) %>%
        ggplot(aes(yronset, MeanAge, color=outcome))+geom_line() +
        ggtitle("Mean Age as function of year of onset and outcome") +
        theme_economist() +  xlab("year of onset") + ylab("mean Age") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
              axis.title.x = element_text(face = "bold"), legend.position = "right",
              title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
      scale_color_discrete(name = "Outcome", labels = c("Live", "Dead"))
        

# GROUPING
      # Change line color --> Highbp histogram
      ggplot(mifem, aes(x = age)) + geom_histogram(aes( fill = highbp), color = "black", position = "identity", bins = 30, alpha = 0.4) + 
        ggtitle("Age histogram as function of high blood pressure") +
        theme_economist() +  xlab("age") + ylab("count") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
              axis.title.x = element_text(face = "bold"), legend.position = "right",
              title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)) + 
      scale_fill_discrete(name = "Stroke", labels = c("No", "Yes", "Not known")))
      
      
# CONTINGENCY TABLE AND MOSAIC PLOT
     #Categorical vbles: outcome and diabetes
      outcome_diabetes <- table(mifem$outcome, mifem$diabetes)
      contingency_table <- xtable::xtable(outcome_diabetes)
    
      # getting the marginals
      rowSums(outcome_diabetes)
      colSums(outcome_diabetes)
      
      # getting percents from the contengency table 
      prop.table(table(mifem$outcome, mifem$diabetes))*100
      # distribution of one variable within groups created by another
      prop_table_diabetes <- prop.table(table(mifem$outcome, mifem$diabetes), margin=1)*100
      table_prop_diabetes <- xtable::xtable(prop_table_diabetes)
      
      prop_table_outcome <- prop.table(table(mifem$outcome, mifem$diabetes), margin=2)*100
      table_prop_outcome <- xtable::xtable(prop_table_outcome)
      #chi-squared test: 
      chisq.test(mifem$outcome, mifem$diabetes) 
      
      # Mosaic plot to illustrate it
      ggplot(data=mifem)+geom_mosaic(aes(x=product(outcome, diabetes), fill=outcome)) +
        ggtitle("Mosaic Plot of Diabetes and Outcome") +
        theme_economist() +  xlab("diabetes (yes, no, not known)") + ylab("Outcome") +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face = "bold"),
              axis.title.x = element_text(face = "bold"), legend.position = "right",
              title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
      scale_fill_discrete(name = "Outcome", labels = c("Live", "Dead")) 
        
      
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    











