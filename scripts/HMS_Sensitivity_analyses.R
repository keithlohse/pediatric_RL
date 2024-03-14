### 2/2024

#Sensitivity analyses and new/updated visualizations and other analyses for the HMS manuscript revision.
# Sensitivity analysis is checking the regression to the mean effect and what random value for the 
# Feedback Response Rate and TTC are by checking trials N-2 and N-4

# 3/2024: note that Feedback Response Rate appears to be a product of the random, sequential error and not a 
# meaningful variable and has been removed from the manuscript. 


library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ez)
library(rstatix)


# LOAD A LIST OF PARTICIPANTS PLUS OTHER TESTING SCORES
FILE <- "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_3/Master_RL.csv"
Master_RL <- read.csv(FILE)

# Trim data set down to TD kids only to create the Aim 4 sample
Remove_list <- list(
  "D003", "D004", "D015","D016", "D017", "D009", "D011", 'D012', 'D014', 'D018', 'D007', 'D008', 'D010', 'D013', 'D001', 'D002')


Master_RL <- Master_RL[!(Master_RL$recordID %in% Remove_list),]

write.csv(Master_RL,"C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Master_RL_Aim4.csv" )
############ Sensitivity analyses ###################################################################


# my_sum <- Hx_dataL %>%
#   group_by(History) %>%
#   summarise( 
#     n=n(),
#     mean=mean(na.omit(TTC)),
#     sd=sd(na.omit(TTC))
#   )

Master_RL$recordID <- as.factor(Master_RL$recordID)

Master_RL$Hit_binary <- ifelse(abs(Master_RL$Y_error) <=25, 1,0 )

### Add conditional statements to mutate: https://www.r-bloggers.com/2022/07/how-to-do-conditional-mutate-in-r/

Master_lag <- Master_RL %>% 
  group_by(recordID) %>%
  mutate(Trial = Trial,
         y_n = Y_error,
         y_n1 = lag(Y_error,1),
         y_n2 = lag(Y_error,2),
         y_n3 = lag(Y_error,3),
         y_n4 = lag(Y_error,4),
         x_n = X_error,
         x_n1 = lag(X_error,1),
         x_n2 = lag(X_error,2),
         x_n3 = lag(X_error,3),
         x_n4 = lag(X_error,4),
         y_ACF1 = round(acf(Y_error, lag.max = 1, type="correlation", plot = F)$acf[2],3),
         x_ACF1 = round(acf(X_error, lag.max = 1, type="correlation", plot = F)$acf[2],3),
         Responsive_y =  case_when(y_n1 < -25 & y_n1 < y_n ~ 1,
                                   y_n1 > 25 & y_n1 > y_n ~ 1,
                                   y_n <= 25 & y_n >= -25 ~ 1),
         Responsive_y_lag2 =  case_when(y_n2 < -25 & y_n2 < y_n ~ 1,
                                   y_n2 > 25 & y_n2 > y_n ~ 1,
                                   y_n <= 25 & y_n >= -25 ~ 1),
         Responsive_y_lag4 =  case_when(y_n4 < -25 & y_n4 < y_n ~ 1,
                                        y_n4 > 25 & y_n4 > y_n ~ 1,
                                        y_n <= 25 & y_n >= -25 ~ 1),
         TTC_y = y_n - y_n1,
         TTC_y_lag2 = y_n - y_n2,
         TTC_y_lag4 = y_n - y_n4,
         TTC_x = x_n - x_n1,
         TTC_x_lag2 = x_n - x_n2,
         TTC_x_lag4 = x_n - x_n4,
         TTC_y_Total_Abs_Median = median(abs(na.omit(TTC_y))),
         TTC_y_Total_Abs_Median_lag2 = median(abs(na.omit(TTC_y_lag2))),
         TTC_y_Total_Abs_Median_lag4 = median(abs(na.omit(TTC_y_lag4))),
         TTC_x_Total_Abs_Median = median(abs(na.omit(TTC_x))),
         TTC_x_Total_Abs_Median_lag2 = median(abs(na.omit(TTC_x_lag2))),
         TTC_x_Total_Abs_Median_lag4 = median(abs(na.omit(TTC_x_lag4))),
         hit_lag = lag(Hit_binary,1)
         )

Master_lag <- Master_lag %>% 
  group_by(recordID) %>%
  mutate(hit_lag2 = lag(Hit_binary,2),
         hit_lag3 = lag(Hit_binary,3))
# Splus_y_median = case_when(hit_lag != 0 & hit_lag != NA ~ median(abs(na.omit(TTC_y)    )) )

Master_lag$Responsive_y <- ifelse(is.na(Master_lag$Responsive_y), 0, Master_lag$Responsive_y)
Master_lag$Responsive_y_lag2 <- ifelse(is.na(Master_lag$Responsive_y_lag2), 0, Master_lag$Responsive_y_lag2)
Master_lag$Responsive_y_lag4 <- ifelse(is.na(Master_lag$Responsive_y_lag4), 0, Master_lag$Responsive_y_lag4)


##### Master lag 2 is only for the ACF at lag2 and lag4 ######################



Master_lag2 <- Master_RL %>% 
  group_by(recordID) %>%
  mutate(y_ACF1 = round(acf(Y_error, lag.max = 4, type="correlation", plot = F)$acf[2],3),
         y_ACF2 = round(acf(Y_error, lag.max = 4, type="correlation", plot = F)$acf[3],3),
         y_ACF4 = round(acf(Y_error, lag.max = 4, type="correlation", plot = F)$acf[5],3),
         
         x_ACF1 = round(acf(X_error, lag.max = 4, type="correlation", plot = F)$acf[2],3),
         x_ACF2 = round(acf(X_error, lag.max = 4, type="correlation", plot = F)$acf[3],3),
         x_ACF4 = round(acf(X_error, lag.max = 4, type="correlation", plot = F)$acf[5],3),
         )
  )

ACF_only <- Master_lag2 %>% 
  group_by(recordID) %>%
  slice(1)

summary(ACF_only$y_ACF1)
summary(ACF_only$y_ACF2)
summary(ACF_only$y_ACF4)

summary(ACF_only$x_ACF1)
summary(ACF_only$x_ACF2)
summary(ACF_only$x_ACF4)

### These all appear near zero

### Perform ANOVA so we can show they are all the same
head(ACF_onlyL)
ACF_onlyL <- pivot_longer(ACF_only, cols = c(13:18), names_to = "Lag_type", values_to = "ACF")

#add factor or the axis
ACF_onlyL$Axis <- ifelse(startsWith(ACF_onlyL$Lag_type,"y" ),"Y","X" )
ACF_onlyL$Lag <- ifelse(endsWith(ACF_onlyL$Lag_type,"1" ),"1","NA" )
ACF_onlyL$Lag <- ifelse(endsWith(ACF_onlyL$Lag_type,"2" ),"2",ACF_onlyL$Lag )
ACF_onlyL$Lag <- ifelse(endsWith(ACF_onlyL$Lag_type,"4" ),"4",ACF_onlyL$Lag )



ACF_onlyL %>%
  group_by(Lag) %>%
  anova_test(dv = ACF, wid = recordID, within = Lag_type) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

ANOVA_lm <- lm(ACF ~ Lag_type, data = ACF_onlyL)
summary(ANOVA_lmer)
Anova(ANOVA_lm)

summary(aov(ACF ~ Lag_type, data = ACF_onlyL))

summary(ACF_onlyL$ACF)

ANOVA_lmer <- lmer(ACF ~ Lag_type + (1|recordID), data = ACF_onlyL)
Anova(ANOVA_lmer)

ezANOVA(data = ACF_onlyL,
        dv =ACF,
        within = Lag_type,
        wid = recordID,
        return_aov = T)
3*72
##### Now need to break out x and y Splus and Sminus TTC ###############
# ?????  https://www.statology.org/r-group-by-count-with-condition/

# There will be the following variables:
# 3 plus Y (lag 1, 2 and 4) and 3 plus X
# 3 minus Y and 3 minus X

### Y axis
Master_lag$Splus_TTC_y <- ifelse(Master_lag$hit_lag == 1, Master_lag$TTC_y, NA)
Master_lag$Splus_TTC_y_lag2 <- ifelse(Master_lag$hit_lag == 1, Master_lag$TTC_y_lag2, NA)
Master_lag$Splus_TTC_y_lag4 <- ifelse(Master_lag$hit_lag == 1, Master_lag$TTC_y_lag4, NA)


Master_lag$Sminus_TTC_y <- ifelse(Master_lag$hit_lag == 0, Master_lag$TTC_y, NA)
Master_lag$Sminus_TTC_y_lag2 <- ifelse(Master_lag$hit_lag == 0, Master_lag$TTC_y_lag2, NA)
Master_lag$Sminus_TTC_y_lag4 <- ifelse(Master_lag$hit_lag == 0, Master_lag$TTC_y_lag4, NA)


### X axis

Master_lag$Splus_TTC_x <- ifelse(Master_lag$hit_lag == 1, Master_lag$TTC_x, NA)
Master_lag$Splus_TTC_x_lag2 <- ifelse(Master_lag$hit_lag == 1, Master_lag$TTC_x_lag2, NA)
Master_lag$Splus_TTC_x_lag4 <- ifelse(Master_lag$hit_lag == 1, Master_lag$TTC_x_lag4, NA)

Master_lag$Sminus_TTC_x <- ifelse(Master_lag$hit_lag == 0, Master_lag$TTC_x, NA)
Master_lag$Sminus_TTC_x_lag2 <- ifelse(Master_lag$hit_lag == 0, Master_lag$TTC_x_lag2, NA)
Master_lag$Sminus_TTC_x_lag4 <- ifelse(Master_lag$hit_lag == 0, Master_lag$TTC_x_lag4, NA)



Master_lag <- Master_lag %>% 
  group_by(recordID) %>%
  mutate(Splus_TTC_y_median = round(median( abs(na.omit(Splus_TTC_y) )),2),
         Splus_TTC_y_lag2_median = round(median( abs(na.omit(Splus_TTC_y_lag2) )),2),
         Splus_TTC_y_lag4_median = round(median( abs(na.omit(Splus_TTC_y_lag4) )),2),
         
         Sminus_TTC_y_median = round(median( abs(na.omit(Sminus_TTC_y) )),2),
         Sminus_TTC_y_lag2_median = round(median( abs(na.omit(Sminus_TTC_y_lag2) )),2),
         Sminus_TTC_y_lag4_median = round(median( abs(na.omit(Sminus_TTC_y_lag4) )),2),
         
         Splus_TTC_x_median = round(median( abs(na.omit(Splus_TTC_x) )),2),
         Splus_TTC_x_lag2_median = round(median( abs(na.omit(Splus_TTC_x_lag2) )),2),
         Splus_TTC_x_lag4_median = round(median( abs(na.omit(Splus_TTC_x_lag4) )),2),
         
         Sminus_TTC_x_median = round(median( abs(na.omit(Sminus_TTC_x) )),2),
         Sminus_TTC_x_lag2_median = round(median( abs(na.omit(Sminus_TTC_x_lag2) )),2),
         Sminus_TTC_x_lag4_median = round(median( abs(na.omit(Sminus_TTC_x_lag4) )),2) )





####### Add FRR at n, n-2, n-4 ###############
Master_lag <- Master_lag %>% 
  group_by(recordID) %>%
  mutate(FRR_n = round(sum(Responsive_y)/length(Responsive_y),2),
         FRR_n2 = round(sum(Responsive_y_lag2)/length(Responsive_y_lag2),2) ,
         FRR_n4 = round(sum(Responsive_y_lag4)/length(Responsive_y_lag4),2) )






write.csv(Master_lag, 
          "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/Sensitivity_analysis_variables.csv")


############### Now plots and tests to examine the differences ############################



##### FRR plots ###################
# FRR at n-1, n-2, & n-4


############# THEME FOR PLOTTING ###################################

text0 <- 24
bp_size0 <- 2.2
h_size0 <- 1.5
q_line0 <- 2.6
q_point0 <- 9
p_size0 <- 5

My_Theme =  theme(
  axis.title.x = element_text(size = text0),
  axis.text.x = element_text(size = text0, color = "black"),
  axis.title.y = element_text(size = text0),
  axis.text.y = element_text(size = text0, color="black"),
  axis.line.x = element_line(),
  axis.line.y = element_line(),
  panel.background = element_rect(fill =  "white"),
  panel.grid.major = element_blank(),
  legend.text = element_text(size = 22),
  title = element_text(size = 15)) 




################### FRR PLOTS #######################################



#Pull out one row from each participant, which will have the FRR values to plot
FRR_plot <- Master_lag %>% 
  group_by(recordID) %>%
  slice(1)
FRR_plot <- FRR_plot[,c(2,65,66,67)]

FRR_plotL <- pivot_longer(FRR_plot, cols = c(2:4), names_to = "Lag_type", values_to = "FRR")


### Distribution of FRRs
FRR_plotL$Lag_type <- factor(FRR_plotL$Lag_type, levels = c("FRR_n", "FRR_n2", "FRR_n4"), 
                             labels = c("n-1", "n-2", "n-4"))

FRR_p1 <- ggplot(data = FRR_plotL, aes(x = FRR)) +
  geom_histogram(aes(fill=Lag_type), position = "stack") +
  xlab("FRR value") +
  My_Theme + scale_fill_grey()


FRR_p1


This.Plot <- FRR_p1
P.name <- deparse(substitute(FRR_p1))
Describe <- paste("FRR_distributions")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  

### Overall value of FRRs

my_EB1 <- FRR_plotL %>%
  group_by(Lag_type) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(FRR)),
    CI_low=t.test(FRR)$conf.int[1],
    CI_high=t.test(FRR)$conf.int[2])
  
my_EB1$Lag_type <- factor(my_EB1$Lag_type, levels = c("FRR_n", "FRR_n2", "FRR_n4"), 
                             labels = c("n-1", "n-2", "n-4"))

Q_plot_4_SD <-  ggplot() +
  geom_line(data = Quartile_Data, aes(x = Quartile, y = Explor_index, group = participant_id),
            size = 0.7, color = "darkgrey", alpha = 1)+
  geom_point(data=my_Q,aes(y = mean, x = Quartile ),size=q_point0,color=q_color) + 
  geom_line(data=my_Q,aes(y = mean, x = Quartile ),color=q_color, size = q_line0) +
  geom_errorbar(data=my_Q,aes(x=Quartile, ymin=mean-sd, ymax=mean+sd ), width=.2,size=q_line0,alpha=1,color=q_color) +
  My_Theme_EI +
  ylab("Exploration Index") + 
  xlab("Block") +
  scale_y_continuous(breaks = c(-.6,-0.3,0,0.3,0.6))
Q_plot_4_SD

# ggplot(data, aes(x = Group, y = Value)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   geom_errorbar(aes(ymin = Value - Error, ymax = Value + Error)

                
                
                
FRR_p2 <- ggplot(data = my_EB1, aes(x= Lag_type, y = mean)) +
  geom_bar(stat = "identity", aes(fill = Lag_type),show.legend = F) +
  xlab("Lag Category") +
  ylab("FRR value") +
  labs(caption = "95% CI") +
  geom_errorbar(aes(x=Lag_type, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey()


FRR_p2


This.Plot <- FRR_p2
P.name <- deparse(substitute(FRR_p2))
Describe <- paste("FRR_values_compared")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  
                
                
################### Total TTC PLOTS #######################################
                
       

#Pull out one row from each participant, which will have the FRR values to plot
One_row <- Master_lag %>% 
  group_by(recordID) %>%
  slice(1)
TTC_Y_plot <- One_row[,c(2,34:36)]
TTC_X_plot <- One_row[,c(2,37:39)]

TTC_Y_plotL <- pivot_longer(TTC_Y_plot, cols = c(2:4), names_to = "Lag_type", values_to = "TTC")
TTC_Y_plotL$Lag_interval <- ifelse(TTC_Y_plotL$Lag_type == "TTC_y_Total_Abs_Median","Lag1",TTC_Y_plotL$Lag_type)
TTC_Y_plotL$Lag_interval <- ifelse(TTC_Y_plotL$Lag_interval == "TTC_y_Total_Abs_Median_lag2","Lag2",TTC_Y_plotL$Lag_interval)
TTC_Y_plotL$Lag_interval <- ifelse(TTC_Y_plotL$Lag_interval == "TTC_y_Total_Abs_Median_lag4","Lag4",TTC_Y_plotL$Lag_interval)

TTC_X_plotL <- pivot_longer(TTC_X_plot, cols = c(2:4), names_to = "Lag_type", values_to = "TTC")
TTC_X_plotL$Lag_interval <- ifelse(TTC_X_plotL$Lag_type == "TTC_x_Total_Abs_Median","Lag1",TTC_X_plotL$Lag_type)
TTC_X_plotL$Lag_interval <- ifelse(TTC_X_plotL$Lag_interval == "TTC_x_Total_Abs_Median_lag2","Lag2",TTC_X_plotL$Lag_interval)
TTC_X_plotL$Lag_interval <- ifelse(TTC_X_plotL$Lag_interval == "TTC_x_Total_Abs_Median_lag4","Lag4",TTC_X_plotL$Lag_interval)





my_TTC_Y <- TTC_Y_plotL %>%
  group_by(Lag_interval) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(TTC)),
    CI_low=t.test(TTC)$conf.int[1],
    CI_high=t.test(TTC)$conf.int[2])

my_TTC_X <- TTC_X_plotL %>%
  group_by(Lag_interval) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(TTC)),
    CI_low=t.test(TTC)$conf.int[1],
    CI_high=t.test(TTC)$conf.int[2])


my_TTC_Y$Lag_interval <- factor(my_TTC_Y$Lag_interval , levels = c("Lag1", "Lag2", "Lag4"), 
                                labels = c("n-1", "n-2", "n-4"))

my_TTC_X$Lag_interval <- factor(my_TTC_X$Lag_interval , levels = c("Lag1", "Lag2", "Lag4"), 
                          labels = c("n-1", "n-2", "n-4"))

### TTC Y Plots


TTC_Y_p1 <- ggplot(data = my_TTC_Y, aes(x= Lag_interval, y = mean)) +
  geom_bar(stat = "identity", aes(fill = Lag_interval),show.legend = F) +
  xlab("Lag Interval") +
  ylab("Total TTC Median (Y axis)") +
  labs(caption = "95% CI") +
  geom_errorbar(aes(x=Lag_interval, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey()


TTC_Y_p1


This.Plot <- TTC_Y_p1
P.name <- deparse(substitute(TTC_Y_p1))
Describe <- paste("Total_TTC_Y_values_compared")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  



### TTC X Plots


TTC_X_p1 <- ggplot(data = my_TTC_X, aes(x= Lag_interval, y = mean)) +
  geom_bar(stat = "identity", aes(fill = Lag_interval),show.legend = F) +
  xlab("Lag Interval") +
  ylab("Total TTC Median (X axis)") +
  labs(caption = "95% CI") +
  geom_errorbar(aes(x=Lag_interval, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey() +
  scale_y_continuous(limits = c(0,40))


TTC_X_p1


This.Plot <- TTC_X_p1
P.name <- deparse(substitute(TTC_X_p1))
Describe <- paste("Total_TTC_X_values_compared")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  

         
                

################### Splus and Sminus PLOTS and Testing #######################################



#Pull out one row from each participant, which will have the FRR values to plot
# Data frame called One_row was created earlier
# One_row <- Master_lag %>% 
#   group_by(recordID) %>%
#   slice(1)
S_Y_plot <- One_row[,c(2,53:58)]
S_X_plot <- One_row[,c(2,59:64)]




S_Y_plotL <- pivot_longer(S_Y_plot, cols = c(2:7), names_to = "Lag_type", values_to = "TTC")
S_Y_plotL$Lag_interval <- ifelse(S_Y_plotL$Lag_type == "Splus_TTC_y_median","Lag1",S_Y_plotL$Lag_type)
S_Y_plotL$Lag_interval <- ifelse(S_Y_plotL$Lag_interval == "Splus_TTC_y_lag2_median","Lag2",S_Y_plotL$Lag_interval)
S_Y_plotL$Lag_interval <- ifelse(S_Y_plotL$Lag_interval == "Splus_TTC_y_lag4_median","Lag4",S_Y_plotL$Lag_interval)

S_Y_plotL$Lag_interval <- ifelse(S_Y_plotL$Lag_type == "Sminus_TTC_y_median","Lag1",S_Y_plotL$Lag_interval)
S_Y_plotL$Lag_interval <- ifelse(S_Y_plotL$Lag_interval == "Sminus_TTC_y_lag2_median","Lag2",S_Y_plotL$Lag_interval)
S_Y_plotL$Lag_interval <- ifelse(S_Y_plotL$Lag_interval == "Sminus_TTC_y_lag4_median","Lag4",S_Y_plotL$Lag_interval)


S_Y_plotL$S_type <- ifelse(S_Y_plotL$Lag_type == "Splus_TTC_y_median","Splus",S_Y_plotL$Lag_type)
S_Y_plotL$S_type <- ifelse(S_Y_plotL$S_type == "Splus_TTC_y_lag2_median","Splus",S_Y_plotL$S_type)
S_Y_plotL$S_type <- ifelse(S_Y_plotL$S_type == "Splus_TTC_y_lag4_median","Splus",S_Y_plotL$S_type)

S_Y_plotL$S_type <- ifelse(S_Y_plotL$Lag_type == "Sminus_TTC_y_median","Sminus",S_Y_plotL$S_type)
S_Y_plotL$S_type <- ifelse(S_Y_plotL$S_type == "Sminus_TTC_y_lag2_median","Sminus",S_Y_plotL$S_type)
S_Y_plotL$S_type <- ifelse(S_Y_plotL$S_type == "Sminus_TTC_y_lag4_median","Sminus",S_Y_plotL$S_type)


my_S_Y <- S_Y_plotL %>%
  group_by(Lag_interval, S_type) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(TTC)),
    CI_low=t.test(TTC)$conf.int[1],
    CI_high=t.test(TTC)$conf.int[2])


my_S_Y$Lag_interval <- factor(my_S_Y$Lag_interval , levels = c("Lag1", "Lag2", "Lag4"), 
                                labels = c("n-1", "n-2", "n-4"))


### TTC Y Plots
new.labs <- c("Failure (S-)","Success (S+)")
names(new.labs) <- c("Sminus", "Splus")

S_Y_p1 <- ggplot(data = my_S_Y, aes(x= Lag_interval, y = mean)) +
  geom_bar(stat = "identity", aes(fill = Lag_interval),show.legend = F) +
  xlab("Lag Interval") +
  ylab("Y-axis TTC Median")  +
  geom_errorbar(aes(x=Lag_interval, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey() + facet_grid(~S_type, labeller = labeller(S_type = new.labs)) + 
  theme(strip.text.x = element_text(
    size = 18, color = "black", face = "bold")  ) #+   labs(caption = "95% CI")


S_Y_p1


This.Plot <- S_Y_p1
P.name <- deparse(substitute(S_Y_p1))
Describe <- paste("Splus_and_Sminus_Y_values_compared")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  



########## Try a 'spagetti' plot #############




TTC_y_spa1 <- S_Y_plotL %>%
  ggplot( aes(x = Lag_interval, y = TTC, group = recordID )) +
  geom_line(size=1.2) +
  ylab("Y axis TTC") +
  xlab("") +
  My_Theme +
  facet_grid(~S_type)


TTC_y_spa1

This.Plot <- TTC_y_spa1
P.name <- deparse(substitute(TTC_y_spa1))
Describe <- paste("Splus_and_Sminus_Y_values_lines_for_each_participantd")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  


######### Try dot with whiskers ####################



my_S_Y$S_type <- factor(my_S_Y$S_type , levels = c("Sminus", "Splus"), 
                              labels = c("Unsuccessful trials (S-)", "Successful trials (S+)"))
head(S_Y_plotL)

# %>% subset(S_type == "Unsuccessful trials (S-)" )
TTC_y_spa2 <-my_S_Y %>% ggplot( aes(x= Lag_interval, y = mean)) + 
  geom_point(show.legend = T,size=8,aes(shape = S_type, color = S_type)) +
  geom_line(aes(x= Lag_interval, y = mean, group = S_type, linetype = S_type, color = S_type),size=line.size2) +
  xlab("") +
  ylab("Y-axis TTC ")  +
  geom_errorbar(aes(x=Lag_interval, ymin=CI_low, ymax=CI_high, color = S_type ), width=.2,size=line.size2,alpha=1) +
  My_Theme +
  theme(strip.text.x = element_text(
    size = 18, color = "black", face = "bold") ,
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2,'cm') ) +
  labs(group = "S_type",linetype = "S_type", shape = "S_type", color = "S_type") +
  scale_color_manual(values = c( "black",
                                 "grey45"))
TTC_y_spa2

This.Plot <- TTC_y_spa2
P.name <- deparse(substitute(TTC_y_spa2))
Describe <- paste("Splus_and_Sminus_Y_values_dot_with_whiskers")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 7.8*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  


######## Repeat the dot whisker plot for the X axis ########################
line.size2 = 2.3
my_S_X$S_type <- factor(my_S_X$S_type , levels = c("Sminus", "Splus"), 
                        labels = c("Unsuccessful trials (S-)", "Successful trials (S+)"))


TTC_x_spa2 <-my_S_X %>% ggplot( aes(x= Lag_interval, y = mean)) + 
  geom_point(show.legend = T,size=8,aes(shape = S_type, color = S_type)) +
  geom_line(aes(x= Lag_interval, y = mean, group = S_type, linetype = S_type, color = S_type),size=line.size2) +
  xlab("") +
  ylab("X-axis TTC ")  +
  geom_errorbar(aes(x=Lag_interval, ymin=CI_low, ymax=CI_high, color = S_type ), width=.2,size=line.size2,alpha=1) +
  My_Theme +
  theme(strip.text.x = element_text(
    size = 18, color = "black", face = "bold") ,
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2,'cm') ) +
  labs(group = "S_type",linetype = "S_type", shape = "S_type")+
  scale_color_manual(values = c( "black",
                                 "grey45"))


#+ facet_grid(~S_type, labeller = labeller(S_type = new.labs)) 

TTC_x_spa2

This.Plot <- TTC_x_spa2
P.name <- deparse(substitute(TTC_x_spa2))
Describe <- paste("Splus_and_Sminus_X_values_dot_with_whiskers")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 7.8*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  


lag_combo <- ggarrange(TTC_y_spa2, TTC_x_spa2, 
                        labels = "AUTO",
                        font.label = list(size = 20, color = "black"),
                       common.legend = T,
                       legend = "bottom")
lag_combo

This.Plot <- lag_combo
P.name <- deparse(substitute(lag_combo))
Describe <- paste("Splus_and_Sminus_with_both_axes")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 7*600, width = 13*600,
  units = "px",
  dpi=600,bg = "white")  

### Add pairwise testing
library(rstatix)

my_pairwise_Y0 <- S_Y_plotL %>%
  group_by(Lag_interval) %>%
  t_test(TTC~S_type, paired = T, p.adjust.method = "bonferroni")

my_pairwise_Y0

my_pairwise_Y <- S_Y_plotL %>%
  group_by(S_type) %>%
  t_test(TTC~Lag_interval, paired = T, p.adjust.method = "bonferroni")

my_pairwise_Y 


######## Repeat the analyses for the X axis ################

S_X_plotL <- pivot_longer(S_X_plot, cols = c(2:7), names_to = "Lag_type", values_to = "TTC")
S_X_plotL$Lag_interval <- ifelse(S_X_plotL$Lag_type == "Splus_TTC_x_median","Lag1",S_X_plotL$Lag_type)
S_X_plotL$Lag_interval <- ifelse(S_X_plotL$Lag_interval == "Splus_TTC_x_lag2_median","Lag2",S_X_plotL$Lag_interval)
S_X_plotL$Lag_interval <- ifelse(S_X_plotL$Lag_interval == "Splus_TTC_x_lag4_median","Lag4",S_X_plotL$Lag_interval)

S_X_plotL$Lag_interval <- ifelse(S_X_plotL$Lag_type == "Sminus_TTC_x_median","Lag1",S_X_plotL$Lag_interval)
S_X_plotL$Lag_interval <- ifelse(S_X_plotL$Lag_interval == "Sminus_TTC_x_lag2_median","Lag2",S_X_plotL$Lag_interval)
S_X_plotL$Lag_interval <- ifelse(S_X_plotL$Lag_interval == "Sminus_TTC_x_lag4_median","Lag4",S_X_plotL$Lag_interval)


S_X_plotL$S_type <- ifelse(S_X_plotL$Lag_type == "Splus_TTC_x_median","Splus",S_X_plotL$Lag_type)
S_X_plotL$S_type <- ifelse(S_X_plotL$S_type == "Splus_TTC_x_lag2_median","Splus",S_X_plotL$S_type)
S_X_plotL$S_type <- ifelse(S_X_plotL$S_type == "Splus_TTC_x_lag4_median","Splus",S_X_plotL$S_type)

S_X_plotL$S_type <- ifelse(S_X_plotL$Lag_type == "Sminus_TTC_x_median","Sminus",S_X_plotL$S_type)
S_X_plotL$S_type <- ifelse(S_X_plotL$S_type == "Sminus_TTC_x_lag2_median","Sminus",S_X_plotL$S_type)
S_X_plotL$S_type <- ifelse(S_X_plotL$S_type == "Sminus_TTC_x_lag4_median","Sminus",S_X_plotL$S_type)


my_S_X <- S_X_plotL %>%
  group_by(Lag_interval, S_type) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(TTC)),
    CI_low=t.test(TTC)$conf.int[1],
    CI_high=t.test(TTC)$conf.int[2])


my_S_X$Lag_interval <- factor(my_S_X$Lag_interval , levels = c("Lag1", "Lag2", "Lag4"), 
                              labels = c("n-1", "n-2", "n-4"))

### TTC X Plots
new.labs <- c("Failure (S-)","Success (S+)")
names(new.labs) <- c("Sminus", "Splus")

S_X_p1 <- ggplot(data = my_S_X, aes(x= Lag_interval, y = mean)) +
  geom_bar(stat = "identity", aes(fill = Lag_interval),show.legend = F) +
  xlab("Lag Interval") +
  ylab("X-axis TTC Median")  +
  geom_errorbar(aes(x=Lag_interval, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey() + facet_grid(~S_type, labeller = labeller(S_type = new.labs)) + 
  theme(strip.text.x = element_text(
    size = 18, color = "black", face = "bold")  ) #+   labs(caption = "95% CI")


S_X_p1


This.Plot <- S_X_p1
P.name <- deparse(substitute(S_X_p1))
Describe <- paste("Splus_and_Sminus_X_values_compared")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  



### Add pairwise testing
library(rstatix)

my_pairwise_X0 <- S_X_plotL %>%
  group_by(Lag_interval) %>%
  t_test(TTC~S_type, paired = T, p.adjust.method = "bonferroni")

my_pairwise_X0

my_pairwise_X <- S_X_plotL %>%
  group_by(S_type) %>%
  t_test(TTC~Lag_interval, paired = T, p.adjust.method = "bonferroni")

my_pairwise_X


#################### ACF in X and Y Plots ##################################


ACF_plot <- One_row[,c(2,23:24)]




ACF_plotL <- pivot_longer(ACF_plot, cols = c(2,3), names_to = "Axis", values_to = "ACF_value")
ACF_plotL$AxisF <- ifelse(ACF_plotL$Axis == "y_ACF1", "Y-axis","X-axis")


my_ACF <- ACF_plotL %>%
  group_by(AxisF) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(ACF_value)),
    CI_low=t.test(ACF_value)$conf.int[1],
    CI_high=t.test(ACF_value)$conf.int[2])




### ACF1 Plots
new.labs <- c("Failure (S-)","Success (S+)")
names(new.labs) <- c("Sminus", "Splus")

ACF_p1 <- ggplot(data = my_ACF, aes(x= AxisF, y = mean)) +
  geom_bar(stat = "identity", aes(fill = AxisF),show.legend = F) +
  xlab("Axis") +
  ylab("ACF1") +
  labs(caption = "95% CI") +
  geom_errorbar(aes(x=AxisF, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey() 

ACF_p1


This.Plot <- ACF_p1
P.name <- deparse(substitute(ACF_p1))
Describe <- paste("ACF1_values_compared")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/Sensitivity_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  



##################################### Redo Figure 4 #########################
###Hip percent by TTC + ACF~acquisition 

One_row$hit_percent  <- 
  Explore_All$hit_percent

One_row$TTC_y_Total_Abs_Median

cor.test(One_row$hit_percent,One_row$TTC_y_Total_Abs_Median)

point.size = 6
Scatter_plot_15b <-  ggplot(data= One_row, 
                            aes(y = TTC_y_Total_Abs_Median, x = hit_percent )) +
  geom_point(size=point.size,color = "grey32") +
  My_Theme  +
  xlab("Percent Hits") +
  ylab("Total TTC (cm)")+
  scale_color_gradient(low = "black", high = "grey83", na.value = NA, lim = c(0.17,0.76),
                       breaks = c(0.2,0.3,0.4,0.5,0.6,0.7))+
  guides(color = guide_legend(title = "Percent \nHits", reverse = T))+ 
  theme(legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size=15),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))  +
  annotate(geom = "text", x = .60, y = 70, label = paste("r = -0.81, adj. p < 0.0001"), size = 7)
# +
#   ggtitle( paste("Cor =",C.val,"p =",P.val))
Scatter_plot_15b



## ACF1 and RL slope
X.cor <- Explore_All$ACF1
Y.cor <- Explore_All$RL_slope
Cor_obj <- cor.test(X.cor,Y.cor, use='complete.obs', method = 'pearson')
C.val <- round(Cor_obj$estimate,2)
P.val <- round(Cor_obj$p.value,5)

Scatter_ACF_RLslope<-  ggplot(data=Explore_All, aes(y = ACF1, x =RL_slope )) +
  geom_point(size=point.size, color = "grey32") +
  My_Theme  +
  annotate(geom = "text", x = -1.7, y = -0.1, label = paste("r =",C.val,"\nadj. p > 0.05"), size = 7) +
  xlab("Acquisition Rate (cm/trial)") +
  ylab("ACF-1") + 
  scale_x_continuous(limits =c(-2,0) )
#+
#geom_smooth(method = "lm", se = F, color = "black", size = 2.5)
Scatter_ACF_RLslope




Fig5_combo <-cowplot::plot_grid(Scatter_ACF_RLslope,Scatter_plot_15b,
                                labels = "AUTO",
                                label_size = 20,
                                vjust =-.02,
                                hjust = -.45,
                                scale = .98,
                                ncol = 2) +
  theme(plot.margin = unit(c(0.58,0,0,0.1), "cm")) 

Fig5_combo

This.Plot <- Fig5_combo
P.name <- deparse(substitute(Fig5_combo))
Describe <- paste("Fig_5")

dpi = 1000

h0 <- 6
w0 <- 12.6

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = h0*dpi, width = w0*dpi,
  units = "px",
  dpi=dpi,bg = "white")  


dpi = 200


ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = h0*dpi, width = w0*dpi,
  units = "px",
  dpi=dpi,bg = "white")  








############# New image (fig 1?, task paradigm and example throws) for reviewer ##########################
### Reviewer 2/ Q4/ suggestion 1

text0 <- 25
#text0 <- 30
bp_size0 <- 2.2
h_size0 <- 1.5
q_line0 <- 2.6
q_point0 <- 9
p_size0 <- 5

My_Theme =  theme(
  axis.title.x = element_text(size = text0),
  axis.text.x = element_text(size = text0, color = "black"),
  axis.title.y = element_text(size = text0),
  axis.text.y = element_text(size = text0, color="black"),
  axis.line.x = element_line(),
  axis.line.y = element_line(),
  panel.background = element_rect(fill =  "white"),
  panel.grid.major = element_blank(),
  legend.text = element_text(size = 22),
  title = element_text(size = 15)) 


# C053 is a good one
Plot_one <- subset(Master_RL, recordID == "C010")


Scatter_example1 <-  ggplot(data= Plot_one, 
                            aes(y = Y_error, x = X_error )) +
  geom_point(size=point.size,color = "grey32") +
  My_Theme  +
  xlab("X-axis error (cm)") +
  ylab("Y-axis error (cm)") +
  geom_vline(xintercept = 0, size=1) +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(limits = c(-100,100)) +
  scale_y_continuous(limits = c(-100,100))+
  geom_hline(yintercept = 25, size = 1,linetype=2)+
  geom_hline(yintercept = -25, size = 1,linetype=2)




Scatter_example1
This.Plot <- Scatter_example1
P.name <- deparse(substitute(Scatter_example1))
Describe <- paste("task_paradigm")

dpi = 1000

h0 <- 6.5
w0 <- 9.5

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = h0*dpi, width = w0*dpi,
  units = "px",
  dpi=dpi,bg = "white")  


# C010 is a good one
Plot_one <- subset(Master_RL, recordID == "C010")

TT_example1 <-  ggplot(data= Plot_one, 
                            aes(y = Y_error, x = Trial )) +
  geom_point(size=point.size,color = "grey32") +
  geom_line( size = 1) +
  My_Theme  +
  xlab("Trial") +
  ylab("Y-axis error (cm)") +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(-100,100))+
  geom_hline(yintercept = 25, size = 1,linetype=2)+
  geom_hline(yintercept = -25, size = 1,linetype=2)




TT_example1

This.Plot <- TT_example1
P.name <- deparse(substitute(TT_example1))
Describe <- paste("task_paradigm")

dpi = 1000

h0 <- 5
w0 <- 14

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = h0*dpi, width = w0*dpi,
  units = "px",
  dpi=dpi,bg = "white")  


############ Redo the regression model for TTC ~ N + N-1 + N-2 ###############################
library(lme4)
library(lmerTest)

Hx_mod <- lm(TTC_y ~ hit_lag + hit_lag2 + hit_lag3,  data = Master_lag)
#Hx_mod <- lmer(TTC_y ~ hit_lag + hit_lag2 + hit_lag3 + (1|recordID),  data = Master_lag)
summary(Hx_mod)
AIC(Hx_mod)


### This is slightly different than previous model, possibly due to small errors in the previous code for HHH, MMM etc. 

# Plot the coefficents in a bar plot
confint.lm(Hx_mod)[,1]
Coef_plot_data <- as.data.frame(coefficients(Hx_mod))
Coef_plot_data$Variable <- rownames(Coef_plot_data)
colnames(Coef_plot_data) <- c("Coefficient","Variable")

Coef_plot_data$CI_low <- confint.lm(Hx_mod)[,1]
Coef_plot_data$CI_high <- confint.lm(Hx_mod)[,2]



new.labs <- c("Intercept","Trail N","Trail N-1", "Trial N-2")
names(new.labs) <- c("(Intercept)","hit_lag", "hit_lag2","hit_lag3")


Coef_plot_data$Variable <- factor(Coef_plot_data$Variable , levels = c("(Intercept)","hit_lag", "hit_lag2","hit_lag3"), 
                              labels = c("Intercept","Trial n","Trial n-1", "Trial n-2"))

Coef_plot1 <- ggplot(data = Coef_plot_data, aes(x= Variable, y = Coefficient)) +
  geom_bar(stat = "identity", aes(fill = Variable),show.legend = F) +
  xlab("") +
  ylab("Regression Coefficient") +
  geom_errorbar(aes(x=Variable, ymin=CI_low, ymax=CI_high ), width=.2,size=q_line0,alpha=1) +
  My_Theme +
  scale_fill_grey() +
  scale_y_continuous(limits = c(-20,10), breaks = c(-20,-15,-10,-5,0,5,10))


Coef_plot1



This.Plot <- Coef_plot1
P.name <- deparse(substitute(Coef_plot1))
Describe <- paste("Regression_terms_plotted")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6*600, width = 10*600,
  units = "px",
  dpi=600,bg = "white")  




length(which(Explore_processed$mabc_totalpercentile <16))/72



###################### ANOVA and pairewise testing for the lags on both axes ##############################

### Get the factors set up right
head(S_Y_plotL)

# S_Y_plotL$Axis <- ifelse(startsWith(S_Y_plotL$Lag_type,"TTC_y" ),"Y","X" )
# S_Y_plotL$Lag <- ifelse(endsWith(S_Y_plotL$Lag_type,"n" ),"1","NA" )
# S_Y_plotL$Lag <- ifelse(endsWith(S_Y_plotL$Lag_type,"2" ),"2",S_Y_plotL$Lag )
# S_Y_plotL$Lag <- ifelse(endsWith(S_Y_plotL$Lag_type,"4" ),"4",S_Y_plotL$Lag )

# Y_axis
summary(aov(TTC ~ S_type*Lag_interval, data = S_Y_plotL))

ezANOVA(data = S_Y_plotL,
        dv =TTC,
        within = Lag_interval,
        between = S_type,
        wid = recordID,
        return_aov = T)

### Pairwise testing
my_pairwise_Y0 <- S_Y_plotL %>%
  group_by(Lag_interval) %>%
  t_test(TTC~S_type, paired = T, p.adjust.method = "none")

my_pairwise_Y0

my_pairwise_Y <- S_Y_plotL %>%
  group_by(S_type) %>%
  t_test(TTC~Lag_interval, paired = T, p.adjust.method = NULL)

my_pairwise_Y 
name_list <- c("Lag_interval",colnames(my_pairwise_Y)[2:length(my_pairwise_Y)])
name_list
colnames(my_pairwise_Y) <- name_list
### Pull out only the comparisons we need
my_pairwise_combo_Y <- my_pairwise_Y0[1,]
my_pairwise_combo_Y <- rbind(my_pairwise_combo_Y, my_pairwise_Y[c(1,2,4,5),1:9])
#my_pairwise_combo_Y <- rbind(my_pairwise_combo_Y, my_pairwise_Y)
### Then adjust p values for the 5 comparisons

my_pairwise_combo_Y$adj_p <- p.adjust(my_pairwise_combo_Y$p, method = "bonferroni")


cor.test(Explo_Data_All_R$Total_absmed_TTC, Explo_Data_All_R$mabc_totalstandard)
cor.test(Explo_Data_All_R, Explo_Data_All_R$mabc_totalstandard)

# Repeat for X_axis
summary(aov(TTC ~ S_type*Lag_interval, data = S_X_plotL))
summary(aov(TTC ~ S_type, data = S_X_plotL))

ezANOVA(data = S_X_plotL,
        dv =TTC,
        within = Lag_interval,
        between = S_type,
        wid = recordID,
        return_aov = T)


ezANOVA(data = S_X_plotL,
        dv =TTC,
        within =S_type ,
        between = Lag_interval,
        wid = recordID,
        return_aov = T)

### Pairwise testing
my_pairwise_X0 <- S_X_plotL %>%
  group_by(Lag_interval) %>%
  t_test(TTC~S_type, paired = T, p.adjust.method = "Bonferroni")

my_pairwise_X0



my_pairwise_X <- S_X_plotL %>%
  group_by(S_type) %>%
  t_test(TTC~Lag_interval, paired = T, p.adjust.method = NULL)

my_pairwise_X 
name_list <- c("Lag_interval",colnames(my_pairwise_Y)[2:length(my_pairwise_X)])
name_list
colnames(my_pairwise_X) <- name_list
### Pull out only the comparisons we need
my_pairwise_combo_X <- my_pairwise_X0[1,]
my_pairwise_combo_X <- rbind(my_pairwise_combo_X, my_pairwise_X[c(1,2,4,5),1:9])

### Then adjust p values for the 5 comparisons

my_pairwise_combo_X$adj_p <- p.adjust(my_pairwise_combo_X$p, method = "bonferroni")

my_pairwise_combo_X



######### REDO Quartile data for ACF and other variables in the X-axis for plotting ####################

### Add factors for quartiles
Master_RL$Quartile <- NA
Master_RL$Quartile <- ifelse(Master_RL$Trial <26, 1,Master_RL$Quartile )
Master_RL$Quartile <- ifelse(Master_RL$Trial >25 & Master_RL$Trial < 51 , 2,Master_RL$Quartile )
Master_RL$Quartile <- ifelse(Master_RL$Trial >50 & Master_RL$Trial < 76 , 3,Master_RL$Quartile )
Master_RL$Quartile <- ifelse(Master_RL$Trial >75 , 4,Master_RL$Quartile )


### Now use pipes to calculate the variables ###

Q_data <- Master_RL %>% 
  group_by(recordID, Quartile) %>%
  mutate(Trial = Trial,
         y_n = Y_error,
         y_n1 = lag(Y_error,1),
         y_n2 = lag(Y_error,2),
         y_n3 = lag(Y_error,3),
         y_n4 = lag(Y_error,4),
         x_n = X_error,
         x_n1 = lag(X_error,1),
         x_n2 = lag(X_error,2),
         x_n3 = lag(X_error,3),
         x_n4 = lag(X_error,4),
         y_ACF1 = round(acf(Y_error, lag.max = 1, type="correlation", plot = F)$acf[2],3),
         x_ACF1 = round(acf(X_error, lag.max = 1, type="correlation", plot = F)$acf[2],3),
         Responsive_y =  case_when(y_n1 < -25 & y_n1 < y_n ~ 1,
                                   y_n1 > 25 & y_n1 > y_n ~ 1,
                                   y_n <= 25 & y_n >= -25 ~ 1),
         Responsive_y_lag2 =  case_when(y_n2 < -25 & y_n2 < y_n ~ 1,
                                        y_n2 > 25 & y_n2 > y_n ~ 1,
                                        y_n <= 25 & y_n >= -25 ~ 1),
         Responsive_y_lag4 =  case_when(y_n4 < -25 & y_n4 < y_n ~ 1,
                                        y_n4 > 25 & y_n4 > y_n ~ 1,
                                        y_n <= 25 & y_n >= -25 ~ 1),
         TTC_y = y_n - y_n1,
         TTC_y_lag2 = y_n - y_n2,
         TTC_y_lag4 = y_n - y_n4,
         TTC_x = x_n - x_n1,
         TTC_x_lag2 = x_n - x_n2,
         TTC_x_lag4 = x_n - x_n4,
         TTC_y_Total_Abs_Median = median(abs(na.omit(TTC_y))),
         TTC_y_Total_Abs_Median_lag2 = median(abs(na.omit(TTC_y_lag2))),
         TTC_y_Total_Abs_Median_lag4 = median(abs(na.omit(TTC_y_lag4))),
         TTC_x_Total_Abs_Median = median(abs(na.omit(TTC_x))),
         TTC_x_Total_Abs_Median_lag2 = median(abs(na.omit(TTC_x_lag2))),
         TTC_x_Total_Abs_Median_lag4 = median(abs(na.omit(TTC_x_lag4))),
         hit_lag = lag(Hit_binary,1)   )






Q_data_One_row <- Q_data %>% 
  group_by(recordID,Quartile) %>%
  slice(1)



#write.csv(Q_data_One_row,"C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Aim4_quartile_data.csv" )



############ Redo the percent hits and misses correctly for the manuscript #####################



summary(Explo_Data_All_R$hit_percent)
sd(Explo_Data_All_R$hit_percent)


