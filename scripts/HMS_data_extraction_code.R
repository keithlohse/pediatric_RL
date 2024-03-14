# 3/2024
# For HMS manuscript
# Extracting variables from the raw throwing error data

library(tidyverse)
library(pracma) # sample_entropy()
library(ggpubr)
library(cowplot)
library(gridExtra)
library(patchwork)
library(reshape)
library(nonlinearTseries)

########### START LOAD DATA ################


# LOAD A LIST OF PARTICIPANTS PLUS OTHER TESTING SCORES
FILE.1 <- "C:/Users/jdkonrad/Box/LangLab/DCD/Data/EncodingInDevelopmen-ScoreReport1.csv"
RECORD <- read.csv(FILE.1)

RECORD$participant_id <- as.factor(RECORD$participant_id)
#remove participant D016
RECORD <- subset(RECORD, participant_id != "D016")

q=1
######## Now A Loop that iterates retrieving each participants data and creating a DF of lagged data ####
for(q in 1:nrow(RECORD)){ 
  FILE.PATH.RL <- paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                          RECORD$participant_id[q],"_RL.csv")  
  
  RL_TEMP <- read.csv(FILE.PATH.RL, header = T)
 
  
  ### Trim empty rows off #
  
  RL_length = nrow(RL_TEMP)
  Empty_Throws <- vector()
  y=1
  for (z in 1:RL_length){
    if (is.na(RL_TEMP$X_error[z]) & RL_TEMP$Throw[z] ==0 & RL_TEMP$Condition[z] == '' ){
      Empty_Throws[y] = z
      y=y+1
    }
  }

  Total_Throws <- min(Empty_Throws) - 1 #This is the length to snip at because the remaining cells are empty

  RL_TEMP <- RL_TEMP[1:Total_Throws,]
  
  ###
  
  
  ### Add factors for short, far, hit, miss
  for(i in 1:nrow(RL_TEMP)){
    if(abs(RL_TEMP$Y_error[i]) <= 25){RL_TEMP$Hit_Category[i] <- "Hit"
    }  else if(RL_TEMP$Y_error[i] > 25 && RL_TEMP$Y_error[i] < 100){RL_TEMP$Hit_Category[i] <- "Far"
    } else if(RL_TEMP$Y_error[i] < -25){RL_TEMP$Hit_Category[i] <- "Short"
    } else if(RL_TEMP$Y_error[i] == 100 ){RL_TEMP$Hit_Category[i] <- "Miss"}
  }
  RL_TEMP$Hit_Category <- as.factor(RL_TEMP$Hit_Category)
  
  
                         "Hit_Category_minus_2" = Hit_Category_minus_2,
                       "Hit_Category_minus_3" = Hit_Category_minus_3)

Lag_Data.Y$TTC <- Lag_Data.Y$t - Lag_Data.Y$t_minus_1

##### Save lag data into participants folder ######


write.csv(Lag_Data.Y, file = 
            paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                   RECORD$participant_id[q],"_Y_Error_Lag_Data.csv"))
  
  print(q)
}  

q=87



######### Retrieve data from folders to create plots #############
for(q in 1:nrow(RECORD)){ 
  FILE.PATH.RL.2 <- paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                         RECORD$participant_id[q],"_Y_Error_Lag_Data.csv")  
  
  Lag_Temp <- read.csv(FILE.PATH.RL.2, header = T)

  
  
 Lag_Plot <- ggplot(data = Lag_Temp) +
                  geom_point(aes(x=t_minus_1, y=t), size=3) +
                  ggtitle(paste("Lag Plot:",RECORD$participant_id[q])) +
                  xlab("Y Error-1") + ylab("Y Error")
 #Lag_Plot
 
 This.Plot <- Lag_Plot
 P.name <- deparse(substitute(Lag_Plot))
 Describe <- paste(RECORD$participant_id[q],"Lag_Plot")
 
 ggsave(
   filename =
     paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/Lag_Plots/",P.name,"_",Describe,".jpg"),
   plot = This.Plot,
   height = 6, width = 8,
   units = "in",
   dpi=500,bg = "white")
 
 
 
 Hit_Plot_All_Line <- ggplot(data = Lag_Temp)  +
   geom_point(aes(x=Trial, y=t,color=Hit_Category), size=6) +
   geom_line(aes(x=Trial, y=t), lwd=.1)+
   ggtitle(paste("Hit Plot 1-100:",RECORD$participant_id[q])) +
   scale_y_continuous(limits = c(-100,100))+
   xlab("Trial") + ylab("Y Error") +
   theme(legend.position = "bottom")+
   scale_color_manual(values = c("Far" = "orange2",
                                 "Hit"="green3",
                                "Short"="dodgerblue2",
                                "Miss" = "red2"))
 #Hit_Plot_All_Line 
 
 
 Hit_Plot_50 <- ggplot(data = Lag_Temp[1:50,])  +
   geom_point(aes(x=Trial, y=t,color=Hit_Category), size=6, show.legend = F) +
   ggtitle(paste("Hit Plot 1-50:",RECORD$participant_id[q])) +
   scale_y_continuous(limits = c(-100,100))+
   xlab("Trial") + ylab("Y Error")+
   scale_color_manual(values = c("Far" = "orange2",
                                 "Hit"="green3",
                                 "Short"="dodgerblue2",
                                 "Miss" = "red2"))
 #Hit_Plot_50 
 

 layout <- matrix(c(1, NA,
                    2, 2), ncol = 2, byrow = TRUE)
 
 Combo_Hit_Plot <- grid.arrange(Hit_Plot_50, Hit_Plot_All_Line, ncol=2, layout_matrix=layout)
 #Combo_Hit_Plot 
 


 This.Plot <- Combo_Hit_Plot 
 P.name <- deparse(substitute(Combo_Hit_Plot ))
 Describe <- paste(RECORD$participant_id[q],"Combo_Hit_Plot ")
 
 ggsave(
   filename =
     paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/Hit_Plots/",P.name,"_",Describe,".jpg"),
   plot = This.Plot,
   height = 6, width = 8,
   units = "in",
   dpi=500,bg = "white")
 
 
 
 
 

  print(q)
}  





q=1

###### Calculate variables #########
### Create variables from all 1-100 trials

col.list <- c("participant_id",
              "trials_range",
              "hit_percent",
              "short_percent",
              "sample_entropy",
              "approx_entropy",
              "Total_absmean_TTC",
              "S_plus_TTC",
              "S_minus_TTC",
              "Total_TTC_sum",
              "S_plus_TTC_sum",
              "S_minus_TTC_sum",
              "ACF1",
              "ACF2",
              "ACF3",
              "Error_TTC_Cor",
              "Error_TTC_pval",
              "Error_TTC_Cor_nonabs",
              "Error_TTC_pval_nonabs",
              "Explor_propor",
              "Adj_Explor_propor",
              'DFA',
              "Explor_index")

Explo_Data_All <- data.frame(matrix(nrow = 87, ncol = length(col.list)))

colnames(Explo_Data_All ) <- col.list




#q=4
######### Retrieve data from folders to perform calculations for exploration variables #############
for(q in 1:nrow(RECORD)){ 
  FILE.PATH.RL.2 <- paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                           RECORD$participant_id[q],"_Y_Error_Lag_Data.csv")  
  
  Lag_Temp <- read.csv(FILE.PATH.RL.2, header = T)
  
  Explo_Data_All$participant_id[q] <- as.character(RECORD$participant_id[q])
  
  ### Add a factor for identify the trials 1-50, 51-100, All
  Explo_Data_All$trials_range[q] <- "All"
  
  ### Add hit % to the calculations 
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Explo_Data_All$hit_percent[q] <- round(length(Hits)/nrow(Lag_Temp),2)
  
  
  ### DFA 
  
  DFA_temp <- dfa(Lag_Temp$t, do.plot = F)
  dfa

  DFA_lm <- lm(log10(DFA_temp$fluctuation.function) ~log10( DFA_temp$window.sizes))
  Explo_Data_All$DFA[q] <- as.numeric(round(DFA_lm$coefficients[2],2))

  ### Entropy 
  #A low value of the entropy indicates that the time series is deterministic; a high value indicates randomness.
  sample_ent_temp <- sample_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t), tau = 1)
  Explo_Data_All$sample_entropy[q] <- round(sample_ent_temp,3)
  
  
  approx_ent_temp <-approx_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t),elag = 1)
  Explo_Data_All$approx_entropy[q] <- round(approx_ent_temp,3)
  
  
  ### Trial to trial change/variability - total
  
  
  #Plot TTC's - does not seem to have any major skew
  # ggplot(data = Lag_Temp) +
  #   geom_histogram(aes(x=TTC))
  
  ########## SHOULD EI BE CALCULATED WITH THE SUM ?? ########
  #Explo_Data_All$Total_absmean_TTC[q] <- round(mean(abs(na.omit(Lag_Temp$TTC))),2)
  Explo_Data_All$Total_TTC_sum[q] <- round(sum(abs(na.omit(Lag_Temp$TTC))),2)
  Explo_Data_All$Total_absmean_TTC[q] <- round(mean(abs(na.omit(Lag_Temp$TTC))),2)
  
  
  
  ### S+ and S- TTC
  Lag_Temp$Hit_Category_minus_1
  
  # separate successes and failures
  S_plus <- subset(Lag_Temp, Hit_Category_minus_1 == "Hit")
  S_minus <- subset(Lag_Temp, Hit_Category_minus_1 != "Hit")

if(length(na.omit(Lag_Temp$TTC)) == nrow(S_plus) + nrow(S_minus)){print("Same")} else{print("Not the Same")}
  
  
  ### TTC after S+
  Explo_Data_All$S_plus_TTC_sum[q] <- round(sum(na.omit(abs(S_plus$TTC))),2)
  Explo_Data_All$S_plus_TTC[q] <- round(mean(na.omit(abs(S_plus$TTC))),2)
  
  ### TTC after S-
  Explo_Data_All$S_minus_TTC_sum[q] <- round(sum(na.omit(abs(S_minus$TTC))),2)
  Explo_Data_All$S_minus_TTC[q] <- round(mean(na.omit(abs(S_minus$TTC))),2)
    
  ### Explor index: Ratio of (S- - S+) / total  = (exploration&noise - noise) / total 
  #- a measure of the degree of exploration
  
  Explo_Data_All$Explor_index[q]<- round((Explo_Data_All$S_minus_TTC[q] - Explo_Data_All$S_plus_TTC[q]) / 
                                           (Explo_Data_All$S_minus_TTC[q] + Explo_Data_All$S_plus_TTC[q]) ,3)
  
  Explo_Data_All$Explor_index_sum[q]<- round((Explo_Data_All$S_minus_TTC[q] - Explo_Data_All$S_plus_TTC[q]) / 
                                           (Explo_Data_All$Total_absmean_TTC[q]) ,3)
  
 
  ### Autocorrelation
  ACF_temp <- acf(Lag_Temp$t, lag.max = 10, type="correlation", plot = F)
  
  Explo_Data_All$ACF1[q] <-  round(ACF_temp$acf[2],3) #lag-1
  Explo_Data_All$ACF2[q] <-round(ACF_temp$acf[3],3) #lag-2
  Explo_Data_All$ACF3[q] <-round(ACF_temp$acf[4],3) #lag-3
  
  
  
  ### Autoregression
  #ar.ols(ts(Lag_Temp$t), intercept = T)
  
  #These give the same results
  # arima(ts(Lag_Temp$t),order = c(1L, 0L, 0L))
  
  #Autoreg <- summary(lm(t ~ t_minus_1  , data = Lag_Temp))
  #Autoreg
  
  
  ### Suggest by KRL: abs change in error as a function of the previous abs error (in cm not hit categories)
# 
  X <- abs(Lag_Temp$TTC)
  Y <- abs(Lag_Temp$t_minus_1)
  Error_TTC_sum <- cor.test(X , Y)
  Error_TTC_cor <- round(Error_TTC_sum$estimate,2)
  Error_TTC_pval <- round(Error_TTC_sum$p.value,6)

  Explo_Data_All$Error_TTC_Cor[q] <- Error_TTC_cor
  Explo_Data_All$Error_TTC_pval[q] <- Error_TTC_pval
  
  
  X <- Lag_Temp$TTC
  Y <- Lag_Temp$t_minus_1
  Error_TTC_sum <- cor.test(X , Y)
  Error_TTC_cor <- round(Error_TTC_sum$estimate,2)
  Error_TTC_pval <- round(Error_TTC_sum$p.value,6)
  
  Explo_Data_All$Error_TTC_Cor_nonabs[q] <- Error_TTC_cor
  Explo_Data_All$Error_TTC_pval_nonabs[q] <- Error_TTC_pval
  

#   
#   Error_by_Change_plot <- ggplot(data = Lag_Temp, aes(y= abs(TTC), x = abs(t_minus_1))) +
#     geom_point() +
#     ggtitle(paste("Previous Error by TTC:",RECORD$participant_id[q])) +
#     scale_y_continuous(limits = c(0,200))+
#     xlab("Abs. T-1 Error") + ylab("Abs. TTC")
#   Error_by_Change_plot 
#   
#     This.Plot <- Error_by_Change_plot 
#   P.name <- deparse(substitute(Error_by_Change_plot  ))
#   Describe <- paste(RECORD$participant_id[q],"Error_by_Change_plot")
#   
#   ggsave(
#     filename =
#       paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/Error_Plots/",P.name,"_",Describe,".jpg"),
#     plot = This.Plot,
#     height = 6, width = 8,
#     units = "in",
#     dpi=500,bg = "white")
  
  
  
  
  #### ADD THE RATIONAL INDEX ###############
  # note KRL's concern was that there is noise so that change in error could be within
  # their level of noise and therefore not reflect the intended exploration
  
  # This version does not address that and is a straight, unfiltered proportion based on Y-error 
  # and hit categories 
  Lag_Temp$Hit_Category <- as.factor(Lag_Temp$Hit_Category)
  Lag_Temp$Explore <- NA 
  i=1
  #Loop and if statements to assign "explore" points
  for(i in 2:nrow(Lag_Temp)){print(i)
    if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] != "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Explore[i] <- 1}
  
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 1}
  
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 1}
  
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Explore[i] <- 1}
  
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Miss" && Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Miss" && Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Miss" && Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Explore[i] <- 1} 
  }
  
  #calculate the explore coefficient
  Explor_sum <- sum(na.omit(Lag_Temp$Explore))
  
  Explor_Prop <- round( Explor_sum / (length(na.omit(Lag_Temp$Explore))) , 3)
  
  Explo_Data_All$Explor_propor[q] <- Explor_Prop
  
  

  
  
  # Adjusted by a threshold, using S- TTC/2
  # Not sure what to do with consecutive hits
  Lag_Temp$Adj_Explore <- NA 
  i=1
  #Loop and if statements to assign "explore" points
  for(i in 2:nrow(Lag_Temp)){print(i)
    if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit" ){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_All$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Hit" && 
            Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Hit" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
    
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_All$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_All$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1] ){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && 
            Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Adj_Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Miss" && 
            Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Miss" && 
            Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Miss" && 
            Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 1} 
  }
  
  #calculate the Adj_Explore coefficient
  Adj_Explor_sum <- sum(na.omit(Lag_Temp$Adj_Explore))
  
  Adj_Explor_Prop <- round( Adj_Explor_sum / (length(na.omit(Lag_Temp$Adj_Explore))) , 3)
  
  Explo_Data_All$Adj_Explor_propor[q] <- Adj_Explor_Prop
  
  ### Add a short percent - percentage of misses thrown short
  Hit_table <- as.data.frame(table(Lag_Temp$Hit_Category))
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Shorts <- which(Lag_Temp$Hit_Category == "Short")

  Shorts_count <- length(Shorts)
  All_misses <- nrow(Lag_Temp) - length(Hits)
  Explo_Data_All$short_percent[q] <- round(Shorts_count / All_misses,2)

  print(q)
}  

cor.test(Explo_Data_All$Explor_index, Explo_Data_All$Explor_index_sum)
### Merge with other data from RECORD data frame

Explo_Data_All <- merge(Explo_Data_All, RECORD[,-c(2,3)], by = "participant_id")

### Merge with key throwing variables (acquisition, baseline variance, baseline bias)

All_throwing_data <- read.csv(file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_3/All_throwing_data.csv")
All_throwing_data <- All_throwing_data[-86,] # Removing D016


Explo_Data_All$RL_slope <- All_throwing_data$Slope1st
Explo_Data_All$EBL_AC <- All_throwing_data$AC
Explo_Data_All$RL_variance <- All_throwing_data$SD_Yerror
Explo_Data_All$Baseline_EBL_variance <- All_throwing_data$Baseline_SD_All
Explo_Data_All$Baseline_EBL_bias <- All_throwing_data$Baseline_Bias_All


######## Save the 1-100 data frame ##########

 write.csv(Explo_Data_All, row.names = F,
   file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Aim4_Explore_Data_All.csv")


########## A look at initial correlations ################

# two values from sample entropy have inf value
Explo_Data_All$sample_entropy <- ifelse(Explo_Data_All$sample_entropy == Inf,5.0,Explo_Data_All$sample_entropy)

X <- Explo_Data_All$RL_slope
Y <- Explo_Data_All$Explor_propor
cor.test(X,Y)





####### RE-RUN FOR TRIALS 1-50 ###############



###### Calculate variables #########
###

# col.list <- c("participant_id",
#     "trials_range",
#     "hit_percent",
#     "short_percent",
#     "sample_entropy",
#     "approx_entropy",
#     "Total_absmean_TTC",
#     "S_plus_TTC",
#     "S_minus_TTC",
#     "ACF1",
#     "ACF2",
#     "ACF3",
#     "Error_TTC_Cor",
#     "Error_TTC_pval",
#     "Explor_propor",
#     "Adj_Explor_propor",
#     "Explor_index")

Explo_Data_1_50 <- data.frame(matrix(nrow = 87, ncol = length(col.list)))

colnames(Explo_Data_1_50 ) <- col.list

q=86
######### Retrieve data from folders to perform calculations for exploration variables #############
for(q in 1:nrow(RECORD)){ 
  FILE.PATH.RL.2 <- paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                           RECORD$participant_id[q],"_Y_Error_Lag_Data.csv")  
  
  Lag_Temp <- read.csv(FILE.PATH.RL.2, header = T)
  if(nrow(Lag_Temp) >=50){
  Lag_Temp <- Lag_Temp[1:50,]}
  
  Explo_Data_1_50$participant_id[q] <- as.character(RECORD$participant_id[q])
  
  ### Add a factor for identify the trials 1-50, 51-100, All
  Explo_Data_1_50$trials_range[q] <- "1_50"
  
  ### Add hit % to the calculations 
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Explo_Data_1_50$hit_percent[q] <- round(length(Hits)/nrow(Lag_Temp),2)
  
  
  ### Entropy 
  #A low value of the entropy indicates that the time series is deterministic; a high value indicates randomness.
  sample_ent_temp <- sample_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t), tau = 1)
  Explo_Data_1_50$sample_entropy[q] <- round(sample_ent_temp,3)
  
  
  approx_ent_temp <-approx_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t),elag = 1)
  Explo_Data_1_50$approx_entropy[q] <- round(approx_ent_temp,3)
  
  
  ### Trial to trial change/variability - total
  
  
  # #Plot TTC's - does not seem to have any major skew
  # ggplot(data = Lag_Temp) +
  #   geom_histogram(aes(x=TTC))
  
  
  Explo_Data_1_50$Total_absmean_TTC[q] <- round(mean(na.omit(abs(Lag_Temp$TTC))),2)
  
  
  ### S+ and S- TTC
  Lag_Temp$Hit_Category_minus_1
  
  # separate successes and failures
  S_plus <- subset(Lag_Temp, Hit_Category_minus_1 == "Hit")
  S_minus <- subset(Lag_Temp, Hit_Category_minus_1 != "Hit")
  
  
  ### TTC after S+
  
  Explo_Data_1_50$S_plus_TTC[q] <- round(mean(na.omit(abs(S_plus$TTC))),2)
  
  ### TTC after S-
  
  Explo_Data_1_50$S_minus_TTC[q] <- round(mean(na.omit(abs(S_minus$TTC))),2)
  ### Explor index: Ratio of (S- - S+) / total  = (exploration&noise - noise) / total 
  #- a measure of the degree of exploration
  
  Explo_Data_1_50$Explor_index[q]<- round((Explo_Data_1_50$S_minus_TTC[q] - Explo_Data_1_50$S_plus_TTC[q]) / 
                                            (Explo_Data_1_50$Total_absmean_TTC[q]) ,3)
  
  ### Autocorrelation
  ACF_temp <- acf(Lag_Temp$t, lag.max = 10, type="correlation", plot = F)
  
  Explo_Data_1_50$ACF1[q] <-  round(ACF_temp$acf[2],3) #lag-1
  Explo_Data_1_50$ACF2[q] <-round(ACF_temp$acf[3],3) #lag-2
  Explo_Data_1_50$ACF3[q] <-round(ACF_temp$acf[4],3) #lag-3
  
  
  
  ### Autoregression
  #ar.ols(ts(Lag_Temp$t), intercept = T)
  
  #These give the same results
  # arima(ts(Lag_Temp$t),order = c(1L, 0L, 0L))
  
  #Autoreg <- summary(lm(t ~ t_minus_1  , data = Lag_Temp))
  #Autoreg
  
  
  ### Suggest by KRL: abs change in error as a function of the previous abs error (in cm not hit categories)
  
  X <- abs(Lag_Temp$TTC)
  Y <- abs(Lag_Temp$t_minus_1)
  Error_TTC_sum <- cor.test(X , Y)
  Error_TTC_cor <- round(Error_TTC_sum$estimate,2)
  Error_TTC_pval <- round(Error_TTC_sum$p.value,6)

  Explo_Data_1_50$Error_TTC_Cor[q] <- Error_TTC_cor
  Explo_Data_1_50$Error_TTC_pval[q] <- Error_TTC_pval

  # 
  # Error_by_Change_plot <- ggplot(data = Lag_Temp, aes(y= abs(TTC), x = abs(t_minus_1))) +
  #   geom_point() +
  #   ggtitle(paste("Previous Error by TTC:",RECORD$participant_id[q])) +
  #   scale_y_continuous(limits = c(0,200))+
  #   xlab("Abs. T-1 Error") + ylab("Abs. TTC")
  # Error_by_Change_plot 
  # 
  # This.Plot <- Error_by_Change_plot 
  # P.name <- deparse(substitute(Error_by_Change_plot  ))
  # Describe <- paste(RECORD$participant_id[q],"Error_by_Change_plot")
  # 
  # ggsave(
  #   filename =
  #     paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/Error_Plots/",P.name,"_",Describe,".jpg"),
  #   plot = This.Plot,
  #   height = 6, width = 8,
  #   units = "in",
  #   dpi=500,bg = "white")
  
  
  
 
  
  
  #### ADD THE RATIONAL INDEX ###############
  # note KRL's concern was that there is noise so that change in error could be within
  # their level of noise and therefore not reflect the intended exploration
  
  # This version does not address that and is a straight, unfiltered proportion based on Y-error 
  # and hit categories 
  Lag_Temp$Hit_Category <- as.factor(Lag_Temp$Hit_Category)
  Lag_Temp$Explore <- NA 
  i=1
  #Loop and if statements to assign "explore" points
  for(i in 2:nrow(Lag_Temp)){print(i)
    if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] != "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Explore[i] <- 1}
    
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Miss" && Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Miss" && Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Miss" && Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Explore[i] <- 1} 
  }
  
  #calculate the explore coefficient
  Explor_sum <- sum(na.omit(Lag_Temp$Explore))
  
  Explor_Prop <- round( Explor_sum / (length(na.omit(Lag_Temp$Explore))) , 3)
  
  Explo_Data_1_50$Explor_propor[q] <- Explor_Prop
  
  
  
  
  
  # Adjusted by a threshold, using S- TTC
  # Not sure what to do with consecutive hits
  Lag_Temp$Adj_Explore <- NA 
  i=1
  #Loop and if statements to assign "explore" points
  for(i in 2:nrow(Lag_Temp)){print(i)
    if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit" ){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_1_50$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
    else if(Lag_Temp$Hit_Category[i] != "Hit" && 
            Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Hit" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
    
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_1_50$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_1_50$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1] ){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && 
            Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Adj_Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Miss" && 
            Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Miss" && 
            Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Miss" && 
            Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 1} 
  }
  
  #calculate the Adj_Explore coefficient
  Adj_Explor_sum <- sum(na.omit(Lag_Temp$Adj_Explore))
  
  Adj_Explor_Prop <- round( Adj_Explor_sum / (length(na.omit(Lag_Temp$Adj_Explore))) , 3)
  
  Explo_Data_1_50$Adj_Explor_propor[q] <- Adj_Explor_Prop
  
  ### Add a short percent - percentage of misses thrown short
  Hit_table <- as.data.frame(table(Lag_Temp$Hit_Category))
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Shorts <- which(Lag_Temp$Hit_Category == "Short")
  
  Shorts_count <- length(Shorts)
  All_misses <- nrow(Lag_Temp) - length(Hits)
  Explo_Data_1_50$short_percent[q] <- round(Shorts_count / All_misses,2)
  
  print(q)
}  

### Merge with other data from RECORD data frame

Explo_Data_1_50 <- merge(Explo_Data_1_50, RECORD[,-c(2,3)], by = "participant_id")

### Merge with key throwing variables (acquisition, baseline variance, baseline bias)

All_throwing_data <- read.csv(file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_3/All_throwing_data.csv")
All_throwing_data <- All_throwing_data[-86,] # Removing D016


Explo_Data_1_50$RL_slope <- All_throwing_data$Slope1st
Explo_Data_1_50$EBL_AC <- All_throwing_data$AC
Explo_Data_1_50$RL_variance <- All_throwing_data$SD_Yerror
Explo_Data_1_50$Baseline_EBL_variance <- All_throwing_data$Baseline_SD_All
Explo_Data_1_50$Baseline_EBL_bias <- All_throwing_data$Baseline_Bias_All


######## Save the 1-50 data frame ##########

write.csv(Explo_Data_1_50, row.names = F,
          file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Aim4_Explore_Data_1_50.csv")







####### RE-RUN FOR TRIALS 51-100 ###############



###### Calculate variables #########
### 

# col.list <- c("participant_id",
#               "trials_range",
#               "hit_percent",
#               "short_percent",
#               "sample_entropy",
#               "approx_entropy",
#               "Total_absmean_TTC",
#               "S_plus_TTC",
#               "S_minus_TTC",
#               "ACF1",
#               "ACF2",
#               "ACF3",
#               "Error_TTC_Cor",
#               "Error_TTC_pval",
#               "Explor_propor",
#               "Adj_Explor_propor",
#               "Explor_index")

Explo_Data_51_100 <- data.frame(matrix(nrow = 87, ncol = length(col.list)))

colnames(Explo_Data_51_100 ) <- col.list

q=1
######### Retrieve data from folders to perform calculations for exploration variables #############
for(q in 1:nrow(RECORD)){ 
  FILE.PATH.RL.2 <- paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                           RECORD$participant_id[q],"_Y_Error_Lag_Data.csv")  
  
  Lag_Temp <- read.csv(FILE.PATH.RL.2, header = T)
  
  # The 51-100 data set needs ome if() statments to address participants that don't 
  # have more than 50 throws, here making sure there are at least 65 total throws so that
  # there are 15 for the 51-100 block.
  if (nrow(Lag_Temp)<65){
    Explo_Data_51_100[q,] <-NA 
    Explo_Data_51_100$participant_id[q] <- as.character(RECORD$participant_id[q])
    Explo_Data_51_100$trials_range[q] <- "51_100"
    
  }else{
  
  Lag_Temp <- Lag_Temp[51:nrow(Lag_Temp),]
  
  Explo_Data_51_100$participant_id[q] <- as.character(RECORD$participant_id[q])
  
  ### Add a factor for identify the trials 1-50, 51-100, All
  Explo_Data_51_100$trials_range[q] <- "51_100"
  
  ### Add hit % to the calculations 
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Explo_Data_51_100$hit_percent[q] <- round(length(Hits)/nrow(Lag_Temp),2)
  
  
  ### Entropy 
  #A low value of the entropy indicates that the time series is deterministic; a high value indicates randomness.
  sample_ent_temp <- sample_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t), tau = 1)
  Explo_Data_51_100$sample_entropy[q] <- round(sample_ent_temp,3)
  
  
  approx_ent_temp <-approx_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t),elag = 1)
  Explo_Data_51_100$approx_entropy[q] <- round(approx_ent_temp,3)
  
  
  ### Trial to trial change/variability - total
  
  
  # #Plot TTC's - does not seem to have any major skew
  # ggplot(data = Lag_Temp) +
  #   geom_histogram(aes(x=TTC))
  
  
  Explo_Data_51_100$Total_absmean_TTC[q] <- round(mean(na.omit(abs(Lag_Temp$TTC))),2)
  
  
  ### S+ and S- TTC
  Lag_Temp$Hit_Category_minus_1
  
  # separate successes and failures
  S_plus <- subset(Lag_Temp, Hit_Category_minus_1 == "Hit")
  S_minus <- subset(Lag_Temp, Hit_Category_minus_1 != "Hit")
  
  
  ### TTC after S+
  
  Explo_Data_51_100$S_plus_TTC[q] <- round(mean(na.omit(abs(S_plus$TTC))),2)
  
  ### TTC after S-
  
  Explo_Data_51_100$S_minus_TTC[q] <- round(mean(na.omit(abs(S_minus$TTC))),2)
  
  ### Explor index: Ratio of (S- - S+) / total  = (exploration&noise - noise) / total 
  #- a measure of the degree of exploration
  
  Explo_Data_51_100$Explor_index[q]<- round((Explo_Data_51_100$S_minus_TTC[q] - Explo_Data_51_100$S_plus_TTC[q]) / 
                                              (Explo_Data_51_100$Total_absmean_TTC[q]) ,3)
  
  ### Autocorrelation
  ACF_temp <- acf(Lag_Temp$t, lag.max = 10, type="correlation", plot = F)
  
  Explo_Data_51_100$ACF1[q] <-  round(ACF_temp$acf[2],3) #lag-1
  Explo_Data_51_100$ACF2[q] <-round(ACF_temp$acf[3],3) #lag-2
  Explo_Data_51_100$ACF3[q] <-round(ACF_temp$acf[4],3) #lag-3
  
  
  
  ### Autoregression
  #ar.ols(ts(Lag_Temp$t), intercept = T)
  
  #These give the same results
  # arima(ts(Lag_Temp$t),order = c(1L, 0L, 0L))
  
  #Autoreg <- summary(lm(t ~ t_minus_1  , data = Lag_Temp))
  #Autoreg
  
  
  ### Suggest by KRL: abs change in error as a function of the previous abs error (in cm not hit categories)
  
  X <- abs(Lag_Temp$TTC)
  Y <- abs(Lag_Temp$t_minus_1)
  Error_TTC_sum <- cor.test(X , Y)
  Error_TTC_cor <- round(Error_TTC_sum$estimate,2)
  Error_TTC_pval <- round(Error_TTC_sum$p.value,6)
  
  Explo_Data_51_100$Error_TTC_Cor[q] <- Error_TTC_cor 
  Explo_Data_51_100$Error_TTC_pval[q] <- Error_TTC_pval 
  
  
  Error_by_Change_plot <- ggplot(data = Lag_Temp, aes(y= abs(TTC), x = abs(t_minus_1))) +
    geom_point() +
    ggtitle(paste("Previous Error by TTC:",RECORD$participant_id[q])) +
    scale_y_continuous(limits = c(0,200))+
    xlab("Abs. T-1 Error") + ylab("Abs. TTC")
  Error_by_Change_plot 
  
  This.Plot <- Error_by_Change_plot 
  P.name <- deparse(substitute(Error_by_Change_plot  ))
  Describe <- paste(RECORD$participant_id[q],"Error_by_Change_plot")
  
  ggsave(
    filename =
      paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/Error_Plots/",P.name,"_",Describe,".jpg"),
    plot = This.Plot,
    height = 6, width = 8,
    units = "in",
    dpi=500,bg = "white")
  
  
  
  
  
  #### ADD THE RATIONAL INDEX ###############
  # note KRL's concern was that there is noise so that change in error could be within
  # their level of noise and therefore not reflect the intended exploration
  
  # This version does not address that and is a straight, unfiltered proportion based on Y-error 
  # and hit categories 
  Lag_Temp$Hit_Category <- as.factor(Lag_Temp$Hit_Category)
  Lag_Temp$Explore <- NA 
  i=1
  #Loop and if statements to assign "explore" points
  for(i in 2:nrow(Lag_Temp)){print(i)
    if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] != "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Explore[i] <- 1}
    
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Miss" && Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Miss" && Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Miss" && Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Explore[i] <- 1} 
  }
  
  #calculate the explore coefficient
  Explor_sum <- sum(na.omit(Lag_Temp$Explore))
  
  Explor_Prop <- round( Explor_sum / (length(na.omit(Lag_Temp$Explore))) , 3)
  
  Explo_Data_51_100$Explor_propor[q] <- Explor_Prop
  
  
  
  
  
  # Adjusted by a threshold, using S- TTC
  # Not sure what to do with consecutive hits
  Lag_Temp$Adj_Explore <- NA 
  i=1
  #Loop and if statements to assign "explore" points
  for(i in 2:nrow(Lag_Temp)){print(i)
    if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit" ){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_51_100$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
    else if(Lag_Temp$Hit_Category[i] != "Hit" && 
            Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Hit" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
    
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_51_100$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            abs(Lag_Temp$TTC[i]) < Explo_Data_51_100$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1] ){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Short" && 
            Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Adj_Explore[i] <- 1}
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
            Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
    
    else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
            Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
    
    else if(Lag_Temp$Hit_Category[i] == "Miss" && 
            Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] == "Miss" && 
            Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
    else if(Lag_Temp$Hit_Category[i] != "Miss" && 
            Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 1} 
  }
  
  #calculate the Adj_Explore coefficient
  Adj_Explor_sum <- sum(na.omit(Lag_Temp$Adj_Explore))
  
  Adj_Explor_Prop <- round( Adj_Explor_sum / (length(na.omit(Lag_Temp$Adj_Explore))) , 3)
  
  Explo_Data_51_100$Adj_Explor_propor[q] <- Adj_Explor_Prop
  
  ### Add a short percent - percentage of misses thrown short
  Hit_table <- as.data.frame(table(Lag_Temp$Hit_Category))
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Shorts <- which(Lag_Temp$Hit_Category == "Short")
  
  Shorts_count <- length(Shorts)
  All_misses <- nrow(Lag_Temp) - length(Hits)
  Explo_Data_51_100$short_percent[q] <- round(Shorts_count / All_misses,2)
  }
  print(q)
}  

### Merge with other data from RECORD data frame

Explo_Data_51_100 <- merge(Explo_Data_51_100, RECORD[,-c(2,3)], by = "participant_id")

### Merge with key throwing variables (acquisition, baseline variance, baseline bias)

All_throwing_data <- read.csv(file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_3/All_throwing_data.csv")
All_throwing_data <- All_throwing_data[-86,] # Removing D016


Explo_Data_51_100$RL_slope <- All_throwing_data$Slope1st
Explo_Data_51_100$EBL_AC <- All_throwing_data$AC
Explo_Data_51_100$RL_variance <- All_throwing_data$SD_Yerror
Explo_Data_51_100$Baseline_EBL_variance <- All_throwing_data$Baseline_SD_All
Explo_Data_51_100$Baseline_EBL_bias <- All_throwing_data$Baseline_Bias_All


######## Save the 51-100 data frame ##########

write.csv(Explo_Data_51_100, row.names = F,
          file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Aim4_Explore_Data_51_100.csv")



### Now merge the data frames for 1-100, 1-50, 51-100 by the block factor ###########

#put all data frames into list
df_list <- list(Explo_Data_All,Explo_Data_1_50,Explo_Data_51_100)      

#Combine the data frames
Explore_Data_Combined <- merge_recurse(df_list)

#sort the ft
Explore_Data_Combined  <- arrange(Explore_Data_Combined, by_group =participant_id)



########## Save the combined df ###################

write.csv(Explore_Data_Combined, row.names = F,
          file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Aim4_Explore_Data_Combined.csv")





Explore_Data_Combined$Explor_index


