# 3/2024: 
# For HMS manuscript
# Note, there are some calculations and code here for the now defunct Feedback Response Rate variable. 

# calculate and store the suit of suit of variables

library(tidyverse)
library(pracma) # sample_entropy()
library(ggpubr)
library(cowplot)
library(gridExtra)
library(patchwork)

########### START LOAD DATA ################


# LOAD A LIST OF PARTICIPANTS PLUS OTHER TESTING SCORES
FILE.1 <- "C:/Users/jdkonrad/Box/LangLab/DCD/Data/EncodingInDevelopmen-ScoreReport1.csv"
RECORD <- read.csv(FILE.1)

RECORD$participant_id <- as.factor(RECORD$participant_id)
#remove participant D016
RECORD <- subset(RECORD, participant_id != "D016")


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
  
  
  
  
  # Add lag columns and new DF for calculations
t <- RL_TEMP$Y_error#[2:nrow(RL_TEMP)]
t_minus_1 <- lag(RL_TEMP$Y_error, n=1)#[2:nrow(RL_TEMP)]
t_minus_2 <- lag(RL_TEMP$Y_error, n=2)
t_minus_3 <- lag(RL_TEMP$Y_error, n=3)


Hit_Category_minus_1 <- lag(RL_TEMP$Hit_Category, n=1)
Hit_Category_minus_2 <- lag(RL_TEMP$Hit_Category, n=2)
Hit_Category_minus_3 <- lag(RL_TEMP$Hit_Category, n=3)


Lag_Data.Y <- data.frame("participant_id" = RECORD$participant_id[q],
                       "Trial" = RL_TEMP$Trial,
                       "Hit_Category" = RL_TEMP$Hit_Category,
                       "t" = t,
                       "t_minus_1" =t_minus_1,
                       "t_minus_2" =t_minus_2,
                       "t_minus_3" =t_minus_3,
                       "Hit_Category_minus_1" = Hit_Category_minus_1,
                       "Hit_Category_minus_2" = Hit_Category_minus_2,
                       "Hit_Category_minus_3" = Hit_Category_minus_3)

Lag_Data.Y$TTC <- Lag_Data.Y$t - Lag_Data.Y$t_minus_1

##### Save lag data into participants folder ######


write.csv(Lag_Data.Y, file = 
            paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                   RECORD$participant_id[q],"_Y_Error_Lag_Data.csv"))
  
  print(q)
}  

q=1



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
   xlab("Y Error-1") + ylab("Y Error") +
   theme(legend.position = "bottom")+
   scale_color_manual(values = c("Far" = "red2",
                                 "Hit"="green3",
                                "Short"="dodgerblue2",
                                "Miss" = "orange2"))
 #Hit_Plot_All_Line 
 
 
 Hit_Plot_50 <- ggplot(data = Lag_Temp[1:50,])  +
   geom_point(aes(x=Trial, y=t,color=Hit_Category), size=6, show.legend = F) +
   ggtitle(paste("Hit Plot 1-50:",RECORD$participant_id[q])) +
   scale_y_continuous(limits = c(-100,100))+
   xlab("Y Error-1") + ylab("Y Error")+
   scale_color_manual(values = c("Far" = "red2",
                                 "Hit"="green3",
                                 "Short"="dodgerblue2",
                                 "Miss" = "orange2"))
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
              "hit_percent",
              "Total_absmean_TTC",
              "Total_absmed_TTC",
              "S_plus_TTC",
              "S_minus_TTC",
              "S_minus_medTTC",
              "S_plus_medTTC",
              "Error_TTC_Cor",
              "Error_TTC_pval",
              "HHH_TTC",
              "MMM_TTC",
              "MHH_TTC",
              "HMM_TTC",
              "Explor_propor",
              "Explor_index_med")

Explo_Data_All_R <- data.frame(matrix(nrow = 87, ncol = length(col.list)))

colnames(Explo_Data_All_R ) <- col.list

#q=1
######### Retrieve data from folders to perform calculations for exploration variables #############
for(q in 1:nrow(RECORD)){ 
  FILE.PATH.RL.2 <- paste0("C:/Users/jdkonrad/Box/LangLab/DCD/Data/",RECORD$participant_id[q],"/",
                           RECORD$participant_id[q],"_Y_Error_Lag_Data.csv")  
  
  Lag_Temp <- read.csv(FILE.PATH.RL.2, header = T)
  
  Explo_Data_All_R$participant_id[q] <- as.character(RECORD$participant_id[q])
  
  ### Add hit % to the calculations 
  Hits <- which(Lag_Temp$Hit_Category == "Hit")
  Explo_Data_All_R$hit_percent[q] <- round(length(Hits)/nrow(Lag_Temp),2)
  
  
  ### Entropy 
  #A low value of the entropy indicates that the time series is deterministic; a high value indicates randomness.
  # sample_ent_temp <- sample_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t), tau = 1)
  # Explo_Data_All_R$sample_entropy[q] <- round(sample_ent_temp,3)
  # 
  # 
  # approx_ent_temp <-approx_entropy(Lag_Temp$t,edim = 2, r=0.2*sd(Lag_Temp$t),elag = 1)
  # Explo_Data_All_R$approx_entropy[q] <- round(approx_ent_temp,3)
  # 
  # 
  ### Trial to trial change/variability - total
  
  
  #Plot TTC's - does not seem to have any major skew
  ggplot(data = Lag_Temp) +
    geom_histogram(aes(x=TTC))
  
  
  Explo_Data_All_R$Total_absmean_TTC[q] <- round(mean(na.omit(abs(Lag_Temp$TTC))),2)
  Explo_Data_All_R$Total_absmed_TTC[q] <- round(median(na.omit(abs(Lag_Temp$TTC))),2)
  
  ### S+ and S- TTC
  Lag_Temp$Hit_Category_minus_1
  
  # separate successes and failures
  S_plus <- subset(Lag_Temp, Hit_Category_minus_1 == "Hit")
  S_minus <- subset(Lag_Temp, Hit_Category_minus_1 != "Hit")
  
  
  ### TTC after S+
  
  Explo_Data_All_R$S_plus_TTC[q] <- round(mean(na.omit(abs(S_plus$TTC))),2)
  Explo_Data_All_R$S_plus_medTTC[q] <- round(median(na.omit(abs(S_plus$TTC))),2)
  ### TTC after S-
  
  Explo_Data_All_R$S_minus_TTC[q] <- round(mean(na.omit(abs(S_minus$TTC))),2)
  Explo_Data_All_R$S_minus_medTTC[q] <- round(median(na.omit(abs(S_minus$TTC))),2)
  
  # ### Autocorrelation
  # ACF_temp <- acf(Lag_Temp$t, lag.max = 10, type="correlation", plot = F)
  # 
  # Explo_Data_All_R$ACF1[q] <-  round(ACF_temp$acf[2],3) #lag-1
  # Explo_Data_All_R$ACF2[q] <-round(ACF_temp$acf[3],3) #lag-2
  # Explo_Data_All_R$ACF3[q] <-round(ACF_temp$acf[4],3) #lag-3
  # 
  # 
  
  ### Autoregression
  #ar.ols(ts(Lag_Temp$t), intercept = T)
  
  #These give the same results
  # arima(ts(Lag_Temp$t),order = c(1L, 0L, 0L))
  
  #Autoreg <- summary(lm(t ~ t_minus_1  , data = Lag_Temp))
  #Autoreg
  
  
  ### Suggest by KRL: abs change in error as a function of the previous abs error (in cm not hit categories)
# 
#   X <- abs(Lag_Temp$TTC)
#   Y <- abs(Lag_Temp$t_minus_1)
#   Error_TTC_sum <- cor.test(X , Y)
#   Error_TTC_cor <- round(Error_TTC_sum$estimate,2)
#   Error_TTC_pval <- round(Error_TTC_sum$p.value,6)
#   
#   Explo_Data_All_R$Error_TTC_Cor[q] <- Error_TTC_cor 
#   Explo_Data_All_R$Error_TTC_pval[q] <- Error_TTC_pval 
#     
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
#   
#   
  
  ### Reward history \
  
  # TTC after different combos, not doing all histories but those that are possible are
  # HHH, MHH, HMH, MMH, HHM, MHM, HMM, MMM (Pekny et al. 2015)
  HHH = 0
  HHH_TTC_sum =0
  
  MMM = 0
  MMM_TTC_sum =0
  
  MHH = 0
  MHH_TTC_sum =0
  
  HMM = 0
  HMM_TTC_sum =0
  
  
  for(zz in 4:nrow(Lag_Temp)){ # start at 3 because there are NA's in the front of the lagged columns
    
    
    if (Lag_Temp$Hit_Category_minus_1[zz] == "Hit" && 
        Lag_Temp$Hit_Category_minus_2[zz] == "Hit" &&
        Lag_Temp$Hit_Category_minus_3[zz] == "Hit" ){
      HHH_TTC_sum <- HHH_TTC_sum + abs(Lag_Temp$TTC[zz])
      HHH <- HHH+1
    }
    
    
    

    if (Lag_Temp$Hit_Category_minus_1[zz] != "Hit" &&
        Lag_Temp$Hit_Category_minus_2[zz] != "Hit" &&
        Lag_Temp$Hit_Category_minus_3[zz] != "Hit"){
      MMM_TTC_sum <- MMM_TTC_sum + abs(Lag_Temp$TTC[zz])
      MMM <- MMM+1
    }

    
    if (Lag_Temp$Hit_Category_minus_1[zz] != "Hit" &&
        Lag_Temp$Hit_Category_minus_2[zz] == "Hit" &&
        Lag_Temp$Hit_Category_minus_3[zz] == "Hit"){
      MHH_TTC_sum <- MHH_TTC_sum + abs(Lag_Temp$TTC[zz])
      MHH <- MHH+1
    }
    
    if (Lag_Temp$Hit_Category_minus_1[zz] == "Hit" &&
        Lag_Temp$Hit_Category_minus_2[zz] != "Hit" &&
        Lag_Temp$Hit_Category_minus_3[zz] != "Hit"){
      HMM_TTC_sum <- HMM_TTC_sum + abs(Lag_Temp$TTC[zz])
      HMM <- HMM+1
    }
    
    print(zz)
  }  
  
  
  Explo_Data_All_R$HHH_TTC[q] <- round(HHH_TTC_sum / HHH,2)
  Explo_Data_All_R$MMM_TTC[q] <- round(MMM_TTC_sum / MMM,2)
  
  Explo_Data_All_R$MHH_TTC[q] <- round(MHH_TTC_sum / MHH,2)
  Explo_Data_All_R$HMM_TTC[q] <- round(HMM_TTC_sum / HMM,2)
  
  
  
  
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
  
  Explo_Data_All_R$Explor_propor[q] <- Explor_Prop
  
  
  ### Explor index: Ratio of (S- - S+) / total  = (exploration&noise - noise) / total 
  #
  
  Explo_Data_All_R$Explor_index_med[q]<- round((Explo_Data_All_R$S_minus_medTTC[q] - Explo_Data_All_R$S_plus_medTTC[q]) / 
                                           (Explo_Data_All_R$S_minus_medTTC[q] + Explo_Data_All_R$S_plus_medTTC[q]) ,3)
  
  # 
  # # Adjusted by a threshold, using S- TTC
  # # Not sure what to do with consecutive hits
  # Lag_Temp$Adj_Explore <- NA 
  # i=1
  # #Loop and if statements to assign "explore" points
  # for(i in 2:nrow(Lag_Temp)){print(i)
  #   if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] == "Hit" ){Lag_Temp$Adj_Explore[i] <- 1}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Hit" && Lag_Temp$Hit_Category[i-1] != "Hit" &&
  #           abs(Lag_Temp$TTC[i]) < Explo_Data_All_R$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
  #   else if(Lag_Temp$Hit_Category[i] != "Hit" && 
  #           Lag_Temp$Hit_Category[i-1] == "Hit"){Lag_Temp$Adj_Explore[i] <- 0}
  #   else if(Lag_Temp$Hit_Category[i] == "Hit" && 
  #           Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
  #   
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
  #           abs(Lag_Temp$TTC[i]) < Explo_Data_All_R$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
  #           abs(Lag_Temp$TTC[i]) < Explo_Data_All_R$S_plus_TTC[q]/2 ){Lag_Temp$Adj_Explore[i] <- NA}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
  #           Lag_Temp$t[i] > Lag_Temp$t[i-1] ){Lag_Temp$Adj_Explore[i] <- 1}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Short" && Lag_Temp$Hit_Category[i-1] == "Short" &&
  #           Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Short" && 
  #           Lag_Temp$Hit_Category[i-1] != "Short" ){Lag_Temp$Adj_Explore[i] <- 1}
  #   else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
  #           Lag_Temp$t[i] < Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 1}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] != "Far" && 
  #           Lag_Temp$Hit_Category[i-1] != "Hit"){Lag_Temp$Adj_Explore[i] <- 1}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Far" && Lag_Temp$Hit_Category[i-1] == "Far" &&
  #           Lag_Temp$t[i] > Lag_Temp$t[i-1]){Lag_Temp$Adj_Explore[i] <- 0}
  #   
  #   else if(Lag_Temp$Hit_Category[i] == "Miss" && 
  #           Lag_Temp$Hit_Category[i-1] != "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
  #   else if(Lag_Temp$Hit_Category[i] == "Miss" && 
  #           Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 0}
  #   else if(Lag_Temp$Hit_Category[i] != "Miss" && 
  #           Lag_Temp$Hit_Category[i-1] == "Miss" ){Lag_Temp$Adj_Explore[i] <- 1} 
  # }
  # 
  # #calculate the Adj_Explore coefficient
  # Adj_Explor_sum <- sum(na.omit(Lag_Temp$Adj_Explore))
  # 
  # Adj_Explor_Prop <- round( Adj_Explor_sum / (length(na.omit(Lag_Temp$Adj_Explore))) , 3)
  # 
  # Explo_Data_All_R$Adj_Explor_propor[q] <- Adj_Explor_Prop
  # 
  # ### Add a short percent - percentage of misses thrown short
  # Hit_table <- as.data.frame(table(Lag_Temp$Hit_Category))
  # Hits <- which(Lag_Temp$Hit_Category == "Hit")
  # Shorts <- which(Lag_Temp$Hit_Category == "Short")
  # 
  # Shorts_count <- length(Shorts)
  # All_misses <- nrow(Lag_Temp) - length(Hits)
  # Explo_Data_All_R$short_percent[q] <- round(Shorts_count / All_misses,2)

  print(q)
}  

write.csv(Explo_Data_All_R, 
"C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/Perormance_Variables_with_median.csv")


# Trim data set down to TD kids only
Remove_list <- list(
  "D003", "D004", "D015","D016", "D017", "D009", "D011", 'D012', 'D014', 'D018', 'D007', 'D008', 'D010', 'D013', 'D001', 'D002')



Explo_Data_All_R <-  Explo_Data_All_R[!(Explo_Data_All_R$participant_id %in% Remove_list),] # Now down to the 72 TD kids
Explo_Data_All_R <- Explo_Data_All_R[1:72,] # removes rows of NAs




summary(Explo_Data_All_R$S_plus_TTC) #mean 34.26
summary(Explo_Data_All_R$S_plus_medTTC) #mean 29.72

summary(Explo_Data_All_R$S_minus_TTC) #mean 51.75
summary(Explo_Data_All_R$S_minus_medTTC) #mean 45.39

summary(Explo_Data_All_R$Total_absmean_TTC) #mean 44.02
summary(Explo_Data_All_R$Total_absmed_TTC) #mean 36.89



summary(Explo_Data_All_R$Explor_index_med) #mean 21.76




############### Manuscript Plots #####################
### Good plotting code is in Aim4_prelim_analysis_code_v1.R

##### Figure 2: Exploration Index/TTC Manuscript Plot ##################
# S-,S+,total boxplot
# EI dist
# EI quartiles
# EI by age

text0 <- 24
bp_size0 <- 2.2
h_size0 <- 1.5
q_line0 <- 2.6
q_point0 <- 9
p_size0 <- 5

My_Theme_EI =  theme(
  axis.title.x = element_text(size = text0),
  axis.text.x = element_text(size = text0, color = "black"),
  axis.title.y = element_text(size = text0),
  axis.text.y = element_text(size = text0, color="black"),
  axis.line.x = element_line(),
  axis.line.y = element_line(),
  panel.background = element_rect(fill =  "white"),
  panel.grid.major = element_blank(),
  legend.text = element_text(size = 13),
  title = element_text(size = 15)) 



## Splus vs Sminus
Temp_T <-Explo_Data_All_R
t.test(x=Temp_T$S_plus_medTTC, y= Temp_T$S_minus_medTTC, paired = T) #now using median, they still differ



Temp_T2 <- data.frame("TTC" = c(Temp_T$S_plus_medTTC,Temp_T$S_minus_medTTC),
                      "S_category" = NA)
Temp_T2$S_category[1:72] <- "S+"
Temp_T2$S_category[73:144] <- "S-"
#Temp_T2$S_category[173:258] <- "Total"

Temp_T2$S_category2[1:72] <- "S+"
Temp_T2$S_category2[73:144] <- "S-"
#Temp_T2$S_category2[173:258] <- NA

Temp_T2$S_category <- as.factor(Temp_T2$S_category)
Temp_T2$S_category2 <- as.factor(Temp_T2$S_category2)
#

S_plus_minus_boxplot1 <- ggplot(Temp_T2, aes(x=S_category, y=TTC)) + 
  geom_boxplot(color = "black", size=2.0, outlier.size = 2.8) +
  My_Theme_EI +
  ylab('Absolute Median TTC (cm)') +
  xlab("TTC Category") +
  scale_y_continuous(breaks = c(25,50,75,100)) +
  geom_pwc(method = "t_test", label = "p", y.position = 94, size = bp_size0, label.size = 6,
           aes(x=S_category2, y=TTC), inherit.aes = F, vjust = -0.15, method.args = list(paired=T))



S_plus_minus_boxplot1


#t_test(data = Temp_T2, TTC~S_category, paired = T)


This.Plot <- S_plus_minus_boxplot1
P.name <- deparse(substitute(S_plus_minus_boxplot1))
Describe <- paste("S_plus_minus_MEDIAN_boxplot1_with_total")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 9*600, width = 12*600,
  units = "px",
  dpi=600,bg = "white")   






### EI distribution

mean1 <- round(mean(Explo_Data_All_R$Explor_index_med),2)
sd1 <- round(sd(Explo_Data_All_R$Explor_index),2)


Dist.EI <- ggplot(data = Explo_Data_All_R) + 
  geom_histogram(aes(x=Explor_index_med),fill="darkgrey",color="black",size=h_size0) +
  xlab("Exploration Index") +
  ylab("Count")+
  My_Theme_EI +
  scale_x_continuous(limits = c(-0.1,0.52),breaks = c(0.0,0.1,0.2,0.3,0.4,0.5)) +
  theme(plot.title=element_text(hjust=0.5)) +
  annotate(geom = "text", x = 0.45, y = 6.3, label = paste0(mean1, "(",sd1, ")"), size = 7)
#annotate(geom = "text", x = 0,y = 9, label = "S- - S+\n----------\nS- + S+")
Dist.EI

This.Plot <-   Dist.EI
P.name <- deparse(substitute(Dist.EI ))
Describe <- "EI_DIST"

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 9*600, width = 15*600,
  units = "px",
  dpi=600,bg = "white")  



############ Quartile plot for EI ############
### Quartiles plot requires Quartile_Data dataframe 

# The explor index goes up in Q3 only compared to Q1
my_Q <- Quartile_Data %>%
  group_by(Quartile) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(Explor_index)),
    sd=sd(na.omit(Explor_index))
  )


q_color <- "black"
my_Q$Quartile<- as.numeric(my_Q$Quartile)
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

EI_Q_plot <- Q_plot_4_SD



This.Plot <- EI_Q_plot
P.name <- deparse(substitute(EI_Q_plot))
Describe <- paste("EI_Q_plot")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6, width = 8,
  units = "in",
  dpi=500,bg = "white")  




### EI by age scatter

X.cor <- Explo_Data_All_R$Explor_index_med
Explo_Data_All_R$age <- Explore_All$age
Y.cor <- Explo_Data_All_R$age 
Cor_obj <- cor.test(X.cor,Y.cor, use='complete.obs', method = 'pearson')
C.val <- round(Cor_obj$estimate,2)
P.val <- round(Cor_obj$p.value,5)

Scatter_EI<-  ggplot(data=Explo_Data_All_R, aes(y = Explor_index_med, x = age )) +
  geom_point(size=p_size0, color = "grey32") +
  My_Theme_EI  +
  annotate(geom = "text", x = 10.5, y = 0.0, label = paste("r =",C.val,"adj. p = 0.064"), size = 7) +
  xlab("Age") +
  ylab("Exploration Index") +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))
#+
#geom_smooth(method = "lm", se = F, color = "black", size = 2.5)
Scatter_EI



This.Plot <- Scatter_EI
P.name <- deparse(substitute(Scatter_EI))
Describe <- paste("Scatter_EI")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6, width = 8,
  units = "in",
  dpi=500,bg = "white")  




### Reward history plots
# Raw HHH, MMM, etc data by median: Reward_History_Data_All_median.csv
# Load that into Reward_Hx_Calc_V1.R line 188 and go from there


####### Read in Rwd Hx Data ###########
Rwd_Hx_Data <- read.csv(file = "C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Reward_History_Data_All.csv")


### Remove participants so that sample is typically developing only:
Remove_list <- list("D003", "D004", "D015","D016", "D017", "D009", "D011", 'D012', 'D014', 'D018',
                    'D007', 'D008', 'D010', 'D013', 'D001', 'D002')

Rwd_Hx_Data<- Rwd_Hx_Data[!(Rwd_Hx_Data$participant_id %in% Remove_list),]




Hx_data <- Rwd_Hx_Data


names(Hx_data)[names(Hx_data) == "HHH_TTC"] <- "HHH"
names(Hx_data)[names(Hx_data) == "MHH_TTC"] <- "MHH"
names(Hx_data)[names(Hx_data) == "HMH_TTC"] <- "HMH"
names(Hx_data)[names(Hx_data) == "MMH_TTC"] <- "MMH"
names(Hx_data)[names(Hx_data) == "HHM_TTC"] <- "HHM"
names(Hx_data)[names(Hx_data) == "MHM_TTC"] <- "MHM"
names(Hx_data)[names(Hx_data) == "HMM_TTC"] <- "HMM"
names(Hx_data)[names(Hx_data) == "MMM_TTC"] <- "MMM"


Hx_dataL <-  Hx_data %>%  gather(History, TTC, c(HHH,MHH,HMH,MMH,HHM,MHM,HMM,MMM), factor_key = T)



my_sum <- Hx_dataL %>%
  group_by(History) %>%
  summarise( 
    n=n(),
    mean=mean(na.omit(TTC)),
    sd=sd(na.omit(TTC))
  )

my_sum2 <- data.frame("H3" = c("S(n-2)",1,0,1,0,1,0,1,0),
                      "H2" = c("S(n-1)",1,1,0,0,1,1,0,0),
                      "H1" = c("S(n)",1,1,1,1,0,0,0,0),
                      "Mean" =c(NA,my_sum$mean),
                      "SD" = c(NA,my_sum$sd))


color1 <- "black"
Hx_plot2 <- ggplot(data =  my_sum2) +
  geom_point(size = 8,colour = color1, aes(y=Mean,x=seq_len(nrow(my_sum2)))) +
  geom_line(size = 2,colour = color1, aes(y=Mean,x=seq_len(nrow(my_sum2)))) +
  geom_errorbar(aes(x=seq_len(nrow(my_sum2)), ymin=Mean-SD, ymax=Mean+SD ), width=0.25,size=2,alpha=1,color=color1) +
  annotate(geom = "text", x = seq_len(nrow(my_sum2)), y = -8, label = my_sum2$H3, size = 8) +
  annotate(geom = "text", x = seq_len(nrow(my_sum2)), y = -14, label = my_sum2$H2, size = 8) +
  annotate(geom = "text", x = c(1.15,2,3,4,5,6,7,8,9), y = -20, label = my_sum2$H1, size = 8) +
  annotate(geom = "text", x = 5.2, y = -26, label = "Feedback History", size = 8) +
  coord_cartesian(ylim = c(-0, 80), expand = T, clip = "off", default = T) +
  theme_bw() +ylab("Absolute Median TTC (cm)") +
  theme(plot.margin = unit(c(1, 1, 7.15, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 24, colour = "black"),
        axis.text.y = element_text(size = 24, colour = "black"),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9)) 


Hx_plot2

This.Plot <- Hx_plot2
P.name <- deparse(substitute(Hx_plot2))
Describe <- paste("Hx_plot2")

ggsave(
  filename =
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/HMS_Review1/New_images/ ",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = 6, width = 8,
  units = "in",
  dpi=500,bg = "white")  





### Combine the exploration plots
#need Hx_plot2 and Hx_plot_age2,

EI_combo <-cowplot::plot_grid(S_plus_minus_boxplot1,
                              Dist.EI,
                              EI_Q_plot,
                              Scatter_EI,
                              Hx_plot2,
                              Hx_plot_age2,
                              labels = "AUTO",
                              label_size = 25,
                              vjust =-.02,
                              hjust = -.45,
                              scale = 1,
                              nrow = 3) +
  theme(plot.margin = unit(c(0.68,0,0,0.1), "cm")) 

#EI_combo

This.Plot <- EI_combo
P.name <- deparse(substitute(EI_combo))
Describe <- paste("Fig_2")
dpi = 1000

h0 <- 18.7 #20
w0 <- 13.5 #14

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
    paste0("C:/Users/jdkonrad/Box/Lab Initiation/Dissertation Planning/Code and Practice Data/AIM_4/Manuscript/Plots3/",P.name,"_",Describe,".png"),
  plot = This.Plot,
  height = h0*dpi, width = w0*dpi,
  units = "px",
  dpi=dpi,bg = "white")  






