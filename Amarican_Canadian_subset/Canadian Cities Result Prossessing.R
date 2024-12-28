
#Last modified by M.H.Kent 
#This script prossesses the bifurcation percentages of the canadian cities for preliminary results

#Delete previous information stored 
rm(list=ls(all=T))

#Import needed libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(plyr)
library(readr)
library(tidyr)
library(stringr)
library(pracma) 
library("data.table")
library(bit64)
library("berryFunctions")
library(ggplot2)

#Setting working dirrectory  

#Import file 
unmodified_data <- fread("C:/Users/miles/Desktop/MATH 499/Data/canadiancities_with prob_FM-12.csv") 
head(unmodified_data) #View the first five elements
#length(unmodified_data$STATION)


#DF for bifrucations
Prob_modified_data <- unmodified_data%>%select("STATION", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECCOND", "TZONE", "500", "-0", "-1", "-2", "-3")%>% #Keeping needed columns 
					     mutate('DATE_TIME' = make_datetime(year=YEAR, month=MONTH, day=DAY, hour=HOUR, min=MINUTE, sec=SECCOND, tz=TZONE))%>% #Creates date column
					     select("STATION", "DATE_TIME", "500", "-0", "-1", "-2", "-3") #Droping seperated date and time collumns
colnames(Prob_modified_data) <- c("STATION","DATE_TIME", "TEMP", "FOLD", "HOPH", "TRANS", "NUL") #Renaming column names to be more understandable
summary(Prob_modified_data)

#Notes from summary: 
#1) Hoph has a very high mean and median. 

#Distributions of bifurcations
table(cut_interval(sort(Prob_modified_data$FOLD), n=20)) #maybe analyse data from (.839,1.00]?
table(cut_interval(sort(Prob_modified_data$HOPH), n=20)) #maybe analyse data from (.949,1.00]? 
table(cut_interval(sort(Prob_modified_data$TRANS), n=20)) #maybe analyse data from (.822, 100]?
table(cut_interval(sort(Prob_modified_data$NUL), n=20)) #Make sure there is no overlap from (.500, 100)

barplot(table(cut_interval(sort(Prob_modified_data$FOLD), n=20)))
barplot(table(cut_interval(sort(Prob_modified_data$HOPH), n=20)))
barplot(table(cut_interval(sort(Prob_modified_data$TRANS), n=20)))
barplot(table(cut_interval(sort(Prob_modified_data$NUL), n=20)))

barplot(sort(Prob_modified_data$FOLD))
barplot(sort(Prob_modified_data$HOPH))
barplot(sort(Prob_modified_data$TRANS))
barplot(sort(Prob_modified_data$NUL))


#Analysing high probabilities
#############################

#Creating dataframes for each high probability
FOLD_ANAL <- Prob_modified_data%>%filter(FOLD > .9)  #base values .839
FOLD_ANAL
#HOPH_ANAL <- Prob_modified_data%>%filter(HOPH > .9)    #base values.9
#HOPH_ANAL
TRANS_ANAL <- Prob_modified_data%>%filter(TRANS > .9) #base values .822
TRANS_ANAL
NUL_ANAL <- Prob_modified_data%>%filter(NUL > .5)
NUL_ANAL

#Make csv of HOPH data for Binbing 
#write.csv(HOPH_ANAL, "C:/Users/miles/Desktop/HOPH_OVER_90.csv" , row.names=FALSE)

#Viewing DF and extracting timeseries with highest probability in date cluster

#Note:all first days are the first day the system is over 90%

FOLD_ANAL %>% tbl_df %>% print(n=length(FOLD_ANAL$STATION))
#Timeseries
#1) 
# 71123099999 2017-12-19 18:00:00 -19   0.910 #
#2) 
# 71123099999 2018-11-30 09:00:00 -22.4 0.976 #This is a part of a system of a few days# This system flickers back and forth. Plot the whole couple of months 
# 71123099999 2018-11-25 09:00:00 -14.5 0.939 #First day of the systm 
# 71123099999 2018-12-11 09:00:00 -28.8 0.900 #Last day of the system
#3) 
# 71265099999 2017-12-05 03:00:00  -0.2 0.933 #Highest
# 71265099999 2017-12-04 15:00:00   2.3 0.907 #First day of system 
# 71265099999 2017-12-12 09:00:00 -10.7 0.913 #Last day of the system
#4) 
# 71265099999 2019-11-18 00:00:00   3.8 0.914 #Highest
# 71265099999 2019-11-15 06:00:00  -7.5 0.930 #First day of the system
# 71265099999 2019-11-18 03:00:00   2.8 0.915 #Last day of system 
#5)  
#71627094792 2016-09-15 21:00:00   6.2 0.903 #Only day this system is over 90%
#6)
# 71801099999 2016-09-07 00:00:00   4.3 #First day 
# 71801099999 2016-09-13 06:00:00   1.1 #Last day
#7)
# 71801099999 2017-09-18 09:00:00  -0.7 0.907 #First day 
# 71801099999 2017-09-23 06:00:00  -1.2 0.906 #Gap around here 
# 71801099999 2017-10-02 06:00:00   4   0.940 #Last day 
#8)
# 71801099999 2020-11-24 06:00:00   0.9 0.937 #First day
# 71801099999 2020-12-02 06:00:00  -1.9 0.906 #Last Day 
#9)
# 71801099999 2021-11-06 03:00:00  -2.5 #First Day
# 71801099999 2021-11-10 00:00:00  -5.8 #Last Day 
10)
# 71877099999 2018-11-18 00:00:00   1   0.952 #First Day
# 71877099999 2018-11-29 06:00:00   1.6 0.947 #Last Day
#11)
# 71892099999 2021-09-16 09:00:00   7.2 0.904 #First Day
# 71892099999 2021-09-17 15:00:00   6.9 0.927 #Last Day 
#12) 
# 71892099999 2021-10-26 03:00:00   1.7 0.908 #First Day
# 71892099999 2021-11-08 06:00:00   1.2 0.940 #Last Day 
#13)
# 371896099999 2015-09-19 09:00:00   3   0.912 #First Day 
# 71896099999 2015-09-29 09:00:00  -4.8  0.902 #Last Day
# There is a gap in the middle of this one 
#14) 
# 71896099999 2016-09-20 12:00:00  -1.1 0.909 #Single mesurement over 90 
#15) 
# 71896099999 2017-12-15 12:00:00  -9.6 0.902 #Start 
# 71896099999 2017-12-16 15:00:00 -32   0.935 #End 
#View the last day in the analysys 
#16)
#71896099999 2021-08-19 00:00:00   2.2 0.910 #Start 
#71896099999 2021-08-20 06:00:00   3.3 0.917 #End 
#71896099999 2021-08-19 12:00:00  -0.3 0.940 #Highest precentage
#17)
#71936099999 2016-08-26 12:00:00   1.5 0.910 #Start 
#71936099999 2016-09-12 06:00:00  -0.3 0.921 #End 
#18)
#71936099999 2018-11-17 15:00:00 -19.1 #Start 
#71936099999 2018-11-19 03:00:00 -26.1 #End 
#19)
# 71936099999 2018-11-29 18:00:00 -35.2 #Start 
# 71936099999 2018-12-05 06:00:00 -32.8 #End 
#20)
# 71936099999 2021-08-14 12:00:00  -1.1 0.922 #Start 
#21)
# 71936099999 2021-10-01 06:00:00 -16.8 0.920 #Start 
# 71936099999 2021-10-03 18:00:00 -23.3 0.953 #End 
#


#Creating timeseries dataframe 
PROB_TS <- unmodified_data%>%mutate('DATE_TIME' = make_datetime(year=YEAR, month=MONTH, day=DAY, hour=HOUR, min=MINUTE, sec=SECCOND, tz=TZONE))%>% #Creates date column
				     select(-c("YEAR", "MONTH", "HOUR", "MINUTE", "SECCOND", "TZONE","-0", "-1", "-2", "-3")) #Gets rid of non needed rows
colnames(PROB_TS)[colnames(PROB_TS) == "500"] <- "TEMP" #Changes column name for filtering reasons
summary(PROB_TS)#
head(PROB_TS)


#FOLD: Sudden crash bifurcation 

#1) 71123099999 2018-11-30 09:00:00 -22.4 #
   #71123099999 2018-11-25 09:00:00 -14.5 0.939
FOLD_1 <- PROB_TS%>%filter(STATION == 71123099999 & TEMP == -14.5 & DAY == 25)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
FOLD_1
colnames(FOLD_1) <- NULL
rownames(Fold_1) <- NULL
fold_vec_1 <- c(FOLD_1[,1])
fold_vec_1

p_F1 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), fold_vec_1))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), fold_vec_1), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71123099999  END DATE_TIME: 2017-12-19 18:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_F1
#Note:this is Edmonton 

#2) 
#71936099999 2018-11-29 18:00:00 -35.2 #Start 
#71936099999 2018-12-05 06:00:00 -32.8 #End 
FOLD_2 <- PROB_TS%>%filter(STATION == 71936099999 & TEMP == -35.2 & DAY == 29)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
FOLD_2
colnames(FOLD_2) <- NULL
rownames(FOLD_2) <- NULL
fold_vec_2 <- c(FOLD_2[,1])
fold_vec_2

p_F2 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), fold_vec_2))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), fold_vec_2), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71936099999 DATE_TIME: 2018-12-05 06:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_F2

#It seems the algorith is picking up the reverse effect of the time series as well i.e way after it has happend 
#Will need to make new classifiers

#71896099999 2021-08-19 12:00:00  -0.3 0.940
FOLD_3 <- PROB_TS%>%filter(STATION == 71896099999 & TEMP == -0.3 & DAY == 19)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
FOLD_3
colnames(FOLD_3) <- NULL
rownames(FOLD_3) <- NULL
fold_vec_3 <- c(FOLD_3[,1])
fold_vec_3

p_F3 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), fold_vec_3))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), fold_vec_3), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71896099999 DATE_TIME: 2021-08-19 12:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_F3


#71892099999 2021-10-26 03:00:00   1.7 0.908
FOLD_4 <- PROB_TS%>%filter(STATION == 71892099999 & TEMP == -1.7 & DAY == 26)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
FOLD_4
colnames(FOLD_3) <- NULL
rownames(FOLD_3) <- NULL
fold_vec_4 <- c(FOLD_4[,1])
fold_vec_4

p_F4 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), fold_vec_4))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), fold_vec_4), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71892099999 DATE_TIME: 2021-10-26 03:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_F4

#71877099999 2018-11-29 06:00:00   1.6 0.947
FOLD_5 <- PROB_TS%>%filter(STATION == 71877099999 & TEMP == 1.6 & DAY == 29)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
FOLD_5
colnames(FOLD_5) <- NULL
rownames(FOLD_5) <- NULL
fold_vec_5 <- c(FOLD_5[,1])
fold_vec_5

p_F5 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), fold_vec_5))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), fold_vec_5), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71877099999 DATE_TIME: 2018-11-29 06:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_F5



#TRANSCRITICAL: change to constant

TRANS_ANAL %>% tbl_df %>% print(n=length(TRANS_ANAL$STATION))
#1) 
# 71896099999 2018-11-23 12:00:00 -11.1  0.902 #Start
# 71896099999 2018-11-26 12:00:00  -0.1  0.928 #End
# Note: Will want to look at the last one 
#2)
# 71896099999 2021-10-18 00:00:00  -3.2 0.903 #Start 
# 71896099999 2021-10-28 12:00:00  -8.5 0.913 #Finish 
# 71896099999 2021-10-22 12:00:00 -12.9 0.960 #Very high point 


#1)  #71896099999 2021-10-22 12:00:00 -12.9 0.960 
TRANS_1 <- PROB_TS%>%filter(STATION == 71896099999 & TEMP == -8.5 & DAY == 28)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
#TRANS_1
colnames(TRANS_1) <- NULL
rownames(TRANS_1) <- NULL
trans_vec_1 <- c(TRANS_1[,1])
trans_vec_1

p_T1 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), trans_vec_1))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), trans_vec_1), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71896099999  END DATE_TIME: 2021-10-22 12:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_T1

#71896099999 2018-11-23 12:00:00 -11.1 
#71896099999 2018-11-26 12:00:00  -0.1
TRANS_2 <- PROB_TS%>%filter(STATION == 71896099999 & TEMP == -.1 & DAY == 26)%>%
	    select(-c("STATION" ,"DATE_TIME", "Unnamed: 0", "DAY"))%>%
	    t() 
TRANS_2
colnames(TRANS_2) <- NULL
rownames(TRANS_2) <- NULL
trans_vec_2 <- c(TRANS_2[,1])
trans_vec_2

p_T2 <- ggplot()+
	  geom_line(aes(x=linspace(1,500,500), trans_vec_2))+ #Sets the line 
	  geom_point(aes(x=linspace(1,500,500), trans_vec_2), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Temprature (C)")+ #Sets x and y lable 
	  ggtitle("STATION: 71896099999  END DATE_TIME: 2018-11-26 12:00:00")+
	  theme_bw() #Sets bakground theme to black white
p_T2


#Create dataframe of timeseries
plot_ts <- as.data.frame(trans_vec_2, )
plot_ts

#Percent increases

perc_inc <- unmodified_data%>%filter(STATION == 71123099999 & YEAR)

head(perc_inc)
per_inc_1 <- perc_inc$"-0"[771:855]
tt_b_1 <- linspace(1, length(per_inc_1), length(per_inc_1))
plot(tt_b_1, per_inc_1)

pr_p_1 <- ggplot()+
	  geom_line(aes(x=tt_b_1, y=per_inc_1))+ #Sets the line 
	  geom_point(aes(x=tt_b_1, y=per_inc_1), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Chance of Bifurcation (.1*%)")+ #Sets x and y lable 
	  ggtitle("STATION: 71123099999 (Edmonton)")+
	  theme_bw() #Sets bakground theme to black white
pr_p_1


per_inc_2 <- perc_inc$"-0"[1241:1341]
tt_b_2 <- linspace(1, length(per_inc_2), length(per_inc_2))
plot(tt_b_2, per_inc_2)

pr_p_2 <- ggplot()+
	  geom_line(aes(x=tt_b_2, y=per_inc_2))+ #Sets the line 
	  geom_point(aes(x=tt_b_2, y=per_inc_2), col="RED")+ #Sets red points over the line 
	  xlab("Time (3-Hr gap)")+ylab("Chance of Bifurcation (.1*%)")+ #Sets x and y lable 
	  ggtitle("STATION: 71123099999 (Edmonton)")+
	  theme_bw() #Sets bakground theme to black white
pr_p_2







