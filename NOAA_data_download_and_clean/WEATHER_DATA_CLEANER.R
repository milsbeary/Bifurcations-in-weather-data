#THIS SCRIPT CLEANS AND ORGANIZES ALL WEATHER DATA

#Job submitted on Narval August 1st, 2023

#Last modified by M. H. Kent August 1st, 2023


#Delete previous information stored 
rm(list=ls(all=T))

#Makes sure there is no scientific notation on outputs
options(scipen=999)

#Import needed libraries
library(tidyverse)
library(lubridate)
library(plyr)
library(pracma) #For linspace 
library(data.table) #For fread and fwrite
library(parallel)
library(doParallel)
library(MASS)

print("ONE")


###Setting script for parallel computing### 

numCores <- detectCores() #Defining what we got in this bad boy
#numCores <- 1
print(numCores) #Seeing how many cores we got 
registerDoParallel(numCores) 

print("TWO")


###Uploading Dirty Data### 

#Import dirrectory list #Will need to change in cluster
#mydramc = "C:/Users/miles/Desktop/ILLME Research/Heatwaves Project and MATH 499/Data/correcthourlycities"
mydramc = "/home/mhkent/scratch/correcthourlycities" #Set dirrectory as varible 
filesamc <- c(list.files(path=mydramc, pattern="*.csv", full.names=TRUE)) #extract a list of file dirrectories and specify the list as a vector 
#filesamc #Take a look 
 
#Getting file information and setting vectors
raw_amnt <- length(filesamc) #Getting the number of raw files
#raw_amnt <- 450 #Manual input
raw_amnt

print("THREE")


#Defining function to upolad data

dirty_uploader <- function(i) {

#print(filesam[i]) #View what is being read
amh_fsize <- file.info(filesamc[i])$size #Calculate size of the file in that dirrectory
#print(day_fsize) 

if (amh_fsize == 244) { #Size of file with no data and error message 

#Dont upload/do nothing
#print("Pass")

} else { #uploads the file and does base filtering

data_inamc  <- data.frame(data.table::fread(filesamc[i], select = c("NAME", "STATION", "DATE", "LATITUDE", "LONGITUDE", "TMP", "WND", "DEW", "SLP", "REPORT_TYPE")))%>%
					    separate(TMP ,  c('TMP' , 'TQC'),                   sep = ',')%>% #Seperates quality control number from mesurement
                                  separate(WND ,  c('ANG' , 'AQC', 'DIR', 'WSP', 'WQC'),   sep = ',')%>% 
					    separate(DEW ,  c('DEW' , 'DQC'),                        sep = ',')%>% 
					    separate(SLP ,  c('SLP' , 'SQC'),                        sep = ',')%>% 
					    mutate(
						     TMP = as.numeric(TMP)/10,       TQC = as.numeric(TQC), #Makes sure that all numerical variables are reccognised as such
                                         ANG = as.numeric(ANG),          AQC = as.numeric(AQC), #The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing
					           WSP = as.numeric(WSP)/10,       WQC = as.numeric(WQC),
						     DEW = as.numeric(DEW)/10,       DQC = as.numeric(DQC),
						     SLP = (as.numeric(SLP)/10)*100, SQC = as.numeric(SQC), #Pressure is converted from hectopascals to pascals. 
						     STATION = as.numeric(STATION),
						     LATITUDE = as.numeric(LATITUDE), 
						     LONGITUDE = as.numeric(LONGITUDE),
						     DATE = ymd_hms(DATE)) #Selects the exact collumns we need from the datasets and imports them into a dataframe

} #End of if else statment

} #End of function 

print("FOUR")


#Uploading in parallel
dirty_data <- foreach (i=1:raw_amnt, .combine='rbind', .multicombine=TRUE, .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% dirty_uploader(i)

print("FIVE")


#Extracting station information 

stat_nums <- unique(dirty_data$STATION) #Creates a vector of all stations for the loop 
print(length(stat_nums))

#stat_nums_df <- data.frame(stat_nums) #Create dataframe of statation numbers to be exported as a CSV

#write.csv(stat_nums_df, "/home/mhkent/scratch/stat_nums.csv") #Writes a csv that contains all stations
#write.csv(stat_nums_df, "C:/Users/miles/Desktop/stat_nums.csv") #Writes a csv for the specified stations

print("SIX")


###Breaking up and exporting csv files 

#Clean all parameters

all_prs_dfs <- function(i) {

cln_all_prs <- dirty_data %>% filter(STATION == stat_nums[i])%>%
					  distinct(.keep_all = TRUE)%>% #Eliminate repeating rows
                                filter(TQC == 1)%>% #Only keeping data that passes all quality control
					  filter(AQC == 1)%>%
					  filter(WQC == 1)%>%
					  filter(DQC == 1)%>%
					  filter(SQC == 1)%>%
					  arrange(DATE) #Aranges dates in ascending order


write.csv(cln_all_prs , paste("/home/mhkent/scratch/clean_allprs_dep/clean_allprs_", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified station and datatype 
#write.csv(cln_all_prs , paste("C:/Users/miles/Desktop/clean_allprs_dep", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified station and datatype 

#rm(cln_all_prs) #Clears after csv file has been ran

} #End of function

#Run in parallel 
#foreach (i=1:length(stat_nums), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% all_prs_dfs(i)

#Run standardly
for (i in 1:length(stat_nums)) { 
all_prs_dfs(i)
}

print("SEVEN")


#Clean temprature

tmp_dfs <- function(i) {

tmp_remove <- c("ANG", "AQC", "DIR", "WSP", "WQC", "DEW" , "DQC", "SLP" , "SQC") #Creates a vector of not needed rows

tmp_dat <-  dirty_data %>% filter(STATION == stat_nums[i])%>%
                           dplyr::select(-tmp_remove)%>%
				   distinct(.keep_all = TRUE)%>% #Eliminate repeating rows
				   filter(TQC == 1)%>%
			         arrange(DATE)

write.csv(tmp_dat, paste("/home/mhkent/scratch/clean_tmp_dep/clean_tmp_", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations
#write.csv(tmp_dat, paste("C:/Users/miles/Desktop/clean_tmp_dep", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations

#rm(tmp_dat)

} #End of function

#Run in parallel
#foreach (i=1:length(stat_nums), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% tmp_dfs(i)

#Run standardly
for (i in 1:length(stat_nums)) { 
tmp_dfs(i)
}

print("EIGHT")


#Clean pressure 

pre_dfs <- function(i) {

pre_remove <- c("ANG" , "AQC", "DIR", "WSP", "WQC", "DEW", "DQC", "TMP", "TQC") #Creates a vector of not needed rows

pre_dat <- dirty_data %>% filter(STATION == stat_nums[i])%>%
                          dplyr::select(-pre_remove)%>%
				  distinct(.keep_all = TRUE)%>% #Eliminate repeating rows
				  filter(SQC == 1)%>%
			        arrange(DATE)

write.csv(pre_dat, paste("/home/mhkent/scratch/clean_pre_dep/clean_pre_", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations
#write.csv(pre_dat, paste("C:/Users/miles/Desktop/clean_pre_dep", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations

#rm(pre_dat)

}  #End of function

#Run in parallel
#foreach (i=1:length(stat_nums), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% pre_dfs(i)

#Run standardly
for (i in 1:length(stat_nums)) { 
pre_dfs(i)
}

print("NINE")


#Clean dewpoint

dew_dfs <- function(i) {

dew_remove <- c("ANG" , "AQC", "DIR", "WSP", "WQC", "SLP", "SQC", "TMP", "TQC") #Creates a vector of not needed rows

dew_dat <- dirty_data %>% filter(STATION == stat_nums[i])%>%
                          dplyr::select(-dew_remove)%>%
				  #distinct(.keep_all = TRUE)%>% #Eliminate repeating rows
				  filter(DQC == 1)%>%
			        arrange(DATE)

write.csv(dew_dat, paste("/home/mhkent/scratch/clean_dew_dep/clean_dew_", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations
#write.csv(dew_dat, paste("C:/Users/miles/Desktop/clean_dew_dep", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations

#rm(dew_dat)

} #End of function

#Run in parallel
#foreach (i=1:length(stat_nums), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% dew_dfs(i)

#Run standardly
for (i in 1:length(stat_nums)) { 
dew_dfs(i)
}

print("TEN")


#Clean wind

wnd_dfs <- function(i) {

wnd_remove <- c("DEW", "DQC", "SLP", "SQC", "TMP", "TQC") #Creates a vector of not needed rows

wnd_dat <- dirty_data %>% filter(STATION == stat_nums[i])%>%
                          dplyr::select(-wnd_remove)%>%
				  distinct(.keep_all = TRUE)%>% #Eliminate repeating rows
				  filter(WQC == 1)%>%
			        arrange(DATE)

write.csv(wnd_dat, paste("/home/mhkent/scratch/clean_wnd_dep/clean_wnd_", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations
#write.csv(wnd_dat, paste("C:/Users/miles/Desktop/clean_wnd_dep", stat_nums[i], ".csv", sep = "")) #Writes a csv for the specified stations

#rm(wnd_dat)

}#End of function

#Run in parallel
#foreach (i=1:length(stat_nums), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% wnd_dfs(i)

#Run standardly
for (i in 1:length(stat_nums)) { 
wnd_dfs(i)
}

print("ELEVEN")

