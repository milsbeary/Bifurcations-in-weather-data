#This script gives a picture and discription of the hourly data.
#The main purpose is to obtain the information nessisary to split the data into 500 and 1500 timeseries.

#Last modified by M. H. Kent March 19th, 2023

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
library(ggplot2)
library(pracma) 
library("data.table")
library(bit64)
library("berryFunctions")

#Setting working dirrectory 
setwd("C:/Users/miles/Desktop/MATH 499/Data/correcthourlycities") 

#Import dirrectory list
mydrth = "C:/Users/miles/Desktop/MATH 499/Data/correcthourlycities" #Set dirrectory as varible 
filesth <- c(list.files(path=mydrth, pattern="*.csv", full.names=TRUE)) #extract a list of file dirrectories and specify the list as a vector 
filesth #Take a look 

#Importing and combining all CSV files

#Setting the initial condition for the loop
data_outth <- fread(filesth[1], select = c("STATION", "DATE", "TMP", "WND", "DEW", "SLP", "REPORT_TYPE"))
data_outth

#Importing and combining files into singular dataframe

for (i in 2:length(filesth)) { #Make sure files are sorted by ascending order in folder

#print((i/length(filesth))*100) #Print percentage done

#print(filesth[i]) #View what is being read
hor_fsize <- file.info(filesth[i])$size #Calculate size of the file in that dirrectory
#print(day_fsize) 

if (hor_fsize == 244) { #Size of file with no data and error message 

#Dont upload/do nothing
print("Pass")

} else { #upload the file

data_inth  <- data.frame(fread(filesth[i], select = c("STATION", "DATE", "TMP", "WND", "DEW", "SLP", "REPORT_TYPE"))) #Selects the exact collumns we need from the datasets and imports them into a dataframe
data_outth <- rbind(data_outth, data_inth) #Combines the incoming data with the data already imported 

} #End of if else statment

} #End of for loop

data_outth #"Ho my g-d, it woooorks!" 
summary(data_outth)


#Exports csv file
write.csv(data_outth, "C:/Users/miles/Desktop/MATH 499/Data/79.08652_Unfilterd_hourly_data.csv", row.names=TRUE)

#cats <- unique(data_outth$REPORT_TYPE) #Gets a list of catagories
#stations <- unique(data_outth$STATION)

#Code descriptions can be found with these resources
#1) World Meteorological Organization: 1995 (revised 2015): Manual on Codes. International Codes Vol. 1.1 Part A - Alphanumeric Codes. WMO-No. 306. 466 pages.
#2) Federal Meteorological Handbook No. 1 (Sept 2005): Surface Weather Observations and Reports. FCM-H1-2005.
#3) https://geo.libretexts.org/Bookshelves/Meteorology_and_Climate_Science/Practical_Meteorology_(Stull)/09%3A_Weather_Reports_and_Map_Analysis/9.01%3A_Meteorological_Reports_and_Observations

#List of catagories
# "FM-15" "FM-12" "SY-MT" "FM-16" "99999" "AUTO "
# [7] "SAO  " "SY-SA" "AERO " "SY-AE" "SAOSP" "FM-14"
#[13] "NSRDB" "SOD  " "SOM  " "MESOW" "SHEF " "SMARS"

#The catagories to include
# "FM-15" "FM-12" "FM-14" "AUTO" "MESOW" "SAO" "FM-16" "SMARS"
#Note:FM-16: An Aviation Selected Special Weather Report (SPECI) is an unscheduled report that happens when a suddend change in temprature is observed. May confirm heatwave or correct incorrect observations 
#Note: include supplimintary reporting because it often has the corrections of bad reports.
#Will need to filter by quality control before anything else
#Will need to keep the data that doesnt round if two are the same  

#The catagores to not include and why 
#No:NSRDB: This is just radiation mesurements. 
#No:99999: Literally means missing data
#No:SOD: This is the summary of the day so it is a daily mesurement
#No:MESOW: This is because this is a snow observation 
#No:SOM: summary of the month report so it is a monthly observation  

#Questionable Catagories
#Questionable bc we need to figure out how reports are being merged
#"SY-AE"
#"SY-MT"
#"SY-SA"
#"SHEF " This will probably be in a different format then the remainder. Need to check before including. 
 

#Taking a look at each catagory

#Dat_FMtw <- data_outth%>%filter(REPORT_TYPE == "FM-12")
#length(unique(Dat_FMtw$STATION))
#Dat_FMfo <- data_outth%>%filter(REPORT_TYPE == "FM-14") 
#Dat_FMfi <- data_outth%>%filter(REPORT_TYPE == "FM-15")
#Dat_SHEF <- data_outth%>%filter(REPORT_TYPE == "SHEF")

#"SHEF "




#Organizing and filtering the data
filterd_datath <-  data_outth %>% separate(TMP ,  c('TMP' , 'TQC'),                    sep = ',')


%>% #Seperates quality control number 
					    mutate(TMP = as.numeric(TMP)/10, TQC = as.numeric(TQC), DATE= ymd_hms(DATE))%>% #Makes sure that all variables that matter are numerical variables
					    filter(TQC == 1)



filterd_datath <-  data_outth %>% separate(TMP ,  c('TMP' , 'TQC'),                    sep = ',')%>% #Seperates quality control number from temprature
                              #separate(WND ,  c('ANG' , 'AQC', 'DIR', 'WSP', 'WQC'),   sep = ',')%>% #Seperates all variables in the WND column 
					#separate(DEW ,  c('DEW' , 'DQC'),                        sep = ',')%>% #Seperates quality control number from dewpoint
					#separate(SLP ,  c('SLP' , 'SQC'),                        sep = ',')%>% #Seperates quality control number from atmospheric pressure
					#separate(DATE,  c('DATE', 'TIME'),                      sep = ' ')%>% #Seperates date from time (not needed for now)
					mutate(TMP = as.numeric(TMP)/10, TQC = as.numeric(TQC), #Makes sure that all variables that matter are numerical variables
                                     #ANG = as.numeric(ANG),   AQC = as,numeric(AQC), #Keep as a character for not becuase code doesnt like the conversion of these two collumns for some reason
					       #WSP = as.numeric(WSP)/10, WQC = as.numeric(WQC),
						 #DEW = as.numeric(DEW)/10, DQC = as.numeric(DQC),
						 #STATION =as.numeric(STATION), REPORT_TYPE = as.factor(REPORT_TYPE),
						 DATE= ymd_hms(DATE), YEAR=year(DATE), MONTH=month(DATE), DAY=day(DATE), HOUR=hour(DATE), MINUTE=minute(DATE), SECOND=second(DATE), TZONE=tz(DATE))%>%  #Formats as a date and time
						 filter(TQC == 1)#%>% #Only keeping data that passes all quality control for temprature
					#filter(WQC == 1)%>%
					#filter(DQC == 1)%>%
					#filter(SQC == 1)
#filterd_datath$DF_ELEMENT <- linspace(1, length(filterd_datath$DATE),  length(filterd_datath$DATE)) #Adds a collumn specifying the rown number of each row. Note: Keep this line at the end of this dataframe			
filterd_datath #"Nice, 2 for 2!"






