#This script imports and filters all three hour weather data CSVs
#This script organizes usable three hour data into 500 element timeseries

#Last modified by M. H. Kent March 5th, 2023

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

print((i/length(filesth))*100) #Print percentage done

#print(filesth[i]) #View what is being read
hor_fsize <- file.info(filesth[i])$size #Calculate size of the file in that dirrectory
#print(day_fsize) 

if (hor_fsize == 244) { #Size of file with error message 

#Dont upload/do nothing
print("Pass")

} else { #upload the file

data_inth  <- data.frame(fread(filesth[i], select = c("STATION", "DATE", "TMP", "WND", "DEW", "SLP", "REPORT_TYPE"))) #Selects the exact collumns we need from the datasets and imports them into a dataframe
data_outth <- rbind(data_outth, data_inth) #Combines the incoming data with the data already imported 

} #End of if else statment

} #End of for loop

data_outth #"Ho my g-d, it woooorks!" 
#summary(data_outth)

#Organizing and filtering the data
filterd_datath <-  data_outth %>% separate(TMP ,  c('TMP' , 'TQC'),                    sep = ',')%>% #Seperates quality control number from temprature
                              separate(WND ,  c('ANG' , 'AQC', 'DIR', 'WSP', 'WQC'),   sep = ',')%>% #Seperates all variables in the WND column 
					separate(DEW ,  c('DEW' , 'DQC'),                        sep = ',')%>% #Seperates quality control number from dewpoint
					separate(SLP ,  c('SLP' , 'SQC'),                        sep = ',')%>% #Seperates quality control number from atmospheric pressure
					#separate(DATE,  c('DATE', 'TIME'),                      sep = ' ')%>% #Seperates date from time (not needed for now)
					mutate(TMP = as.numeric(TMP)/10, TQC = as.numeric(TQC), #Makes sure that all variables that matter are numerical variables
                                     #ANG = as.numeric(ANG),   AQC = as,numeric(AQC), #Keep as a character for not becuase code doesnt like the conversion of these two collumns for some reason
					       WSP = as.numeric(WSP)/10, WQC = as.numeric(WQC),
						 DEW = as.numeric(DEW)/10, DQC = as.numeric(DQC),
						 STATION =as.numeric(STATION),
						 DATE= ymd_hms(DATE), YEAR=year(DATE), MONTH=month(DATE), DAY=day(DATE), HOUR=hour(DATE), MINUTE=minute(DATE), SECOND=second(DATE), TZONE=tz(DATE))%>%  #Formats as a date and time
						 filter(TQC == 1)#%>% #Only keeping data that passes all quality control for temprature
					#filter(WQC == 1)%>%
					#filter(DQC == 1)%>%
					#filter(SQC == 1)
filterd_datath$DF_ELEMENT <- linspace(1, length(filterd_datath$DATE),  length(filterd_datath$DATE)) #Adds a collumn specifying the rown number of each row. Note: Keep this line at the end of this dataframe			
filterd_datath #"Nice, 2 for 2!"

#Report types that are good
#Useful link 
#https://geo.libretexts.org/Bookshelves/Meteorology_and_Climate_Science/Practical_Meteorology_(Stull)/09%3A_Weather_Reports_and_Map_Analysis/9.01%3A_Meteorological_Reports_and_Observations 
fm-12 #Land station wether reports 
fm-15 #Weather report from an airport
SAO
any of SY are good
S-S-A

#Get list of catigorical variable of report type to see what you actually have  




#Extracting usable 500 day timeseries and appending them into a dataframe 

#Creating a black dataframe for 500 day time series to be appended to
thcol_nms <- c("STATION", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECCOND", "TZONE", linspace(1,500,500)) #Creates vector of specified column names 
Thour_Timeseries <- data.frame(matrix(ncol = length(thcol_nms), nrow = 1)) #Creating blank dataframe  
colnames(Thour_Timeseries) <- thcol_nms #Gives dataframe specified collumn names 
Thour_Timeseries #View blank dataframe


#Appending unbroken 500 day timeseries into "Daily_Timeseries" dataframe

th_itnum <- length(filterd_datath$DATE) #Extracts length of filterd dataset to specify number of needed itterations for for loop
print(th_itnum) #Seeing how many itterations are needed

for (i in 500:th_itnum) { #Start at 500 to accound for timeseries length

print((i/th_itnum)*100) #Seeing the percent completed 

th_df_window <- filterd_datath%>%filter(filterd_datath$DF_ELEMENT >= i-499, filterd_datath$DF_ELEMENT <= i) #Creates a 500 element window to be analysed
#print(th_df_window$DF_ELEMENT) #Seeing if it works

for (j in 2:500) { #Goes through the 500 element dataframe 

th_dftime <- as.numeric(difftime(th_df_window$DATE[j], th_df_window$DATE[j-1], units="hours")) #Calculates the difference in time between two dates
#print(th_dftime) #Seeing if it works

if (th_dftime < 4 & j < 500) {

#Do nothing, keep counting

} else if (th_dftime >= 4) { 

break #Break the loop if there is a gap in the time series

} else if (th_dftime < 4 & j == 500) { #If no gap by the end of the loop append timeseries
#Need to modify this so it only keeps gaps of three hours
#if dates repeat, then we should only keep 1 of the highest resolution 



THS_NAME   <- th_df_window$STATION[1] #Extracts the station number
THS_YEAR   <- th_df_window$YEAR[1] #Extracts start year of the 500 day time series
THS_MONTH  <- th_df_window$MONTH[1] #Extracts start month of the 500 day time series
THS_DAY    <- th_df_window$DAY[1] #Extracts start day of the 500 day time series
THS_HOUR   <- th_df_window$HOUR[1] #Extracts start hour of the 500 day time series
THS_MINUTE <- th_df_window$MINUTE[1] #Extracts start minute of the 500 day time series
THS_SECOND <- th_df_window$SECOND[1] #Extracts start seccond of the 500 day time series
THS_TZONE  <- th_df_window$TZONE[1] #Extracts the time zone of the 500 day time series
THT_TIMS   <- th_df_window$TMP #Extracts the unbroken temprature time series
THT_TIMS

TH_APND <- c(THS_NAME, THS_YEAR, THS_MONTH, THS_DAY, THS_HOUR, THS_MINUTE, THS_SECOND, THS_TZONE,  THT_TIMS) #Creates a vector to append  to "Thour_Timeseries"

Thour_Timeseries[nrow(Thour_Timeseries) + 1,] <- TH_APND #Appends the row to the daily timeseries 
print("Appended timeseries") #Shows that a time series has been appended to Thour_Timeseries 


} else {

break #If not conditions are met there must be an error so we will break the loop

} #End of if else loop

} #End of nested 4-loop

} #End of OG 4-loop

Thour_Timeseries_Final <- Thour_Timeseries%>%drop_na()#%>% #Removes row of NAs
				                  #mutate(DATE = make_date(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND)) #creates a singular date column 
								

summary(Thour_Timeseries_Final) #View dataframe of time series
print(Thour_Timeseries_Final)
head(Thour_Timeseries_Final)



