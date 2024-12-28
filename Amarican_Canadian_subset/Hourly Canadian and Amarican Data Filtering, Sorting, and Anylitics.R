This script filters, orginizes, and obtains the anylitics for the Amarican and Canadian Hourly data

#Last modified by M. H. Kent March 22th, 2023

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

#Setting working dirrectory 
setwd("C:/Users/miles/Desktop/MATH 499/Data/hourlycanadianamaricancities") 

#Import dirrectory list
mydram = "C:/Users/miles/Desktop/MATH 499/Data/hourlycanadianamaricancities" #Set dirrectory as varible 
filesam <- c(list.files(path=mydram, pattern="*.csv", full.names=TRUE)) #extract a list of file dirrectories and specify the list as a vector 
filesam #Take a look 

#Importing and combining all CSV files
######################################

#Setting the initial condition for the loop
#data_outam <- fread(filesam[1], select = c("STATION", "DATE", "TMP", "WND", "DEW", "SLP", "REPORT_TYPE")) #Full data, will use later
data_outam <- fread(filesam[1], select = c("STATION", "DATE", "TMP", "REPORT_TYPE")) #Just for temprature bifurcation analysis
data_outam

#Importing and combining files into singular dataframe

for (i in 2:length(filesam)) { #Make sure files are sorted by ascending order in folder

print((i/length(filesam))*100) #Print percentage done

#print(filesam[i]) #View what is being read
amh_fsize <- file.info(filesam[i])$size #Calculate size of the file in that dirrectory
#print(day_fsize) 

if (amh_fsize == 244) { #Size of file with no data and error message 

#Dont upload/do nothing
print("Pass")

} else { #upload the file

#data_inam  <- data.frame(fread(filesam[i], select = c("STATION", "DATE", "TMP", "WND", "DEW", "SLP", "REPORT_TYPE"))) #Selects the exact collumns we need from the datasets and imports them into a dataframe
data_inam  <- data.frame(fread(filesam[i], select = c("STATION", "DATE", "TMP", "REPORT_TYPE"))) #Only for temprature data analysis
data_outam <- rbind(data_outam, data_inam) #Combines the incoming data with the data already imported 

} #End of if else statment

} #End of for loop

data_outam #"Ho my g-d, it woooorks!" 
summary(data_outam)

#Canadian cities (Using this bc both is too much data to prosses)
can_dat <- data_outam%>%filter(STATION %in% c(71892099999 ,71896099999, 71123099999, 71877099999, 71866099999, 71852099999, 71936099999, 71749099999, 71265099999, 71627094792, 71727099999, 71395099999, 71801099999))


#Filtering and Anylitics
########################

#Filtering dataframe
#filterd_dataam <-  data_outam 
filterd_dataam <- can_dat%>%  separate(TMP ,  c('TMP' , 'TQC'),                    sep = ',')%>% #Seperates quality control number from temprature
                              #separate(WND ,  c('ANG' , 'AQC', 'DIR', 'WSP', 'WQC'),   sep = ',')%>% #Seperates all variables in the WND column 
					#separate(DEW ,  c('DEW' , 'DQC'),                        sep = ',')%>% #Seperates quality control number from dewpoint
					#separate(SLP ,  c('SLP' , 'SQC'),                        sep = ',')%>% #Seperates quality control number from atmospheric pressure
					#separate(DATE,  c('DATE', 'TIME'),                      sep = ' ')%>% #Seperates date from time (not needed for now)
					mutate(TMP = as.numeric(TMP)/10, TQC = as.numeric(TQC), #Makes sure that all variables that matter are numerical variables
                                     #ANG = as.numeric(ANG),   AQC = as,numeric(AQC), #Keep as a character for not becuase code doesnt like the conversion of these two collumns for some reason
					       #WSP = as.numeric(WSP)/10, WQC = as.numeric(WQC),
						 #DEW = as.numeric(DEW)/10, DQC = as.numeric(DQC),
						 STATION =as.numeric(STATION),
						 DATE= ymd_hms(DATE), YEAR=year(DATE), MONTH=month(DATE), DAY=day(DATE), HOUR=hour(DATE), MINUTE=minute(DATE), SECOND=second(DATE), TZONE=tz(DATE))%>%  #Formats as a date and time
					filter(TQC == 1)#%>% #Only keeping data that passes all quality control for temprature
					#filter(WQC == 1)%>%
					#filter(DQC == 1)%>%
					#filter(SQC == 1)%>%
					#arrange(arrange((can_dat$DATE)))%>%
					#group_by(STATION) #Makes sure that data from each station are grouped together
filterd_dataam$DF_ELEMENT <- linspace(1, length(filterd_dataam$DATE),  length(filterd_dataam$DATE)) #Adds a collumn specifying the row number of each row. Note: Keep this line at the end of this dataframe filtering block			
summary(filterd_dataam) #"Nice, 2 for 2!"

#Extracting the time gaps of each catagory

am_cats  <- unique(filterd_dataam$REPORT_TYPE) #Gets a list of catagories
print(am_cats)
am_stats <- unique(filterd_dataam$STATION) #Gets a list of the stations
print(am_stats)

for (i in 1:length(am_cats)) {

print(am_cats[i]) #Shows the report me are looking at

am_cats_data <- filterd_dataam%>%filter(REPORT_TYPE == am_cats[i]) #Gets the filterd data
#print(summary(am_cats_data))

am_bl_ln <- rep(NA, length(am_cats_data$DF_ELEMENT)) #creates a blank vector to be appended into

for (j in 2:length(am_cats_data$DF_ELEMENT)) {

am_dftime <- as.numeric(difftime(am_cats_data$DATE[j], am_cats_data$DATE[j-1], units="hours")) #Calculates the difference in time between two dates
am_bl_ln[j-1] <- am_dftime #appends the difference in time to am_bl_ln

} #End of imbeded loop 

print(unique(am_bl_ln)) #Print the count

} #End of loop

#For now, keeping the DFs with whole number hour gaps
#dfs to keep: "SY-MT" "SY-SA" "FM-12"

#Creating a df that meets this criteria

filterd_dataam_whole <- filterd_dataam%>%filter(REPORT_TYPE %in% c("FM-12", "SY-SA", "SY-MT"))

#%>%
#						     arrange(DATE)%>% #Make sure date is in accending order
#						     group_by(STATION) #Make sure it is grouped by station
						     
#print(filterd_dataam_whole)
#unique(filterd_dataam_whole$REPORT_TYPE) #Making sure it filterd correctly

#Making sure difftime intact

am_bl_ln_whole <- rep(NA, length(filterd_dataam_whole$DF_ELEMENT))
print(am_bl_ln_whole)

for (j in 2:length(filterd_dataam_whole$DF_ELEMENT)) {

am_dftime <- as.numeric(difftime(filterd_dataam_whole$DATE[j], filterd_dataam_whole$DATE[j-1], units="hours")) #Calculates the difference in time between two dates
am_bl_ln_whole[j-1] <- am_dftime #appends the difference in time to am_bl_ln_whole

} #End of imbeded loop 

#Summary statistics of difftimes
table(am_bl_ln_whole)
unique(am_bl_ln_whole)
length(am_bl_ln_whole)
summary(am_bl_ln_whole)
 
#From inspection, we cam see that most of the mesurements are 3 hours apart for all df therefore this is what ill give binbing for now
#It is crucial to note that there is not enough date in any of the dataframes to make a time series of 1 hour a part data 


#Seeing if there is an overlap in types of mesurements per location 
filterd_dataam_whole_fm12 <- filterd_dataam_whole%>%filter(REPORT_TYPE == "FM-12")
filterd_dataam_whole_SY_SA <- filterd_dataam_whole%>%filter(REPORT_TYPE == "SY-SA")
filterd_dataam_whole_SY_MT <- filterd_dataam_whole%>%filter(REPORT_TYPE == "SY-MT")
unique(filterd_dataam_whole_fm12$STATION)
unique(filterd_dataam_whole_SY_SA$STATION)
unique(filterd_dataam_whole_SY_MT$STATION)
#From inspection, we see that all stations have all report types 

#Taking a look at the values of each and the length
sort(unique(filterd_dataam_whole_fm12$TMP))
length(filterd_dataam_whole_fm12$TMP)
sort(unique(filterd_dataam_whole_SY_SA$TMP))
length(filterd_dataam_whole_SY_SA$TMP)
sort(unique(filterd_dataam_whole_SY_MT$TMP))
length(filterd_dataam_whole_SY_MT$TMP)


#Creating Timeseries
####################

#Creating a black dataframe for 500 day time series to be appended to

thcol_nms <- c("STATION", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECCOND", "TZONE", linspace(1,500,500)) #Creates vector of specified column names 
Thour_Timeseries <- data.frame(matrix(ncol = length(thcol_nms), nrow = 1)) #Creating blank dataframe  
colnames(Thour_Timeseries) <- thcol_nms #Gives dataframe specified collumn names 
Thour_Timeseries #View blank dataframe

filterd_datath <- filterd_dataam_whole_SY_SA
filterd_datath$DF_ELEMENT <- linspace(1, length(filterd_datath$DATE),  length(filterd_datath$DATE))
print(filterd_datath$DF_ELEMENT)

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

if (th_dftime == 3 & j < 500) {

#Do nothing, keep counting

} else if (th_dftime > 3) { 

break #Break the loop if there is a gap in the time series that is smaller then wanted

} else if (th_dftime < 3) {

break #Break the loop if there is a gap in the time series

} else if (th_dftime ==3 & j == 500) { #If no gap by the end of the loop append timeseries
#Need to modify this so it only keeps gaps of three hours
#if dates repeat, then we should only keep 1 of the highest resolution 

THS_NAME   <- th_df_window$STATION[60] #Extracts the station number
THS_YEAR   <- th_df_window$YEAR[60] #Extracts end year of the 500 day time series
THS_MONTH  <- th_df_window$MONTH[60] #Extracts end month of the 500 day time series
THS_DAY    <- th_df_window$DAY[60] #Extracts end day of the 500 day time series
THS_HOUR   <- th_df_window$HOUR[60] #Extracts end hour of the 500 day time series
THS_MINUTE <- th_df_window$MINUTE[60] #Extracts end minute of the 500 day time series
THS_SECOND <- th_df_window$SECOND[60] #Extracts end seccond of the 500 day time series
THS_TZONE  <- th_df_window$TZONE[60] #Extracts the end time zone of the 500 day time series
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
length(Thour_Timeseries_Final$STATION)

write.csv(Thour_Timeseries_Final, "C:/Users/miles/Desktop/MATH 499/Data/canadiancities_SY_MT.csv")





