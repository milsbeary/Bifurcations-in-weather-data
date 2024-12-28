#THIS SCRIPT CONVERTS CLEAN SINGLE VARIABLE DATA INTO TIME SERIES FOR WIND 3HOUR GAPS

#Last modified by M. H. Kent August 4rd, 2023

#Submitted to Narval August 4rd, 2023


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


#Setting paths and dirrectory 

#tmp_path <- "C:/Users/miles/Documents/clean_wnd_dep"
tmp_path <- "/home/mhkent/scratch/clean_data/clean_wnd_dep"
tmp_files <- c(list.files(path=tmp_path, pattern="*.csv", full.names=TRUE))
tmp_files


#Specifying parallel inputs 

#numNodes <- makeCluster()
numCores <- detectCores() #Defining what we got in this bad boy
print(numCores) #Seeing how many cores we got 
registerDoParallel(numCores) 


#Specifying parameters 

#foreach (q=1:length(tmp_files), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate', 'pracma')) %dopar% { #Running the loop in parallel 

#for (q in 1:length(tmp_files)) { #Running the loop old school 

variable_generator_wnd <- function(q) { #If we want to define the prosses as a function

L = 500 #Specifying the time series length 
gap = 3 #Specifying the spaceing in hours

print(q)

tryCatch({df_ub <- data.frame(data.table::fread(tmp_files[q]))%>% #Import dataframe
					filter(AQC == 1)%>% #Forgot this in the datacleaning so doing it here 
					filter(WQC == 1) #Have this just in case anything slipped throught the cracks
#df_in <- data.frame(data.table::fread(tmp_files[18]))
#df_ub <- df_in[1:2000,] 

rep_types <- unique(df_ub$REPORT_TYPE) #Extracts report types 

for (m in 1:length(rep_types)) { 

#Creating a black dataframe for the time series to be appended to
thcol_nms <- c("STATION", "REPORT_TYPE", "LATITUDE", "LONGITUDE", "START_DATETIME", "END_DATETIME", "PARAMETER" , linspace(1,L,L)) #Creates vector of specified column names and length 
Thour_Timeseries <- data.frame(matrix(ncol = length(thcol_nms), nrow = 1)) #Creating blank dataframe  
colnames(Thour_Timeseries) <- thcol_nms #Gives dataframe specified collumn names 
Thour_Timeseries #View blank dataframe

df <- df_ub%>%filter(REPORT_TYPE == rep_types[m]) #Extracting our dataframe with our report type 
df_clnms <- colnames(df) #Extracts a list of column names 
#print(rep_types[m])

th_itnum <- length(df$DATE) #Extracts length of filterd dataset to specify number of needed itterations for for loop
#print(th_itnum) #Seeing how many itterations are needed

df$el_num <- linspace(1, th_itnum, th_itnum) #create a column that lables the row number

if (th_itnum < L) {

#Do nothing bc we cant make a time series out of this 

} else { #Run normal prosses

for (i in L:th_itnum) { #Start at L to accound for timeseries length

#print((i/th_itnum)*100) #Seeing the percent completed 

th_df_window <- df%>%filter(el_num >= i-(L-1), el_num <= i) #Creates a window to be analysed
#print(th_df_window) #Seeing if it works

for (j in 2:L) { #Goes through the L elements dataframe 

th_dftime <- as.numeric(difftime(th_df_window$DATE[j], th_df_window$DATE[j-1], units="hours")) #Calculates the difference in time between two dates
#print(th_dftime) #Seeing if it works

if (th_dftime == gap & j < L) {

#Do nothing, keep counting

} else if (th_dftime > gap & j < L) { 

break #Break the loop if there is a gap in the time series

} else if (th_dftime < gap & j < L) {

break #Break because there is non uniformity in the time series

} else if (th_dftime == gap & j == L) { #If length L timeseries is uniform, append timeseries

THS_NAME   <- unique(th_df_window$STATION) #Extracts the station number
THS_REPT   <- unique(th_df_window$REPORT_TYPE) #Extracts the report type
THS_LATI   <- unique(th_df_window$LATITUDE) #Extracts the station latitude 
THS_LONG   <- unique(th_df_window$LONGITUDE) #Extracts the station longitude 
THS_STAR   <- th_df_window$DATE[1] #Extracts the start date time for the time series 
THS_ENDT   <- th_df_window$DATE[L] #Extracts the end date time for the time series

THS_PRAMone   <- colnames(df)[7] #Selects the parameter we are using 
THT_TIMSone   <- th_df_window[1:L, 7] #Extracts the unbroken temprature time series
TH_APNDone <- c(as.numeric(THS_NAME), THS_REPT, THS_LATI, THS_LONG, as.character(THS_STAR), as.character(THS_ENDT), THS_PRAMone, THT_TIMSone) #Creates a vector to append  to "Thour_Timeseries"
Thour_Timeseries[nrow(Thour_Timeseries) + 1,] <- TH_APNDone #Appends the row to the dataframe 

THS_PRAMtwo   <- colnames(df)[9]
THT_TIMStwo   <- th_df_window[1:L, 9]
TH_APNDtwo <- c(as.numeric(THS_NAME), THS_REPT, THS_LATI, THS_LONG, as.character(THS_STAR), as.character(THS_ENDT), THS_PRAMtwo, THT_TIMStwo)
Thour_Timeseries[nrow(Thour_Timeseries) + 1,] <- TH_APNDtwo 

THS_PRAMthree   <- colnames(df)[10]
THT_TIMSthree   <- th_df_window[1:L, 10]
TH_APNDthree <- c(as.numeric(THS_NAME), THS_REPT, THS_LATI, THS_LONG, as.character(THS_STAR), as.character(THS_ENDT), THS_PRAMthree, THT_TIMSthree)
Thour_Timeseries[nrow(Thour_Timeseries) + 1,] <- TH_APNDthree 

} else {

break #If not conditions are met there must be an error so we will break the loop

} #End of nested if else statement

} #End of nested 4-loop

} #End of OG 4-loop

Thour_Timeseries_Final <- Thour_Timeseries %>% drop_na()#Removing the single NA row
#print(Thour_Timeseries_Final)

if (nrow(Thour_Timeseries_Final) < 1) {

#Do nothing, dont export blank dataframe

} else {

#Export CSV and put into the proper files 
write.csv(Thour_Timeseries_Final, paste("/home/mhkent/scratch/clean_timeseries/clean_wnd_ts_3/3hour_ts_", unique(df$STATION), rep_types[m], ".csv", sep = "")) #Writes a csv for the specified stations
#write.csv(Thour_Timeseries_Final, paste("C:/Users/miles/Desktop/test_dep/3hour_ts_", unique(df$STATION), rep_types[m],  ".csv", sep = "")) #Writes a csv for the specified stations

print(paste("csv written")) #Lets us know that a csv has been written

} #End of export loop

} #End of OG if else statment 

} #End of REPORT_TYPE 4-loop 

}, # End of first trycatch bracket 

error = function(e){ #Error function 

print(paste("Error in path:"))
print(tmp_files[q])

}, #End of error bracket

warning = function(w) { #Warning function

print(paste("Warning in path:"))
print(tmp_files[q])

}) #End of trycatch 

} #End of OG for loop (or function)							


#Applying the function over all dataframes
mclapply(1:length(tmp_files), variable_generator_wnd, mc.cores = numCores) 




