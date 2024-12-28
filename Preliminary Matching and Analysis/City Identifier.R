#This script matches our three hundred cities to the climate norms

#Last modified by M.H.Kent August 16th, 2023

#Submitted to Narval August 16th, 2023


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


#Upload and Modify Total City List

tot_path <- "/home/mhkent/scratch/Climate_Norms/TotalCityList.txt"
#tot_path <- "C:/Users/miles/Desktop/ILLME Research/Heatwaves Project and MATH 499/Data/Climate_Norms/Total City List.txt"
tot_cities <- read.table(tot_path, header = FALSE, fill=TRUE)
#head(tot_cities) #View blank dataframe

tot_cities_imp <- data.frame(tot_cities)%>% #Making our initial file into a dataframe
			dplyr::select(V1, V2, V3, V4, V5, V6)%>% #Selecting only needed columns
			mutate(ID = as.character(V1), #Changing column names to something more meaningful 
                         LAT = as.numeric(V2),
				 LON = as.numeric(V3))%>%
                  unite(STAT, c("V5", "V6"))%>% #Combining two columns into the station name column
			dplyr::select(-V1, -V2, -V3, -V4)%>% #Getting rid of repreated columns
                  na.omit() #Removes all NA rows
#summary(tot_cities_imp) 


#Upload our normal city list

city_path <- "/home/mhkent/scratch/Climate_Norms/CityList.csv"
#city_path <- "C:/Users/miles/Desktop/ILLME Research/Heatwaves Project and MATH 499/Data/CityList.csv"
city_list <- read.csv(city_path)
#summary(city_list)


#Matching each of the three hundred stations

#Defining needed functions

#Calculates the geodesic distance between two points specified by radian latitude/longitude using the Haversine formula (hf)
gcd_hf <- function(long1, lat1, long2, lat2) {
	R <- 6371 # Earth mean radius [km]
	delta.long <- (long2 - long1) #need to be converted to radians first 
	delta.lat <- (lat2 - lat1)
	a <- (sin(delta.lat/2)^2) + (cos(lat1) * cos(lat2) * (sin(delta.long/2)^2))
	c <- 2 * asin(min(1,sqrt(a)))
 	d = R * c
	return(d) # Distance in km
}

#Convert degrees to radians
deg2rad <- function(deg) { 
return(deg*pi/180)
}

#Setting up parallel
#numCores <- detectCores() #Defining what we got in this bad boy
#numCores <- 1
#print(numCores) #Seeing how many cores we got 
#registerDoParallel(numCores) 


#Creating a blank table to be appended to

match_colnms <- c("city_stat", "city_lat", "city_long", "tot_stat", "LAT", "LOG", "DISTN", "ID")
match_df <- data.frame(matrix(,nrow = nrow(city_list), ncol = length(match_colnms)))
colnames(match_df) <- match_colnms
head(match_df)

#foreach (i=1:nrow(city_list), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate', 'doParallel', 'MASS')) %dopar% {

#city_matcher <- function(i) {

for (i in 1:nrow(city_list)) { 

print(i)

match_df[i, "city_stat"] <- city_list$STATION_NAME[i]
match_df[i, "city_lat"]  <- city_list$LAT[i]
match_df[i, "city_long"] <- city_list$LON[i]

#Creating a dataframe for distances
dist_colnm <- c("tot_stat", "LAT", "LON", "DIST", "ID")
dist_df <- data.frame(matrix(,nrow = nrow(tot_cities_imp) , ncol = length(dist_colnm)))
colnames(dist_df) <- dist_colnm
#head(dist_df)

summary(dist_df)

#foreach (j=1:nrow(tot_cities_imp), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate', 'doParallel', 'MASS')) %dopar% {

for (j in 1:nrow(tot_cities_imp)) { 

dist_df[j, "tot_stat"] <- tot_cities_imp$STAT[j]
dist_df[j, "LAT"]      <- tot_cities_imp$LAT[j]
dist_df[j, "LON"]      <- tot_cities_imp$LON[j]
dist_df[j, "ID"]      <- tot_cities_imp$ID[j]
dist_df[j, "DIST"]     <- gcd_hf(deg2rad(city_list$LON[i]), deg2rad(city_list$LAT[i]), deg2rad(tot_cities_imp$LON[j]), deg2rad(tot_cities_imp$LAT[j]))

} #End of imbeded loop 

dist_sorted <- dist_df%>%arrange(DIST) #Sort dataframe by order

match_df[i, "tot_stat"] <- dist_sorted$tot_stat[1]
match_df[i, "LAT"]      <- dist_sorted$LAT[1]
match_df[i, "LOG"]      <- dist_sorted$LON[1]
match_df[i, "ID"]      <- dist_sorted$ID[1]
match_df[i, "DISTN"]    <- dist_sorted$DIST[1]

} #End of for loop


#mclapply(1:nrow(city_list), city_matcher, mc.cores = numCores)

write.csv(match_df, "/home/mhkent/scratch/Climate_Norms/stat_match.txt")


