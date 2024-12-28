#This script matches our three hundred cities to the climate norms using both csv files

#Last modified by M.H.Kent August 22th, 2023


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


#Upload and Modify Total City List

#tot_path <- "/home/mhkent/scratch/Climate_Norms/ghcnd_stations.csv"
tot_path <- "C:/Users/miles/Desktop/ILLME Research/Heatwaves Project and MATH 499/Data/Climate_Norms/ghcnd_stations.csv"
tot_cities <- data.frame(read.csv(tot_path, header = FALSE))
head(tot_cities) #View blank dataframe

tot_cities_imp <- tot_cities%>% #Making our initial file into a dataframe
			mutate(across(c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"), ~ifelse(.=="", NA, as.character(.))))
colnames(tot_cities_imp) <- c("stat_ID", "Latitude", "Longitude", "Elevation", "State", "Stat_Name", "Code", "USAF_5")
summary(tot_cities_imp) 


#Upload the list of our stations

#city_path <- "/home/mhkent/scratch/Climate_Norms/CityList.csv"
city_path <- "C:/Users/miles/Desktop/ILLME Research/Heatwaves Project and MATH 499/Data/CityList.csv"
city_list <- data.frame(read.csv(city_path))
summary(city_list)


#Uploading previous matched stations
match_path <- "C:/Users/miles/Desktop/ILLME Research/Heatwaves Project and MATH 499/Data/Climate_Norms/stat_match.csv"
matched_cities <- data.frame(read.csv(match_path))
summary(matched_cities)
head(matched_cities)

#Matching stations 

#Creating blank dataframe for basic match
match_colnms <- c("city_stat", "USAF", "tot_stat", "USAF_5", "stat_ID", "Dist")
match_df <- data.frame(matrix(,nrow = nrow(city_list), ncol = length(match_colnms)))
colnames(match_df) <- match_colnms
head(match_df)

#Creating blank dataframe for double cities

match_doub_colnms <- c("city_stat", "USAF", "tot_stat", "USAF_5", "stat_ID", "Dist")
match_doub_df <- data.frame(matrix(,nrow = 1, ncol = length(match_colnms)))
colnames(match_doub_df) <- match_colnms
head(match_doub_df)

for (i in 1:nrow(city_list)) {

city_stprms <- city_list%>%filter(USAF == USAF[i]) #Extract the row we need

match_df[i, "city_stat"] <- city_stprms$STATION_NAME[1] #Import city station name

if (nchar(as.integer(city_stprms$USAF[1])) == 6) {

usaf_el5 <- as.numeric(substr(city_stprms$USAF[1], 1, 5))
#print(usaf_el5)
match_df[i, "USAF"] <- usaf_el5 

} else if (nchar(as.integer(city_stprms$USAF[1])) == 5) {

usaf_el5 <- as.numeric(substr(city_stprms$USAF[1], 1, 4))
#print(usaf_el5)
match_df[i, "USAF"] <- usaf_el5

} else {

print("error at", i)

} #End of if else 1

totcit_stati <- tot_cities_imp%>%filter(USAF_5 == usaf_el5)

if (nrow(totcit_stati) == 1) {

dist_to <- gcd_hf(deg2rad(city_stprms$LON[1]), deg2rad(city_stprms$LAT[1]), deg2rad(totcit_stati$Longitude[1]), deg2rad(totcit_stati$Latitude[1]))

match_df[i, "Dist"] <- dist_to
match_df[i, "tot_stat"] <- totcit_stati$Stat_Name[1]
match_df[i, "USAF_5"] <- totcit_stati$USAF_5[1]
match_df[i, "stat_ID"] <- totcit_stati$stat_ID[1]

} else if (nrow(totcit_stati) > 1) {

imp_doub_colnms <- c("city_stat", "USAF", "tot_stat", "USAF_5", "stat_ID", "Dist")
imp_doub_df <- data.frame(matrix(,nrow = nrow(totcit_stati), ncol = length(imp_doub_colnms)))
colnames(imp_doub_df) <- imp_doub_colnms
imp_doub_df

for (j in 1:nrow(totcit_stati)) { 

dist_to <- gcd_hf(deg2rad(city_stprms$LON[1]), deg2rad(city_stprms$LAT[1]), deg2rad(totcit_stati$Longitude[j]), deg2rad(totcit_stati$Latitude[j]))

imp_doub_df[j, "Dist"] <- dist_to
imp_doub_df[j, "tot_stat"] <- totcit_stati$Stat_Name[j]
imp_doub_df[j, "USAF_5"] <- totcit_stati$USAF_5[j]
imp_doub_df[j, "stat_ID"] <- totcit_stati$stat_ID[j]
imp_doub_df[j, "city_stat"] <- city_stprms$STATION_NAME[1]
imp_doub_df[j, "USAF"] <- usaf_el5

match_doub_df <- rbind(match_doub_df, imp_doub_df)

} #end of imbeded loop 

} else { 


} #End of if else 2


} #End of OG for loop 


match_df

#Figureing out which matches are good for singular matches

match_good_1 <- match_df%>%na.omit()%>%
				filter(Dist > 0)#%>%
				#filter(Dist >= 1)%>%
match_good_1

#Concern, missing a ISAF number with could result in everything being taken from different locations, for examples; one gets the airport, the other is the town

#If distance is very far and names are different, I will exclude, these rows are
#29 CABOSANLUCAS 76750 CHETUMAL INTL 76750 MXM00076750 2298.83050258
#48 MERIGNAC  7510 BORDEAUX-MERIGNAC   7510 FR000007510  110.98738124 #If these stations are this far apart, may be a problem getting proper norms
#54 PULKOVO 26063  ST. PETERSBURG  26063 RSM00026063   18.65079177 #This may be a different location that is too far apart
#59 TALAGI 22550 ARHANGELSK  22550 RSM00022550 11.14607323 #Could be two different locations
#62 KOLTSOVO 28440 EKATERINBURG 28440 RSM00028440 14.40318755
#66 TOLMACHEVO 29634 NOVOSIBIRSK 29634 RSM00029634 17.66875506
#69 KADALA 30758 CHITA  30758 RSM00030758 13.67280196
#71 SOKOL 25913 MAGADAN  25913 RSM00025913   40.29686843
#74 AKTAU 38001 FORT SHEVCHENKO 38001 KZ000038001  101.93596128
#102 LANSERIA 68263 PRETORIA/IRENE  68263 SFM00068263 29.20336504
#124 BEIJING-CAPITALINTERNATION 54511 BEIJING  54511 CHM00054511   30.47620923
#125 ZHOUSHUIZI 54662 DALIAN 54662 CHM00054662 10.95292936
#129 LONGJIA 54161 CHANGCHUN  54161 CHM00054161   38.64975372
#127 SANJIAZI 50745 QIQIHAR 50745 CHM00050745 15.90105324
#128 BAITA 53463 HOHHOT  53463 CHM00053463   12.45033795
#129 XINZHENG 57083 ZHENGZHOU  57083 CHM00057083   28.02371355
#130 LIUTING 54857 QINGDAO  54857 CHM00054857   22.43178877
#132 CHANGBEIINTL 58606 NANCHANG  58606 CHM00058606   29.51323951
#134 SANYAPHOENIXINTL 57494 WUHAN  57494 CHM00057494 1445.23017065
#135 LIANGJIANG 57957 GUILIN  57957 CHM00057957   29.19310318
#138 TIANHE 57494 WUHAN  57494 CHM00057494   25.43328579
#139 JIANGBEI 57516 CHONGQING  57516 CHM00057516   22.68640931
#140 SHUANGLIU 56294 CHENGDU  56294 CHM00056294   11.85807788
#143 DIWOPU 51463 WU LU MU QI  51463 CHM00051463   18.45857444
#151 MELBOURNEESSENDON 94864 COLDSTREAM  94864 ASN00086383   44.62771652
#155 AUCKLANDINTL 93119 KAITAIA  93119 NZ000093012  252.58938301


#Looking at double dataframe
match_doub_df <- match_doub_df%>%na.omit()
match_doub_df
#Most of hese are matched very well, would need to look into why there is 2 or more 


#Looking at dataframe with no matches by WBAN
no_match <- match_df%>%filter(is.na(tot_stat))%>%
			     subset(!(USAF %in% unique(match_doub_df$USAF)))
no_match


#Taking a look at previously matched airports to see if they have been matched

no_match_match <- matched_cities%>%subset(city_stat %in% no_match$city_stat)%>%
                                   filter(DISTN > 1)
no_match_match

#As you can see, many do not have matching monthy norms


