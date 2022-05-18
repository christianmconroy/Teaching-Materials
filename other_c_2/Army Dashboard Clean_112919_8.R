################################### Data Clean up, interpolation, time series synthesis, and exploration for the unit and equipment level datasets and scenario generation for equipment, personnel, and units #####################################

setwd("~/ALD")

##### Load Packages 
library(R.utils)
#install.packages('dplyr')
library(dplyr)
library(jsonlite)
#install.packages('Amelia')
library(Amelia)
library(synthpop)
library(zoo)
library(ggplot2)
library(tabulizer)
#install.packages('RMySQL')
library(RMySQL)
library(stringr)

################################ Set up SQL Interaction ############################
con <- dbConnect(RMySQL::MySQL(), host = "bdvus10oaimys01.cubatn5jy5z4.us-gov-west-1.rds.amazonaws.com", port = 3306,
                 user = "christian.conroy", dbname='DLAB_ALD', password = "*w$fZ%cT56^Cq8r*")

dbListTables(con)

################################### Unit ######################################
########### Import Data 
demo_ald_unit <- jsonlite::stream_in(file("demo_ald_unit.json"), pagesize = 10000, verbose = TRUE, flatten = TRUE)
colnames(demo_ald_unit)

########### Fill in NAs through interpolation
#### Prep for Interpolation
names(demo_ald_unit) <- gsub("_source.", "", names(demo_ald_unit))
demo_ald_unit$idint <- 1:nrow(demo_ald_unit)
colnames(demo_ald_unit)# Drop non-numeric

#### Interpolation
demo_ald_unit_iterp_full <- amelia(demo_ald_unit, m = 1, noms=c("DAYS_TO_LAD"), idvars = c("_index","_type","_id","_score", "UIC","UNIT_NAME","HOME_STATION","STATION_STATE","STATION_CNTRY","STATION_COUNTRY", "E_DATE","COMPO_NUMBER","COMPO_NAME", "ASGMT","DP99", "DP99_TITLE","UIC4","COMPO","CONUS_OCONUS","X_docType","MACOM_UIC","MACOM_NAME","CAP_UIC", "FORCE_GROUP", "SRM_PHASE","LOCATION","LATITUDE","LONGITUDE", "C_LEVEL","P_LEVEL","R_LEVEL","S_LEVEL","T_LEVEL","MODULAR_TYPE","ARFORGEN_DISPLAY","LOCATION_P.lat","LOCATION_P.lon"))

## Post-Interpolation Processing
demo_ald_unit_iterp_full$imputations$imp1$ASSIGNED_PRSNL <- round((abs(demo_ald_unit_iterp_full$imputations$imp1$ASSIGNED_PRSNL) + 10),0)
demo_ald_unit_iterp_full$imputations$imp1$AUTHORIZED_PRSNL <- round((abs(demo_ald_unit_iterp_full$imputations$imp1$AUTHORIZED_PRSNL) + 10),0)
demo_ald_unit_iterp_full$imputations$imp1$PROJECTED_PRSNL <- round((abs(demo_ald_unit_iterp_full$imputations$imp1$PROJECTED_PRSNL) + 10),0)

demo_ald_unit_iterp_full <- as.data.frame(demo_ald_unit_iterp_full$imputations$imp1)

demo_ald_unit_iterp_full$idint <- NULL

######### Updating the C, P, S, R, T (Different than interpolation to able to account for unique distribution where 1s are most common, 2s are less, and 3s are rare)

n = nrow(demo_ald_unit_iterp_full)
demo_ald_unit_iterp_full$var = 1  #initialize all value to be "C or 1"
index = 1:n
indexa = sample(index,0.2*n)  #pick 20% index for "A"
indexb = sample(index[-indexa],0.3*n) #pick 30% index for "B" need to rule out the "A"s you already picked
demo_ald_unit_iterp_full$var[indexa] = 3 #assign "A" to df$var at indexa
demo_ald_unit_iterp_full$var[indexb] = 2 #assign "B" to df$var at indexb
#the rest 50% is "C or 1"
demo_ald_unit_iterp_full$P_LEVEL <- ifelse(is.na(demo_ald_unit_iterp_full$P_LEVEL), demo_ald_unit_iterp_full$var, demo_ald_unit_iterp_full$P_LEVEL)

n = nrow(demo_ald_unit_iterp_full)
demo_ald_unit_iterp_full$var = 1  #initialize all value to be "C or 1"
index = 1:n
indexa = sample(index,0.1*n)  #pick 20% index for "A"
indexb = sample(index[-indexa],0.2*n) #pick 30% index for "B" need to rule out the "A"s you already picked
demo_ald_unit_iterp_full$var[indexa] = 3 #assign "A" to df$var at indexa
demo_ald_unit_iterp_full$var[indexb] = 2 #assign "B" to df$var at indexb
#the rest 50% is "C or 1"
demo_ald_unit_iterp_full$R_LEVEL <- ifelse(is.na(demo_ald_unit_iterp_full$R_LEVEL), demo_ald_unit_iterp_full$var, demo_ald_unit_iterp_full$R_LEVEL) 

n = nrow(demo_ald_unit_iterp_full)
demo_ald_unit_iterp_full$var = 1  #initialize all value to be "C or 1"
index = 1:n
indexa = sample(index,0.3*n)  #pick 20% index for "A"
indexb = sample(index[-indexa],0.4*n) #pick 30% index for "B" need to rule out the "A"s you already picked
demo_ald_unit_iterp_full$var[indexa] = 3 #assign "A" to df$var at indexa
demo_ald_unit_iterp_full$var[indexb] = 2 #assign "B" to df$var at indexb
#the rest 50% is "C or 1"
demo_ald_unit_iterp_full$S_LEVEL <- ifelse(is.na(demo_ald_unit_iterp_full$S_LEVEL), demo_ald_unit_iterp_full$var, demo_ald_unit_iterp_full$S_LEVEL) 

n = nrow(demo_ald_unit_iterp_full)
demo_ald_unit_iterp_full$var = 1  #initialize all value to be "C or 1"
index = 1:n
indexa = sample(index,0.3*n)  #pick 20% index for "A"
indexb = sample(index[-indexa],0.3*n) #pick 30% index for "B" need to rule out the "A"s you already picked
demo_ald_unit_iterp_full$var[indexa] = 3 #assign "A" to df$var at indexa
demo_ald_unit_iterp_full$var[indexb] = 2 #assign "B" to df$var at indexb

#the rest 50% is "C or 1"
demo_ald_unit_iterp_full$T_LEVEL <- ifelse(is.na(demo_ald_unit_iterp_full$T_LEVEL), demo_ald_unit_iterp_full$var, demo_ald_unit_iterp_full$T_LEVEL)

n = nrow(demo_ald_unit_iterp_full)
demo_ald_unit_iterp_full$var = 1  #initialize all value to be "C or 1"
index = 1:n
indexa = sample(index,0.1*n)  #pick 20% index for "A"
indexb = sample(index[-indexa],0.2*n) #pick 30% index for "B" need to rule out the "A"s you already picked
demo_ald_unit_iterp_full$var[indexa] = 3 #assign "A" to df$var at indexa
demo_ald_unit_iterp_full$var[indexb] = 2 #assign "B" to df$var at indexb
#the rest 50% is "C or 1"

demo_ald_unit_iterp_full$C_LEVEL <- (as.numeric(demo_ald_unit_iterp_full$P_LEVEL) + as.numeric(demo_ald_unit_iterp_full$S_LEVEL) + as.numeric(demo_ald_unit_iterp_full$R_LEVEL) + as.numeric(demo_ald_unit_iterp_full$T_LEVEL))/ 4

demo_ald_unit_iterp_full$C_LEVEL <- as.character(demo_ald_unit_iterp_full$C_LEVEL)

demo_ald_unit_iterp_full$var <- NULL

# Check to make sure all worked
summary(as.numeric(demo_ald_unit_iterp_full$P_LEVEL))
summary(as.numeric(demo_ald_unit_iterp_full$S_LEVEL))
summary(as.numeric(demo_ald_unit_iterp_full$R_LEVEL))
summary(as.numeric(demo_ald_unit_iterp_full$T_LEVEL))
summary(as.numeric(demo_ald_unit_iterp_full$C_LEVEL))
colnames(demo_ald_unit_iterp_full)

sapply(demo_ald_unit_iterp_full, function(x) sum(is.na(x)))

############# Synthesize - Turn the data into a time series by creating duplicates of existing data with variability added in to show differences and trends over quarters

##### Separate information variables from time-sensitive variables
colnames(demo_ald_unit_iterp_full)
unit_forsynth <- demo_ald_unit_iterp_full[,c(5, 10, 25:27, 33:37)]
unit_info <- demo_ald_unit_iterp_full[,-c(10, 25:27, 33:37)]
colnames(unit_forsynth)
# Notes: E_Date
unit_forsynth[2:10] <- sapply(unit_forsynth[2:10], as.numeric)
##### Loop through to Synthesize and add dates
datelist <- list("2019-07-01", "2019-04-01", "2019-01-01", "2018-10-01", "2018-07-01", "2018-04-01", "2018-01-01", "2017-10-01", "2017-07-01", "2017-04-01", "2017-01-01", "2016-10-01", "2016-07-01", "2016-04-01", "2016-01-01")

synthfunc <- function(dl) {
  unit_forsynth$quarter <- dl
  synth <- syn(unit_forsynth, m=1, default.method = "norm
", visit.sequence = c(2:10))
  unit_synth <- full_join(unit_info, synth$syn, by = "UIC")
  print(unit_synth)
}

data <- lapply(datelist,synthfunc)
unit_ts <- do.call(rbind.data.frame, data)

unit_ts$year <- format(as.Date(unit_ts$quarter, format="%Y-%m-%d"),"%Y")

unit_ts$quartertext <- as.yearqtr(unit_ts$quarter, format = "%Y-%m-%d")

demo_ald_unit_iterp_full$quarter <- "2019-10-01"

demo_ald_unit_iterp_full$year <- format(as.Date(demo_ald_unit_iterp_full$quarter, format="%Y-%m-%d"),"%Y")

demo_ald_unit_iterp_full$quartertext <- as.yearqtr(demo_ald_unit_iterp_full$quarter, format = "%Y-%m-%d")

unit_ts <- rbind(demo_ald_unit_iterp_full, unit_ts)

##### Bring in new supplementary Data (More coordinates we were missing data for)
cocom <- read.csv("uic_to_location.csv", na.strings = c("NA", ""))
colnames(cocom)

cocom$lat2 <- as.numeric(as.character(cocom$lat2))
cocom$lon2 <- as.numeric(as.character(cocom$lon2))

unit_ts_lat <- full_join(unit_ts, cocom[,c(1, 5:6,12)], by = c("UIC"))

unit_ts_lat$LATITUDE <- ifelse(is.na(unit_ts_lat$LATITUDE), unit_ts_lat$lat2, ifelse(!is.na(unit_ts_lat$LATITUDE), unit_ts_lat$LATITUDE, NA)) 

unit_ts_lat$LOCATION_P.lat <- ifelse(is.na(unit_ts_lat$LOCATION_P.lat), unit_ts_lat$lat2, ifelse(!is.na(unit_ts_lat$LOCATION_P.lat), unit_ts_lat$LOCATION_P.lat, NA)) 

unit_ts_lat$LONGITUDE <- ifelse(is.na(unit_ts_lat$LONGITUDE), unit_ts_lat$lon2, ifelse(!is.na(unit_ts_lat$LONGITUDE), unit_ts_lat$LONGITUDE, NA)) 

unit_ts_lat$LOCATION_P.lon <- ifelse(is.na(unit_ts_lat$LOCATION_P.lon), unit_ts_lat$lon2, ifelse(!is.na(unit_ts_lat$LOCATION_P.lon), unit_ts_lat$LOCATION_P.lon, NA)) 

unit_ts_lat$lon2 <- NULL
unit_ts_lat$lat2 <- NULL
colnames(unit_ts_lat)

############## Explore results and add variance
unit_ts_quarter <- unit_ts_lat %>%
  group_by(quartertext) %>%
  summarise(assigned = sum(ASSIGNED_PRSNL),
            authorized = sum(AUTHORIZED_PRSNL),
            projected = sum(PROJECTED_PRSNL),
            plevel = mean(as.numeric(P_LEVEL)),
            slevel = mean(as.numeric(S_LEVEL)),
            rlevel = mean(as.numeric(R_LEVEL)),
            tlevel = mean(as.numeric(T_LEVEL)),
            clevel = mean(as.numeric(C_LEVEL)))

ggplot(unit_ts_quarter, aes(x = quartertext, y = clevel)) + geom_bar(stat="identity")

##### Readiness Scores 
unit_ts_lat$C_LEVEL <- as.numeric(unit_ts_lat$C_LEVEL)
unit_ts_lat$P_LEVEL <- as.numeric(unit_ts_lat$P_LEVEL)
unit_ts_lat$S_LEVEL <- as.numeric(unit_ts_lat$S_LEVEL)
unit_ts_lat$R_LEVEL <- as.numeric(unit_ts_lat$R_LEVEL)
unit_ts_lat$T_LEVEL <- as.numeric(unit_ts_lat$T_LEVEL)

unit_ts_lat$R_LEVEL <- ifelse(unit_ts_lat$R_LEVEL == 1 & unit_ts_lat$year == "2019" & unit_ts_lat$ASSIGNED_PRSNL <= 25, unit_ts_lat$R_LEVEL + 1, ifelse(unit_ts_lat$R_LEVEL == 2 & unit_ts_lat$year == "2018" & unit_ts_lat$ASSIGNED_PRSNL <= 25, unit_ts_lat$R_LEVEL - 1, unit_ts_lat$R_LEVEL))
unit_ts_lat$S_LEVEL <- ifelse(unit_ts_lat$S_LEVEL == 1 & unit_ts_lat$year == "2019" & unit_ts_lat$ASSIGNED_PRSNL <= 25, unit_ts_lat$S_LEVEL + 1, ifelse(unit_ts_lat$S_LEVEL == 2 & unit_ts_lat$year == "2018" & unit_ts_lat$ASSIGNED_PRSNL <= 25, unit_ts_lat$S_LEVEL - 1, unit_ts_lat$S_LEVEL))

unit_ts_lat$T_LEVEL <- ifelse(unit_ts_lat$quarter  == "2019-01-01" & unit_ts_lat$T_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=85, unit_ts_lat$T_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2019-04-01" & unit_ts_lat$T_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=90, unit_ts_lat$T_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2019-07-01" & unit_ts_lat$T_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=80, unit_ts_lat$T_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2019-10-01" & unit_ts_lat$T_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=75, unit_ts_lat$T_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2018-01-01" & unit_ts_lat$T_LEVEL == 3 & unit_ts_lat$ASSIGNED_PRSNL <= 20, unit_ts_lat$T_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2018-04-01" & unit_ts_lat$T_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL <= 20, unit_ts_lat$T_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2018-07-01" & unit_ts_lat$T_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL <= 15, unit_ts_lat$T_LEVEL + 1, ifelse(unit_ts_lat$quarter  == "2018-10-01" & unit_ts_lat$T_LEVEL == 3 & unit_ts_lat$ASSIGNED_PRSNL <= 15, unit_ts_lat$T_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-01-01" & unit_ts_lat$T_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 200, unit_ts_lat$T_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-04-01" & unit_ts_lat$T_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 150, unit_ts_lat$T_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-07-01" & unit_ts_lat$T_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 100, unit_ts_lat$T_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-10-01"  & unit_ts_lat$T_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 180, unit_ts_lat$T_LEVEL  - 1, unit_ts_lat$T_LEVEL))))))))))))

unit_ts_lat$P_LEVEL <- ifelse(unit_ts_lat$quarter  == "2019-01-01" & unit_ts_lat$P_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=122, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2019-04-01" & unit_ts_lat$P_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=90, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2019-07-01" & unit_ts_lat$P_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=80, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2019-10-01" & unit_ts_lat$P_LEVEL == 1 & unit_ts_lat$ASSIGNED_PRSNL >=90, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2018-01-01" & unit_ts_lat$P_LEVEL == 3 & unit_ts_lat$ASSIGNED_PRSNL <= 20, unit_ts_lat$P_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2018-04-01" & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL <= 15, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2018-07-01" & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL <= 15, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2018-10-01" & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL <= 10, unit_ts_lat$P_LEVEL  + 1, ifelse(unit_ts_lat$quarter  == "2017-01-01" & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 100, unit_ts_lat$P_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-04-01" & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 100, unit_ts_lat$P_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-07-01" & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 50, unit_ts_lat$P_LEVEL  - 1, ifelse(unit_ts_lat$quarter  == "2017-10-01"  & unit_ts_lat$P_LEVEL == 2 & unit_ts_lat$ASSIGNED_PRSNL >= 175, unit_ts_lat$P_LEVEL  - 1,unit_ts_lat$P_LEVEL))))))))))))


unit_ts_lat$C_LEVEL <- (as.numeric(unit_ts_lat$P_LEVEL) + as.numeric(unit_ts_lat$S_LEVEL) + as.numeric(unit_ts_lat$R_LEVEL) + as.numeric(unit_ts_lat$T_LEVEL))/ 4

##### Personnel Numbers 
# Assigned 
unit_ts_lat$ASSIGNED_PRSNL <- ifelse(unit_ts_lat$quarter  == "2019-01-01", unit_ts_lat$ASSIGNED_PRSNL * .85, ifelse(unit_ts_lat$quarter  == "2019-04-01", unit_ts_lat$ASSIGNED_PRSNL * .83, ifelse(unit_ts_lat$quarter  == "2019-07-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.81, ifelse(unit_ts_lat$quarter  == "2019-10-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.8, ifelse(unit_ts_lat$quarter  == "2018-01-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.89, ifelse(unit_ts_lat$quarter  == "2018-04-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.88, ifelse(unit_ts_lat$quarter  == "2018-07-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.87, ifelse(unit_ts_lat$quarter  == "2018-10-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.86, ifelse(unit_ts_lat$quarter  == "2017-01-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.95, ifelse(unit_ts_lat$quarter  == "2017-04-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.93, ifelse(unit_ts_lat$quarter  == "2017-07-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.91, ifelse(unit_ts_lat$quarter  == "2017-10-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.9, ifelse(unit_ts_lat$quarter  == "2016-01-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.99, ifelse(unit_ts_lat$quarter  == "2016-04-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.98, ifelse(unit_ts_lat$quarter  == "2016-07-01" , unit_ts_lat$ASSIGNED_PRSNL * 0.97, unit_ts_lat$ASSIGNED_PRSNL))))))))))))))) 

# Authorized
unit_ts_lat$AUTHORIZED_PRSNL <- ifelse(unit_ts_lat$quarter  == "2019-01-01", unit_ts_lat$AUTHORIZED_PRSNL * .85, ifelse(unit_ts_lat$quarter  == "2019-04-01", unit_ts_lat$AUTHORIZED_PRSNL * .83, ifelse(unit_ts_lat$quarter  == "2019-07-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.81, ifelse(unit_ts_lat$quarter  == "2019-10-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.8, ifelse(unit_ts_lat$quarter  == "2018-01-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.89, ifelse(unit_ts_lat$quarter  == "2018-04-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.88, ifelse(unit_ts_lat$quarter  == "2018-07-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.87, ifelse(unit_ts_lat$quarter  == "2018-10-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.86, ifelse(unit_ts_lat$quarter  == "2017-01-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.95, ifelse(unit_ts_lat$quarter  == "2017-04-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.93, ifelse(unit_ts_lat$quarter  == "2017-07-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.91, ifelse(unit_ts_lat$quarter  == "2017-10-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.9, ifelse(unit_ts_lat$quarter  == "2016-01-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.99, ifelse(unit_ts_lat$quarter  == "2016-04-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.98, ifelse(unit_ts_lat$quarter  == "2016-07-01" , unit_ts_lat$AUTHORIZED_PRSNL * 0.97, unit_ts_lat$AUTHORIZED_PRSNL))))))))))))))) 

# Projected
unit_ts_lat$PROJECTED_PRSNL <- ifelse(unit_ts_lat$quarter  == "2019-01-01", unit_ts_lat$PROJECTED_PRSNL * .85, ifelse(unit_ts_lat$quarter  == "2019-04-01", unit_ts_lat$PROJECTED_PRSNL * .83, ifelse(unit_ts_lat$quarter  == "2019-07-01" , unit_ts_lat$PROJECTED_PRSNL * 0.81, ifelse(unit_ts_lat$quarter  == "2019-10-01" , unit_ts_lat$PROJECTED_PRSNL * 0.8, ifelse(unit_ts_lat$quarter  == "2018-01-01" , unit_ts_lat$PROJECTED_PRSNL * 0.89, ifelse(unit_ts_lat$quarter  == "2018-04-01" , unit_ts_lat$PROJECTED_PRSNL * 0.88, ifelse(unit_ts_lat$quarter  == "2018-07-01" , unit_ts_lat$PROJECTED_PRSNL * 0.87, ifelse(unit_ts_lat$quarter  == "2018-10-01" , unit_ts_lat$PROJECTED_PRSNL * 0.86, ifelse(unit_ts_lat$quarter  == "2017-01-01" , unit_ts_lat$PROJECTED_PRSNL * 0.95, ifelse(unit_ts_lat$quarter  == "2017-04-01" , unit_ts_lat$PROJECTED_PRSNL * 0.93, ifelse(unit_ts_lat$quarter  == "2017-07-01" , unit_ts_lat$PROJECTED_PRSNL * 0.91, ifelse(unit_ts_lat$quarter  == "2017-10-01" , unit_ts_lat$PROJECTED_PRSNL * 0.9, ifelse(unit_ts_lat$quarter  == "2016-01-01" , unit_ts_lat$PROJECTED_PRSNL * 0.99, ifelse(unit_ts_lat$quarter  == "2016-04-01" , unit_ts_lat$PROJECTED_PRSNL * 0.98, ifelse(unit_ts_lat$quarter  == "2016-07-01" , unit_ts_lat$PROJECTED_PRSNL * 0.97, unit_ts_lat$PROJECTED_PRSNL))))))))))))))) 

class(unit_ts_lat$quarter)

unit_ts_lat$quarter <- as.Date(unit_ts_lat$quarter, "%Y-%m-%d")

unit_ts_quarter <- unit_ts_lat %>%
  group_by(quartertext) %>%
  summarise(assigned = sum(ASSIGNED_PRSNL),
            authorized = sum(AUTHORIZED_PRSNL),
            projected = sum(PROJECTED_PRSNL),
            plevel = mean(P_LEVEL),
            slevel = mean(S_LEVEL),
            rlevel = mean(R_LEVEL),
            tlevel = mean(T_LEVEL),
            clevel = mean(C_LEVEL))

# Check the results of added variance 
ggplot(unit_ts_quarter, aes(x = quartertext, y = assigned)) + geom_bar(stat="identity")

ggplot(unit_ts_quarter, aes(x = quartertext, y = projected)) + geom_bar(stat="identity")

ggplot(unit_ts_quarter, aes(x = quartertext, y = authorized)) + geom_bar(stat="identity")

unit_ts_lat_sql <- unit_ts_lat[,-c(1:4, 11, 12, 21)]
unit_ts_lat$quarter <- format(unit_ts_lat$quarter, "%m/%d/%Y")
class(unit_ts_lat$quarter)
RMySQL::dbWriteTable(con, "ALD_Unit", unit_ts_lat, overwrite = TRUE)

################################# Equipment #################################
demo_ald_equipment <- jsonlite::stream_in(file("demo_ald_equipment.json"), pagesize = 10000, verbose = TRUE, flatten = TRUE)

# Prep for Interpolation
sapply(demo_ald_equipment, function(x) sum(is.na(x)))
names(demo_ald_equipment) <- gsub("_source.", "", names(demo_ald_equipment))
demo_ald_equipment$idint <- 1:nrow(demo_ald_equipment)
colnames(demo_ald_equipment)# Drop non-numeric

bds <- matrix(c(29, 30, 0, 0, 20, 20), nrow = 2, ncol = 3)

demo_ald_equipment_iterp_full <- amelia(demo_ald_equipment, m = 1, noms=c(), idvars = c("_index","_type","_id", "_score","BASE","UIC_TITLE","UIC", "UICNAME", "LIN_SUB_FAMILY", "LIN_NOMENCLATURE", "COMPO","EQUIPMENT_NAME","NOMEN", "DIV_UIC","OPCON", "SNAME","ID", "X_docType","NMCS_CT","NMCM_CT", "DOWN", "HAS_DOWN","UP","FMC%", "LIN", "MAJOR_CAPABILITY","LIN_FAMILY"), bounds = bds)

demo_ald_equipment_iterp_full <- as.data.frame(demo_ald_equipment_iterp_full$imputations$imp1)

# Recalculate totals
demo_ald_equipment_iterp_full$NMCM_CT <- round(demo_ald_equipment_iterp_full$QTY_ON_HAND * (demo_ald_equipment_iterp_full$`NMCM%`/100), 0)
demo_ald_equipment_iterp_full$NMCS_CT <- round(demo_ald_equipment_iterp_full$QTY_ON_HAND * (demo_ald_equipment_iterp_full$`NMCS%`/100), 0)
demo_ald_equipment_iterp_full$DOWN <- demo_ald_equipment_iterp_full$NMCM_CT + demo_ald_equipment_iterp_full$NMCS_CT
demo_ald_equipment_iterp_full$UP <- demo_ald_equipment_iterp_full$QTY_ON_HAND - demo_ald_equipment_iterp_full$DOWN
demo_ald_equipment_iterp_full$`FMC%` <- 100 - (demo_ald_equipment_iterp_full$`NMCM%` + demo_ald_equipment_iterp_full$`NMCS%`)
demo_ald_equipment_iterp_full$HAS_DOWN <- (demo_ald_equipment_iterp_full$DOWN > 0)

demo_ald_equipment_iterp_full$idint <- NULL
colnames(demo_ald_equipment_iterp_full)

# Scrape equipment cost data from https://prhome.defense.gov/Portals/52/Documents/RFM/Readiness/docs/FY%202020%20NGRER.PDF?ver=2019-03-20-162550-750

# Get
# locate_areas("https://prhome.defense.gov/Portals/52/Documents/RFM/Readiness/docs/FY%202020%20NGRER.PDF?ver=2019-03-20-162550-750", pages = 61)

eqcosttable <- extract_tables("https://prhome.defense.gov/Portals/52/Documents/RFM/Readiness/docs/FY%202020%20NGRER.PDF?ver=2019-03-20-162550-750", pages = c(60:68, 111:116, 148:152, 180:181,213:214, 244, 269), output = "data.frame", area = list(c(92.04956,53.83019,713.03363,558.16981)), columns = list(c(43.67634, 277.38650, 322.8113,381.65094, 406.8679,  447.49528, 481.1179, 516.1415, 558.1698)), guess = FALSE)

colnames <- c("Index","Nomenclature","LIN","Eqcost", "FY2019_BEG", "FY2020_BEG", "FY2021_BEG", "FY2021_END", "FY2021_REQ") 
eqcosttable <- lapply(eqcosttable, setNames, colnames)

eqcosttable[[1]] <- eqcosttable[[1]][-c(1:9),]
eqcosttable[[2]] <- eqcosttable[[2]][-c(1:3),]
eqcosttable[[3]] <- eqcosttable[[3]][-c(1:3),]
eqcosttable[[4]] <- eqcosttable[[4]][-c(1:3),]
eqcosttable[[5]] <- eqcosttable[[5]][-c(1:3),]
eqcosttable[[6]] <- eqcosttable[[6]][-c(1:3),]
eqcosttable[[7]] <- eqcosttable[[7]][-c(1:3),]
eqcosttable[[8]] <- eqcosttable[[8]][-c(1:3),]
eqcosttable[[9]] <- eqcosttable[[9]][-c(1:3,15),]
eqcosttable[[10]] <- eqcosttable[[10]][-c(1:11),]
eqcosttable[[11]] <- eqcosttable[[11]][-c(1:5),]
eqcosttable[[12]] <- eqcosttable[[12]][-c(1:5),]
eqcosttable[[13]] <- eqcosttable[[13]][-c(1:5),]
eqcosttable[[14]] <- eqcosttable[[14]][-c(1:5),]
eqcosttable[[15]] <- eqcosttable[[15]][-c(1:5),]
eqcosttable[[16]] <- eqcosttable[[16]][-c(1:11),]
eqcosttable[[17]] <- eqcosttable[[17]][-c(1:5),]
eqcosttable[[18]] <- eqcosttable[[18]][-c(1:5),]
eqcosttable[[19]] <- eqcosttable[[19]][-c(1:5),]
eqcosttable[[20]] <- eqcosttable[[20]][-c(1:5, 17:20),]
eqcosttable[[21]] <- eqcosttable[[21]][-c(1:11),]
eqcosttable[[22]] <- eqcosttable[[22]][-c(1:5),]
eqcosttable[[23]] <- eqcosttable[[23]][-c(1:11),]
eqcosttable[[24]] <- eqcosttable[[24]][-c(1:5,10),]
eqcosttable[[25]] <- eqcosttable[[25]][-c(1:11),]
eqcosttable[[26]] <- eqcosttable[[26]][-c(1:5, 47),]

eqcosttable <- rbind(eqcosttable[[1]], eqcosttable[[2]], eqcosttable[[3]], eqcosttable[[4]], eqcosttable[[5]], eqcosttable[[6]], eqcosttable[[7]], eqcosttable[[8]], eqcosttable[[9]], eqcosttable[[10]], eqcosttable[[11]], eqcosttable[[12]], eqcosttable[[13]], eqcosttable[[14]], eqcosttable[[15]], eqcosttable[[16]],eqcosttable[[17]],eqcosttable[[18]],eqcosttable[[19]],eqcosttable[[20]],eqcosttable[[21]],eqcosttable[[22]],eqcosttable[[23]],eqcosttable[[24]],eqcosttable[[25]],eqcosttable[[26]])

eqcosttable$Index <- NULL
eqcosttable$Eqcost <-  as.numeric(gsub('[$]([0-9]+)[,]([0-9]+)','\\1\\2',eqcosttable$Eqcost))

# Get rid of duplicates 
eqcosttable <- eqcosttable[!duplicated(eqcosttable$LIN),]

# Merge Equipment Costs with existing unitequipment dataset
equipment_full <- left_join(demo_ald_equipment_iterp_full, eqcosttable, by = "LIN")
colnames(equipment_full)
sum(is.na(equipment_full$Eqcost))

# Take average for each lin 
uneq_fornas_2_LinFam <- equipment_full %>%
  filter(!is.na(equipment_full$Eqcost)) %>%
  group_by(LIN_FAMILY) %>%
  summarise(LFCost = mean(Eqcost))

equipment_full <- left_join(equipment_full, uneq_fornas_2_LinFam, by = "LIN_FAMILY")
sum(is.na(equipment_full$LFCost))

# Take Average for Each MC
uneq_fornas_2_MC <- equipment_full %>%
  filter(!is.na(equipment_full$Eqcost)) %>%
  group_by(MAJOR_CAPABILITY) %>%
  summarise(MCCost = mean(Eqcost))

equipment_full <- left_join(equipment_full, uneq_fornas_2_MC, by = "MAJOR_CAPABILITY")
sum(is.na(equipment_full$MCCost))

equipment_full <- equipment_full[,-c(33:37)]

##### Loop through to Synthesize and add dates
##### Separate information variables from time-sensitive variables
colnames(equipment_full)
equip_forsynth <- equipment_full[,c(3, 10, 29:30)]
equip_info <- equipment_full[,-c(10, 23:30, 33:34)]
colnames(equip_forsynth)
colnames(equip_info)

# Bring in inflation to alter equipment costs over time

inflation <- read.csv("inflation_long.csv", stringsAsFactors = FALSE)
View(inflation)

# Create the time series

datelist <- list("2019-07-01", "2019-04-01", "2019-01-01", "2018-10-01", "2018-07-01", "2018-04-01", "2018-01-01", "2017-10-01", "2017-07-01", "2017-04-01", "2017-01-01", "2016-10-01", "2016-07-01", "2016-04-01", "2016-01-01")

synthfunc <- function(dl) {
  equip_forsynth$quarter <- dl
  synth <- syn(equip_forsynth, m=1, default.method = "norm
", visit.sequence = c(2:4))
  syn <- synth$syn
  # Recalculate totals
  syn$NMCM_CT <- round(syn$QTY_ON_HAND * (syn$`NMCM%`/100), 0)
  syn$NMCS_CT <- round(syn$QTY_ON_HAND * (syn$`NMCS%`/100), 0)
  syn$DOWN <- syn$NMCM_CT + syn$NMCS_CT
  syn$UP <- syn$QTY_ON_HAND - syn$DOWN
  syn$`FMC%` <- 100 - (syn$`NMCM%` + syn$`NMCS%`)
  syn$HAS_DOWN <- (syn$DOWN > 0)
  syn$year <- format(as.Date(syn$quarter, format="%Y-%m-%d"),"%Y")
  syn$quartertext <- as.character(as.yearqtr(syn$quarter, format = "%Y-%m-%d"))
  equip_synth <- full_join(equip_info, syn, by = "_id")
  equip_synth <- left_join(equip_synth, inflation[,2:3], by = "quartertext")
  equip_synth$Eqcost <- equip_synth$Eqcost/(1+equip_synth$infrate)
  
  # Take average for each lin 
  uneq_fornas_2_LinFam <- equip_synth %>%
    filter(!is.na(equip_synth$Eqcost)) %>%
    group_by(LIN_FAMILY) %>%
    summarise(LFCost = mean(Eqcost))
  equip_synth <- left_join(equip_synth, uneq_fornas_2_LinFam, by = "LIN_FAMILY")
  
  # Take Average for Each MC
  uneq_fornas_2_MC <- equip_synth %>%
    filter(!is.na(equip_synth$Eqcost)) %>%
    group_by(MAJOR_CAPABILITY) %>%
    summarise(MCCost = mean(Eqcost))
  equip_synth  <- left_join(equip_synth, uneq_fornas_2_MC, by = "MAJOR_CAPABILITY")
  
  write.csv(equip_synth, paste0("equip_ts", gsub("-", "", dl), ".csv", collapse = ""))
}

lapply(datelist,synthfunc)

equipment_full$quarter <- "2019-10-01"

equipment_full$year <- format(as.Date(equipment_full$quarter, format="%Y-%m-%d"),"%Y")

equipment_full$quartertext <- as.yearqtr(equipment_full$quarter, format = "%Y-%m-%d")

equipment_full$quartertext <- as.character(equipment_full$quartertext)
equipment_full <- left_join(equipment_full, inflation[,2:3], by = "quartertext")
colnames(equipment_full)

# Combine annual data back together to get full set

######### 2019

equipment_full_2019q3 <- read.csv("equip_ts20190701.csv", stringsAsFactors = FALSE)
equipment_full_2019q2 <- read.csv("equip_ts20190401.csv", stringsAsFactors = FALSE)
equipment_full_2019q1 <- read.csv("equip_ts20190101.csv", stringsAsFactors = FALSE)

equipment_full_2019q1$X <- NULL
equipment_full_2019q2$X <- NULL
equipment_full_2019q3$X <- NULL

names(equipment_full_2019q1)[1:4] <- gsub("X", "", names(equipment_full_2019q1[,1:4]))
names(equipment_full_2019q2)[1:4] <- gsub("X", "", names(equipment_full_2019q2[,1:4]))
names(equipment_full_2019q3)[1:4] <- gsub("X", "", names(equipment_full_2019q3[,1:4]))
names(equipment_full_2019q1) <- gsub("[.]", "", names(equipment_full_2019q1))
names(equipment_full_2019q2) <- gsub("[.]", "", names(equipment_full_2019q2))
names(equipment_full_2019q3) <- gsub("[.]", "", names(equipment_full_2019q3))

names(equipment_full) <- gsub("%", "", names(equipment_full))

equipment_full <- equipment_full[,c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,31,32,10,29,30,35,24,23, 25, 27, 28,26,36,37,38,33,34)]

eqfu <- rbind(equipment_full_2019q1, equipment_full_2019q2, equipment_full_2019q3, equipment_full)

write.csv(eqfu, "equip_ts2019.csv")
rm(equipment_full_2019q1, equipment_full_2019q2, equipment_full_2019q3)

####### 2018

equipment_full_2018q1 <- read.csv("equip_ts20180701.csv", stringsAsFactors = FALSE)
equipment_full_2018q2 <- read.csv("equip_ts20180401.csv", stringsAsFactors = FALSE)
equipment_full_2018q3 <- read.csv("equip_ts20180101.csv", stringsAsFactors = FALSE)
equipment_full_2018q4 <- read.csv("equip_ts20181001.csv", stringsAsFactors = FALSE)

equipment_full_2018q1$X <- NULL
equipment_full_2018q2$X <- NULL
equipment_full_2018q3$X <- NULL
equipment_full_2018q4$X <- NULL

names(equipment_full_2018q1)[1:4] <- gsub("X", "", names(equipment_full_2018q1[,1:4]))
names(equipment_full_2018q2)[1:4] <- gsub("X", "", names(equipment_full_2018q2[,1:4]))
names(equipment_full_2018q3)[1:4] <- gsub("X", "", names(equipment_full_2018q3[,1:4]))
names(equipment_full_2018q4)[1:4] <- gsub("X", "", names(equipment_full_2018q4[,1:4]))

names(equipment_full_2018q1) <- gsub("[.]", "", names(equipment_full_2018q1))
names(equipment_full_2018q2) <- gsub("[.]", "", names(equipment_full_2018q2))
names(equipment_full_2018q3) <- gsub("[.]", "", names(equipment_full_2018q3))
names(equipment_full_2018q4) <- gsub("[.]", "", names(equipment_full_2018q4))

eqfu2018 <- rbind(equipment_full_2018q1, equipment_full_2018q2, equipment_full_2018q3, equipment_full_2018q4)

write.csv(eqfu2018, "equip_ts2018.csv")
rm(equipment_full_2018q1, equipment_full_2018q2, equipment_full_2018q3, equipment_full_2018q4)

####### 2017

equipment_full_2017q1 <- read.csv("equip_ts20170701.csv", stringsAsFactors = FALSE)
equipment_full_2017q2 <- read.csv("equip_ts20170401.csv", stringsAsFactors = FALSE)
equipment_full_2017q3 <- read.csv("equip_ts20170101.csv", stringsAsFactors = FALSE)
equipment_full_2017q4 <- read.csv("equip_ts20171001.csv", stringsAsFactors = FALSE)

equipment_full_2017q1$X <- NULL
equipment_full_2017q2$X <- NULL
equipment_full_2017q3$X <- NULL
equipment_full_2017q4$X <- NULL

names(equipment_full_2017q1)[1:4] <- gsub("X", "", names(equipment_full_2017q1[,1:4]))
names(equipment_full_2017q2)[1:4] <- gsub("X", "", names(equipment_full_2017q2[,1:4]))
names(equipment_full_2017q3)[1:4] <- gsub("X", "", names(equipment_full_2017q3[,1:4]))
names(equipment_full_2017q4)[1:4] <- gsub("X", "", names(equipment_full_2017q4[,1:4]))

names(equipment_full_2017q1) <- gsub("[.]", "", names(equipment_full_2017q1))
names(equipment_full_2017q2) <- gsub("[.]", "", names(equipment_full_2017q2))
names(equipment_full_2017q3) <- gsub("[.]", "", names(equipment_full_2017q3))
names(equipment_full_2017q4) <- gsub("[.]", "", names(equipment_full_2017q4))

eqfu2017 <- rbind(equipment_full_2017q1, equipment_full_2017q2, equipment_full_2017q3, equipment_full_2017q4)

write.csv(eqfu2017, "equip_ts2017.csv")
rm(equipment_full_2017q1, equipment_full_2017q2, equipment_full_2017q3, equipment_full_2017q4)

####### 2016

equipment_full_2016q1 <- read.csv("equip_ts20160701.csv", stringsAsFactors = FALSE)
equipment_full_2016q2 <- read.csv("equip_ts20160401.csv", stringsAsFactors = FALSE)
equipment_full_2016q3 <- read.csv("equip_ts20160101.csv", stringsAsFactors = FALSE)
equipment_full_2016q4 <- read.csv("equip_ts20161001.csv", stringsAsFactors = FALSE)

equipment_full_2016q1$X <- NULL
equipment_full_2016q2$X <- NULL
equipment_full_2016q3$X <- NULL
equipment_full_2016q4$X <- NULL

names(equipment_full_2016q1)[1:4] <- gsub("X", "", names(equipment_full_2016q1[,1:4]))
names(equipment_full_2016q2)[1:4] <- gsub("X", "", names(equipment_full_2016q2[,1:4]))
names(equipment_full_2016q3)[1:4] <- gsub("X", "", names(equipment_full_2016q3[,1:4]))
names(equipment_full_2016q4)[1:4] <- gsub("X", "", names(equipment_full_2016q4[,1:4]))

names(equipment_full_2016q1) <- gsub("[.]", "", names(equipment_full_2016q1))
names(equipment_full_2016q2) <- gsub("[.]", "", names(equipment_full_2016q2))
names(equipment_full_2016q3) <- gsub("[.]", "", names(equipment_full_2016q3))
names(equipment_full_2016q4) <- gsub("[.]", "", names(equipment_full_2016q4))

eqfu2016 <- rbind(equipment_full_2016q1, equipment_full_2016q2, equipment_full_2016q3, equipment_full_2016q4)

write.csv(eqfu2016, "equip_ts2016.csv")
rm(equipment_full_2016q1, equipment_full_2016q2, equipment_full_2016q3, equipment_full_2016q4)

equipment_full_all <- rbind (eqfu, eqfu2018, eqfu2017, eqfu2016)

# Bring in opcons
major <- read.csv("major_unit_names.csv")
colnames(major)

equipment_full_all <- left_join(equipment_full_all, major[,c(1, 4)], by = c("OPCON"))

colnames(equipment_full_all)

################### Explore and add variance where needed 


# Quantity 
equipment_full_all$QTY_ON_HAND <- ifelse(equipment_full_all$year == "2018", round((equipment_full_all$QTY_ON_HAND * 0.98), 0), ifelse(equipment_full_all$year == "2017", round((equipment_full_all$QTY_ON_HAND * 0.94), 0), ifelse(equipment_full_all$year == "2016", round((equipment_full_all$QTY_ON_HAND * 0.91), 0), equipment_full_all$QTY_ON_HAND)))

# Maintenance
equipment_full_all$NMCM <- ifelse(equipment_full_all$quarter == "2019-01-01", equipment_full_all$NMCM * 0.70, ifelse(equipment_full_all$quarter  == "2019-04-01", equipment_full_all$NMCM * 1.15, ifelse(equipment_full_all$quarter  == "2019-07-01", equipment_full_all$NMCM * 1.30, ifelse(equipment_full_all$quarter  == "2019-10-01", equipment_full_all$NMCM * 1.38, ifelse(equipment_full_all$quarter == "2018-01-01", equipment_full_all$NMCM * 0.75, ifelse(equipment_full_all$quarter  == "2018-04-01", equipment_full_all$NMCM * 0.90, ifelse(equipment_full_all$quarter  == "2018-07-01", equipment_full_all$NMCM * 1.02, ifelse(equipment_full_all$quarter  == "2018-10-01", equipment_full_all$NMCM * 1.05, ifelse(equipment_full_all$quarter  == "2017-10-01", equipment_full_all$NMCM * 0.65, ifelse(equipment_full_all$quarter  == "2017-07-01", equipment_full_all$NMCM * 0.88, ifelse(equipment_full_all$quarter  == "2017-04-01", equipment_full_all$NMCM * 1.05, ifelse(equipment_full_all$quarter  == "2017-01-01", equipment_full_all$NMCM * 0.84, ifelse(equipment_full_all$quarter  == "2016-01-01", equipment_full_all$NMCM * 1, ifelse(equipment_full_all$quarter  == "2016-04-01", equipment_full_all$NMCM * 0.80, ifelse(equipment_full_all$quarter  == "2016-07-01", equipment_full_all$NMCM * 0.68, ifelse(equipment_full_all$quarter  == "2016-10-01", equipment_full_all$NMCM * 0.72, equipment_full_all$NMCM))))))))))))))))



# Supply
equipment_full_all$NMCS <- ifelse(equipment_full_all$quarter == "2019-01-01", equipment_full_all$NMCS * 0.70, ifelse(equipment_full_all$quarter  == "2019-04-01", equipment_full_all$NMCS * 1.15, ifelse(equipment_full_all$quarter  == "2019-07-01", equipment_full_all$NMCS * 1.30, ifelse(equipment_full_all$quarter  == "2019-10-01", equipment_full_all$NMCS * 1.38, ifelse(equipment_full_all$quarter == "2018-01-01", equipment_full_all$NMCS * 0.75, ifelse(equipment_full_all$quarter  == "2018-04-01", equipment_full_all$NMCS * 0.90, ifelse(equipment_full_all$quarter  == "2018-07-01", equipment_full_all$NMCS * 1.02, ifelse(equipment_full_all$quarter  == "2018-10-01", equipment_full_all$NMCS * 1.05, ifelse(equipment_full_all$quarter  == "2017-10-01", equipment_full_all$NMCS * 0.65, ifelse(equipment_full_all$quarter  == "2017-07-01", equipment_full_all$NMCS * 0.88, ifelse(equipment_full_all$quarter  == "2017-04-01", equipment_full_all$NMCS * 1.05, ifelse(equipment_full_all$quarter  == "2017-01-01", equipment_full_all$NMCS * 0.84, ifelse(equipment_full_all$quarter  == "2016-01-01", equipment_full_all$NMCS * 1, ifelse(equipment_full_all$quarter  == "2016-04-01", equipment_full_all$NMCS * 0.80, ifelse(equipment_full_all$quarter  == "2016-07-01", equipment_full_all$NMCS * 0.68, ifelse(equipment_full_all$quarter  == "2016-10-01", equipment_full_all$NMCS * 0.72, equipment_full_all$NMCS))))))))))))))))

equipment_full_all$NMCM <- ifelse(equipment_full_all$NMCM > 100, 90, ifelse(equipment_full_all$NMCM < 0, 0, equipment_full_all$NMCM))
equipment_full_all$NMCS <- ifelse(equipment_full_all$NMCS > 100, 90, ifelse(equipment_full_all$NMCS < 0, 0, equipment_full_all$NMCS))

equipment_full_all$NMCS <- ifelse(equipment_full_all$NMCM + equipment_full_all$NMCS > 100, 0, equipment_full_all$NMCS)

sum((equipment_full_all$NMCM + equipment_full_all$NMCS > 100))


# Recalculate totals
equipment_full_all$NMCM_CT <- round(equipment_full_all$QTY_ON_HAND * (equipment_full_all$NMCM/100), 0)
equipment_full_all$NMCS_CT <- round(equipment_full_all$QTY_ON_HAND * (equipment_full_all$NMCS/100), 0)
equipment_full_all$DOWN <- equipment_full_all$NMCM_CT + equipment_full_all$NMCS_CT
equipment_full_all$UP <- equipment_full_all$QTY_ON_HAND - equipment_full_all$DOWN
equipment_full_all$FMC <- 100 - (equipment_full_all$NMCM + equipment_full_all$NMCS)
equipment_full_all$HAS_DOWN <- (equipment_full_all$DOWN > 0)

# Explore results of synthesis
equipment_full_all_quarter <- equipment_full_all %>%
  group_by(quarter) %>%
  summarise(FMC = mean(FMC),
            NMCS = mean(NMCS),
            NMCM = mean(NMCM),
            NMCM_CT = sum(NMCM_CT),
            NMCS_CT = sum(NMCS_CT),
            DOWN = sum(DOWN),
            UP = sum(UP),
            QTY_ON_HAND = sum(QTY_ON_HAND))

ggplot(equipment_full_all_quarter, aes(x = quarter, y = FMC)) + geom_bar(stat="identity")

# Create threshold totals after adding variance 

######## Create FMC, NMCM, NMCS Thresholds 
colnames(equipment_full_all)
equipment_full_all_thresholds <- equipment_full_all[,-c(36:44)]
###### FMC
#All
equipment_full_all_thresholds$fullfmcthresh <- mean(equipment_full_all$FMC)

#Major Cap 
uneq_fornas_2_MC <- equipment_full_all_thresholds  %>%
  filter(!is.na(equipment_full_all_thresholds$FMC)) %>%
  group_by(MAJOR_CAPABILITY) %>%
  summarise(mcfmcthresh = mean(FMC))

equipment_full_all_thresholds <- left_join(equipment_full_all_thresholds, uneq_fornas_2_MC, by = "MAJOR_CAPABILITY")

# Lin Family 
uneq_fornas_2_LinFam <- equipment_full_all_thresholds %>%
  filter(!is.na(equipment_full_all_thresholds$FMC)) %>%
  group_by(LIN_FAMILY) %>%
  summarise(lcfmcthresh = mean(FMC))

equipment_full_all_thresholds <- left_join(equipment_full_all_thresholds, uneq_fornas_2_LinFam, by = "LIN_FAMILY")

###### NMCM
#All
equipment_full_all_thresholds$fullnmcmthresh <- mean(equipment_full_all_thresholds$NMCM)

#Major Cap 
uneq_fornas_2_MC <- equipment_full_all_thresholds %>%
  filter(!is.na(equipment_full_all_thresholds$NMCM)) %>%
  group_by(MAJOR_CAPABILITY) %>%
  summarise(mcnmcmthresh = mean(NMCM))

equipment_full_all_thresholds <- left_join(equipment_full_all_thresholds, uneq_fornas_2_MC, by = "MAJOR_CAPABILITY")

# Lin Family 
uneq_fornas_2_LinFam <- equipment_full_all_thresholds %>%
  filter(!is.na(equipment_full_all_thresholds$NMCM)) %>%
  group_by(LIN_FAMILY) %>%
  summarise(lcnmcmthresh = mean(NMCM))

equipment_full_all_thresholds <- left_join(equipment_full_all_thresholds, uneq_fornas_2_LinFam, by = "LIN_FAMILY")

###### NMCS
#All
equipment_full_all_thresholds$fullnmcsthresh <- mean(equipment_full_all$NMCS)

#Major Cap 
uneq_fornas_2_MC <- equipment_full_all_thresholds %>%
  filter(!is.na(equipment_full_all_thresholds$NMCS)) %>%
  group_by(MAJOR_CAPABILITY) %>%
  summarise(mcnmcsthresh = mean(NMCS))

equipment_full_all_thresholds <- left_join(equipment_full_all_thresholds, uneq_fornas_2_MC, by = "MAJOR_CAPABILITY")

# Lin Family 
uneq_fornas_2_LinFam <- equipment_full_all_thresholds %>%
  filter(!is.na(equipment_full_all_thresholds$NMCS)) %>%
  group_by(LIN_FAMILY) %>%
  summarise(lcnmcsthresh = mean(NMCS))

equipment_full_all_thresholds <- left_join(equipment_full_all_thresholds, uneq_fornas_2_LinFam, by = "LIN_FAMILY")

colnames(equipment_full_all_thresholds)

fieldtypes <- c(row_names = "int(11)", LIN = "char(6)", BASE = "varchar(200)", UIC_TITLE = "varchar(255)", UIC = "char(6)", UICNAME = "varchar(255)", MAJOR_CAPABILITY = "varchar(200)", LIN_FAMILY = "varchar(255)", LIN_SUB_FAMILY = "varchar(255)", LIN_NOMENCLATURE = "varchar(255)", COMPO = "varchar(50)", EQUIPMENT_NAME = "varchar(255)", NOMEN = "varchar(255)", DIV_UIC = "char(6)", OPCON = "char(6)", SNAME = "varchar(50)", ID = "varchar(50)",Nomenclature = "varchar(255)", Eqcost = "decimal(10,2)", QTY_ON_HAND = "int(11)", NMCS = "decimal(10,2)", NMCM = "decimal(10,2)", quarter = "char(10)", NMCM_CT = "int(11)", NMCS_CT = "int(11)", DOWN = "int(11)", UP = "int(11)", FMC = "decimal(10,2)", HAS_DOWN = "int(11)", year = "int(11)", quartertext = "char(7)", infrate = "decimal(10,2)", LFCost = "decimal(15,2)", MCCost = "decimal(15,2)", OPCON_NAME = "varchar(100)", fullfmcthresh = "decimal(10,5)", mcfmcthresh = "decimal(10,5)", lcfmcthresh = "decimal(10,5)", fullnmcmthresh = "decimal(10,5)", mcnmcmthresh = "decimal(10,5)", lcnmcmthresh = "decimal(10,5)", mcnmcsthresh = "decimal(10,5)", lcnmcsthresh = "decimal(10,5)", fullnmcsthresh = "decimal(10,5)", Scenario = "varchar(25)")

RMySQL::dbWriteTable(con, "ALD_Equipment3", field.type = fieldtypes, equipment_full_all_thresholds, overwrite = TRUE)


################################## Threshold Table ###########################

######## Create PSRTC Thresholds 
unit_ts_lat_thresh <- unit_ts_lat[,c(5,33:37,42, 45)]
colnames(unit_ts_lat_thresh)
# All
unit_ts_lat_thresh$Pthresh <- mean(as.numeric(unit_ts_lat$P_LEVEL), na.rm = TRUE)
unit_ts_lat_thresh$Sthresh <- mean(as.numeric(unit_ts_lat$S_LEVEL), na.rm = TRUE)
unit_ts_lat_thresh$Rthresh <- mean(as.numeric(unit_ts_lat$R_LEVEL), na.rm = TRUE)
unit_ts_lat_thresh$Tthresh <- mean(as.numeric(unit_ts_lat$T_LEVEL), na.rm = TRUE)
unit_ts_lat_thresh$Cthresh <- mean(as.numeric(unit_ts_lat$C_LEVEL), na.rm = TRUE)

# By Opcon
Pthresh <- unit_ts_lat %>%
  filter(!is.na(unit_ts_lat$P_LEVEL)) %>%
  group_by(COCOM) %>%
  summarise(Pthresh_COCOM = mean(as.numeric(P_LEVEL)))

unit_ts_lat_thresh <- left_join(unit_ts_lat_thresh, Pthresh, by = "COCOM")

Sthresh <- unit_ts_lat %>%
  filter(!is.na(unit_ts_lat$S_LEVEL)) %>%
  group_by(COCOM) %>%
  summarise(Sthresh_COCOM = mean(as.numeric(S_LEVEL)))

unit_ts_lat_thresh <- left_join(unit_ts_lat_thresh, Sthresh, by = "COCOM")

Rthresh <- unit_ts_lat %>%
  filter(!is.na(unit_ts_lat$R_LEVEL)) %>%
  group_by(COCOM) %>%
  summarise(Rthresh_COCOM = mean(as.numeric(R_LEVEL)))

unit_ts_lat_thresh <- left_join(unit_ts_lat_thresh, Rthresh, by = "COCOM")

Tthresh <- unit_ts_lat %>%
  filter(!is.na(unit_ts_lat$T_LEVEL)) %>%
  group_by(COCOM) %>%
  summarise(Tthresh_COCOM = mean(as.numeric(T_LEVEL)))

unit_ts_lat_thresh <- left_join(unit_ts_lat_thresh, Tthresh, by = "COCOM")

Cthresh <- unit_ts_lat %>%
  filter(!is.na(unit_ts_lat$C_LEVEL)) %>%
  group_by(COCOM) %>%
  summarise(Cthresh_COCOM = mean(as.numeric(C_LEVEL)))

unit_ts_lat_thresh <- left_join(unit_ts_lat_thresh, Cthresh, by = "COCOM")

RMySQL::dbWriteTable(con, "ALD_Threshold_Unit", unit_ts_lat_thresh, overwrite = TRUE)

################################## Bring in Budget Info ##################
# Note this budget table is not used in the final product and is here for reference. We ended up manufacturing the budgets for the purpose of showing a better simulation. 
# Bring in budget info 
budget <- read.csv("Annual Spending Metrics.csv", na.strings = c("NA", ""))
budget$update_year <- as.character(budget$update_year)
colnames(budget)[1] <- c("year")
budget <- left_join(unit_ts_lat[,c(42:44)], budget, by = "year")
# Divide by 4 to reflect quarterly numbers 
budget$Budget..Millions. <- budget$Budget..Millions./4
budget$Personnel <- budget$Personnel/4
budget$Procurement <- budget$Procurement/4
budget$Maintenance <- budget$Maintenance/4
  
budget <- budget %>%
  group_by(quarter, quartertext, year) %>%
  summarize(budget = mean(Budget..Millions.),
            personnel = mean(Personnel),
            procurement = mean(Procurement),
            maintenance = mean(Maintenance))

budget$quartertext <- as.character(budget$quartertext)
budget <- as.data.frame(budget)

write.csv(budget, "updated_budget_quarters.csv")

RMySQL::dbWriteTable(con, "ALD_Budget", budget, overwrite = TRUE)

################################## Scenarios ######################## 
################ Scenario 1 - Sequestration ##########################
# To revert back for future scenario calculation
equipment_full_all_a <- equipment_full_all

# Subset to scenario impact year 
equipment_full_all <- equipment_full_all[equipment_full_all$year == 2019,]

##### Add in maintenance costs 
# Changing basic equipment costs based on major availability maintenance costs 
equipment_full_all$eqmaintcost <- ifelse(equipment_full_all$MAJOR_CAPABILITY == "SOLDIER WPNS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.14, ifelse(equipment_full_all$MAJOR_CAPABILITY == "SOLDIER SYSTEMS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.10, ifelse(equipment_full_all$MAJOR_CAPABILITY == "FIELD LOG" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.08, ifelse(equipment_full_all$MAJOR_CAPABILITY == "FORCE PROTECTION" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.15, ifelse(equipment_full_all$MAJOR_CAPABILITY == "MANEUVER CBT VEH" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.15, ifelse(equipment_full_all$MAJOR_CAPABILITY == "AIR DEFENSE" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.25, ifelse(equipment_full_all$MAJOR_CAPABILITY == "TRUCKS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.30, ifelse(equipment_full_all$MAJOR_CAPABILITY == "BC TRANSPORT NETWORKS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.20, ifelse(equipment_full_all$MAJOR_CAPABILITY == "BATTLE CMD C2" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.20, ifelse(equipment_full_all$MAJOR_CAPABILITY == "CBT MOBILITY" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.14, ifelse(equipment_full_all$MAJOR_CAPABILITY == "TRAILERS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.20, ifelse(equipment_full_all$MAJOR_CAPABILITY == "MEDICAL FIELD SYSTEMS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.15, ifelse(equipment_full_all$MAJOR_CAPABILITY == "AVIATION" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.25, ifelse(equipment_full_all$MAJOR_CAPABILITY == "GEN ENGINEERING" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.10, ifelse(equipment_full_all$MAJOR_CAPABILITY == "MANEUVER SYSTEMS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.15, ifelse(equipment_full_all$MAJOR_CAPABILITY == "SUPPORT SYSTEMS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.15, ifelse(equipment_full_all$MAJOR_CAPABILITY == "AIRCRAFT" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.30, ifelse(equipment_full_all$MAJOR_CAPABILITY == "BATTLESPACE AWARENESS" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.10, ifelse(equipment_full_all$MAJOR_CAPABILITY == "STRIKE" & !is.na(equipment_full_all$MCCost), equipment_full_all$MCCost * 0.15, ifelse(equipment_full_all$MAJOR_CAPABILITY == "OTHER SYSTEMS", equipment_full_all$MCCost * 0.15, NA))))))))))))))))))))

# Calculate total costs for supply and maintenenance by quarter
equipment_full_all_quarter_MCCost <- equipment_full_all %>%
  filter(!is.na(equipment_full_all$MCCost)) %>%
  group_by(quartertext) %>%
  summarise(TotalQEqCost = sum(MCCost), # Serves as equipment supply budget
            TotalEqMaintCost = sum(eqmaintcost)) # Serves as equipment maintenance budget

# Bring budgets back into equipment data frame
equipment_full_all <- left_join(equipment_full_all, equipment_full_all_quarter_MCCost, by = "quartertext")

# Calculate total cost per ID
equipment_full_all$MCCostTotal <- equipment_full_all$QTY_ON_HAND * equipment_full_all$MCCost
equipment_full_all$eqmaintcostTotal <- equipment_full_all$NMCM_CT * equipment_full_all$eqmaintcost

# Calculate percentage composition to total budget of each ID
equipment_full_all$MCCostTotalprop <- equipment_full_all$MCCostTotal/equipment_full_all$TotalQEqCost
equipment_full_all$eqmaintcostTotalprop <- equipment_full_all$eqmaintcostTotal/equipment_full_all$TotalEqMaintCost

# Factor in Sequestration hitting in 2019 
# Cost 
equipment_full_all$TotalQEqCost_Seq <- ifelse(equipment_full_all$quarter == "2019-01-01", equipment_full_all$TotalQEqCost * .90, ifelse(equipment_full_all$quarter == "2019-04-01", equipment_full_all$TotalQEqCost * .90, ifelse(equipment_full_all$quarter == "2019-07-01", equipment_full_all$TotalQEqCost * .90, ifelse(equipment_full_all$quarter == "2019-10-01", equipment_full_all$TotalQEqCost * .90, equipment_full_all$TotalQEqCost)))) 
# Maintenance   
equipment_full_all$TotalEqMaintCost_Seq <- ifelse(equipment_full_all$quarter == "2019-01-01", equipment_full_all$TotalEqMaintCost * .90, ifelse(equipment_full_all$quarter == "2019-04-01", equipment_full_all$TotalEqMaintCost * .90, ifelse(equipment_full_all$quarter == "2019-07-01", equipment_full_all$TotalEqMaintCost * .90, ifelse(equipment_full_all$quarter == "2019-10-01", equipment_full_all$TotalEqMaintCost * .90, equipment_full_all$TotalEqMaintCost)))) 

# Working backword to address impact on quantity and other numbers 
# Get total LIN Item costs with adjusted budget
equipment_full_all$MCCostTotal_seq <- equipment_full_all$TotalQEqCost_Seq * equipment_full_all$MCCostTotalprop

equipment_full_all$eqmaintcostTotal_seq <- equipment_full_all$TotalEqMaintCost_Seq * equipment_full_all$eqmaintcostTotalprop

# Get quantities by dividing new total costs by unit costs 
equipment_full_all$QTY_ON_HAND_seq <- round((equipment_full_all$MCCostTotal_seq/equipment_full_all$MCCost), 0)
equipment_full_all$NMCM_CT_seq <- ifelse(equipment_full_all$eqmaintcostTotal_seq >0, round((equipment_full_all$eqmaintcostTotal_seq/equipment_full_all$eqmaintcost),0), 0)

# Restore quantity for equipment for first period of sequestration back to original in order to simulate a forward impact (i.e. cuts in 2019-01-01 impact how much we can buy in 2019-04-01)

equipment_full_all$QTY_ON_HAND_seq <- ifelse(equipment_full_all$quarter == "2019-01-01", equipment_full_all$QTY_ON_HAND, equipment_full_all$QTY_ON_HAND_seq)

equipment_full_all$Scenario <- "Sequestration" 

equipment_full_all <- equipment_full_all[,-c(46:56)]
equipment_full_all <- equipment_full_all[,-c(20,24)]

names(equipment_full_all) <- gsub("_seq", "", names(equipment_full_all))

# Recalculate out key metrics 
# Recalculate totals
equipment_full_all$NMCM <- equipment_full_all$NMCM_CT/equipment_full_all$QTY_ON_HAND
equipment_full_all$NMCS_CT <- round(equipment_full_all$QTY_ON_HAND * (equipment_full_all$NMCS/100), 0)
equipment_full_all$DOWN <- equipment_full_all$NMCM_CT + equipment_full_all$NMCS_CT
equipment_full_all$UP <- equipment_full_all$QTY_ON_HAND - equipment_full_all$DOWN
equipment_full_all$FMC <- 100 - (equipment_full_all$NMCM + equipment_full_all$NMCS)
equipment_full_all$HAS_DOWN <- (equipment_full_all$DOWN > 0)

fieldtypes <- c(row_names = "int(11)", LIN = "char(6)", BASE = "varchar(200)", UIC_TITLE = "varchar(255)", UIC = "char(6)", UICNAME = "varchar(255)", MAJOR_CAPABILITY = "varchar(200)", LIN_FAMILY = "varchar(255)", LIN_SUB_FAMILY = "varchar(255)", LIN_NOMENCLATURE = "varchar(255)", COMPO = "varchar(50)", EQUIPMENT_NAME = "varchar(255)", NOMEN = "varchar(255)", DIV_UIC = "char(6)", OPCON = "char(6)", SNAME = "varchar(50)", ID = "varchar(50)",Nomenclature = "varchar(255)", Eqcost = "decimal(10,2)", QTY_ON_HAND = "int(11)", NMCS = "decimal(10,2)", NMCM = "decimal(10,2)", quarter = "char(10)", NMCM_CT = "int(11)", NMCS_CT = "int(11)", DOWN = "int(11)", UP = "int(11)", FMC = "decimal(10,2)", HAS_DOWN = "int(11)", year = "int(11)", quartertext = "char(7)", infrate = "decimal(10,2)", LFCost = "decimal(15,2)", MCCost = "decimal(15,2)", OPCON_NAME = "varchar(100)", fullfmcthresh = "decimal(10,5)", mcfmcthresh = "decimal(10,5)", lcfmcthresh = "decimal(10,5)", fullnmcmthresh = "decimal(10,5)", mcnmcmthresh = "decimal(10,5)", lcnmcmthresh = "decimal(10,5)", mcnmcsthresh = "decimal(10,5)", lcnmcsthresh = "decimal(10,5)", fullnmcsthresh = "decimal(10,5)", Scenario = "varchar(25)")

RMySQL::dbWriteTable(con, "ALD_Equipment_SequestrationScenario", equipment_full_all, field.type = fieldtypes, overwrite = TRUE)

#### Load in personnel sequestration impact
personnel_full_seq <- dbGetQuery(con,'
  select *
  from ALD_Personnel2
  WHERE year = 2019 AND Scenario = "Base"
')

unique(personnel_full_seq$quarter)

# Calculating impact of sequestration - by unit
personnel_full_seq_qp <- personnel_full_seq %>%
  filter(!is.na(personnel_full_seq$quarterly_pay)) %>%
  group_by(UIC, UPDATE_DT) %>%
  summarise(TotalSalary_uicqu = sum(quarterly_pay),
            Totalpeople_uicqu = n())

# Calculating impact of sequestration - by rank 
personnel_full_seq_qp_rank <- personnel_full_seq %>%
  filter(!is.na(personnel_full_seq$quarterly_pay)) %>%
  group_by(UIC, RANK_AB, UPDATE_DT) %>%
  summarise(TotalSalary_uicrankqu = sum(quarterly_pay),
            AvgSalary_uicrankqu = mean(quarterly_pay),
            Totalpeople_uicrankqu = n())

personnel_full_seq_qp_rank <- left_join(personnel_full_seq_qp_rank, personnel_full_seq_qp, by = c("UIC", "UPDATE_DT"))

# Calculate percentage each rank comprises of the unit-level budget and for a given quarter
personnel_full_seq_qp_rank$TotalSalary_rankofunit <- personnel_full_seq_qp_rank$TotalSalary_uicrankqu/personnel_full_seq_qp_rank$TotalSalary_uicqu
personnel_full_seq_qp_rank$Totalpeople_rankofunit <- personnel_full_seq_qp_rank$Totalpeople_uicrankqu/personnel_full_seq_qp_rank$Totalpeople_uicqu

# Factor in Sequestration hitting in 2019 
# Cost 
# Calculating impact of sequestration - by quarter
personnel_full_seq_quarter <- personnel_full_seq %>%
  filter(!is.na(personnel_full_seq$quarterly_pay)) %>%
  group_by(UPDATE_DT) %>%
  summarise(TotalSalary_qu = sum(quarterly_pay),
            Totalpeople_qu = n())

personnel_full_seq_qp_rank <- left_join(personnel_full_seq_qp_rank, personnel_full_seq_quarter, by = "UPDATE_DT")

personnel_full_seq_qp_rank$TotalSalary_unitofqu<- personnel_full_seq_qp_rank$TotalSalary_uicqu/personnel_full_seq_qp_rank$TotalSalary_qu
personnel_full_seq_qp_rank$TotalPeople_unitofqu<- personnel_full_seq_qp_rank$Totalpeople_uicqu/personnel_full_seq_qp_rank$Totalpeople_qu

# Sequestration
personnel_full_seq_qp_rank$TotalSalary_qu_seq <- personnel_full_seq_qp_rank$TotalSalary_qu * .90

##### Working backword to address impact on rank numbers 
personnel_full_seq_qp_rank$TotalSalary_uicqu_seq <- personnel_full_seq_qp_rank$TotalSalary_qu_seq * personnel_full_seq_qp_rank$TotalSalary_unitofqu

personnel_full_seq_qp_rank$TotalSalary_uicrankqu_seq <- personnel_full_seq_qp_rank$TotalSalary_uicqu_seq * personnel_full_seq_qp_rank$TotalSalary_rankofunit

personnel_full_seq_qp_rank$Totalpeople_uicrankqu_seq <- round(personnel_full_seq_qp_rank$TotalSalary_uicrankqu_seq/personnel_full_seq_qp_rank$AvgSalary_uicrankqu, 0)
# How many of each rank a unit would have if sequestration hits 

spec <- personnel_full_seq_qp_rank %>%
  group_by(UIC, UPDATE_DT) %>%
  summarise(TotalPeople_uicqu_seq = sum(Totalpeople_uicrankqu_seq))

personnel_full_seq_qp_rank <- left_join(personnel_full_seq_qp_rank, spec, by = c("UIC", "UPDATE_DT"))

personnel_full_seq_qp_rank <- as.data.frame(personnel_full_seq_qp_rank)

personnel_full_seq_sequestration <- left_join(personnel_full_seq, personnel_full_seq_qp_rank, by = c("UIC", "RANK_AB", "UPDATE_DT"))

personnel_full_seq_sequestration$Scenario <- "Sequestration"

personnel_seqnums <- personnel_full_seq_sequestration %>% 
  group_by(UIC, UPDATE_DT) %>%
  summarise(newassigned = mean(TotalPeople_uicqu_seq),
            oldassigned = n())

personnel_seqnums$difference <- personnel_seqnums$oldassigned - personnel_seqnums$newassigned
personnel_seqnums$differenceperc <- personnel_seqnums$difference/personnel_seqnums$oldassigned

# To make non-deployed in period 1
personnel_full_seq_sequestration <- left_join(personnel_full_seq_sequestration, personnel_seqnums, by = c("UIC", "UPDATE_DT"))

samples1 <- personnel_full_seq_sequestration  %>% 
  filter(UPDATE_DT == "2019-04-01" & !is.na(UIC)) %>%
  group_by(UIC) %>% 
  sample_frac(differenceperc)

personnel_full_seq_sequestration$DEPLOYABLE_seq <- ifelse((personnel_full_seq_sequestration$id %in% samples1$id) & (personnel_full_seq_sequestration$UPDATE_DT == "2019-04-01" | personnel_full_seq_sequestration$UPDATE_DT == "2019-07-01" | personnel_full_seq_sequestration$UPDATE_DT == "2019-10-01") , "Non-Deployable",  personnel_full_seq_sequestration$DEPLOYABLE)

colnames(personnel_full_seq_sequestration)
personnel_full_seq_sequestration_sim <- personnel_full_seq_sequestration[,-c(28, 39:58)]
names(personnel_full_seq_sequestration_sim) <- gsub("_seq", "", names(personnel_full_seq_sequestration_sim))

unique(personnel_full_seq_sequestration_sim$UPDATE_DT)

fieldtypes <- c(id = "int(11)", DRRSA_PERS_ID = "varchar(20)", FIRST_NM = "varchar(50)", MIDDLE_NM = "varchar(50)", LAST_NM = "varchar(50)", GENDER_CD  = "varchar(6)", BIRTH_DT = "date", BIRTH_CNTRY_CD = "char(2)", CTZSP_CNTRY_CD = "char(2)", MOS_AND_TITLE = "varchar(200)", BLOOD_TYP_ANT_CD = "char(1)", BLOOD_TYP_GRP_CD = "varchar(2)", UIC = "char(6)", LANG_ALL = "varchar(200)", LANG_LISTEN = "varchar(200)",LANG_READ = "varchar(200)", LANG_SPEAK = "varchar(200)",COMPO = "varchar(10)", HEIGHWEIGHTPASS = "varchar(5)",RANK_DT = "date",RANK_SORT = "int(11)",BODYCOMPDATE = "date",BODYCOMPPASS = "varchar(5)", BODYFATPASS = "varchar(5)", APFTPASS = "varchar(5)", APFTDATE = "date", MIL_ETS_DT = "date", DEPLOYABLE = "varchar(20)", ADMIN_DEPLOYABLE = "varchar(100)", NON_DEPLOYABLE_REASON = "varchar(100)",DWELL_TIME = "int(11)", UPDATE_DT = "date",year = "int(11)", RANK_CD = "char(2)", RANK_AB = "char(3)", monthly_pay = "decimal(10,2)", quarterly_pay = "decimal(10,2)", Scenario = "varchar(25)")

RMySQL::dbWriteTable(con, "ALD_Personnel_SequestrationScenario", personnel_full_seq_sequestration_sim, field.type = fieldtypes, row.names = FALSE, overwrite = TRUE)

# Units
unit_full_all <- dbGetQuery(con,'
  select *
  from ALD_Unit
  where Scenario = "Base"
')

unit_full_all_seq <- unit_full_all[unit_full_all$year == 2019,]
colnames(personnel_seqnums)[2] <- c("quarter")
unit_full_all_seq <- left_join(unit_full_all_seq, personnel_seqnums[,c(1:2,6)], by = c("UIC", "quarter"))

unit_full_all_seq$ASSIGNED_PRSNL <- round(ifelse(!is.na(unit_full_all_seq$differenceperc), unit_full_all_seq$ASSIGNED_PRSNL * unit_full_all_seq$differenceperc, unit_full_all_seq$ASSIGNED_PRSNL),0)
unit_full_all_seq$AUTHORIZED_PRSNL <- round(ifelse(!is.na(unit_full_all_seq$differenceperc), unit_full_all_seq$AUTHORIZED_PRSNL * unit_full_all_seq$differenceperc, unit_full_all_seq$AUTHORIZED_PRSNL),0)

unit_full_all_seq$P_LEVEL <- ifelse(unit_full_all_seq$P_LEVEL != 3 & (unit_full_all_seq$quarter == "2019-04-01" | unit_full_all_seq$quarter == "2019-07-01" | unit_full_all_seq$quarter == "2019-10-01"), unit_full_all_seq$P_LEVEL + 1, unit_full_all_seq$P_LEVEL)

unit_full_all_seq$T_LEVEL <- ifelse(unit_full_all_seq$T_LEVEL != 3 & (unit_full_all_seq$quarter == "2019-04-01" | unit_full_all_seq$quarter == "2019-07-01" | unit_full_all_seq$quarter == "2019-10-01"), unit_full_all_seq$T_LEVEL + 1, unit_full_all_seq$T_LEVEL)

unit_full_all_seq$R_LEVEL <- ifelse(unit_full_all_seq$R_LEVEL != 3 & (unit_full_all_seq$year == 2019), unit_full_all_seq$R_LEVEL + 1, unit_full_all_seq$R_LEVEL)

unit_full_all_seq$S_LEVEL <- ifelse(unit_full_all_seq$S_LEVEL != 3 & (unit_full_all_seq$quarter == "2019-04-01" | unit_full_all_seq$quarter == "2019-07-01" | unit_full_all_seq$quarter == "2019-10-01"), unit_full_all_seq$S_LEVEL + 1, unit_full_all_seq$S_LEVEL)

unit_full_all_seq$C_LEVEL <- (unit_full_all_seq$P_LEVEL + unit_full_all_seq$T_LEVEL + unit_full_all_seq$R_LEVEL + unit_full_all_seq$S_LEVEL)/4

unit_full_all_seq$differenceperc <- NULL
unit_full_all_seq$Scenario <- "Sequestration"

fieldtypes <- c(row_names = "int(11)", UIC = "varchar(50)", UNIT_NAME = "varchar(50)", HOME_STATION = "varchar(50)", STATION_STATE = "char(2)", STATION_CNTRY = "char(2)", DAYS_TO_LAD = "int(11)", COMPO_NUMBER = "int(11)", COMPO_NAME = "varchar(50)", ASGMT = "char(2)", DP99 = "char(4)", DP99_TITLE = "varchar(50)", UIC4 = "char(4)", COMPO = "varchar(50)", CONUS_OCONUS = "varchar(60)", MACOM_UIC = "char(6)", MACOM_NAME = "varchar(50)", CAP_UIC = "char(6)", ASSIGNED_PRSNL = "decimal(10,2)", AUTHORIZED_PRSNL = "decimal(10,2)", PROJECTED_PRSNL = "decimal(10,2)", FORCE_GROUP = "varchar(5)", SRM_PHASE = "varchar(7)", LOCATION ="varchar(50)", LATITUDE = "decimal(6,3)", LONGITUDE = "decimal(6,3)", C_LEVEL = "int(11)", P_LEVEL = "int(11)", R_LEVEL= "int(11)", S_LEVEL = "int(11)", T_LEVEL = "int(11)", MODULAR_TYPE = "varchar(10)", ARFORGEN_DISPLAY = "varchar(7)", LOCATION_P.lat = "decimal(6,3)",  LOCATION_P.lon = "decimal(6,3)", quarter = "date", year = "int(11)", quartertext = "char(7)", COCOM = "varchar(20)", Scenario = "varchar(25)")

colnames(unit_full_all_seq)

RMySQL::dbWriteTable(con, "ALD_Unit_SequestrationScenario", unit_full_all_seq, field.type = fieldtypes, overwrite = TRUE)

############################################## Scenario 2 - Weather Event ##########################
#### Load in personnel data to start calculating impact
personnel_full_seq <- dbGetQuery(con,'
  select *
  from ALD_Personnel
')

#### Load in equipment data to start calculating impact 
equipment_full_all <- dbGetQuery(con,'
  select *
  from ALD_Equipment
  WHERE year = 2018 OR year = 2019
')

#### Load in unit data to start calculating impact 
unit_full_all <- dbGetQuery(con,'
  select *
  from ALD_Unit
')

# Evaluate impacted areas 

unit_full_all_florence <- unit_full_all[(unit_full_all$STATION_STATE == "FL" | unit_full_all$STATION_STATE == "NC" | unit_full_all$STATION_STATE == "SC" | unit_full_all$STATION_STATE == "MD" | unit_full_all$STATION_STATE == "WV" | unit_full_all$STATION_STATE == "GA" | unit_full_all$STATION_STATE == "DC" | unit_full_all$STATION_STATE == "VA" | unit_full_all$STATION_STATE == "KY" | unit_full_all$STATION_STATE == "TN" | unit_full_all$STATION_STATE == "PA" | unit_full_all$STATION_STATE == "NY" | unit_full_all$STATION_STATE == "NJ" | unit_full_all$STATION_STATE == "MA" | unit_full_all$STATION_STATE == "CT" | unit_full_all$STATION_STATE == "NH" | unit_full_all$STATION_STATE == "VT" | unit_full_all$STATION_STATE == "ME" | unit_full_all$STATION_STATE == "OH" | unit_full_all$STATION_STATE == "DE" | unit_full_all$STATION_STATE == "RI") & (unit_full_all$HOME_STATION != "ALAMO" | unit_full_all$HOME_STATION != "ALBANY" | unit_full_all$HOME_STATION != "AUBURN" | unit_full_all$HOME_STATION != "BOWLING GREEN"  | unit_full_all$HOME_STATION != "CALHOUN"  | unit_full_all$HOME_STATION != "CARIBOU"  | unit_full_all$HOME_STATION != "DAYTON"  | unit_full_all$HOME_STATION != "JACKSON"  | unit_full_all$HOME_STATION != "MADISONVILLE"  | unit_full_all$HOME_STATION != "MEMPHIS"  | unit_full_all$HOME_STATION != "SPRINGFIELD"  | unit_full_all$HOME_STATION != "TALLAHASSEE"  | unit_full_all$HOME_STATION != "TOLEDO"  | unit_full_all$HOME_STATION != "TRENTON") & !is.na(unit_full_all$STATION_STATE) & !is.na(unit_full_all$LATITUDE) & unit_full_all$quarter == "2018-07-01",]

# Took out those that are relatively west of the path the hurricane would travers - make them the first responding groups 
unit_full_all_help <- unit_full_all_florence <- unit_full_all[(unit_full_all$HOME_STATION == "ALAMO" | unit_full_all$HOME_STATION == "ALBANY" | unit_full_all$HOME_STATION == "AUBURN" | unit_full_all$HOME_STATION == "BOWLING GREEN"  | unit_full_all$HOME_STATION == "CALHOUN"  | unit_full_all$HOME_STATION == "CARIBOU"  | unit_full_all$HOME_STATION == "DAYTON"  | unit_full_all$HOME_STATION == "JACKSON"  | unit_full_all$HOME_STATION == "MADISONVILLE"  | unit_full_all$HOME_STATION == "MEMPHIS"  | unit_full_all$HOME_STATION == "SPRINGFIELD"  | unit_full_all$HOME_STATION == "TALLAHASSEE"  | unit_full_all$HOME_STATION == "TOLEDO"  | unit_full_all$HOME_STATION == "TRENTON" | unit_full_all$UIC == "WPPCFF") & !is.na(unit_full_all$STATION_STATE) & !is.na(unit_full_all$LATITUDE) & (unit_full_all$quarter == "2018-07-01" | unit_full_all$quarter == "2018-07-01" | unit_full_all$quarter == "2018-07-01"),]

######### Personnel impact - 30% reduction in troop numbers for impacted and 10% for those that are deploying troops to help 
## Unit
# Actual troop number adjustments - impacted
unit_full_all$ASSIGNED_PRSNL_hurr <- ifelse(unit_full_all$HOME_STATION %in% unit_full_all_florence$HOME_STATION & unit_full_all$quarter == "2018-07-01", unit_full_all$ASSIGNED_PRSNL * .70, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_florence$HOME_STATION & unit_full_all$quarter == "2018-10-01", unit_full_all$ASSIGNED_PRSNL * .80, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_florence$HOME_STATION & unit_full_all$quarter == "2019-01-01", unit_full_all$ASSIGNED_PRSNL * .90, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_help$HOME_STATION & unit_full_all$quarter == "2018-07-01", unit_full_all$ASSIGNED_PRSNL * .90, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_help$HOME_STATION & unit_full_all$quarter == "2018-10-01", unit_full_all$ASSIGNED_PRSNL * .93, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_help$HOME_STATION & unit_full_all$quarter == "2019-01-01", unit_full_all$ASSIGNED_PRSNL * .96,  unit_full_all$ASSIGNED_PRSNL))))))

unit_full_all$AUTHORIZED_PRSNL_hurr <- ifelse(unit_full_all$HOME_STATION %in% unit_full_all_florence$HOME_STATION & unit_full_all$quarter == "2018-07-01", unit_full_all$AUTHORIZED_PRSNL * .70, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_florence$HOME_STATION & unit_full_all$quarter == "2018-10-01", unit_full_all$AUTHORIZED_PRSNL * .80, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_florence$HOME_STATION & unit_full_all$quarter == "2019-01-01", unit_full_all$AUTHORIZED_PRSNL * .90, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_help$HOME_STATION & unit_full_all$quarter == "2018-07-01", unit_full_all$AUTHORIZED_PRSNL * .90, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_help$HOME_STATION & unit_full_all$quarter == "2018-10-01", unit_full_all$AUTHORIZED_PRSNL * .93, ifelse(unit_full_all$HOME_STATION %in% unit_full_all_help$HOME_STATION & unit_full_all$quarter == "2019-01-01", unit_full_all$AUTHORIZED_PRSNL * .96,  unit_full_all$AUTHORIZED_PRSNL))))))


# Bring the P and T of PSRTC down 1 for affected units 
unit_full_all$P_LEVEL_hurr <- ifelse(unit_full_all$P_LEVEL != 3 & (unit_full_all$quarter == "2018-07-01" | unit_full_all$quarter == "2018-10-01" | unit_full_all$quarter == "2019-01-01"), unit_full_all$P_LEVEL + 1, unit_full_all$P_LEVEL)

unit_full_all$T_LEVEL_hurr <- ifelse(unit_full_all$T_LEVEL != 3 & (unit_full_all$quarter == "2018-07-01" | unit_full_all$quarter == "2018-10-01" | unit_full_all$quarter == "2019-01-01"), unit_full_all$T_LEVEL + 1, unit_full_all$T_LEVEL)

unit_full_all$R_LEVEL_hurr <- ifelse(unit_full_all$R_LEVEL != 3 & (unit_full_all$quarter == "2018-07-01" | unit_full_all$quarter == "2018-10-01" | unit_full_all$quarter == "2019-01-01"), unit_full_all$R_LEVEL + 1, unit_full_all$R_LEVEL)

unit_full_all$S_LEVEL_hurr <- ifelse(unit_full_all$S_LEVEL != 3 & (unit_full_all$quarter == "2018-07-01" | unit_full_all$quarter == "2018-10-01" | unit_full_all$quarter == "2019-01-01"), unit_full_all$S_LEVEL + 1, unit_full_all$S_LEVEL)

unit_full_all$C_LEVEL_hurr <- (unit_full_all$P_LEVEL_hurr + unit_full_all$T_LEVEL_hurr + unit_full_all$R_LEVEL_hurr + unit_full_all$S_LEVEL_hurr)/4

colnames(unit_full_all)
unit_full_all_hurr <- unit_full_all
unit_full_all_hurr$Scenario <- "Hurricane"

unit_full_all_hurr <- unit_full_all_hurr[-c(19:20, 27:31)]
names(unit_full_all_hurr) <- gsub("_hurr", "", names(unit_full_all_hurr))
unit_full_all_hurr <- unit_full_all_hurr[unit_full_all_hurr$quarter == "2018-07-01" | unit_full_all_hurr$quarter == "2018-10-01" | unit_full_all_hurr$quarter == "2019-01-01",]

colnames(unit_full_all_hurr)

fieldtypes <- c(row_names = "int(11)", UIC = "varchar(50)", UNIT_NAME = "varchar(50)", HOME_STATION = "varchar(50)", STATION_STATE = "char(2)", STATION_CNTRY = "char(2)", DAYS_TO_LAD = "int(11)", COMPO_NUMBER = "int(11)", COMPO_NAME = "varchar(50)", ASGMT = "char(2)", DP99 = "char(4)", DP99_TITLE = "varchar(50)", UIC4 = "char(4)", COMPO = "varchar(50)", CONUS_OCONUS = "varchar(60)", MACOM_UIC = "char(6)", MACOM_NAME = "varchar(50)", CAP_UIC = "char(6)", ASSIGNED_PRSNL = "decimal(10,2)", AUTHORIZED_PRSNL = "decimal(10,2)", PROJECTED_PRSNL = "decimal(10,2)", FORCE_GROUP = "varchar(5)", SRM_PHASE = "varchar(7)", LOCATION ="varchar(50)", LATITUDE = "decimal(6,3)", LONGITUDE = "decimal(6,3)", C_LEVEL = "int(11)", P_LEVEL = "int(11)", R_LEVEL= "int(11)", S_LEVEL = "int(11)", T_LEVEL = "int(11)", MODULAR_TYPE = "varchar(10)", ARFORGEN_DISPLAY = "varchar(7)", LOCATION_P.lat = "decimal(6,3)",  LOCATION_P.lon = "decimal(6,3)", quarter = "date", year = "int(11)", quartertext = "char(7)", COCOM = "varchar(20)", Scenario = "varchar(25)")

RMySQL::dbWriteTable(con, "ALD_Unit_HurricaneScenario", unit_full_all_hurr, field.type = fieldtypes, overwrite = TRUE)

# Personnel - Need to make certain proportion undeployable 
personnel_full_all_florence <- personnel_full_seq[personnel_full_seq$UIC %in% unit_full_all_florence$UIC,]

personnel_full_all_florence_lag <-  personnel_full_all_florence[personnel_full_all_florence$UPDATE_DT == "2018-04-01",]

personnel_full_all_florence <- personnel_full_all_florence[personnel_full_all_florence$UPDATE_DT == "2018-07-01" | personnel_full_all_florence$UPDATE_DT == "2018-10-01" | personnel_full_all_florence$UPDATE_DT == "2019-01-01",]

# Conditions to make non-deployable- 1. Comes in after hurricane hits (anyone that was not in 2018-04-01 and is in 2018-07-01 becomes non-deployable, 2. 30% of each UIC after that)

# To make non-deployed in period 1
samples1 <- personnel_full_all_florence %>% 
  filter(UPDATE_DT == "2018-07-01") %>%
  group_by(UIC) %>% 
  sample_frac(.30)
# To make non-deployed in period 2
samples2 <- samples1 %>% 
  group_by(UIC) %>% 
  sample_frac(.66)
samples3 <- samples2 %>% 
  group_by(UIC) %>% 
  sample_frac(.50)
# To make non-deployed in period 3

personnel_full_all_florence$DEPLOYABLE_hurr <- ifelse(!(personnel_full_all_florence$id %in% personnel_full_all_florence_lag$id), "Non-Deployable",  ifelse(personnel_full_all_florence$id %in% samples1$id & personnel_full_all_florence$UPDATE_DT == "2018-07-01", "Non-Deployable", ifelse(personnel_full_all_florence$id %in% samples2$id & personnel_full_all_florence$UPDATE_DT == "2018-10-01", "Non-Deployable", ifelse(personnel_full_all_florence$id %in% samples3$id & personnel_full_all_florence$UPDATE_DT == "2019-10-01", "Non-Deployable", personnel_full_all_florence$DEPLOYABLE))))

# Personnel - Need to make certain proportion of the helping units undeployable because they are helping in affected areas
personnel_full_all_help <- personnel_full_seq[personnel_full_seq$UIC %in% unit_full_all_help$UIC,]

personnel_full_all_help_lag <-  personnel_full_all_help[personnel_full_all_help$UPDATE_DT == "2018-04-01",]

personnel_full_all_help <- personnel_full_all_help[personnel_full_all_help$UPDATE_DT == "2018-07-01" | personnel_full_all_help$UPDATE_DT == "2018-10-01" | personnel_full_all_help$UPDATE_DT == "2019-01-01",]

# To make non-deployed in period 1
samples1 <- personnel_full_all_help %>% 
  filter(UPDATE_DT == "2018-07-01") %>%
  group_by(UIC) %>% 
  sample_frac(.10)
# To make non-deployed in period 2
samples2 <- samples1 %>% 
  group_by(UIC) %>% 
  sample_frac(.70)
samples3 <- samples2 %>% 
  group_by(UIC) %>% 
  sample_frac(.57)
# To make non-deployed in period 3

personnel_full_all_help$DEPLOYABLE_hurr <- ifelse(!(personnel_full_all_help$id %in% personnel_full_all_help_lag$id), "Non-Deployable",  ifelse(personnel_full_all_help$id %in% samples1$id & personnel_full_all_help$UPDATE_DT == "2018-07-01", "Non-Deployable", ifelse(personnel_full_all_help$id %in% samples2$id & personnel_full_all_help$UPDATE_DT == "2018-10-01", "Non-Deployable", ifelse(personnel_full_all_help$id %in% samples3$id & personnel_full_all_help$UPDATE_DT == "2019-10-01", "Non-Deployable", personnel_full_all_help$DEPLOYABLE))))

### Merge back in 
colnames(personnel_full_all_florence)
personnel_full_all_florence <- personnel_full_all_florence[,c(1,32, 38)]
personnel_full_all_help <- personnel_full_all_help[,c(1,32, 38)]
personnel_full_all_hurr <- left_join(personnel_full_seq, personnel_full_all_florence, by = c("id", "UPDATE_DT"))
personnel_full_all_hurr <- left_join(personnel_full_all_hurr, personnel_full_all_help, by = c("id", "UPDATE_DT"))

personnel_full_all_hurr$DEPLOYABLE_hurr <- ifelse(!is.na(personnel_full_all_hurr$DEPLOYABLE_hurr.x), personnel_full_all_hurr$DEPLOYABLE_hurr.x, ifelse(!is.na(personnel_full_all_hurr$DEPLOYABLE_hurr.y), personnel_full_all_hurr$DEPLOYABLE_hurr.y, personnel_full_all_hurr$DEPLOYABLE))

personnel_full_all_hurr$Scenario <- "Hurricane"

colnames(personnel_full_all_hurr)
personnel_full_all_hurr <- personnel_full_all_hurr[,-c(28,38:39)]

names(personnel_full_all_hurr) <- gsub("_hurr", "", names(personnel_full_all_hurr))
personnel_full_all_hurr <- personnel_full_all_hurr[personnel_full_all_hurr$UPDATE_DT == "2018-07-01" | personnel_full_all_hurr$UPDATE_DT == "2018-10-01" | personnel_full_all_hurr$UPDATE_DT == "2019-01-01",]

fieldtypes <- c(id = "int(11) AI PK", DRRSA_PERS_ID = "varchar(20)", FIRST_NM = "varchar(50)", MIDDLE_NM = "varchar(50)", LAST_NM = "varchar(50)", GENDER_CD  = "varchar(6)", BIRTH_DT = "date", BIRTH_CNTRY_CD = "char(2)", CTZSP_CNTRY_CD = "char(2)", MOS_AND_TITLE = "varchar(200)", BLOOD_TYP_ANT_CD = "char(1)", BLOOD_TYP_GRP_CD = "varchar(2)", UIC = "char(6)", LANG_ALL = "varchar(200)", LANG_LISTEN = "varchar(200)",LANG_READ = "varchar(200)", LANG_SPEAK = "varchar(200)",COMPO = "varchar(10)", HEIGHWEIGHTPASS = "varchar(5)",RANK_DT = "date",RANK_SORT = "int(11)",BODYCOMPDATE = "date",BODYCOMPPASS = "varchar(5)", BODYFATPASS = "varchar(5)", APFTPASS = "varchar(5)", APFTDATE = "date", MIL_ETS_DT = "date", DEPLOYABLE = "varchar(20)", ADMIN_DEPLOYABLE = "varchar(100)", NON_DEPLOYABLE_REASON = "varchar(100)",DWELL_TIME = "int(11)", UPDATE_DT = "date",year = "int(11)", RANK_CD = "char(2)", RANK_AB = "char(3)", monthly_pay = "decimal(10,2)", quarterly_pay = "decimal(10,2)", Scenario = "varchar(25)")

RMySQL::dbWriteTable(con, "ALD_Personnel_HurricaneScenario", personnel_full_all_hurr, overwrite = TRUE)

#Equipment 
equipment_full_all <- equipment_full_all_a
# Equipment for affected areas
equipment_full_all_florence <- equipment_full_all[equipment_full_all$UIC %in% unit_full_all_florence$UIC,]

equipment_full_all_florence_lag <-  equipment_full_all_florence[equipment_full_all_florence$quarter == "2018-04-01",]

equipment_full_all_florence <- equipment_full_all_florence[equipment_full_all_florence$quarter == "2018-07-01" | equipment_full_all_florence$quarter == "2018-10-01" | equipment_full_all_florence$quarter == "2019-10-01",]

unique(equipment_full_all_florence$MAJOR_CAPABILITY)

# Take down affected equipment 
# Update supply and maintenance
equipment_full_all_florence$NMCS_hurr <- ifelse((equipment_full_all_florence$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_florence$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_florence$MAJOR_CAPABILITY == "AVIATION") & equipment_full_all_florence$quarter == "2018-07-01", 85, ifelse((equipment_full_all_florence$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_florence$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_florence$MAJOR_CAPABILITY == "AVIATION") & (equipment_full_all_florence$quarter == "2018-10-01" | equipment_full_all_florence$quarter == "2019-01-01"), 5, equipment_full_all_florence$NMCS))

equipment_full_all_florence$NMCM_hurr <- ifelse((equipment_full_all_florence$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_florence$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_florence$MAJOR_CAPABILITY == "AVIATION") & (equipment_full_all_florence$quarter == "2018-10-01" | equipment_full_all_florence$quarter == "2019-01-01"), 85, ifelse((equipment_full_all_florence$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_florence$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_florence$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_florence$MAJOR_CAPABILITY == "AVIATION") & equipment_full_all_florence$quarter == "2018-07-01", 5, equipment_full_all_florence$NMCM))

equipment_full_all_florence$NMCS_CT_hurr <- equipment_full_all_florence$QTY_ON_HAND * (equipment_full_all_florence$NMCS_hurr/100)
equipment_full_all_florence$NMCM_CT_hurr <- equipment_full_all_florence$QTY_ON_HAND * (equipment_full_all_florence$NMCM_hurr/100)
equipment_full_all_florence$DOWN_hurr <- equipment_full_all_florence$NMCM_CT_hurr + equipment_full_all_florence$NMCS_CT_hurr
equipment_full_all_florence$UP_hurr <- equipment_full_all_florence$QTY_ON_HAND - equipment_full_all_florence$DOWN_hurr
equipment_full_all_florence$FMC_hurr <- equipment_full_all_florence$UP_hurr/equipment_full_all_florence$QTY_ON_HAND

View(equipment_full_all_florence[,c(20:22, 46:52)])

# Equipment for helping

equipment_full_all_help <- equipment_full_all[equipment_full_all$UIC %in% unit_full_all_help$UIC,]

equipment_full_all_help_lag <-  equipment_full_all_help[equipment_full_all_help$quarter == "2018-04-01",]

equipment_full_all_help <- equipment_full_all_help[equipment_full_all_help$quarter == "2018-07-01" | equipment_full_all_help$quarter == "2018-10-01" | equipment_full_all_help$quarter == "2019-10-01",]

unique(equipment_full_all_help$MAJOR_CAPABILITY)

# Take down affected equipment 
# Update supply and maintenance
equipment_full_all_help$NMCS_hurr <- ifelse((equipment_full_all_help$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_help$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_help$MAJOR_CAPABILITY == "AIRCRAFT") & equipment_full_all_help$quarter == "2018-07-01", equipment_full_all_help$NMCM + 10, ifelse((equipment_full_all_help$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_help$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_help$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_help$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_help$MAJOR_CAPABILITY == "AVIATION") & (equipment_full_all_help$quarter == "2018-10-01" | equipment_full_all_help$quarter == "2019-01-01"), equipment_full_all_help$NMCM + 5, equipment_full_all_help$NMCS))

equipment_full_all_help$NMCM_hurr <- ifelse((equipment_full_all_help$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_help$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_help$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_help$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_help$MAJOR_CAPABILITY == "AVIATION") & (equipment_full_all_help$quarter == "2018-10-01" | equipment_full_all_help$quarter == "2019-01-01"), equipment_full_all_help$NMCM + 5, ifelse((equipment_full_all_help$MAJOR_CAPABILITY == "TRAILERS" | equipment_full_all_help$MAJOR_CAPABILITY == "TRUCKS" | equipment_full_all_help$MAJOR_CAPABILITY == "AIR DEFENSE" | equipment_full_all_help$MAJOR_CAPABILITY == "AIRCRAFT" | equipment_full_all_help$MAJOR_CAPABILITY == "AVIATION") & equipment_full_all_help$quarter == "2018-07-01", equipment_full_all_help$NMCM + 3, equipment_full_all_help$NMCM))

equipment_full_all_help$NMCS_CT_hurr <- equipment_full_all_help$QTY_ON_HAND * (equipment_full_all_help$NMCS_hurr/100)
equipment_full_all_help$NMCM_CT_hurr <- equipment_full_all_help$QTY_ON_HAND * (equipment_full_all_help$NMCM_hurr/100)
equipment_full_all_help$DOWN_hurr <- equipment_full_all_help$NMCM_CT_hurr + equipment_full_all_help$NMCS_CT_hurr
equipment_full_all_help$UP_hurr <- equipment_full_all_help$QTY_ON_HAND - equipment_full_all_help$DOWN_hurr
equipment_full_all_help$FMC_hurr <- equipment_full_all_help$UP_hurr/equipment_full_all_help$QTY_ON_HAND

colnames(equipment_full_all_help)
equipment_full_all_florence <- equipment_full_all_florence[,c(17,23, 46:52)]
equipment_full_all_help <- equipment_full_all_help[,c(17,23, 46:52)]
equipment_full_all_hurr <- left_join(equipment_full_all, equipment_full_all_florence, by = c("ID", "quarter"))
equipment_full_all_hurr <- left_join(equipment_full_all_hurr, equipment_full_all_help, by = c("ID", "quarter"))

equipment_full_all_hurr$NMCS_hurr <- ifelse(!is.na(equipment_full_all_hurr$NMCS_hurr.x), equipment_full_all_hurr$NMCS_hurr.x, ifelse(!is.na(equipment_full_all_hurr$NMCS_hurr.y), equipment_full_all_hurr$NMCS_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$NMCM_hurr <- ifelse(!is.na(equipment_full_all_hurr$NMCM_hurr.x), equipment_full_all_hurr$NMCM_hurr.x, ifelse(!is.na(equipment_full_all_hurr$NMCM_hurr.y), equipment_full_all_hurr$NMCM_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$NMCS_CT_hurr <- ifelse(!is.na(equipment_full_all_hurr$NMCS_CT_hurr.x), equipment_full_all_hurr$NMCS_CT_hurr.x, ifelse(!is.na(equipment_full_all_hurr$NMCS_CT_hurr.y), equipment_full_all_hurr$NMCS_CT_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$NMCM_CT_hurr <- ifelse(!is.na(equipment_full_all_hurr$NMCM_CT_hurr.x), equipment_full_all_hurr$NMCM_CT_hurr.x, ifelse(!is.na(equipment_full_all_hurr$NMCM_CT_hurr.y), equipment_full_all_hurr$NMCM_CT_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$DOWN_hurr <- ifelse(!is.na(equipment_full_all_hurr$DOWN_hurr.x), equipment_full_all_hurr$DOWN_hurr.x, ifelse(!is.na(equipment_full_all_hurr$DOWN_hurr.y), equipment_full_all_hurr$DOWN_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$UP_hurr <- ifelse(!is.na(equipment_full_all_hurr$UP_hurr.x), equipment_full_all_hurr$UP_hurr.x, ifelse(!is.na(equipment_full_all_hurr$UP_hurr.y), equipment_full_all_hurr$UP_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$FMC_hurr <- ifelse(!is.na(equipment_full_all_hurr$FMC_hurr.x), equipment_full_all_hurr$FMC_hurr.x, ifelse(!is.na(equipment_full_all_hurr$FMC_hurr.y), equipment_full_all_hurr$FMC_hurr.y, equipment_full_all_hurr$NMCS))
equipment_full_all_hurr$HAS_DOWN <- ifelse(equipment_full_all_hurr$DOWN_hurr > 0, 1,0)

colnames(equipment_full_all_hurr)

equipment_full_all_hurr$Scenario <- "Hurricane"

equipment_full_all_hurr <- equipment_full_all_hurr[,-c(21:22,24:28, 46:59)]

names(equipment_full_all_hurr) <- gsub("_hurr", "", names(equipment_full_all_hurr))

equipment_full_all_hurr <- equipment_full_all_hurr[equipment_full_all_hurr$quarter == "2018-07-01" | equipment_full_all_hurr$quarter == "2018-10-01" | equipment_full_all_hurr$quarter == "2019-01-01",]

equipment_full_all_hurr <- equipment_full_all_hurr %>% mutate_at(vars(NMCS_CT, NMCM_CT, QTY_ON_HAND, DOWN, UP), funs(round(., 0)))

View(equipment_full_all_hurr[,c(20, 39:45)])

fieldtypes <- c(row_names = "int(11)", LIN = "char(6)", BASE = "varchar(200)", UIC_TITLE = "varchar(255)", UIC = "char(6)", UICNAME = "varchar(255)", MAJOR_CAPABILITY = "varchar(200)", LIN_FAMILY = "varchar(255)", LIN_SUB_FAMILY = "varchar(255)", LIN_NOMENCLATURE = "varchar(255)", COMPO = "varchar(50)", EQUIPMENT_NAME = "varchar(255)", NOMEN = "varchar(255)", DIV_UIC = "char(6)", OPCON = "char(6)", SNAME = "varchar(50)", ID = "varchar(50)",Nomenclature = "varchar(255)", Eqcost = "decimal(10,2)", QTY_ON_HAND = "int(11)", NMCS = "decimal(10,2)", NMCM = "decimal(10,2)", quarter = "char(10)", NMCM_CT = "int(11)", NMCS_CT = "int(11)", DOWN = "int(11)", UP = "int(11)", FMC = "decimal(10,2)", HAS_DOWN = "int(11)", year = "int(11)", quartertext = "char(7)", infrate = "decimal(10,2)", LFCost = "decimal(15,2)", MCCost = "decimal(15,2)", OPCON_NAME = "varchar(100)", fullfmcthresh = "decimal(10,5)", mcfmcthresh = "decimal(10,5)", lcfmcthresh = "decimal(10,5)", fullnmcmthresh = "decimal(10,5)", mcnmcmthresh = "decimal(10,5)", lcnmcmthresh = "decimal(10,5)", mcnmcsthresh = "decimal(10,5)", lcnmcsthresh = "decimal(10,5)", fullnmcsthresh = "decimal(10,5)", Scenario = "varchar(25)")

RMySQL::dbWriteTable(con, "ALD_Equipment_HurricaneScenario", field.type = fieldtypes, equipment_full_all_hurr, overwrite = TRUE)
