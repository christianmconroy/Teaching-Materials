# ## Week 9 Solutions ##

# Looping, Logs, government data, loading in multiple files and combining, labeling scatter points, lags, subsetting 

# Load in Packages 
library(readxl)
library(haven)
library(dplyr)
library(ggplot2)

# Question 2

rawdatapath <- "/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/Week 9 data"
output <- "/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/output"

rawdatapath

setwd(rawdatapath)

# Question 3
allfiles <- list.files(path = ".", pattern="*.xlsx", full.names = TRUE)
allfiles

seriesex <- read_excel("seriesreport-20170914163828_f73e7a.xlsx", range = "A12:D511")

# Question 4
datalist <- list()
for(i in allfiles){
  stats <- read_excel(i, range = "A12:D511")
  datalist[[i]] <- stats
}

fullframe = as.data.frame(do.call(rbind, datalist))
write.csv(fullframe, "fullframe.csv")
head(fullframe)

# Question 5
setwd(output)
names <- read_dta("names.dta")

names (fullframe)[1] <- "SeriesID"
fullfile <- inner_join(fullframe, names, by='SeriesID', match='all')

# Question 6
fullfile$Period <- substring(fullfile$Period, 2)
fullfile$YearMonth <- paste(as.character(fullfile$Year), fullfile$Period)
fullfile$time <- as.Date(strptime(paste(1, fullfile$YearMonth),"%d %Y %m"))

write.csv(fullfile, "data_and_time.csv")

# Question 7
names (fullfile)[4] <- "Employment"
fullfile$ID <- as.numeric(factor(fullfile$SeriesID, levels=unique(fullfile$SeriesID)))

fullfile$log_emp <- log(fullfile$Employment)
summary(fullfile$log_emp)

fullfile$emp_growth <- ave(fullfile$log_emp, fullfile$ID, FUN=function(x) c(0, diff(x)))
summary(fullfile$emp_growth)

# Question 8 # 

# Collapse to average employment growth by State over the whole time period

fullfile_bystate <- aggregate(emp_growth ~ ID, fullfile, mean, drop = FALSE)
idarea <- aggregate(emp_growth ~ Area, fullfile, mean, drop = FALSE)
fullfile_bystate <- cbind(fullfile_bystate, idarea[1])

ggplot(fullfile_bystate, aes(x= ID, y= emp_growth, colour="green", label=Area)) + geom_point() +geom_text(aes(label=Area),hjust=0, vjust=0)

# Question 9
write.csv(fullfile, "final_data.csv")

lg <- function(x)c(NA, x[1:(length(x)-1)])
lg2 <- function(x)c(NA, NA, x[1:(length(x)-2)])
lg3 <- function(x)c(NA, NA, NA, x[1:(length(x)-3)])
lg4 <- function(x)c(NA, NA, NA, NA, x[1:(length(x)-4)])


fullfile$emp_growthL1 <-unlist(tapply(fullfile$emp_growth, fullfile$ID, lg))
fullfile$emp_growthL2 <-unlist(tapply(fullfile$emp_growth, fullfile$ID, lg2))
fullfile$emp_growthL3 <-unlist(tapply(fullfile$emp_growth, fullfile$ID, lg3))
fullfile$emp_growthL4 <-unlist(tapply(fullfile$emp_growth, fullfile$ID, lg4))

reg1 <- lm(emp_growth ~ emp_growthL1 + emp_growthL2 + emp_growthL3 + emp_growthL4, data = fullfile)
summary(reg1)

fullfile_sunbelt <- fullfile[fullfile$Area == "Arizona" | fullfile$Area == "Nevada" | fullfile$Area == "California" | fullfile$Area =="Florida",]
fullfile_farmstates <- fullfile[fullfile$Area == "Idaho" | fullfile$Area == "North Dakota" | fullfile$Area == "Iowa" | fullfile$Area =="South Dakota",]
fullfile_oilstates <- fullfile[fullfile$Area == "Louisiana" | fullfile$Area == "Texas" | fullfile$Area == "Wyoming" | fullfile$Area =="Oklahoma",]

reg2 <- lm(emp_growth ~ emp_growthL1 + emp_growthL2 + emp_growthL3 + emp_growthL4, data = fullfile_sunbelt)
summary(reg2)

reg3 <- lm(emp_growth ~ emp_growthL1 + emp_growthL2 + emp_growthL3 + emp_growthL4, data = fullfile_farmstates)
summary(reg3)

reg4 <- lm(emp_growth ~ emp_growthL1 + emp_growthL2 + emp_growthL3 + emp_growthL4, data = fullfile_oilstates)
summary(reg4)

#   
#   * for all states, employment growth in percent today = 0.7* % growth last month + 0.11 * % change two months ago ...
#   
#   * what about other states? Note the double quotation marks when the local is used
#   
#   local sun_belt "Arizona","Nevada","California","Florida"
#   
#   local farm_states "Idaho","North Dakota","Iowa","South Dakota"
#   
#   local oil_states "Louisianna","Texas","Wyoming","Oklahoma"
#   
#   
#   reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth if inlist(Area,"`sun_belt'")
#   * about same as national economy
#   
#   reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth if inlist(Area,"`farm_states'")
#   * impact of previous emp_growth lower than in national economy
#   
#   reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth if inlist(Area,"`oil_states'")
#   * really strong short-term impact, second lag already insignificant
#   
#   
#   
