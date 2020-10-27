### Loading the libraries
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
library(stats)
library(lattice)
library(lubridate)
library(tidyr)
detach(package:plyr)
#Set a working directory
getwd()
setwd("/Users/jimmymarcel/Documents/One Acre Fund/DATA_FILES/Key Stats")
d21 <- read.csv("Season Clients Detailed_20201005-094416.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")
v21 <- read.csv("Detailed_20200928-115814.csv", header = TRUE,   
                stringsAsFactors = FALSE, na.strings = "")

#To put dates in standard format,
v21$RepaymentDate <- substr(v21$RepaymentDate, 1, 10)
v21$RepaymentDate <- gsub(" ", "", v21$RepaymentDate)
v21$RepaymentDate <- gsub("-", "/", v21$RepaymentDate)
v21$RepaymentDate <- as.Date(v21$RepaymentDate)
class(v21$RepaymentDate)

#Selecting payment transactions for Mobile money or receipt
v201 <- v21[rev(order(as.Date(v21$RepaymentDate))),] %>%
  subset(Type == "Receipt" | Type == "MobileMoney") %>%
  na.omit()
table(v21$Type)
#Adding unique ID for groups an sites
d21$grp.id <- paste(d21$DistrictName, d21$SiteName, d21$GroupName, sep = "")
d21$site.id <- paste(d21$DistrictName, d21$SiteName, sep = "")
#Adjusting the data sets
colnames(d21)
d21 <- d21 %>% select(-ends_with("Trial.qty"))
#Sum of all products
colnames(d21)
d21$sumpro <- rowSums(d21[,48:75])
#Removing clients without products(did not enroll)
length(unique(d21$GlobalClientID[d21$sumpro == 0]))
d21 <- subset(d21, sumpro > 0)
#Remove irrelevant columns
d21 <- d21 %>% select(-ends_with(".qty")) %>% select(-ends_with(".kg"))
#Date as of the analysis
d <- as.Date("2020-09-28")
#Setting date limits and counting stagnation time
#Selecting weeks from day as the analysis
oneweek <- d
twoweeks <- d-14
threeweeks <- d - 21
fourweeks <- d - 28
fiveweeks <- d - 35
sixweeks <- d - 42
sevenweeks <- d - 49
eightweeks <- d - 56
eightweeksplus <- eightweeks - 1
twelveweeksplus <- eightweeks - 29

#Selecting the most recent date of payments by removing duplicates
v202 <- v21 %>% group_by(GlobalClientID) %>% slice(which.max(RepaymentDate)) 
summary(v202$RepaymentDate)
#Merging both season clients and vertical repayment
df <- merge(d21, v202, by = c("GlobalClientID"), all.x = TRUE, ignore.case = TRUE )
table(df$Type)
#For each clients we add the number of weeks since last payment to track stagnation period
df1 <- df %>% mutate(weeks = ifelse(RepaymentDate <= oneweek & RepaymentDate > twoweeks , "<= 1 week",
                                     ifelse(RepaymentDate < oneweek & RepaymentDate > threeweeks, "<2 weeks",
                                            ifelse(RepaymentDate < twoweeks & RepaymentDate > fourweeks, "<3 weeks",
                                                   ifelse(RepaymentDate < threeweeks & RepaymentDate > fiveweeks,"<4 weeks",
                                                          ifelse(RepaymentDate < fourweeks & RepaymentDate > sixweeks, "<5 weeks",
                                                                 ifelse(RepaymentDate < fiveweeks & RepaymentDate > sevenweeks, "<6 weeks",
                                                                        ifelse(RepaymentDate < sixweeks & RepaymentDate > eightweeks, "<7 weeks",
                                                                               ifelse(RepaymentDate < sevenweeks & RepaymentDate > eightweeksplus, "<8 weeks",
                                                                                      ifelse(RepaymentDate < eightweeks & RepaymentDate > twelveweeksplus, ">=8 weeks",
                                                                                             ifelse(RepaymentDate <= twelveweeksplus, ">=12 weeks", NA)))))))))))




#Creating data set of heatMap overall
AnalysT <- df1 %>%
  group_by(weeks) %>%
  summarise(
    p60 = length(unique(GlobalClientID[X..Repaid >= 60])),
    p47 = length(unique(GlobalClientID[X..Repaid >= 47 & X..Repaid < 60])),
    p42 = length(unique(GlobalClientID[X..Repaid >= 42 & X..Repaid < 47])),
    p40 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 42])),
    p35 =  length(unique(GlobalClientID[X..Repaid >= 35 & X..Repaid < 40 ])),
    P30 = length(unique(GlobalClientID[X..Repaid >= 30 & X..Repaid < 35])),
    p25 = length(unique(GlobalClientID[X..Repaid >= 25 & X..Repaid < 30])),
    p20 = length(unique(GlobalClientID[X..Repaid  >= 20 & X..Repaid < 25 ])),
    p15 = length(unique(GlobalClientID[X..Repaid >= 15 & X..Repaid < 20])),
    p10 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 15])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )
write.csv(AnalysT, "heatmapdata0928.csv")
#Creating data set of heatMap on regional level
Region <- df1 %>%
  group_by(RegionName,weeks) %>%
  summarise(
    p60 = length(unique(GlobalClientID[X..Repaid >= 60])),
    p47 = length(unique(GlobalClientID[X..Repaid >= 47 & X..Repaid < 60])),
    p42 = length(unique(GlobalClientID[X..Repaid >= 42 & X..Repaid < 47])),
    p40 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 42])),
    p35 =  length(unique(GlobalClientID[X..Repaid >= 35 & X..Repaid < 40 ])),
    P30 = length(unique(GlobalClientID[X..Repaid >= 30 & X..Repaid < 35])),
    p25 = length(unique(GlobalClientID[X..Repaid >= 25 & X..Repaid < 30])),
    p20 = length(unique(GlobalClientID[X..Repaid  >= 20 & X..Repaid < 25 ])),
    p15 = length(unique(GlobalClientID[X..Repaid >= 15 & X..Repaid < 20])),
    p10 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 15])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )
#Creating data set of heatMap on District level
Region <- df1 %>%
  group_by(RegionName,DistrictName, weeks) %>%
  summarise(
    p60 = length(unique(GlobalClientID[X..Repaid >= 60])),
    p47 = length(unique(GlobalClientID[X..Repaid >= 47 & X..Repaid < 60])),
    p42 = length(unique(GlobalClientID[X..Repaid >= 42 & X..Repaid < 47])),
    p40 = length(unique(GlobalClientID[X..Repaid >= 40 & X..Repaid < 42])),
    p35 =  length(unique(GlobalClientID[X..Repaid >= 35 & X..Repaid < 40 ])),
    P30 = length(unique(GlobalClientID[X..Repaid >= 30 & X..Repaid < 35])),
    p25 = length(unique(GlobalClientID[X..Repaid >= 25 & X..Repaid < 30])),
    p20 = length(unique(GlobalClientID[X..Repaid  >= 20 & X..Repaid < 25 ])),
    p15 = length(unique(GlobalClientID[X..Repaid >= 15 & X..Repaid < 20])),
    p10 = length(unique(GlobalClientID[X..Repaid >= 10 & X..Repaid < 15])),
    p10L = length(unique(GlobalClientID[X..Repaid < 10]))
  )


########### For each client we put category according to stagnation and repayment position ########### 
datatrack <- df1 %>% mutate(Rank = ifelse(X..Repaid >= 60, "Elegible for B Season",
                                           ifelse(X..Repaid >= 47 & X..Repaid < 60, "On Track",
                                                  ifelse(X..Repaid >= 40  & X..Repaid < 47 & weeks <= "<4 weeks","On Track",
                                                         ifelse(X..Repaid >= 42 & X..Repaid < 47 & weeks <= "<4 weeks","On Track",
                                                                ifelse(X..Repaid >= 42 & X..Repaid < 47 & weeks >= "<5 weeks" , "Off Track", 
                                                                       ifelse(X..Repaid >= 40 & X..Repaid < 42 & weeks >= "<5 weeks" & weeks <= "<8 weeks", "Off Track",
                                                                              ifelse(X..Repaid >= 30 & X..Repaid < 40 & weeks <= "<4 weeks", "Off Track",
                                                                                     ifelse( X..Repaid >= 40 & X..Repaid < 42 & weeks > "<8 weeks" , "Of Concern",
                                                                                             ifelse(X..Repaid >= 30 & X..Repaid < 40 & weeks >= "<5 weeks","Of Concern",
                                                                                                    ifelse(X..Repaid >= 25 & X..Repaid < 30 & weeks <= "<6 weeks", "Of Concern",
                                                                                                           ifelse(X..Repaid >= 20 & X..Repaid < 25 & weeks <= "<4 weeks", "Of Concern",
                                                                                                                  ifelse(X..Repaid >= 25 & X..Repaid < 30 & weeks >= "<= 7 weeks", "Most Concerning",
                                                                                                                         ifelse( X..Repaid >= 20 & X..Repaid < 25 & weeks >= "<5 weeks", "Most Concerning",
                                                                                                                                ifelse( X..Repaid < 15, "Most Concerning",NA)))))))))))))))



table(datatrack$Rank)
#Averages credit for every client's category
mean(datatrack$TotalCredit)
mean(datatrack$TotalCredit[datatrack$Rank == "Elegible for B Season"], na.rm = TRUE)
mean(datatrack$TotalCredit[datatrack$Rank == "On Track"], na.rm = TRUE)
mean(datatrack$TotalCredit[datatrack$Rank == "Off Track"],na.rm = TRUE)
mean(datatrack$TotalCredit[datatrack$Rank == "Of Concern"],na.rm = TRUE)
mean(datatrack$TotalCredit[datatrack$Rank == "Most Concerning"],na.rm = TRUE)
#Average outstanding credit for every client's category
sum(datatrack$RemainingCredit)
mean(datatrack$RemainingCredit[datatrack$Rank == "Elegible for B Season"], na.rm = TRUE)
mean(datatrack$RemainingCredit[datatrack$Rank == "On Track"],na.rm = TRUE)
mean(datatrack$RemainingCredit[datatrack$Rank == "Off Track"],na.rm = TRUE)
mean(datatrack$RemainingCredit[datatrack$Rank == "Of Concern"],na.rm = TRUE)
mean(datatrack$RemainingCredit[datatrack$Rank == "Most Concerning"],na.rm = TRUE)

#New clients groups
dfNew <- subset(datatrack, NewMember == "True")
table(dfNew$Rank)
#Returning Clients Completed their Loan/ numbers
dfreturfin <- subset(datatrack,NewMember =="False" & X2021A_Enrollment.Fee.adjustment ==0)
table(dfreturfin$Rank)
#Returning Clients Carried Over/ Numbers
dfreturcar <- subset(datatrack,NewMember =="False" & X2021A_Enrollment.Fee.adjustment >0)
table(dfreturcar$Rank)





###################  Ranking the sites by repayment position and participation ###################
#Clients made payments in the last week
v201 <- v21[rev(order(as.Date(v21$RepaymentDate))),] %>%
  subset(Type == "Receipt" | Type == "MobileMoney") %>%
  subset(RepaymentDate == "2020-09-28" & RepaymentDate == "2020-09-28")%>%
  na.omit()
# In the season clients we see the clients who made payments in the last week
dfD$Paidweek <- match(dfD$GlobalClientID, v201$GlobalClientID, nomatch = 0)
########conditions########
#FD and RL level		
#Ranking	% repaid	##% participation
#On track	40-47%	##80-100
#Off Track 30-35%	##60-80
#Of Concern	20-25%	##40-60
#Most Concerning	##<25%	<40
dsite <- dfD %>%
  group_by(RegionName, DistrictName, SiteName) %>%
  summarise(
    tot.cl = length(unique(GlobalClientID)),
    Percrepaid = round(100*(sum(TotalRepaid)/sum(TotalCredit)),1),
    participation = length(unique(GlobalClientID[Paidweek >0])))
  









