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
#Setting data and output directories
dd <-  "D:/Drop analysis/Repayment/data"
od <-  "D:/Drop analysis/Repayment/output"

# load libraries
#libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
#lapply(libs, require, character.only = T)
#---------------------------------------------------------------------------
d21 <- read.csv(paste(dd, "SC_2021_2020.10.08.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")

v21 <- read.csv(paste(dd, "V_2021_2020.10.08.csv",sep = "/"), header = TRUE,
               stringsAsFactors = FALSE, na.strings = "")

#To put dates in standard format,
#V21
v21$RepaymentDate <- substr(v21$RepaymentDate, 1, 10)
v21$RepaymentDate <- gsub(" ", "", v21$RepaymentDate)
v21$RepaymentDate <- gsub("-", "/", v21$RepaymentDate)
v21$RepaymentDate <- as.Date(v21$RepaymentDate)
class(v21$RepaymentDate)

#Selecting payment transactions for Mobile money or receipt
table(v21$Type)
v211 <- v21[rev(order(as.Date(v21$RepaymentDate))),] %>%
  subset(Type == "Receipt" | Type == "MobileMoney") %>%
  na.omit()
table(v211$Type)
# clients made payments in october
October.pymt <-subset(v211, v211$RepaymentDate >= "2020-10-01")
October.pymt.clt <- October.pymt%>% group_by(GlobalClientID)%>% summarise(October.pymt=sum(Amount))
names(October.pymt.clt)
nrow(October.pymt.clt)
#Adding unique ID for groups an sites
d21$grp.id <- paste(d21$DistrictName, d21$SiteName, d21$GroupName, sep = "")
d21$site.id <- paste(d21$DistrictName, d21$SiteName, sep = "")
#Adjusting the data sets
colnames(d21)
#d21 <- d21 %>% select(-ends_with("Trial.qty"))
d21 <- subset(d21, d21$TotalCredit>0)
nrow(d21)
#Sum of all products
#colnames(d21)
#d21$sumpro <- rowSums(d21[,48:75])
#Removing clients without products(did not enroll)
#length(unique(d21$GlobalClientID[d21$sumpro == 0]))
#d21 <- subset(d21, sumpro > 0)
#Remove irrelevant columns
d21 <- d21 %>% select(-ends_with(".qty")) %>% select(-ends_with(".kg"))
names(d21)
d21 <- select(d21,RegionName,DistrictName, SiteName, GroupName,LastName,FirstName,Facilitator,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,
              AccountNumber,LastRepayment,GlobalClientID,grp.id,site.id)
names(d21)
#Date as of the analysis
d <- as.Date("2020-10-08")
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
v202 <- v211 %>% group_by(GlobalClientID) %>% slice(which.max(RepaymentDate)) 
summary(v202$RepaymentDate)
table(v202$Type)
#Merging both season clients and vertical repayment
df <- merge(d21, v202, by = c("GlobalClientID"), all.x = TRUE, ignore.case = TRUE )
table(df$Type)
names(df)
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


names(df1)
table(df1$weeks, useNA = "ifany")
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
View(AnalysT)
#write.csv(AnalysT, "heatmapdata1005.csv")

########### For each client we put category according to stagnation and repayment position ########### 
datatrack <- df1 %>% mutate(Rank = ifelse(X..Repaid >= 60, "Icyatsi Kibisi",
                                          ifelse(X..Repaid >= 47 & X..Repaid < 60, "Icyatsi Kibisi",
                                                 ifelse(X..Repaid >= 40  & X..Repaid < 47 & weeks <= "<4 weeks","Icyatsi Kibisi",
                                                        ifelse(X..Repaid >= 42 & X..Repaid < 47 & weeks <= "<4 weeks","Icyatsi Kibisi",
                                                               ifelse(X..Repaid >= 42 & X..Repaid < 47 & weeks >= "<5 weeks" , "Umuhondo", 
                                                                      ifelse(X..Repaid >= 40 & X..Repaid < 42 & weeks >= "<5 weeks" & weeks <= "<8 weeks", "Umuhondo",
                                                                             ifelse(X..Repaid >= 30 & X..Repaid < 40 & weeks <= "<4 weeks", "Umuhondo",
                                                                                    ifelse( X..Repaid >= 40 & X..Repaid < 42 & weeks > "<8 weeks" , "Icunga rihishije",
                                                                                            ifelse(X..Repaid >= 30 & X..Repaid < 40 & weeks >= "<5 weeks","Icunga rihishije",
                                                                                                   ifelse(X..Repaid >= 25 & X..Repaid < 30 & weeks <= "<6 weeks", "Icunga rihishije",
                                                                                                          ifelse(X..Repaid >= 20 & X..Repaid < 25 & weeks <= "<4 weeks", "Icunga rihishije",
                                                                                                                 ifelse(X..Repaid >= 25 & X..Repaid < 30 & weeks >= "<= 7 weeks", "Umutuku",
                                                                                                                        ifelse( X..Repaid >= 20 & X..Repaid < 25 & weeks >= "<5 weeks", "Umutuku",
                                                                                                                                ifelse( X..Repaid < 20, "Umutuku","NA")))))))))))))))

table(datatrack$Rank, useNA = "ifany")
datatrack$Rank <- ifelse(is.na(datatrack$Rank) & datatrack$X..Repaid >=40, "icyatsi Kibisi", datatrack$Rank)
datatrack$Rank <- ifelse(is.na(datatrack$Rank) & datatrack$X..Repaid >=30 & datatrack$X..Repaid <40 , "Umuhondo", datatrack$Rank)
datatrack$Rank <- ifelse(is.na(datatrack$Rank) & datatrack$X..Repaid >=20 & datatrack$X..Repaid <30 , "Icunga rihishije", datatrack$Rank)
table(datatrack$Rank, useNA = "ifany")

#Adding the amount remining to be on HP
datatrack$AmountRemainingonHP = ifelse(datatrack$X..Repaid < 47, (datatrack$TotalCredit*0.47)-datatrack$TotalRepaid,0 )
table(datatrack$Rank)
sum(datatrack$AmountRemainingonHP)

# add clients made payment in October in SC
names(October.pymt.clt)
datatrack1 <- merge(datatrack, October.pymt.clt, by = c("GlobalClientID"), all.x = TRUE, ignore.case = TRUE )
names(datatrack1)
datatrack1$October.pymt <- ifelse(is.na(datatrack1$October.pymt), 0, datatrack1$October.pymt)
length(unique(datatrack1$GlobalClientID[datatrack1$October.pymt>0]))
datatrack1$clt.made.pymt.oct <- ifelse(datatrack1$October.pymt>0,1,0)
table(datatrack1$clt.made.pymt.oct, useNA = "ifany")

#select useful column
names(datatrack)
datatrack2 <- select(datatrack1,DistrictName,SiteName,GroupName,FirstName.x,LastName.x,AccountNumber.x,Facilitator,Rank,TotalCredit,RemainingCredit,TotalRepaid,
                     X..Repaid,AmountRemainingonHP,LastRepayment,clt.made.pymt.oct)

write.table(datatrack2, file = paste(od, "21A_October_repayment_Database.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")


