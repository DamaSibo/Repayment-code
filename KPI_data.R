#clear environment
rm(list = ls()); cat("\014")

#Setting data and output directories
dd <-  "D:/Drop analysis/Repayment/data"
od <-  "D:/Drop analysis/Repayment/output"

# load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape")
lapply(libs, require, character.only = T)
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
#loading objects/data/data frame

d21 <- read.csv(paste(dd, "SC_2021_2020.10.23.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")
d21 <- subset(d21, d21$TotalCredit>0)

v21 <- read.csv(paste(dd, "V_2021_2020.10.25.csv",sep = "/"), header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")
#---------------------------------------------------------------------------------------------------------------
#To put dates in standard format,
v21$RepaymentDate <- substr(v21$RepaymentDate, 1, 10)
v21$RepaymentDate <- gsub(" ", "", v21$RepaymentDate)
v21$RepaymentDate <- gsub("-", "/", v21$RepaymentDate)
v21$RepaymentDate <- as.Date(v21$RepaymentDate)
class(v21$RepaymentDate)
#Selecting the last Repayment date per ID
v201 <- v21 %>% group_by(GlobalClientID) %>% slice(which.max(RepaymentDate))
#Add months to each clients
v201$monthspaid = months(as.Date(v201$RepaymentDate,format="%Y-%m-%d"))
#Merging both season clients and vertical repayment
df <- merge(d21, v201, by = c("GlobalClientID"), all.x = TRUE, ignore.case = TRUE )
#subset clients paid only before August and in August
#---------------------------------------------------------------#
df1 <- subset(df,df$RepaymentDate < "2020-09-01" | is.na(df$RepaymentDate))
length(unique(df1$GlobalClientID[df1$X..Repaid>=47]))
length(unique(df1$GlobalClientID[df1$X..Repaid<47]))/nrow(df) # targetted 30.95% in his week 26th.Oct
#Adding the amount remining to be on HP
df1$AmountRemainingonHP = ifelse(df1$X..Repaid < 47, (df1$TotalCredit*0.47)-df1$TotalRepaid,0 )
sum(df1$AmountRemainingonHP)
# Remove deceased clients
df1 <- subset(df1, df1$Deceased=="False")
#paid before August and in August below to HP 47%
df2 <- subset(df1, df1$X..Repaid <47)
#check
length(unique(df2$GlobalClientID[df2$X2021A_Enrollment.Fee.adjustment>0]))
#Remaining necessary column
df3 <- select(df2,RegionName,DistrictName,SiteName,GroupName,LastName.x,FirstName.x,AccountNumber.x,Facilitator,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,
              AmountRemainingonHP,RepaymentDate,monthspaid,ClientPhone,X2021A_Enrollment.Fee.adjustment,GlobalClientID)

write.table(df3, file = paste(od, "21A_October_repayment_Database.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
#-----------------------------------------------------------------------------------------------------
#subset clients paid only Sept
#---------------------------------------------------------------#
df.S1 <- subset(df,df$RepaymentDate >= "2020-09-01" & df$RepaymentDate < "2020-10-01")
length(unique(df.S1$GlobalClientID[df.S1$X..Repaid>=47]))
length(unique(df.S1$GlobalClientID[df.S1$X..Repaid<47]))/nrow(df) # targetted 30.95% in his week 26th.Oct
#Adding the amount remining to be on HP
df.S1$AmountRemainingonHP = ifelse(df.S1$X..Repaid < 47, (df.S1$TotalCredit*0.47)-df.S1$TotalRepaid,0 )
sum(df.S1$AmountRemainingonHP)
# Remove deceased clients
df.S1 <- subset(df.S1, df.S1$Deceased=="False")
#paid before August and in August below to HP 47%
df.S2 <- subset(df.S1, df.S1$X..Repaid <47)
#check
length(unique(df.S2$GlobalClientID[df.S2$X2021A_Enrollment.Fee.adjustment>0]))
#Remaining necessary column
df.S3 <- select(df.S2,RegionName,DistrictName,SiteName,GroupName,LastName.x,FirstName.x,AccountNumber.x,Facilitator,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,
              AmountRemainingonHP,RepaymentDate,monthspaid,ClientPhone,X2021A_Enrollment.Fee.adjustment,GlobalClientID)

write.table(df.S3, file = paste(od, "21A_October_repayment_Database.csv", sep = "/"),
            row.names = FALSE, col.names = TRUE, sep = ",")
#-----------------------------------------------------------------------------------------------------


#Clients Paid Only before August
beforeAugust <- df %>%
  filter(RepaymentDate < "2020-08-01") %>%
  group_by(RegionName, DistrictName) %>%
  summarise(
    totalclients = length(unique(GlobalClientID)),
    belowHp = length(unique(GlobalClientID[X..Repaid < 47]))
  )

##Clients Paid Only in August
August <- df %>%
  filter(RepaymentDate > "2020-07-31" & RepaymentDate < "2020-09-01" ) %>%
  group_by(RegionName, DistrictName) %>%
  summarise(
    totalclients = length(unique(GlobalClientID)),
    belowHp = length(unique(GlobalClientID[X..Repaid < 47]))
  )

#Clients Paid in September
September <- df %>%
  filter(RepaymentDate > "2020-08-31" & RepaymentDate < "2020-10-01" ) %>%
  group_by(RegionName,DistrictName) %>%
  summarise(
    totalclients = length(unique(GlobalClientID)),
    belowHp = length(unique(GlobalClientID[X..Repaid < 47]))
  )

#Clients Paid in October
October <- df %>%
  filter(RepaymentDate > "2020-09-30" & RepaymentDate < "2020-11-01" ) %>%
  group_by(RegionName, DistrictName) %>%
  summarise(
    totalclients = length(unique(GlobalClientID)),
    belowHp = length(unique(GlobalClientID[X..Repaid < 47]))
  )

#####
getwd()
write.csv(beforeAugust, "BeforeAug.csv")
write.csv(August, "August.csv")
write.csv(September, "September.csv")
write.csv(October, "October.csv")


###### OPtion TWO
Test <- df %>% 
  group_by(RegionName, DistrictName, SiteName) %>%
  summarise(
    Thismonth = length(unique(GlobalClientID[months !="October"])),
    Lastmonth = length(unique(GlobalClientID[months !="October"| months != "September"])),
    LasttwoMonths = length(unique(GlobalClientID[months !="October"| months != "September" | months != "August"])),
    Lastthreeabove = length(unique(GlobalClientID[RepaymentDate < "2020-08-01"]))
  )

