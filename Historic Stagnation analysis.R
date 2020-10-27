### Historical stagnation performance


#cleaning the environment
rm(list = ls()); cat("\014")

#setting directories
dd <- "D:/Drop analysis/Repayment/data"
od <- "D:/Drop analysis/Repayment/output"

# Load libraries
libs <- c("lubridate", "plyr", "zoo", "reshape2", "ggplot2", "dplyr","doBy","reshape", "anytime")
# install.packages("plyr", dependencies = TRUE)

lapply(libs, require, character.only = T)

#############################################################################################
# Read data
d20 <- read.csv(paste(dd, "SC_2020_2020.07.20.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d20 <- subset(d20, d20$TotalCredit >0)
nrow(d20)

d19 <- read.csv(paste(dd, "SC_2019_2020.09.03.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d19 <- subset(d19, d19$TotalCredit >0)
nrow(d19)
d18 <- read.csv(paste(dd, "SC_2018_2020.09.03.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d18 <- subset(d18, d18$TotalCredit >0)
nrow(d18)
d17 <- read.csv(paste(dd, "SC_2017_2020.09.03.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
d17 <- subset(d17, d17$TotalCredit >0)
nrow(d17)
v201 <- read.csv(paste(dd, "V_2020_2020.08.19.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
v191 <- read.csv(paste(dd, "V_2019_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
v181 <- read.csv(paste(dd, "V_2018_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
v171 <- read.csv(paste(dd, "V_2017_2020.07.07.csv", sep = "/"), header = TRUE,  
                stringsAsFactors = FALSE, na.strings = "")
#--------------------------------------------------------------------------
# select columns
names(d20)
names(d19)
names(d18)
names(d17)
d20 <- select(d20,DistrictName,SiteName,GroupName,LastName,FirstName,GlobalClientID,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,TotalRepaid_IncludingOverpayments,
              X2020A_CycleCredit,X2020B_CycleCredit)
d19 <- select(d19,DistrictName,SiteName,GroupName,LastName,FirstName,GlobalClientID,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,TotalRepaid_IncludingOverpayments,
              X2019A_CycleCredit,X2019B_CycleCredit)
d18 <- select(d18,DistrictName,SiteName,GroupName,LastName,FirstName,GlobalClientID,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,TotalRepaid_IncludingOverpayments,
              X2018A_CycleCredit,X2018B_CycleCredit)
d17 <- select(d17,DistrictName,SiteName,GroupName,LastName,FirstName,GlobalClientID,TotalCredit,TotalRepaid,RemainingCredit,X..Repaid,TotalRepaid_IncludingOverpayments,
              X2017A_CycleCredit,X2017B_CycleCredit)
#---------------------------------------------------------------
#To put dates in standard format,
#2020
class(v20$RepaymentDate)
v201$date <- substr(v201$RepaymentDate, 1, 10)
v201$date <- gsub(" ", "", v201$date)
v201$date <- gsub("-", "/", v201$date)
v201$date <- as.Date(v201$date)
class(v201$date)
#2019
class(v19$RepaymentDate)
v191$date <- substr(v191$RepaymentDate, 1, 10)
v191$date <- gsub(" ", "", v191$date)
v191$date <- gsub("-", "/", v191$date)
v191$date <- as.Date(v191$date)
class(v191$date)
#2018
class(v181$RepaymentDate)
v181$date <- substr(v181$RepaymentDate, 1, 10)
v181$date <- gsub(" ", "", v181$date)
v181$date <- gsub("-", "/", v181$date)
v181$date <- as.Date(v181$date)
class(v181$date)
#2017
class(v171$RepaymentDate)
v171$date <- substr(v171$RepaymentDate, 1, 10)
v171$date <- gsub(" ", "", v171$date)
v171$date <- gsub("-", "/", v171$date)
v171$date <- as.Date(v171$date)
class(v171$date)
#-------------------------------------------------------------
# Separate clients payments made in each month
# Subset only payment from a client
#table(v20$Type)
#v201 <- subset(v20, v20$Type=="MobileMoney" | v20$Type=="Receipt")
#v191 <- subset(v19, v19$Type=="MobileMoney" | v19$Type=="Receipt")
#v181 <- subset(v18, v18$Type=="MobileMoney" | v18$Type=="Receipt")
#v171 <- subset(v17, v17$Type=="MobileMoney" | v17$Type=="Receipt")

# check
#table(v201$Type)
#table(v191$Type)
#table(v181$Type)
#table(v171$Type)
#------------------------------------------------------------------
#create each month's table to be able to allocate each transactions and put transactions on client level
names(v201)
# For 2020
Aug_20 <-subset(v201, v201$date <= "2019-08-31")
Aug_20.pymt <- Aug_20%>% group_by(GlobalClientID)%>% summarise(Aug_20.pymt=sum(Amount))

Sept_20 <-subset(v201, v201$date > "2019-08-31" & v201$date <= "2019-09-30")
Sept_20.pymt <- Sept_20%>% group_by(GlobalClientID)%>% summarise(Sept_20.pymt=sum(Amount))

Octor_20 <-subset(v201, v201$date > "2019-09-30" & v201$date <= "2019-10-31")
Octor_20.pymt <- Octor_20%>% group_by(GlobalClientID)%>% summarise(Octor_20.pymt=sum(Amount))

Nov_20 <-subset(v201, v201$date > "2019-10-31" & v201$date <= "2019-11-30")
Nov_20.pymt <- Nov_20%>% group_by(GlobalClientID)%>% summarise(Nov_20.pymt=sum(Amount))

Dec_20 <-subset(v201, v201$date > "2019-11-30" & v201$date <= "2019-12-31")
Dec_20.pymt <- Dec_20%>% group_by(GlobalClientID)%>% summarise(Dec_20.pymt=sum(Amount))

Jan_20 <-subset(v201, v201$date > "2019-12-31" & v201$date <= "2020-01-31")
Jan_20.pymt <- Jan_20%>% group_by(GlobalClientID)%>% summarise(Jan_20.pymt=sum(Amount))

Feb_20 <-subset(v201, v201$date > "2020-01-31" & v201$date <= "2020-02-29")
Feb_20.pymt <-Feb_20%>% group_by(GlobalClientID)%>% summarise(Feb_20.pymt=sum(Amount))

Mar_20 <-subset(v201, v201$date > "2020-02-29" & v201$date <= "2020-03-31")
Mar_20.pymt <- Mar_20%>% group_by(GlobalClientID)%>% summarise(Mar_20.pymt=sum(Amount))

Apr_20 <-subset(v201, v201$date > "2020-03-31" & v201$date <= "2020-04-30")
Apr_20.pymt <- Apr_20%>% group_by(GlobalClientID)%>% summarise(Apr_20.pymt=sum(Amount))

May_20 <-subset(v201, v201$date > "2020-04-30" & v201$date <= "2020-05-31")
May_20.pymt <- May_20%>% group_by(GlobalClientID)%>% summarise(May_20.pymt=sum(Amount))

Jun_20 <-subset(v201, v201$date > "2020-05-31" & v201$date <= "2020-06-30")
Jun_20.pymt <- Jun_20%>% group_by(GlobalClientID)%>% summarise(Jun_20.pymt=sum(Amount))

Jul_20 <-subset(v201, v201$date > "2020-06-30" & v201$date <= "2020-07-31")
Jul_20.pymt <- Jul_20%>% group_by(GlobalClientID)%>% summarise(Jul_20.pymt=sum(Amount))
#check if it works
names(Jul_20.pymt)
sum(Jul_20.pymt$Jul_20.pymt)
names(Jun_20.pymt)
sum(Jun_20.pymt$Jun_20.pymt)
names(May_20.pymt)
sum(May_20.pymt$May_20.pymt)
names(Apr_20.pymt)
sum(Apr_20.pymt$Apr_20.pymt)
names(Mar_20.pymt)
sum(Mar_20.pymt$Mar_20.pymt)
names(Feb_20.pymt)
sum(Feb_20.pymt$Feb_20.pymt)
names(Jan_20.pymt)
sum(Jan_20.pymt$Jan_20.pymt)
names(Dec_20.pymt)
sum(Dec_20.pymt$Dec_20.pymt)
names(Nov_20.pymt)
sum(Nov_20.pymt$Nov_20.pymt)
names(Octor_20.pymt)
sum(Octor_20.pymt$Octor_20.pymt)
names(Sept_20.pymt)
sum(Sept_20.pymt$Sept_20.pymt)
names(Aug_20.pymt)
sum(Aug_20.pymt$Aug_20.pymt)

#create columns in the SC for each month and put inside amount paid by each farmer

d20$Aug_20.pymt  <- Aug_20.pymt$Aug_20.pymt[match(d20$GlobalClientID,Aug_20.pymt$GlobalClientID)]
d20$Sept_20.pymt <-Sept_20.pymt$Sept_20.pymt[match(d20$GlobalClientID,Sept_20.pymt$GlobalClientID)]
d20$Oct_20.pymt  <- Octor_20.pymt$Octor_20.pymt[match(d20$GlobalClientID,Octor_20.pymt$GlobalClientID)]
d20$Nov_20.pymt  <- Nov_20.pymt$Nov_20.pymt[match(d20$GlobalClientID,Nov_20.pymt$GlobalClientID)]
d20$Dec_20.pymt  <- Dec_20.pymt$Dec_20.pymt[match(d20$GlobalClientID,Dec_20.pymt$GlobalClientID)]
d20$Jan_20.pymt  <- Jan_20.pymt$Jan_20.pymt[match(d20$GlobalClientID,Jan_20.pymt$GlobalClientID)]
d20$Feb_20.pymt  <- Feb_20.pymt$Feb_20.pymt[match(d20$GlobalClientID,Feb_20.pymt$GlobalClientID)]
d20$Mar_20.pymt  <- Mar_20.pymt$Mar_20.pymt[match(d20$GlobalClientID,Mar_20.pymt$GlobalClientID)]
d20$Apr_20.pymt  <- Apr_20.pymt$Apr_20.pymt[match(d20$GlobalClientID,Apr_20.pymt$GlobalClientID)]
d20$May_20.pymt  <- May_20.pymt$May_20.pymt[match(d20$GlobalClientID,May_20.pymt$GlobalClientID)]
d20$Jun_20.pymt  <- Jun_20.pymt$Jun_20.pymt[match(d20$GlobalClientID,Jun_20.pymt$GlobalClientID)]
d20$Jul_20.pymt  <- Jul_20.pymt$Jul_20.pymt[match(d20$GlobalClientID,Jul_20.pymt$GlobalClientID)]

d20[is.na(d20)] <- 0
#Check if it works
names(d20)
d20$clt.made.paymt <- ifelse((d20$Aug_20.pymt >0
                             |d20$Sept_20.pymt >0
                             |d20$Oct_20.pymt >0
                             |d20$Nov_20.pymt >0
                             |d20$Dec_20.pymt >0
                             |d20$Jan_20.pymt >0                     
                             |d20$Feb_20.pymt >0                 
                             |d20$Mar_20.pymt >0                 
                             |d20$Apr_20.pymt >0                 
                             |d20$May_20.pymt >0                 
                             |d20$Jun_20.pymt >0                 
                             |d20$Jul_20.pymt >0),1,0)
table(d20$clt.made.paymt, useNA = "ifany")

#let's now calculate if a farmer has finished his/her credit

d20$Aug_fin <- ifelse(d20$Aug_20.pymt>=d20$X2020A_CycleCredit & d20$X2020A_CycleCredit>0,1,0)
d20$Sept_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt)>=d20$X2020A_CycleCredit & d20$X2020A_CycleCredit>0,1,0)
d20$Oct_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt)>=d20$X2020A_CycleCredit & d20$X2020A_CycleCredit>0,1,0)
d20$Nov_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt)>=d20$X2020A_CycleCredit & d20$X2020A_CycleCredit>0,1,0)
d20$Dec_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt)>=d20$X2020A_CycleCredit & d20$X2020A_CycleCredit>0,1,0)
d20$Jan_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt)>=d20$TotalCredit,1,0)
d20$Feb_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt)>=d20$TotalCredit,1,0)
d20$Mar_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt+d20$Mar_20.pymt)>=d20$TotalCredit,1,0)
d20$Apr_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt+d20$Mar_20.pymt+d20$Apr_20.pymt)>=d20$TotalCredit,1,0)
d20$May_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt+d20$Mar_20.pymt+d20$Apr_20.pymt+d20$May_20.pymt)>=d20$TotalCredit,1,0)
d20$Jun_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt+d20$Mar_20.pymt+d20$Apr_20.pymt+d20$May_20.pymt+d20$Jun_20.pymt)>=d20$TotalCredit,1,0)
d20$July_fin <- ifelse((d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt+d20$Mar_20.pymt+d20$Apr_20.pymt+d20$May_20.pymt+d20$Jun_20.pymt+d20$Jul_20.pymt)>=d20$TotalCredit,1,0)

# stagnation--------------------------------------------------------
# on client level
d20$Aug_Stagn <- ifelse(d20$Aug_20.pymt==0 & d20$Aug_fin==0, 1,0)
d20$Sept_Stagn<- ifelse(d20$Sept_20.pymt==0 & d20$Sept_fin==0, 1,0)
d20$Oct_Stagn <- ifelse(d20$Oct_20.pymt==0 & d20$Oct_fin==0, 1,0)
d20$Nov_Stagn <- ifelse(d20$Nov_20.pymt==0 & d20$Nov_fin==0, 1,0)
d20$Dec_Stagn <- ifelse(d20$Dec_20.pymt==0 & d20$Dec_fin==0, 1,0)
d20$Jan_Stagn <- ifelse(d20$Jan_20.pymt==0 & d20$Jan_fin==0, 1,0)
d20$Feb_Stagn <- ifelse(d20$Feb_20.pymt==0 & d20$Feb_fin==0, 1,0)
d20$Mar_Stagn <- ifelse(d20$Mar_20.pymt==0 & d20$Mar_fin==0, 1,0)
d20$Apr_Stagn <- ifelse(d20$Apr_20.pymt==0 & d20$Apr_fin==0, 1,0)
d20$May_Stagn <- ifelse(d20$May_20.pymt==0 & d20$May_fin==0, 1,0)
d20$Jun_Stagn <- ifelse(d20$Jun_20.pymt==0 & d20$Jun_fin==0, 1,0)
d20$July_Stagn<- ifelse(d20$Jul_20.pymt==0 & d20$July_fin==0, 1,0)


#
table(d20$Feb_Stagn, useNA = "ifany")
sum(d20$TotalRepaid[d20$Feb_Stagn==1])
sum(d20$TotalCredit[d20$Feb_Stagn==1])
sum(d20$TotalRepaid[d20$Feb_Stagn==1])/sum(d20$TotalCredit[d20$Feb_Stagn==1])
sum(d20$TotalRepaid[d20$Feb_Stagn==0])/sum(d20$TotalCredit[d20$Feb_Stagn==0])

#--------------------------------------------------------------------------------------------------
# For 2019
#create each month's table to be able to allocate each transactions and put transactions on client level

names(v191)
Aug_19 <-subset(v191, v191$date <= "2018-08-31")
Aug_19.pymt <- Aug_19%>% group_by(GlobalClientID)%>% summarise(Aug_19.pymt=sum(Amount))

Sept_19 <-subset(v191, v191$date > "2018-08-31" & v191$date <= "2018-09-30")
Sept_19.pymt <- Sept_19%>% group_by(GlobalClientID)%>% summarise(Sept_19.pymt=sum(Amount))

Oct_19 <-subset(v191, v191$date > "2018-09-30" & v191$date <= "2018-10-31")
Oct_19.pymt <- Oct_19%>% group_by(GlobalClientID)%>% summarise(Oct_19.pymt=sum(Amount))

Nov_19 <-subset(v191, v191$date > "2018-10-31" & v191$date <= "2018-11-30")
Nov_19.pymt <- Nov_19%>% group_by(GlobalClientID)%>% summarise(Nov_19.pymt=sum(Amount))

Dec_19 <-subset(v191, v191$date > "2018-11-30" & v191$date <= "2018-12-31")
Dec_19.pymt <- Dec_19%>% group_by(GlobalClientID)%>% summarise(Dec_19.pymt=sum(Amount))

Jan_19 <-subset(v191, v191$date > "2018-12-31" & v191$date <= "2019-01-31")
Jan_19.pymt <- Jan_19%>% group_by(GlobalClientID)%>% summarise(Jan_19.pymt=sum(Amount))

Feb_19 <-subset(v191, v191$date > "2019-01-31" & v191$date <= "2019-02-28")
Feb_19.pymt <-Feb_19%>% group_by(GlobalClientID)%>% summarise(Feb_19.pymt=sum(Amount))

Mar_19 <-subset(v191, v191$date > "2019-02-28" & v191$date <= "2019-03-31")
Mar_19.pymt <- Mar_19%>% group_by(GlobalClientID)%>% summarise(Mar_19.pymt=sum(Amount))

Apr_19 <-subset(v191, v191$date > "2019-03-31" & v191$date <= "2019-04-30")
Apr_19.pymt <- Apr_19%>% group_by(GlobalClientID)%>% summarise(Apr_19.pymt=sum(Amount))

May_19 <-subset(v191, v191$date > "2019-04-30" & v191$date <= "2019-05-31")
May_19.pymt <- May_19%>% group_by(GlobalClientID)%>% summarise(May_19.pymt=sum(Amount))

Jun_19 <-subset(v191, v191$date > "2019-05-31" & v191$date <= "2019-06-30")
Jun_19.pymt <- Jun_19%>% group_by(GlobalClientID)%>% summarise(Jun_19.pymt=sum(Amount))

Jul_19 <-subset(v191, v191$date > "2019-06-30" & v191$date <= "2019-07-31")
Jul_19.pymt <- Jul_19%>% group_by(GlobalClientID)%>% summarise(Jul_19.pymt=sum(Amount))

# #create columns in the SC for each month and put inside amount paid by each farmer
names(Aug_19.pymt)
names(d19)
d19$Aug_19.pymt <- Aug_19.pymt$Aug_19.pymt[match(d19$GlobalClientID,Aug_19.pymt$GlobalClientID)]
d19$Sept_19.pymt <- Sept_19.pymt$Sept_19.pymt[match(d19$GlobalClientID,Sept_19.pymt$GlobalClientID)]
d19$Oct_19.pymt <- Octor_19.pymt$Octor_19.pymt[match(d19$GlobalClientID,Octor_19.pymt$GlobalClientID)]
d19$Nov_19.pymt <- Nov_19.pymt$Nov_19.pymt[match(d19$GlobalClientID,Nov_19.pymt$GlobalClientID)]
d19$Dec_19.pymt <- Dec_19.pymt$Dec_19.pymt[match(d19$GlobalClientID,Dec_19.pymt$GlobalClientID)]
d19$Jan_19.pymt <- Jan_19.pymt$Jan_19.pymt[match(d19$GlobalClientID,Jan_19.pymt$GlobalClientID)]
d19$Feb_19.pymt <- Feb_19.pymt$Feb_19.pymt[match(d19$GlobalClientID,Feb_19.pymt$GlobalClientID)]
d19$Mar_19.pymt <- Mar_19.pymt$Mar_19.pymt[match(d19$GlobalClientID,Mar_19.pymt$GlobalClientID)]
d19$Apr_19.pymt <- Apr_19.pymt$Apr_19.pymt[match(d19$GlobalClientID,Apr_19.pymt$GlobalClientID)]
d19$May_19.pymt <- May_19.pymt$May_19.pymt[match(d19$GlobalClientID,May_19.pymt$GlobalClientID)]
d19$Jun_19.pymt <- Jun_19.pymt$Jun_19.pymt[match(d19$GlobalClientID,Jun_19.pymt$GlobalClientID)]
d19$Jul_19.pymt <- Jul_19.pymt$Jul_19.pymt[match(d19$GlobalClientID,Jul_19.pymt$GlobalClientID)]

d19[is.na(d19)] <- 0
#Check if it works
names(d19)
d19$clt.made.paymt <- ifelse((d19$Aug_19.pymt >0
                              |d19$Sept_19.pymt >0
                              |d19$Oct_19.pymt >0
                              |d19$Nov_19.pymt >0
                              |d19$Dec_19.pymt >0
                              |d19$Jan_19.pymt >0                     
                              |d19$Feb_19.pymt >0                 
                              |d19$Mar_19.pymt >0                 
                              |d19$Apr_19.pymt >0                 
                              |d19$May_19.pymt >0                 
                              |d19$Jun_19.pymt >0                 
                              |d19$Jul_19.pymt >0),1,0)
table(d19$clt.made.paymt, useNA = "ifany")

#let's now calculate if a farmer has finished his/her credit

d19$Aug_fin <- ifelse(d19$Aug_19.pymt>=d19$X2019A_CycleCredit & d19$X2019A_CycleCredit>0,1,0)
d19$Sept_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt)>=d19$X2019A_CycleCredit & d19$X2019A_CycleCredit>0,1,0)
d19$Oct_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt)>=d19$X2019A_CycleCredit & d19$X2019A_CycleCredit>0,1,0)
d19$Nov_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt)>=d19$X2019A_CycleCredit & d19$X2019A_CycleCredit>0,1,0)
d19$Dec_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt)>=d19$X2019A_CycleCredit & d19$X2019A_CycleCredit>0,1,0)
d19$Jan_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt)>=d19$TotalCredit,1,0)
d19$Feb_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt)>=d19$TotalCredit,1,0)
d19$Mar_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt+d19$Mar_19.pymt)>=d19$TotalCredit,1,0)
d19$Apr_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt+d19$Mar_19.pymt+d19$Apr_19.pymt)>=d19$TotalCredit,1,0)
d19$May_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt+d19$Mar_19.pymt+d19$Apr_19.pymt+d19$May_19.pymt)>=d19$TotalCredit,1,0)
d19$Jun_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt+d19$Mar_19.pymt+d19$Apr_19.pymt+d19$May_19.pymt+d19$Jun_19.pymt)>=d19$TotalCredit,1,0)
d19$July_fin <- ifelse((d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt+d19$Mar_19.pymt+d19$Apr_19.pymt+d19$May_19.pymt+d19$Jun_19.pymt+d19$Jul_19.pymt)>=d19$TotalCredit,1,0)

# stagnation--------------------------------------------------------
# on client level
d19$Aug_Stagn <- ifelse(d19$Aug_19.pymt==0 & d19$Aug_fin==0, 1,0)
d19$Sept_Stagn<- ifelse(d19$Sept_19.pymt==0 & d19$Sept_fin==0, 1,0)
d19$Oct_Stagn <- ifelse(d19$Oct_19.pymt==0 & d19$Oct_fin==0, 1,0)
d19$Nov_Stagn <- ifelse(d19$Nov_19.pymt==0 & d19$Nov_fin==0, 1,0)
d19$Dec_Stagn <- ifelse(d19$Dec_19.pymt==0 & d19$Dec_fin==0, 1,0)
d19$Jan_Stagn <- ifelse(d19$Jan_19.pymt==0 & d19$Jan_fin==0, 1,0)
d19$Feb_Stagn <- ifelse(d19$Feb_19.pymt==0 & d19$Feb_fin==0, 1,0)
d19$Mar_Stagn <- ifelse(d19$Mar_19.pymt==0 & d19$Mar_fin==0, 1,0)
d19$Apr_Stagn <- ifelse(d19$Apr_19.pymt==0 & d19$Apr_fin==0, 1,0)
d19$May_Stagn <- ifelse(d19$May_19.pymt==0 & d19$May_fin==0, 1,0)
d19$Jun_Stagn <- ifelse(d19$Jun_19.pymt==0 & d19$Jun_fin==0, 1,0)
d19$July_Stagn<- ifelse(d19$Jul_19.pymt==0 & d19$July_fin==0,1,0)


#check if it works
table(d19$Feb_Stagn, useNA = "ifany")
sum(d19$TotalRepaid[d19$Feb_Stagn==1])
sum(d19$TotalCredit[d19$Feb_Stagn==1])
sum(d19$TotalRepaid[d19$Feb_Stagn==1])/sum(d19$TotalCredit[d19$Feb_Stagn==1])
sum(d19$TotalRepaid[d19$Feb_Stagn==0])/sum(d19$TotalCredit[d19$Feb_Stagn==0])

#---------------------------------------------------------------------------------------------------
# For 2018
#create each month's table to be able to allocate each transactions and put transactions on client level
names(v181)
Aug_18 <-subset(v181, v181$date <= "2017-08-31")
Aug_18.pymt <- Aug_18%>% group_by(GlobalClientID)%>% summarise(Aug_18.pymt=sum(Amount))

Sept_18 <-subset(v181, v181$date > "2017-08-31" & v181$date <= "2017-09-30")
Sept_18.pymt <- Sept_18%>% group_by(GlobalClientID)%>% summarise(Sept_18.pymt=sum(Amount))

Oct_18 <-subset(v181, v181$date > "2017-09-30" & v181$date <= "2017-10-31")
Oct_18.pymt <- Oct_18%>% group_by(GlobalClientID)%>% summarise(Oct_18.pymt=sum(Amount))

Nov_18 <-subset(v181, v181$date > "2017-10-31" & v181$date <= "2017-11-30")
Nov_18.pymt <- Nov_18%>% group_by(GlobalClientID)%>% summarise(Nov_18.pymt=sum(Amount))

Dec_18 <-subset(v181, v181$date > "2017-11-30" & v181$date <= "2017-12-31")
Dec_18.pymt <- Dec_18%>% group_by(GlobalClientID)%>% summarise(Dec_18.pymt=sum(Amount))

Jan_18 <-subset(v181, v181$date > "2017-12-31" & v181$date <= "2018-01-31")
Jan_18.pymt <- Jan_18%>% group_by(GlobalClientID)%>% summarise(Jan_18.pymt=sum(Amount))

Feb_18 <-subset(v181, v181$date > "2018-01-31" & v181$date <= "2018-02-28")
Feb_18.pymt <-Feb_18%>% group_by(GlobalClientID)%>% summarise(Feb_18.pymt=sum(Amount))

Mar_18 <-subset(v181, v181$date > "2018-02-28" & v181$date <= "2018-03-31")
Mar_18.pymt <- Mar_18%>% group_by(GlobalClientID)%>% summarise(Mar_18.pymt=sum(Amount))

Apr_18 <-subset(v181, v181$date > "2018-03-31" & v181$date <= "2018-04-30")
Apr_18.pymt <- Apr_18%>% group_by(GlobalClientID)%>% summarise(Apr_18.pymt=sum(Amount))

May_18 <-subset(v181, v181$date > "2018-04-30" & v181$date <= "2018-05-31")
May_18.pymt <- May_18%>% group_by(GlobalClientID)%>% summarise(May_18.pymt=sum(Amount))

Jun_18 <-subset(v181, v181$date > "2018-05-31" & v181$date <= "2018-06-30")
Jun_18.pymt <- Jun_18%>% group_by(GlobalClientID)%>% summarise(Jun_18.pymt=sum(Amount))

Jul_18 <-subset(v181, v181$date > "2018-06-30" & v181$date <= "2018-07-31")
Jul_18.pymt <- Jul_18%>% group_by(GlobalClientID)%>% summarise(Jul_18.pymt=sum(Amount))

#create columns in the SC for each month and put inside amount paid by each farmer

d18$Aug_18.pymt <- Aug_18.pymt$Aug_18.pymt[match(d18$GlobalClientID,Aug_18.pymt$GlobalClientID)]
d18$Sept_18.pymt <- Sept_18.pymt$Sept_18.pymt[match(d18$GlobalClientID,Sept_18.pymt$GlobalClientID)]
d18$Oct_18.pymt <- Oct_18.pymt$Oct_18.pymt[match(d18$GlobalClientID,Oct_18.pymt$GlobalClientID)]
d18$Nov_18.pymt <- Nov_18.pymt$Nov_18.pymt[match(d18$GlobalClientID,Nov_18.pymt$GlobalClientID)]
d18$Dec_18.pymt <- Dec_18.pymt$Dec_18.pymt[match(d18$GlobalClientID,Dec_18.pymt$GlobalClientID)]
d18$Jan_18.pymt <- Jan_18.pymt$Jan_18.pymt[match(d18$GlobalClientID,Jan_18.pymt$GlobalClientID)]
d18$Feb_18.pymt <- Feb_18.pymt$Feb_18.pymt[match(d18$GlobalClientID,Feb_18.pymt$GlobalClientID)]
d18$Mar_18.pymt <- Mar_18.pymt$Mar_18.pymt[match(d18$GlobalClientID,Mar_18.pymt$GlobalClientID)]
d18$Apr_18.pymt <- Apr_18.pymt$Apr_18.pymt[match(d18$GlobalClientID,Apr_18.pymt$GlobalClientID)]
d18$May_18.pymt <- May_18.pymt$May_18.pymt[match(d18$GlobalClientID,May_18.pymt$GlobalClientID)]
d18$Jun_18.pymt <- Jun_18.pymt$Jun_18.pymt[match(d18$GlobalClientID,Jun_18.pymt$GlobalClientID)]
d18$Jul_18.pymt <- Jul_18.pymt$Jul_18.pymt[match(d18$GlobalClientID,Jul_18.pymt$GlobalClientID)]

d18[is.na(d18)] <- 0
#Check if it works
names(d18)
d18$clt.made.paymt <- ifelse((d18$Aug_18.pymt >0
                              |d18$Sept_18.pymt >0
                              |d18$Oct_18.pymt >0
                              |d18$Nov_18.pymt >0
                              |d18$Dec_18.pymt >0
                              |d18$Jan_18.pymt >0                     
                              |d18$Feb_18.pymt >0                 
                              |d18$Mar_18.pymt >0                 
                              |d18$Apr_18.pymt >0                 
                              |d18$May_18.pymt >0                 
                              |d18$Jun_18.pymt >0                 
                              |d18$Jul_18.pymt >0),1,0)
table(d18$clt.made.paymt,useNA = "ifany")

#let's now calculate if a farmer has finished his/her credit
d18$Aug_fin <- ifelse(d18$Aug_18.pymt>=d18$X2018A_CycleCredit & d18$X2018A_CycleCredit>0,1,0)
d18$Sept_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt)>=d18$X2018A_CycleCredit & d18$X2018A_CycleCredit>0,1,0)
d18$Oct_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt)>=d18$X2018A_CycleCredit & d18$X2018A_CycleCredit>0,1,0)
d18$Nov_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt)>=d18$X2018A_CycleCredit & d18$X2018A_CycleCredit>0,1,0)
d18$Dec_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt)>=d18$X2018A_CycleCredit & d18$X2018A_CycleCredit>0,1,0)
d18$Jan_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt)>=d18$TotalCredit,1,0)
d18$Feb_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt)>=d18$TotalCredit,1,0)
d18$Mar_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt+d18$Mar_18.pymt)>=d18$TotalCredit,1,0)
d18$Apr_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt+d18$Mar_18.pymt+d18$Apr_18.pymt)>=d18$TotalCredit,1,0)
d18$May_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt+d18$Mar_18.pymt+d18$Apr_18.pymt+d18$May_18.pymt)>=d18$TotalCredit,1,0)
d18$Jun_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt+d18$Mar_18.pymt+d18$Apr_18.pymt+d18$May_18.pymt+d18$Jun_18.pymt)>=d18$TotalCredit,1,0)
d18$July_fin <- ifelse((d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt+d18$Mar_18.pymt+d18$Apr_18.pymt+d18$May_18.pymt+d18$Jun_18.pymt+d18$Jul_18.pymt)>=d18$TotalCredit,1,0)

# stagnation--------------------------------------------------------
# on client level
d18$Aug_Stagn <- ifelse(d18$Aug_18.pymt==0 & d18$Aug_fin==0, 1,0)
d18$Sept_Stagn<- ifelse(d18$Sept_18.pymt==0 & d18$Sept_fin==0, 1,0)
d18$Oct_Stagn <- ifelse(d18$Oct_18.pymt==0 & d18$Oct_fin==0, 1,0)
d18$Nov_Stagn <- ifelse(d18$Nov_18.pymt==0 & d18$Nov_fin==0, 1,0)
d18$Dec_Stagn <- ifelse(d18$Dec_18.pymt==0 & d18$Dec_fin==0, 1,0)
d18$Jan_Stagn <- ifelse(d18$Jan_18.pymt==0 & d18$Jan_fin==0, 1,0)
d18$Feb_Stagn <- ifelse(d18$Feb_18.pymt==0 & d18$Feb_fin==0, 1,0)
d18$Mar_Stagn <- ifelse(d18$Mar_18.pymt==0 & d18$Mar_fin==0, 1,0)
d18$Apr_Stagn <- ifelse(d18$Apr_18.pymt==0 & d18$Apr_fin==0, 1,0)
d18$May_Stagn <- ifelse(d18$May_18.pymt==0 & d18$May_fin==0, 1,0)
d18$Jun_Stagn <- ifelse(d18$Jun_18.pymt==0 & d18$Jun_fin==0, 1,0)
d18$July_Stagn<- ifelse(d18$Jul_18.pymt==0 & d18$July_fin==0, 1,0)

#check if it works
table(d18$Feb_Stagn, useNA = "ifany")
sum(d18$TotalRepaid[d18$Feb_Stagn==1])
sum(d18$TotalCredit[d18$Feb_Stagn==1])
sum(d18$TotalRepaid[d18$Feb_Stagn==1])/sum(d18$TotalCredit[d18$Feb_Stagn==1])
sum(d18$TotalRepaid[d18$Feb_Stagn==0])/sum(d18$TotalCredit[d18$Feb_Stagn==0])

#-------------------------------------------------------------------------------------------------
# For 2017
names(v171)
#create each month's table to be able to allocate each transactions and put transactions on client level
Aug_17 <-subset(v171, v171$date <= "2016-08-31")
Aug_17.pymt <- Aug_17%>% group_by(GlobalClientID)%>% summarise(Aug_17.pymt=sum(Amount))

Sept_17 <-subset(v171, v171$date > "2016-08-31" & v171$date <= "2016-09-30")
Sept_17.pymt <- Sept_17%>% group_by(GlobalClientID)%>% summarise(Sept_17.pymt=sum(Amount))

Oct_17 <-subset(v171, v171$date > "2016-09-30" & v171$date <= "2016-10-31")
Oct_17.pymt <- Oct_17%>% group_by(GlobalClientID)%>% summarise(Oct_17.pymt=sum(Amount))

Nov_17 <-subset(v171, v171$date > "2016-10-31" & v171$date <= "2016-11-30")
Nov_17.pymt <- Nov_17%>% group_by(GlobalClientID)%>% summarise(Nov_17.pymt=sum(Amount))

Dec_17 <-subset(v171, v171$date > "2016-11-30" & v171$date <= "2016-12-31")
Dec_17.pymt <- Dec_17%>% group_by(GlobalClientID)%>% summarise(Dec_17.pymt=sum(Amount))

Jan_17 <-subset(v171, v171$date > "2016-12-31" & v171$date <= "2017-01-31")
Jan_17.pymt <- Jan_17%>% group_by(GlobalClientID)%>% summarise(Jan_17.pymt=sum(Amount))

Feb_17 <-subset(v171, v171$date > "2017-01-31" & v171$date <= "2017-02-28")
Feb_17.pymt <-Feb_17%>% group_by(GlobalClientID)%>% summarise(Feb_17.pymt=sum(Amount))

Mar_17 <-subset(v171, v171$date > "2017-02-28" & v171$date <= "2017-03-31")
Mar_17.pymt <- Mar_17%>% group_by(GlobalClientID)%>% summarise(Mar_17.pymt=sum(Amount))

Apr_17 <-subset(v171, v171$date > "2017-03-31" & v171$date <= "2017-04-30")
Apr_17.pymt <- Apr_17%>% group_by(GlobalClientID)%>% summarise(Apr_17.pymt=sum(Amount))

May_17 <-subset(v171, v171$date > "2017-04-30" & v171$date <= "2017-05-31")
May_17.pymt <- May_17%>% group_by(GlobalClientID)%>% summarise(May_17.pymt=sum(Amount))

Jun_17 <-subset(v171, v171$date > "2017-05-31" & v171$date <= "2017-06-30")
Jun_17.pymt <- Jun_17%>% group_by(GlobalClientID)%>% summarise(Jun_17.pymt=sum(Amount))

Jul_17 <-subset(v171, v171$date > "2017-06-30" & v171$date <= "2017-07-31")
Jul_17.pymt <- Jul_17%>% group_by(GlobalClientID)%>% summarise(Jul_17.pymt=sum(Amount))

#create each month's table to be able to allocate each transactions and put transactions on client level
d17$Aug_17.pymt <- Aug_17.pymt$Aug_17.pymt[match(d17$GlobalClientID,Aug_17.pymt$GlobalClientID)]
d17$Sept_17.pymt <- Sept_17.pymt$Sept_17.pymt[match(d17$GlobalClientID,Sept_17.pymt$GlobalClientID)]
d17$Oct_17.pymt <- Oct_17.pymt$Oct_17.pymt[match(d17$GlobalClientID,Oct_17.pymt$GlobalClientID)]
d17$Nov_17.pymt <- Nov_17.pymt$Nov_17.pymt[match(d17$GlobalClientID,Nov_17.pymt$GlobalClientID)]
d17$Dec_17.pymt <- Dec_17.pymt$Dec_17.pymt[match(d17$GlobalClientID,Dec_17.pymt$GlobalClientID)]
d17$Jan_17.pymt <- Jan_17.pymt$Jan_17.pymt[match(d17$GlobalClientID,Jan_17.pymt$GlobalClientID)]
d17$Feb_17.pymt <- Feb_17.pymt$Feb_17.pymt[match(d17$GlobalClientID,Feb_17.pymt$GlobalClientID)]
d17$Mar_17.pymt <- Mar_17.pymt$Mar_17.pymt[match(d17$GlobalClientID,Mar_17.pymt$GlobalClientID)]
d17$Apr_17.pymt <- Apr_17.pymt$Apr_17.pymt[match(d17$GlobalClientID,Apr_17.pymt$GlobalClientID)]
d17$May_17.pymt <- May_17.pymt$May_17.pymt[match(d17$GlobalClientID,May_17.pymt$GlobalClientID)]
d17$Jun_17.pymt <- Jun_17.pymt$Jun_17.pymt[match(d17$GlobalClientID,Jun_17.pymt$GlobalClientID)]
d17$Jul_17.pymt <- Jul_17.pymt$Jul_17.pymt[match(d17$GlobalClientID,Jul_17.pymt$GlobalClientID)]

d17[is.na(d17)] <- 0
#Check if it works
names(d17)
d17$clt.made.paymt <- ifelse((d17$Aug_17.pymt >0
                              |d17$Sept_17.pymt >0
                              |d17$Oct_17.pymt >0
                              |d17$Nov_17.pymt >0
                              |d17$Dec_17.pymt >0
                              |d17$Jan_17.pymt >0                     
                              |d17$Feb_17.pymt >0                 
                              |d17$Mar_17.pymt >0                 
                              |d17$Apr_17.pymt >0                 
                              |d17$May_17.pymt >0                 
                              |d17$Jun_17.pymt >0                 
                              |d17$Jul_17.pymt >0),1,0)
table(d17$clt.made.paymt,useNA = "ifany")
#-------------------------------------------------------------------------------------------------
#let's now calculate if a farmer has finished his/her credit
d17$Aug_fin <- ifelse(d17$Aug_17.pymt>=d17$X2017A_CycleCredit & d17$X2017A_CycleCredit>0,1,0)
d17$Sept_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt)>=d17$X2017A_CycleCredit & d17$X2017A_CycleCredit>0,1,0)
d17$Oct_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt)>=d17$X2017A_CycleCredit & d17$X2017A_CycleCredit>0,1,0)
d17$Nov_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt)>=d17$X2017A_CycleCredit & d17$X2017A_CycleCredit>0,1,0)
d17$Dec_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt)>=d17$X2017A_CycleCredit & d17$X2017A_CycleCredit>0,1,0)
d17$Jan_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt)>=d17$TotalCredit,1,0)
d17$Feb_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt)>=d17$TotalCredit,1,0)
d17$Mar_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt+d17$Mar_17.pymt)>=d17$TotalCredit,1,0)
d17$Apr_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt+d17$Mar_17.pymt+d17$Apr_17.pymt)>=d17$TotalCredit,1,0)
d17$May_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt+d17$Mar_17.pymt+d17$Apr_17.pymt+d17$May_17.pymt)>=d17$TotalCredit,1,0)
d17$Jun_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt+d17$Mar_17.pymt+d17$Apr_17.pymt+d17$May_17.pymt+d17$Jun_17.pymt)>=d17$TotalCredit,1,0)
d17$July_fin <- ifelse((d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt+d17$Mar_17.pymt+d17$Apr_17.pymt+d17$May_17.pymt+d17$Jun_17.pymt+d17$Jul_17.pymt)>=d17$TotalCredit,1,0)

# stagnation--------------------------------------------------------
# on client level
d17$Aug_Stagn <- ifelse(d17$Aug_17.pymt==0 & d17$Aug_fin==0, 1,0)
d17$Sept_Stagn<- ifelse(d17$Sept_17.pymt==0 & d17$Sept_fin==0, 1,0)
d17$Oct_Stagn <- ifelse(d17$Oct_17.pymt==0 & d17$Oct_fin==0, 1,0)
d17$Nov_Stagn <- ifelse(d17$Nov_17.pymt==0 & d17$Nov_fin==0, 1,0)
d17$Dec_Stagn <- ifelse(d17$Dec_17.pymt==0 & d17$Dec_fin==0, 1,0)
d17$Jan_Stagn <- ifelse(d17$Jan_17.pymt==0 & d17$Jan_fin==0, 1,0)
d17$Feb_Stagn <- ifelse(d17$Feb_17.pymt==0 & d17$Feb_fin==0, 1,0)
d17$Mar_Stagn <- ifelse(d17$Mar_17.pymt==0 & d17$Mar_fin==0, 1,0)
d17$Apr_Stagn <- ifelse(d17$Apr_17.pymt==0 & d17$Apr_fin==0, 1,0)
d17$May_Stagn <- ifelse(d17$May_17.pymt==0 & d17$May_fin==0, 1,0)
d17$Jun_Stagn <- ifelse(d17$Jun_17.pymt==0 & d17$Jun_fin==0, 1,0)
d17$July_Stagn<- ifelse(d17$Jul_17.pymt==0 & d17$July_fin==0, 1,0)

#check if it works
table(d17$Feb_Stagn, useNA = "ifany")
sum(d17$TotalRepaid[d17$Feb_Stagn==1])
sum(d17$TotalCredit[d17$Feb_Stagn==1])
sum(d17$TotalRepaid[d17$Feb_Stagn==1])/sum(d17$TotalCredit[d17$Feb_Stagn==1])
sum(d17$TotalRepaid[d17$Feb_Stagn==0])/sum(d17$TotalCredit[d17$Feb_Stagn==0])
#----------------------------------------------------------------------------------------------------#
# focus only A clients
#2020
names(d20)
length(unique(d20$GlobalClientID[d20$X2020A_CycleCredit>0 & d20$X2020B_CycleCredit==0]))# A-only clients 141,093
length(unique(d20$GlobalClientID[d20$X2020A_CycleCredit==0 & d20$X2020B_CycleCredit>0]))# B-only clients 69,672
length(unique(d20$GlobalClientID[d20$X2020A_CycleCredit>0 & d20$X2020B_CycleCredit>0]))#AB clients 253,545  

# A-only stagnanted clients
length(unique(d20$GlobalClientID[d20$X2020A_CycleCredit>0 & d20$X2020B_CycleCredit==0 & d20$Feb_Stagn==1]))
length(unique(d19$GlobalClientID[d19$X2019A_CycleCredit>0 & d19$X2019B_CycleCredit==0 & d19$Feb_Stagn==1]))
length(unique(d18$GlobalClientID[d18$X2018A_CycleCredit>0 & d18$X2018B_CycleCredit==0 & d18$Feb_Stagn==1]))
length(unique(d17$GlobalClientID[d17$X2017A_CycleCredit>0 & d17$X2017B_CycleCredit==0 & d17$Feb_Stagn==1]))

#A-only non-stagnanted clients
length(unique(d20$GlobalClientID[d20$X2020A_CycleCredit>0 & d20$X2020B_CycleCredit==0 & d20$Feb_Stagn==0]))
length(unique(d19$GlobalClientID[d19$X2019A_CycleCredit>0 & d19$X2019B_CycleCredit==0 & d19$Feb_Stagn==0]))
length(unique(d18$GlobalClientID[d18$X2018A_CycleCredit>0 & d18$X2018B_CycleCredit==0 & d18$Feb_Stagn==0]))
length(unique(d17$GlobalClientID[d17$X2017A_CycleCredit>0 & d17$X2017B_CycleCredit==0 & d17$Feb_Stagn==0]))

# A-only stagnanted clients reached 100 by July
d20$pymt.by.July_20 <- d20$Aug_20.pymt + d20$Sept_20.pymt + d20$Oct_20.pymt +d20$Nov_20.pymt +d20$Dec_20.pymt+d20$Jan_20.pymt +d20$Feb_20.pymt+d20$Mar_20.pymt+d20$Apr_20.pymt+d20$May_20.pymt+d20$Jun_20.pymt+d20$Jul_20.pymt
d19$pymt.by.July_19 <- d19$Aug_19.pymt + d19$Sept_19.pymt + d19$Oct_19.pymt +d19$Nov_19.pymt +d19$Dec_19.pymt+d19$Jan_19.pymt +d19$Feb_19.pymt+d19$Mar_19.pymt+d19$Apr_19.pymt+d19$May_19.pymt+d19$Jun_19.pymt+d19$Jul_19.pymt
d18$pymt.by.July_18 <- d18$Aug_18.pymt + d18$Sept_18.pymt + d18$Oct_18.pymt +d18$Nov_18.pymt +d18$Dec_18.pymt+d18$Jan_18.pymt +d18$Feb_18.pymt+d18$Mar_18.pymt+d18$Apr_18.pymt+d18$May_18.pymt+d18$Jun_18.pymt+d18$Jul_18.pymt
d17$pymt.by.July_17 <- d17$Aug_17.pymt + d17$Sept_17.pymt + d17$Oct_17.pymt +d17$Nov_17.pymt +d17$Dec_17.pymt+d17$Jan_17.pymt +d17$Feb_17.pymt+d17$Mar_17.pymt+d17$Apr_17.pymt+d17$May_17.pymt+d17$Jun_17.pymt+d17$Jul_17.pymt

length(unique(d20$GlobalClientID[d20$Feb_Stagn==1 & d20$X2020A_CycleCredit>0 & d20$X2020B_CycleCredit==0 & d20$pymt.by.July_20 >=d20$TotalCredit]))
length(unique(d19$GlobalClientID[d19$Feb_Stagn==1 & d19$X2019A_CycleCredit>0 & d19$X2019B_CycleCredit==0 & d19$pymt.by.July_19 >=d19$TotalCredit]))
length(unique(d18$GlobalClientID[d18$Feb_Stagn==1 & d18$X2018A_CycleCredit>0 & d18$X2018B_CycleCredit==0 & d18$pymt.by.July_18 >=d18$TotalCredit]))
length(unique(d17$GlobalClientID[d17$Feb_Stagn==1 & d17$X2017A_CycleCredit>0 & d17$X2017B_CycleCredit==0 & d17$pymt.by.July_17 >=d17$TotalCredit]))

# A-only non-stagnanted clients reached 100 by July
length(unique(d20$GlobalClientID[d20$Feb_Stagn==0 & d20$X2020A_CycleCredit>0 & d20$X2020B_CycleCredit==0 & d20$pymt.by.July_20 >=d20$TotalCredit]))
length(unique(d19$GlobalClientID[d19$Feb_Stagn==0 & d19$X2019A_CycleCredit>0 & d19$X2019B_CycleCredit==0 & d19$pymt.by.July_19 >=d19$TotalCredit]))
length(unique(d18$GlobalClientID[d18$Feb_Stagn==0 & d18$X2018A_CycleCredit>0 & d18$X2018B_CycleCredit==0 & d18$pymt.by.July_18 >=d18$TotalCredit]))
length(unique(d17$GlobalClientID[d17$Feb_Stagn==0 & d17$X2017A_CycleCredit>0 & d17$X2017B_CycleCredit==0 & d17$pymt.by.July_17 >=d17$TotalCredit]))




table(d20$clt.made.paymt,useNA = "ifany")
table(d19$clt.made.paymt,useNA = "ifany")
table(d18$clt.made.paymt,useNA = "ifany")
table(d17$clt.made.paymt,useNA = "ifany")

d201 <- d20%>% group_by(DistrictName)%>% summarise(
  tot.clt = length(unique(GlobalClientID)),
  clt.made.paymt.Aug = length(unique(GlobalClientID[Aug_20.pymt>0])),
  clt.made.paymt.Sept = length(unique(GlobalClientID[Sept_20.pymt>0])),
  clt.made.paymt.Octor = length(unique(GlobalClientID[Octor_20.pymt>0])),
  clt.made.paymt.Nov = length(unique(GlobalClientID[Nov_20.pymt>0])),
  clt.made.paymt.Dec = length(unique(GlobalClientID[Dec_20.pymt>0])),
  clt.made.paymt.Jan = length(unique(GlobalClientID[Jan_20.pymt>0])),
  clt.made.paymt.Feb = length(unique(GlobalClientID[Feb_20.pymt>0])),
  clt.made.paymt.Mar = length(unique(GlobalClientID[Mar_20.pymt>0])),
  clt.made.paymt.Apr = length(unique(GlobalClientID[Apr_20.pymt>0])),
  clt.made.paymt.May = length(unique(GlobalClientID[May_20.pymt>0])),
  clt.made.paymt.Jun = length(unique(GlobalClientID[Jun_20.pymt>0])),
  clt.made.paymt.Jul = length(unique(GlobalClientID[Jul_20.pymt>0])))
View(d201)


d201_rw <- d20%>% group_by()%>% summarise(
  tot.clt = length(unique(GlobalClientID)),
  clt.made.paymt.Aug = length(unique(GlobalClientID[Aug_20.pymt>0])),
  clt.made.paymt.Sept = length(unique(GlobalClientID[Sept_20.pymt>0])),
  clt.made.paymt.Octor = length(unique(GlobalClientID[Octor_20.pymt>0])),
  clt.made.paymt.Nov = length(unique(GlobalClientID[Nov_20.pymt>0])),
  clt.made.paymt.Dec = length(unique(GlobalClientID[Dec_20.pymt>0])),
  clt.made.paymt.Jan = length(unique(GlobalClientID[Jan_20.pymt>0])),
  clt.made.paymt.Feb = length(unique(GlobalClientID[Feb_20.pymt>0])),
  clt.made.paymt.Mar = length(unique(GlobalClientID[Mar_20.pymt>0])),
  clt.made.paymt.Apr = length(unique(GlobalClientID[Apr_20.pymt>0])),
  clt.made.paymt.May = length(unique(GlobalClientID[May_20.pymt>0])),
  clt.made.paymt.Jun = length(unique(GlobalClientID[Jun_20.pymt>0])),
  clt.made.paymt.Jul = length(unique(GlobalClientID[Jul_20.pymt>0])))

View(d201_rw)

d191 <- d19%>% group_by(DistrictName)%>% summarise(
  tot.clt = length(unique(GlobalClientId)),
  clt.made.paymt.Aug = length(unique(GlobalClientId[Aug_19.pymt>0])),
  clt.made.paymt.Sept = length(unique(GlobalClientId[Sept_19.pymt>0])),
  clt.made.paymt.Octor = length(unique(GlobalClientId[Octor_19.pymt>0])),
  clt.made.paymt.Nov = length(unique(GlobalClientId[Nov_19.pymt>0])),
  clt.made.paymt.Dec = length(unique(GlobalClientId[Dec_19.pymt>0])),
  clt.made.paymt.Jan = length(unique(GlobalClientId[Jan_19.pymt>0])),
  clt.made.paymt.Feb = length(unique(GlobalClientId[Feb_19.pymt>0])),
  clt.made.paymt.Mar = length(unique(GlobalClientId[Mar_19.pymt>0])),
  clt.made.paymt.Apr = length(unique(GlobalClientId[Apr_19.pymt>0])),
  clt.made.paymt.May = length(unique(GlobalClientId[May_19.pymt>0])),
  clt.made.paymt.Jun = length(unique(GlobalClientId[Jun_19.pymt>0])),
  clt.made.paymt.Jul = length(unique(GlobalClientId[Jul_19.pymt>0])))
View(d191)


d191_rw <- d19%>% group_by()%>% summarise(
  tot.clt = length(unique(GlobalClientId)),
  clt.made.paymt.Aug = length(unique(GlobalClientId[Aug_19.pymt>0])),
  clt.made.paymt.Sept = length(unique(GlobalClientId[Sept_19.pymt>0])),
  clt.made.paymt.Octor = length(unique(GlobalClientId[Octor_19.pymt>0])),
  clt.made.paymt.Nov = length(unique(GlobalClientId[Nov_19.pymt>0])),
  clt.made.paymt.Dec = length(unique(GlobalClientId[Dec_19.pymt>0])),
  clt.made.paymt.Jan = length(unique(GlobalClientId[Jan_19.pymt>0])),
  clt.made.paymt.Feb = length(unique(GlobalClientId[Feb_19.pymt>0])),
  clt.made.paymt.Mar = length(unique(GlobalClientId[Mar_19.pymt>0])),
  clt.made.paymt.Apr = length(unique(GlobalClientId[Apr_19.pymt>0])),
  clt.made.paymt.May = length(unique(GlobalClientId[May_19.pymt>0])),
  clt.made.paymt.Jun = length(unique(GlobalClientId[Jun_19.pymt>0])),
  clt.made.paymt.Jul = length(unique(GlobalClientId[Jul_19.pymt>0])))

View(d191_rw)

d181 <- d18%>% group_by(DistrictName)%>% summarise(
  tot.clt = length(unique(GlobalClientId)),
  clt.made.paymt.Aug = length(unique(GlobalClientId[Aug_18.pymt>0])),
  clt.made.paymt.Sept = length(unique(GlobalClientId[Sept_18.pymt>0])),
  clt.made.paymt.Octor = length(unique(GlobalClientId[Octor_18.pymt>0])),
  clt.made.paymt.Nov = length(unique(GlobalClientId[Nov_18.pymt>0])),
  clt.made.paymt.Dec = length(unique(GlobalClientId[Dec_18.pymt>0])),
  clt.made.paymt.Jan = length(unique(GlobalClientId[Jan_18.pymt>0])),
  clt.made.paymt.Feb = length(unique(GlobalClientId[Feb_18.pymt>0])),
  clt.made.paymt.Mar = length(unique(GlobalClientId[Mar_18.pymt>0])),
  clt.made.paymt.Apr = length(unique(GlobalClientId[Apr_18.pymt>0])),
  clt.made.paymt.May = length(unique(GlobalClientId[May_18.pymt>0])),
  clt.made.paymt.Jun = length(unique(GlobalClientId[Jun_18.pymt>0])),
  clt.made.paymt.Jul = length(unique(GlobalClientId[Jul_18.pymt>0])))
View(d181)


d181_rw <- d18%>% group_by()%>% summarise(
  tot.clt = length(unique(GlobalClientId)),
  clt.made.paymt.Aug = length(unique(GlobalClientId[Aug_18.pymt>0])),
  clt.made.paymt.Sept = length(unique(GlobalClientId[Sept_18.pymt>0])),
  clt.made.paymt.Octor = length(unique(GlobalClientId[Octor_18.pymt>0])),
  clt.made.paymt.Nov = length(unique(GlobalClientId[Nov_18.pymt>0])),
  clt.made.paymt.Dec = length(unique(GlobalClientId[Dec_18.pymt>0])),
  clt.made.paymt.Jan = length(unique(GlobalClientId[Jan_18.pymt>0])),
  clt.made.paymt.Feb = length(unique(GlobalClientId[Feb_18.pymt>0])),
  clt.made.paymt.Mar = length(unique(GlobalClientId[Mar_18.pymt>0])),
  clt.made.paymt.Apr = length(unique(GlobalClientId[Apr_18.pymt>0])),
  clt.made.paymt.May = length(unique(GlobalClientId[May_18.pymt>0])),
  clt.made.paymt.Jun = length(unique(GlobalClientId[Jun_18.pymt>0])),
  clt.made.paymt.Jul = length(unique(GlobalClientId[Jul_18.pymt>0])))

View(d181_rw)




